port module SyncManager.Update exposing (subscriptions, update)

import App.Model exposing (SubModelReturn)
import Device.Encoder
import Device.Model exposing (Device)
import Editable
import Error.Utils exposing (decodeError, maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withJsonBody, withQueryParams)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import List.Zipper as Zipper
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Decoder exposing (decodeDownloadSyncResponseAuthority, decodeDownloadSyncResponseGeneral)
import SyncManager.Encoder
import SyncManager.Model exposing (BackendAuthorityEntity(..), BackendGeneralEntity(..), DownloadPhotos(..), IndexDbQueryType(..), IndexDbQueryTypeResult(..), Model, Msg(..), SyncStatus(..), emptyRevisionIdPerAuthority)
import SyncManager.Utils exposing (getSyncSpeedForSubscriptions)
import Time
import Utils.WebData


update : NominalDate -> Int -> Device -> Msg -> Model -> SubModelReturn Model Msg
update currentDate dbVersion device msg model =
    let
        noChange =
            SubModelReturn model Cmd.none noError []

        returnDetermineSyncStatus =
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus model)
                Cmd.none
                noError
                []
    in
    case msg of
        BackendAuthorityFetch ->
            case model.syncStatus of
                SyncDownloadAuthority webData ->
                    if RemoteData.isLoading webData then
                        -- We are already loading.
                        noChange

                    else
                        case model.revisionIdPerAuthorityZipper of
                            Nothing ->
                                -- No zipper, means not subscribed yet to any
                                -- authority. `determineSyncStatus` will take care of
                                -- rotating if we're not on automatic sync.
                                returnDetermineSyncStatus

                            Just zipper ->
                                if RemoteData.isLoading webData then
                                    -- We are already loading.
                                    noChange

                                else
                                    let
                                        currentZipper =
                                            Zipper.current zipper

                                        cmd =
                                            HttpBuilder.get (device.backendUrl ++ "/api/sync/" ++ currentZipper.uuid)
                                                |> withQueryParams
                                                    [ ( "access_token", device.accessToken )
                                                    , ( "db_version", String.fromInt dbVersion )
                                                    , ( "base_revision", String.fromInt currentZipper.revisionId )
                                                    ]
                                                |> withExpectJson decodeDownloadSyncResponseAuthority
                                                |> HttpBuilder.send (RemoteData.fromResult >> BackendAuthorityFetchHandle zipper)
                                    in
                                    SubModelReturn
                                        { model | syncStatus = SyncDownloadAuthority RemoteData.Loading }
                                        cmd
                                        noError
                                        []

                _ ->
                    returnDetermineSyncStatus

        BackendAuthorityFetchHandle zipper webData ->
            let
                currentZipper =
                    Zipper.current zipper

                cmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                dataToSend =
                                    data.entities
                                        |> List.foldl
                                            (\entity accum -> SyncManager.Utils.getDataToSendAuthority entity accum)
                                            []
                                        |> List.reverse
                            in
                            sendSyncedDataToIndexDb { table = "Authority", data = dataToSend, shard = currentZipper.uuid }

                        Nothing ->
                            Cmd.none

                deferredPhotosCmd =
                    -- Prepare a list of the photos, so we could grab them in a later
                    -- time.
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                dataToSend =
                                    data.entities
                                        |> List.foldl
                                            (\entity accum ->
                                                case SyncManager.Utils.getPhotoFromBackendAuthorityEntity entity of
                                                    Just photoUrl ->
                                                        (entity
                                                            |> SyncManager.Utils.getBackendAuthorityEntityIdentifier
                                                            |> SyncManager.Encoder.encodeDataForDeferredPhotos photoUrl
                                                        )
                                                            :: accum

                                                    Nothing ->
                                                        accum
                                            )
                                            []
                                        |> List.reverse
                            in
                            if List.isEmpty dataToSend then
                                Cmd.none

                            else
                                sendSyncedDataToIndexDb { table = "DeferredPhotos", data = dataToSend, shard = currentZipper.uuid }

                        Nothing ->
                            Cmd.none

                lastFetchedRevisionId =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            -- Get the last item.
                            data.entities
                                |> List.reverse
                                |> List.head
                                |> Maybe.map
                                    (\entity ->
                                        SyncManager.Utils.getBackendAuthorityEntityIdentifier entity
                                            |> (\entityIdentifier -> entityIdentifier.revision)
                                    )
                                |> Maybe.withDefault currentZipper.revisionId

                        Nothing ->
                            currentZipper.revisionId

                zipperUpdated =
                    Zipper.mapCurrent (\old -> { old | revisionId = lastFetchedRevisionId }) zipper

                modelWithSyncStatus =
                    SyncManager.Utils.determineSyncStatus
                        { model
                            | syncStatus = SyncDownloadAuthority webData
                            , revisionIdPerAuthorityZipper = Just zipperUpdated
                        }
            in
            SubModelReturn
                modelWithSyncStatus
                (Cmd.batch
                    [ cmd
                    , deferredPhotosCmd

                    -- Send to JS the updated revision ID. We send the entire
                    -- list.
                    , sendRevisionIdPerAuthority (Zipper.toList zipperUpdated)
                    ]
                )
                (maybeHttpError webData "Backend.SyncManager.Update" "BackendAuthorityFetchHandle")
                []

        BackendFetchMain ->
            case model.syncStatus of
                SyncIdle ->
                    returnDetermineSyncStatus

                SyncUploadGeneral _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        FetchFromIndexDbUploadGeneral
                        model

                SyncUploadPhotoAuthority _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        BackendPhotoUploadAuthority
                        model

                SyncUploadAuthority _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        FetchFromIndexDbUploadAuthority
                        model

                SyncDownloadGeneral _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        BackendGeneralFetch
                        model

                SyncDownloadAuthority _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        BackendAuthorityFetch
                        model

                SyncDownloadPhotos _ ->
                    update
                        currentDate
                        dbVersion
                        device
                        FetchFromIndexDbDeferredPhoto
                        model

        RevisionIdAuthorityAdd uuid ->
            -- Add a new authority to Local storage.
            let
                uuidAsString =
                    fromEntityUuid uuid

                revisionIdPerAuthorityZipper =
                    case model.revisionIdPerAuthorityZipper of
                        Just zipper ->
                            zipper
                                |> Zipper.toList
                                -- Before adding, lets remove the same UUID, so in case it's already
                                -- we won't have duplicates.
                                |> List.filter (\row -> row.uuid /= uuidAsString)
                                |> (\list -> emptyRevisionIdPerAuthority uuidAsString :: list)
                                |> Zipper.fromList

                        Nothing ->
                            [ emptyRevisionIdPerAuthority uuidAsString ]
                                |> Zipper.fromList

                cmd =
                    case revisionIdPerAuthorityZipper of
                        Just zipper ->
                            sendRevisionIdPerAuthority (Zipper.toList zipper)

                        Nothing ->
                            Cmd.none
            in
            SubModelReturn
                { model | revisionIdPerAuthorityZipper = revisionIdPerAuthorityZipper }
                cmd
                noError
                []

        RevisionIdAuthorityRemove uuid ->
            -- Remove authority from Local storage.
            let
                uuidAsString =
                    fromEntityUuid uuid

                revisionIdPerAuthorityZipper =
                    case model.revisionIdPerAuthorityZipper of
                        Just zipper ->
                            zipper
                                |> Zipper.toList
                                |> List.filter (\row -> row.uuid /= uuidAsString)
                                |> Zipper.fromList

                        Nothing ->
                            -- We don't seem to have a zipper, so we cannot add
                            -- it.
                            Nothing

                cmd =
                    case revisionIdPerAuthorityZipper of
                        Just zipper ->
                            sendRevisionIdPerAuthority (Zipper.toList zipper)

                        Nothing ->
                            Cmd.none
            in
            SubModelReturn
                { model | revisionIdPerAuthorityZipper = revisionIdPerAuthorityZipper }
                cmd
                noError
                []

        BackendGeneralFetch ->
            case model.syncStatus of
                SyncDownloadGeneral webData ->
                    if RemoteData.isLoading webData then
                        -- We are already loading.
                        noChange

                    else
                        let
                            cmd =
                                HttpBuilder.get (device.backendUrl ++ "/api/sync")
                                    |> withQueryParams
                                        [ ( "access_token", device.accessToken )
                                        , ( "db_version", String.fromInt dbVersion )
                                        , ( "base_revision", String.fromInt model.lastFetchedRevisionIdGeneral )
                                        ]
                                    |> withExpectJson decodeDownloadSyncResponseGeneral
                                    |> HttpBuilder.send (RemoteData.fromResult >> BackendGeneralFetchHandle)
                        in
                        SubModelReturn
                            { model | syncStatus = SyncDownloadGeneral RemoteData.Loading }
                            cmd
                            noError
                            []

                _ ->
                    SubModelReturn
                        (SyncManager.Utils.determineSyncStatus model)
                        Cmd.none
                        noError
                        []

        BackendGeneralFetchHandle webData ->
            let
                cmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                dataToSend =
                                    data.entities
                                        |> List.foldl (\entity accum -> SyncManager.Utils.getDataToSendGeneral entity accum) []
                                        |> List.reverse
                            in
                            if List.isEmpty dataToSend then
                                Cmd.none

                            else
                                sendSyncedDataToIndexDb { table = "General", data = dataToSend, shard = "" }

                        Nothing ->
                            Cmd.none

                -- We have successfully downloaded the entities, so
                -- we can delete them fom the `nodeChanges` table.
                -- We will do it, by their localId.
                deleteLocalIdsCmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                localIds =
                                    List.map
                                        (\entity ->
                                            let
                                                identifier =
                                                    SyncManager.Utils.getBackendGeneralEntityIdentifier entity
                                            in
                                            identifier.uuid
                                        )
                                        data.entities
                            in
                            if List.isEmpty localIds then
                                Cmd.none

                            else
                                sendLocalIdsForDelete { type_ = "General", uuid = localIds }

                        _ ->
                            Cmd.none

                lastFetchedRevisionIdGeneral =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            -- Get the last item.
                            data.entities
                                |> List.reverse
                                |> List.head
                                |> Maybe.map
                                    (\entity ->
                                        let
                                            identifier =
                                                SyncManager.Utils.getBackendGeneralEntityIdentifier entity
                                        in
                                        identifier.revision
                                    )
                                |> Maybe.withDefault model.lastFetchedRevisionIdGeneral

                        Nothing ->
                            model.lastFetchedRevisionIdGeneral

                modelWithSyncStatus =
                    SyncManager.Utils.determineSyncStatus { model | syncStatus = SyncDownloadGeneral webData }
            in
            SubModelReturn
                { modelWithSyncStatus | lastFetchedRevisionIdGeneral = lastFetchedRevisionIdGeneral }
                (Cmd.batch
                    [ cmd
                    , deleteLocalIdsCmd
                    , sendLastFetchedRevisionIdGeneral lastFetchedRevisionIdGeneral
                    ]
                )
                (maybeHttpError webData "Backend.SyncManager.Update" "BackendGeneralFetchHandle")
                []

        SetLastFetchedRevisionIdAuthority zipper revisionId ->
            let
                zipperUpdated =
                    Zipper.mapCurrent (\old -> { old | revisionId = revisionId }) zipper
            in
            SubModelReturn
                { model | revisionIdPerAuthorityZipper = Just zipperUpdated }
                Cmd.none
                noError
                []

        SetLastFetchedRevisionIdGeneral revisionId ->
            SubModelReturn
                { model | lastFetchedRevisionIdGeneral = revisionId }
                Cmd.none
                noError
                []

        FetchFromIndexDbDeferredPhoto ->
            -- Get a deferred photo from IndexDB.
            case model.syncStatus of
                SyncDownloadPhotos DownloadPhotosNone ->
                    noChange

                SyncDownloadPhotos (DownloadPhotosBatch record) ->
                    if RemoteData.isLoading record.indexDbRemoteData || RemoteData.isLoading record.backendRemoteData then
                        -- We are already loading.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | indexDbRemoteData = RemoteData.Loading
                                    , backendRemoteData = RemoteData.NotAsked
                                }
                        in
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryDeferredPhoto)
                            { model | syncStatus = SyncDownloadPhotos (DownloadPhotosBatch recordUpdated) }

                SyncDownloadPhotos (DownloadPhotosAll record) ->
                    if RemoteData.isLoading record.indexDbRemoteData || RemoteData.isLoading record.backendRemoteData then
                        -- We are already loading.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | indexDbRemoteData = RemoteData.Loading
                                    , backendRemoteData = RemoteData.NotAsked
                                }
                        in
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryDeferredPhoto)
                            { model | syncStatus = SyncDownloadPhotos (DownloadPhotosAll recordUpdated) }

                _ ->
                    noChange

        FetchFromIndexDbUploadGeneral ->
            -- Get a entities for upload from IndexDB.
            case model.syncStatus of
                SyncUploadGeneral record ->
                    if RemoteData.isLoading record.indexDbRemoteData || RemoteData.isLoading record.backendRemoteData then
                        -- We are already loading.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | indexDbRemoteData = RemoteData.Loading
                                    , backendRemoteData = RemoteData.NotAsked
                                }
                        in
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadGeneral)
                            { model | syncStatus = SyncUploadGeneral recordUpdated }

                _ ->
                    noChange

        FetchFromIndexDbUploadAuthority ->
            -- Get a entities for upload from IndexDB.
            case model.syncStatus of
                SyncUploadAuthority record ->
                    if RemoteData.isLoading record.indexDbRemoteData || RemoteData.isLoading record.backendRemoteData then
                        -- We are already loading.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | indexDbRemoteData = RemoteData.Loading
                                    , backendRemoteData = RemoteData.NotAsked
                                }
                        in
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadAuthority)
                            { model | syncStatus = SyncUploadAuthority recordUpdated }

                _ ->
                    noChange

        BackendPhotoUploadAuthority ->
            case model.syncStatus of
                SyncUploadPhotoAuthority webData ->
                    if RemoteData.isLoading webData then
                        noChange

                    else
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadPhotoAuthority)
                            { model | syncStatus = SyncUploadPhotoAuthority RemoteData.Loading }

                _ ->
                    noChange

        BackendUploadAuthority Nothing ->
            let
                syncStatus =
                    -- There are no entities for upload.
                    case model.syncStatus of
                        SyncUploadAuthority record ->
                            SyncUploadAuthority { record | indexDbRemoteData = RemoteData.Success Nothing }

                        _ ->
                            model.syncStatus
            in
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                Cmd.none
                noError
                []

        BackendUploadAuthority (Just result) ->
            case model.syncStatus of
                SyncUploadAuthority record ->
                    if RemoteData.isLoading record.backendRemoteData then
                        -- We are already POSTing to the backend.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | backendRemoteData =
                                        if List.isEmpty result.entities then
                                            -- There were no entities for upload.
                                            RemoteData.NotAsked

                                        else
                                            RemoteData.Loading
                                    , indexDbRemoteData = RemoteData.Success (Just result)
                                }

                            modelUpdated =
                                { model | syncStatus = SyncUploadAuthority recordUpdated }

                            cmd =
                                if List.isEmpty result.entities then
                                    -- There were no entities for upload.
                                    Cmd.none

                                else
                                    HttpBuilder.post (device.backendUrl ++ "/api/sync")
                                        |> withQueryParams
                                            [ ( "access_token", device.accessToken )
                                            , ( "db_version", String.fromInt dbVersion )
                                            ]
                                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIndexDbQueryUploadAuthorityResultRecord result)
                                        -- We don't need to decode anything, as we just want to have
                                        -- the browser download it.
                                        |> HttpBuilder.send (RemoteData.fromResult >> BackendUploadAuthorityHandle result)
                        in
                        SubModelReturn
                            (SyncManager.Utils.determineSyncStatus modelUpdated)
                            cmd
                            noError
                            []

                _ ->
                    noChange

        BackendUploadAuthorityHandle result webData ->
            case model.syncStatus of
                SyncUploadAuthority record ->
                    case webData of
                        RemoteData.Failure error ->
                            let
                                syncStatus =
                                    SyncUploadAuthority { record | backendRemoteData = RemoteData.Failure error }
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                                Cmd.none
                                (maybeHttpError webData "Backend.SyncManager.Update" "BackendUploadAuthorityHandle")
                                []

                        RemoteData.Success _ ->
                            let
                                syncStatus =
                                    SyncUploadAuthority { record | backendRemoteData = RemoteData.Success () }

                                -- We have successfully uploaded the entities, so
                                -- we can mark them as `isSynced`.
                                cmd =
                                    let
                                        localIds =
                                            List.map
                                                (\( entity, _ ) ->
                                                    let
                                                        identifier =
                                                            SyncManager.Utils.getBackendAuthorityEntityIdentifier entity
                                                    in
                                                    identifier.revision
                                                )
                                                result.entities
                                    in
                                    deleteEntitiesThatWereUploaded { type_ = "Authority", localId = localIds }
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                                cmd
                                noError
                                []

                        _ ->
                            -- Satisfy the compiler.
                            noChange

                _ ->
                    noChange

        BackendUploadGeneral Nothing ->
            let
                syncStatus =
                    -- There are no entities for upload.
                    case model.syncStatus of
                        SyncUploadGeneral record ->
                            SyncUploadGeneral { record | indexDbRemoteData = RemoteData.Success Nothing }

                        _ ->
                            model.syncStatus
            in
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                Cmd.none
                noError
                []

        BackendUploadGeneral (Just result) ->
            case model.syncStatus of
                SyncUploadGeneral record ->
                    if RemoteData.isLoading record.backendRemoteData then
                        -- We are already POSTing to the backend.
                        noChange

                    else
                        let
                            recordUpdated =
                                { record
                                    | backendRemoteData =
                                        if List.isEmpty result.entities then
                                            -- There were no entities for upload.
                                            RemoteData.NotAsked

                                        else
                                            RemoteData.Loading
                                    , indexDbRemoteData = RemoteData.Success (Just result)
                                }

                            modelUpdated =
                                { model | syncStatus = SyncUploadGeneral recordUpdated }

                            cmd =
                                if List.isEmpty result.entities then
                                    -- There were no entities for upload.
                                    Cmd.none

                                else
                                    HttpBuilder.post (device.backendUrl ++ "/api/sync")
                                        |> withQueryParams
                                            [ ( "access_token", device.accessToken )
                                            , ( "db_version", String.fromInt dbVersion )
                                            ]
                                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIndexDbQueryUploadGeneralResultRecord result)
                                        -- We don't need to decode anything, as we just want to have
                                        -- the browser download it.
                                        |> HttpBuilder.send (RemoteData.fromResult >> BackendUploadGeneralHandle result)
                        in
                        SubModelReturn
                            (SyncManager.Utils.determineSyncStatus modelUpdated)
                            cmd
                            noError
                            []

                _ ->
                    noChange

        BackendUploadGeneralHandle result webData ->
            case model.syncStatus of
                SyncUploadGeneral record ->
                    case webData of
                        RemoteData.Failure error ->
                            let
                                syncStatus =
                                    SyncUploadGeneral { record | backendRemoteData = RemoteData.Failure error }
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                                Cmd.none
                                (maybeHttpError webData "Backend.SyncManager.Update" "BackendUploadGeneralHandle")
                                []

                        RemoteData.Success _ ->
                            let
                                syncStatus =
                                    SyncUploadGeneral { record | backendRemoteData = RemoteData.Success () }

                                -- We have successfully uploaded the entities, so
                                -- we can mark them as `isSynced`.
                                cmd =
                                    let
                                        localIds =
                                            List.map
                                                (\( entity, _ ) ->
                                                    let
                                                        identifier =
                                                            SyncManager.Utils.getBackendGeneralEntityIdentifier entity
                                                    in
                                                    identifier.revision
                                                )
                                                result.entities
                                    in
                                    deleteEntitiesThatWereUploaded { type_ = "General", localId = localIds }
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                                cmd
                                noError
                                []

                        _ ->
                            -- Satisfy the compiler.
                            noChange

                _ ->
                    noChange

        BackendUploadPhotoAuthorityHandle remoteData ->
            -- Uploading of photos happened through JS, since it involves working
            -- with file blobs. This handler however is for post upload attempt
            -- (success or not), to set RemoteData accordingly.
            case model.syncStatus of
                SyncUploadPhotoAuthority _ ->
                    SubModelReturn
                        (SyncManager.Utils.determineSyncStatus { model | syncStatus = SyncUploadPhotoAuthority remoteData })
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange

        BackendDeferredPhotoFetch Nothing ->
            let
                syncStatus =
                    -- There are no deferred photos matching the query.
                    case model.syncStatus of
                        SyncDownloadPhotos (DownloadPhotosBatch record) ->
                            SyncDownloadPhotos (DownloadPhotosBatch { record | indexDbRemoteData = RemoteData.Success Nothing })

                        SyncDownloadPhotos (DownloadPhotosAll record) ->
                            SyncDownloadPhotos (DownloadPhotosAll { record | indexDbRemoteData = RemoteData.Success Nothing })

                        _ ->
                            model.syncStatus
            in
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus { model | syncStatus = syncStatus })
                Cmd.none
                noError
                []

        BackendDeferredPhotoFetch (Just result) ->
            let
                isLoading =
                    case model.syncStatus of
                        SyncDownloadPhotos (DownloadPhotosBatch record) ->
                            RemoteData.isLoading record.backendRemoteData

                        SyncDownloadPhotos (DownloadPhotosAll record) ->
                            RemoteData.isLoading record.backendRemoteData

                        _ ->
                            False
            in
            if isLoading then
                noChange

            else
                let
                    syncStatus =
                        case model.syncStatus of
                            SyncDownloadPhotos (DownloadPhotosBatch record) ->
                                let
                                    recordUpdated =
                                        { record
                                            | backendRemoteData = RemoteData.Loading
                                            , indexDbRemoteData = RemoteData.Success (Just result)
                                        }
                                in
                                SyncDownloadPhotos (DownloadPhotosBatch recordUpdated)

                            SyncDownloadPhotos (DownloadPhotosAll record) ->
                                let
                                    recordUpdated =
                                        { record
                                            | backendRemoteData = RemoteData.Loading
                                            , indexDbRemoteData = RemoteData.Success (Just result)
                                        }
                                in
                                SyncDownloadPhotos (DownloadPhotosAll recordUpdated)

                            _ ->
                                model.syncStatus

                    modelUpdated =
                        { model | syncStatus = syncStatus }

                    cmd =
                        -- As the image is captured with the image token (`itok`), we
                        -- don't use `withQueryParams` to add the access token, as it will
                        -- result with something like this:
                        -- image-1234.jpg?itok=[image-token]?access_token=[access-token]
                        -- Instead, we manually add the access token with a `&`.
                        HttpBuilder.get (result.photo ++ "&" ++ "access_token=" ++ device.accessToken)
                            -- We don't need to decode anything, as we just want to have
                            -- the browser download it.
                            |> HttpBuilder.send (RemoteData.fromResult >> BackendDeferredPhotoFetchHandle result)
                in
                SubModelReturn
                    (SyncManager.Utils.determineSyncStatus modelUpdated)
                    cmd
                    noError
                    []

        BackendDeferredPhotoFetchHandle result webData ->
            case webData of
                RemoteData.Failure error ->
                    if Utils.WebData.isNetworkError error then
                        -- We're offline, so this doesn't qualify as an attempt.
                        noChange

                    else
                        let
                            syncStatus =
                                case model.syncStatus of
                                    SyncDownloadPhotos (DownloadPhotosBatch deferredPhoto) ->
                                        let
                                            deferredPhotoUpdated =
                                                { deferredPhoto
                                                  -- Reduce the batch counter.
                                                    | batchCounter = deferredPhoto.batchCounter - 1
                                                    , backendRemoteData = RemoteData.Failure error
                                                }
                                        in
                                        SyncDownloadPhotos (DownloadPhotosBatch deferredPhotoUpdated)

                                    SyncDownloadPhotos (DownloadPhotosAll deferredPhoto) ->
                                        let
                                            deferredPhotoUpdated =
                                                { deferredPhoto | backendRemoteData = RemoteData.Failure error }
                                        in
                                        SyncDownloadPhotos (DownloadPhotosAll deferredPhotoUpdated)

                                    _ ->
                                        model.syncStatus
                        in
                        update
                            currentDate
                            dbVersion
                            device
                            (QueryIndexDb <| IndexDbQueryUpdateDeferredPhotoAttempts result)
                            { model | syncStatus = syncStatus }

                RemoteData.Success queryResult ->
                    let
                        syncStatus =
                            case model.syncStatus of
                                SyncDownloadPhotos (DownloadPhotosBatch deferredPhoto) ->
                                    let
                                        deferredPhotoUpdated =
                                            { deferredPhoto
                                              -- Reduce the batch counter.
                                                | batchCounter = deferredPhoto.batchCounter - 1
                                                , backendRemoteData = RemoteData.Success queryResult
                                            }
                                    in
                                    SyncDownloadPhotos (DownloadPhotosBatch deferredPhotoUpdated)

                                SyncDownloadPhotos (DownloadPhotosAll deferredPhoto) ->
                                    let
                                        deferredPhotoUpdated =
                                            { deferredPhoto | backendRemoteData = RemoteData.Success queryResult }
                                    in
                                    SyncDownloadPhotos (DownloadPhotosAll deferredPhotoUpdated)

                                _ ->
                                    model.syncStatus
                    in
                    -- We've fetched the image, so we can remove the record from
                    -- `deferredPhotos` table.
                    update
                        currentDate
                        dbVersion
                        device
                        (QueryIndexDb <| IndexDbQueryRemoveDeferredPhotoAttempts result.uuid)
                        { model | syncStatus = syncStatus }

                _ ->
                    -- Satisfy the compiler.
                    noChange

        QueryIndexDb indexDbQueryType ->
            let
                record =
                    case indexDbQueryType of
                        IndexDbQueryUploadPhotoAuthority ->
                            let
                                -- Send the device info so on JS, we'd know how
                                -- to contact the backend.
                                encodedData =
                                    Device.Encoder.encode device
                                        |> Json.Encode.encode 0
                            in
                            { queryType = "IndexDbQueryUploadPhotoAuthority"
                            , data = Just encodedData
                            }

                        IndexDbQueryUploadGeneral ->
                            { queryType = "IndexDbQueryUploadGeneral"
                            , data = Nothing
                            }

                        IndexDbQueryUploadAuthority ->
                            { queryType = "IndexDbQueryUploadAuthority"
                            , data = Nothing
                            }

                        IndexDbQueryDeferredPhoto ->
                            { queryType = "IndexDbQueryDeferredPhoto"
                            , data = Nothing
                            }

                        IndexDbQueryRemoveDeferredPhotoAttempts uuid ->
                            { queryType = "IndexDbQueryRemoveDeferredPhotoAttempts"
                            , data = Just uuid
                            }

                        IndexDbQueryUpdateDeferredPhotoAttempts record_ ->
                            let
                                -- Increment the number of attempts.
                                encodedData =
                                    Json.Encode.object
                                        [ ( "uuid", Json.Encode.string record_.uuid )
                                        , ( "attempts", Json.Encode.int (record_.attempts + 1) )
                                        ]
                                        |> Json.Encode.encode 0
                            in
                            { queryType = "IndexDbQueryUpdateDeferredPhotoAttempts"
                            , data = Just encodedData
                            }
            in
            SubModelReturn
                model
                (askFromIndexDb record)
                noError
                []

        QueryIndexDbHandle val ->
            case decodeValue SyncManager.Decoder.decodeIndexDbQueryTypeResult val of
                Ok indexDbQueryTypeResult ->
                    case indexDbQueryTypeResult of
                        IndexDbQueryUploadPhotoAuthorityResult remoteData ->
                            update
                                currentDate
                                dbVersion
                                device
                                (BackendUploadPhotoAuthorityHandle remoteData)
                                model

                        IndexDbQueryUploadAuthorityResult result ->
                            update
                                currentDate
                                dbVersion
                                device
                                (BackendUploadAuthority result)
                                model

                        IndexDbQueryUploadGeneralResult result ->
                            update
                                currentDate
                                dbVersion
                                device
                                (BackendUploadGeneral result)
                                model

                        IndexDbQueryDeferredPhotoResult result ->
                            update
                                currentDate
                                dbVersion
                                device
                                (BackendDeferredPhotoFetch result)
                                model

                Err error ->
                    let
                        location =
                            -- Try to decode at least the queryType so we'd have better
                            -- knowledge of what caused the decoder error.
                            case decodeValue (Json.Decode.field "queryType" Json.Decode.string) val of
                                Ok queryType ->
                                    "FetchFromIndexDbHandle (" ++ queryType ++ ")"

                                Err _ ->
                                    "FetchFromIndexDbHandle (unknown queryType)"
                    in
                    SubModelReturn
                        model
                        Cmd.none
                        (decodeError "Backend.SyncManager.Update" location error)
                        []

        ResetSettings ->
            let
                syncSpeed =
                    { idle = 10000
                    , cycle = 50
                    , offline = 3000
                    }
            in
            SubModelReturn
                { model | syncSpeed = Editable.ReadOnly syncSpeed }
                (sendSyncSpeed syncSpeed)
                noError
                []

        SaveSettings ->
            let
                syncSpeed =
                    Editable.value model.syncSpeed

                -- Safe guard against to0 low values.
                syncSpeedUpdated =
                    { syncSpeed
                        | idle =
                            if syncSpeed.idle < 3000 then
                                3000

                            else
                                syncSpeed.idle
                        , cycle =
                            if syncSpeed.cycle < 50 then
                                50

                            else
                                syncSpeed.cycle
                        , offline =
                            if syncSpeed.offline < 1000 then
                                1000

                            else
                                syncSpeed.offline
                    }
            in
            SubModelReturn
                { model | syncSpeed = Editable.ReadOnly syncSpeedUpdated }
                (sendSyncSpeed syncSpeedUpdated)
                noError
                []

        SetSyncCycle syncCycle ->
            SubModelReturn
                { model | syncCycle = syncCycle }
                Cmd.none
                noError
                []

        SetSyncSpeedIdle str ->
            case String.toInt str of
                Just val ->
                    let
                        syncSpeed =
                            model.syncSpeed
                                |> Editable.edit
                                |> Editable.map (\old -> { old | idle = val })
                    in
                    SubModelReturn
                        { model | syncSpeed = syncSpeed }
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange

        SetSyncSpeedCycle str ->
            case String.toInt str of
                Just val ->
                    let
                        syncSpeed =
                            model.syncSpeed
                                |> Editable.edit
                                |> Editable.map (\old -> { old | cycle = val })
                    in
                    SubModelReturn
                        { model | syncSpeed = syncSpeed }
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange

        SetSyncSpeedOffline str ->
            case String.toInt str of
                Just val ->
                    let
                        syncSpeed =
                            model.syncSpeed
                                |> Editable.edit
                                |> Editable.map (\old -> { old | offline = val })
                    in
                    SubModelReturn
                        { model | syncSpeed = syncSpeed }
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        backendFetchMain =
            case model.syncCycle of
                SyncManager.Model.SyncCyclePause ->
                    Sub.none

                _ ->
                    Time.every (getSyncSpeedForSubscriptions model) (\_ -> BackendFetchMain)
    in
    Sub.batch
        [ backendFetchMain
        , getFromIndexDb QueryIndexDbHandle
        ]


{-| Send to JS data we have synced, e.g. `person`, `health center`, etc.
-}
port sendSyncedDataToIndexDb : { table : String, data : List String, shard : String } -> Cmd msg


{-| Send to JS the last revision ID used to download General.
-}
port sendLastFetchedRevisionIdGeneral : Int -> Cmd msg


{-| Send to JS a list with the last revision ID used to download Authority,
along with their UUID.
-}
port sendRevisionIdPerAuthority : List { uuid : String, revisionId : Int } -> Cmd msg


{-| Send to JS a list of local ID that were uploaded.

The `type_` can be `General` or `Authority`.

-}
port deleteEntitiesThatWereUploaded : { type_ : String, localId : List Int } -> Cmd msd


{-| Send to JS a uuid list of local changes that can be deleted.
This is done after they are uploaded to the backend, and re-downloaded.

The `type_` can be `General` or `Authority`.

-}
port sendLocalIdsForDelete : { type_ : String, uuid : List String } -> Cmd msd


{-| Send to JS model.syncSpeed
-}
port sendSyncSpeed : { idle : Int, cycle : Int, offline : Int } -> Cmd msd


{-| Ask JS to send us data from IndexDB. We send the query type, and in case
we have some related data (e.g. the child ID to query), we send it as-well.
-}
port askFromIndexDb : { queryType : String, data : Maybe String } -> Cmd msg


{-| Get data requested from IndexDB.

For now we don't care who asked for the data, we just fill it in where
needed.

-}
port getFromIndexDb : (Value -> msg) -> Sub msg
