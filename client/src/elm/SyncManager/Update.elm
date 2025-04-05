port module SyncManager.Update exposing (subscriptions, update)

import App.Model exposing (SubModelReturn)
import App.Ports exposing (bindDropZone, initRollbar)
import App.Utils exposing (sequenceSubModelReturn)
import AssocList as Dict
import Backend.Model
import Debouncer.Basic as Debouncer exposing (provideInput)
import Device.Encoder
import Device.Model exposing (Device)
import Editable
import Error.Utils exposing (decoderError, maybeHttpError, noError)
import GeoLocation.Utils exposing (getGeoInfo, getReverseGeoInfo)
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import HttpBuilder exposing (..)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import List.Zipper as Zipper
import Maybe.Extra
import Pages.Page exposing (Page)
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Decoder exposing (decodeDownloadSyncResponseAuthority, decodeDownloadSyncResponseAuthorityStats, decodeDownloadSyncResponseGeneral)
import SyncManager.Encoder
import SyncManager.Model exposing (..)
import SyncManager.Utils
    exposing
        ( backendAuthorityEntityToRevision
        , backendGeneralEntityToRevision
        , getDownloadPhotosSpeedForSubscriptions
        , getSyncSpeedForSubscriptions
        , resolveIncidentDetailsMsg
        , syncInfoAuthorityForPort
        , syncInfoGeneralForPort
        )
import Time
import Utils.WebData
import Version


update : NominalDate -> Time.Posix -> Page -> Int -> Device -> Msg -> Model -> SubModelReturn Model Msg
update currentDate currentTime activePage dbVersion device msg model =
    let
        noChange =
            SubModelReturn model Cmd.none noError []

        sendSyncInfoGeneralCmd info =
            syncInfoGeneralForPort info
                |> sendSyncInfoGeneral

        sendSyncInfoAuthoritiesCmd zipper =
            Zipper.toList zipper
                |> List.map syncInfoAuthorityForPort
                |> sendSyncInfoAuthorities

        determineSyncStatus =
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus activePage model)
                Cmd.none
                noError
                []

        handleNewRevisionsMsg toRevisionFunc backendEntities =
            List.map toRevisionFunc backendEntities
                |> Backend.Model.HandleRevisions
                |> App.Model.MsgIndexedDb

        requestTimestamp =
            Time.posixToMillis model.downloadRequestTime
                |> String.fromInt
    in
    case msg of
        MsgDebouncer subMsg ->
            let
                ( subModel, subCmd, extraMsg ) =
                    Debouncer.update subMsg model.debouncer
            in
            SubModelReturn
                { model | debouncer = subModel }
                (Cmd.map MsgDebouncer subCmd)
                noError
                []
                |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device) (Maybe.Extra.toList extraMsg)

        NoOp ->
            noChange

        SchedulePageRefresh ->
            noChange
                |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device)
                    [ MsgDebouncer <| provideInput RefreshPage ]

        SchedulePhotosDownload ->
            noChange
                |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device)
                    [ MsgDebouncer <| provideInput TryDownloadingPhotos ]

        RefreshPage ->
            SubModelReturn model (refreshPage ()) noError []

        BackendAuthorityFetch ->
            case model.syncStatus of
                SyncDownloadAuthority webData ->
                    if
                        RemoteData.isLoading webData
                            && (Time.posixToMillis currentTime - Time.posixToMillis model.downloadRequestTime < downloadRequestTimeout)
                    then
                        -- We are already loading, and request did not timed out.
                        noChange

                    else
                        case model.syncInfoAuthorities of
                            Nothing ->
                                -- No zipper, means not subscribed yet to any
                                -- authority. `determineSyncStatus` will take care of
                                -- rotating if we're not on automatic sync.
                                -- We also, schdule photos download send devicestate report,
                                -- so that version and synced authorities get updated on backend.
                                determineSyncStatus
                                    |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device)
                                        [ SchedulePhotosDownload, QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]

                            Just zipper ->
                                let
                                    currentZipper =
                                        Zipper.current zipper

                                    zipperUpdated =
                                        if currentZipper.status == Downloading then
                                            zipper

                                        else
                                            Zipper.mapCurrent
                                                (\old -> { old | status = Downloading })
                                                zipper

                                    ( syncInfoAuthorities, setSyncInfoAurhoritiesCmd ) =
                                        if currentZipper.status == Downloading then
                                            ( model.syncInfoAuthorities, Cmd.none )

                                        else
                                            ( Just zipperUpdated, sendSyncInfoAuthoritiesCmd zipperUpdated )

                                    cmd =
                                        HttpBuilder.get (device.backendUrl ++ "/ShowSyncManagerHealthCenter")
                                            |> withQueryParams
                                                [ ( "healthCenterId", currentZipper.uuid )
                                                , ( "accessToken", device.accessToken )
                                                , ( "db_version", String.fromInt dbVersion )
                                                , ( "serial", String.fromInt currentZipper.lastFetchedRevisionId )
                                                , ( "stats_cache_hash", currentZipper.statsCacheHash )
                                                ]
                                            |> withExpectJson decodeDownloadSyncResponseAuthority
                                            |> HttpBuilder.send (RemoteData.fromResult >> BackendAuthorityFetchHandle zipperUpdated)
                                in
                                SubModelReturn
                                    { model
                                        | syncStatus = SyncDownloadAuthority RemoteData.Loading
                                        , syncInfoAuthorities = syncInfoAuthorities
                                        , downloadRequestTime = currentTime
                                    }
                                    (Cmd.batch [ cmd, setSyncInfoAurhoritiesCmd ])
                                    noError
                                    []

                _ ->
                    determineSyncStatus

        BackendAuthorityFetchHandle zipper webData ->
            let
                ( saveFetchedDataCmd, modelUpdated, extraMsgs ) =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                currentZipper =
                                    Zipper.current zipper

                                dataToSend =
                                    data.entities
                                        |> List.foldl
                                            (\entity accum -> SyncManager.Utils.getDataToSendAuthority entity accum)
                                            []
                                        |> List.reverse

                                -- When authority completes download, we mark stock managemend data of that
                                -- health center as obsolete, as there may have been Fbf/Stock update
                                -- entries downloaded.
                                stockManagementDataMsg =
                                    if data.revisionCount == 0 then
                                        [ Backend.Model.MarkForRecalculationStockManagementData (toEntityUuid currentZipper.uuid)
                                            |> App.Model.MsgIndexedDb
                                        ]

                                    else
                                        []
                            in
                            ( sendSyncedDataToIndexDb { table = "Authority", data = dataToSend, shard = currentZipper.uuid, timestamp = requestTimestamp }
                            , { model | downloadAuthorityResponse = webData }
                            , stockManagementDataMsg
                            )

                        Nothing ->
                            ( Cmd.none
                            , SyncManager.Utils.determineSyncStatus activePage
                                { model | syncStatus = SyncDownloadAuthority webData }
                            , []
                            )
            in
            SubModelReturn
                modelUpdated
                saveFetchedDataCmd
                (maybeHttpError webData "Backend.SyncManager.Update" "BackendAuthorityFetchHandle")
                extraMsgs

        BackendAuthorityFetchedDataSavedHandle timestamp ->
            if requestTimestamp /= timestamp then
                -- Request timestamp does not match. This indicates that timeout
                -- has occured, and another request was issued instead.
                -- Therefore, we drop this request.
                noChange

            else
                Maybe.map2
                    (\zipper data ->
                        let
                            currentZipper =
                                Zipper.current zipper

                            deferredPhotosCmd =
                                -- Prepare a list of the photos, so we
                                -- could grab them  in a later time.
                                let
                                    dataToSend =
                                        data.entities
                                            |> List.foldl
                                                (\entity accum ->
                                                    case SyncManager.Utils.getImageFromBackendAuthorityEntity entity of
                                                        Just imageUrl ->
                                                            (entity
                                                                |> SyncManager.Utils.getBackendAuthorityEntityIdentifier
                                                                |> SyncManager.Encoder.encodeDataForDeferredPhotos imageUrl
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
                                    sendSyncedDataToIndexDb { table = "DeferredPhotos", data = dataToSend, shard = currentZipper.uuid, timestamp = "" }

                            appMsgs =
                                [ handleNewRevisionsMsg backendAuthorityEntityToRevision data.entities ]

                            syncInfoAuthorities =
                                let
                                    status =
                                        if data.revisionCount == 0 then
                                            Success

                                        else
                                            currentZipper.status

                                    lastFetchedRevisionId =
                                        data.entities
                                            |> List.sortBy (SyncManager.Utils.getBackendAuthorityEntityIdentifier >> .revision)
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.map (SyncManager.Utils.getBackendAuthorityEntityIdentifier >> .revision)
                                            |> Maybe.withDefault currentZipper.lastFetchedRevisionId
                                in
                                Zipper.mapCurrent
                                    (\old ->
                                        { old
                                            | lastFetchedRevisionId = lastFetchedRevisionId
                                            , lastSuccesfulContact = Time.posixToMillis currentTime
                                            , remainingToDownload = data.revisionCount
                                            , status = status
                                        }
                                    )
                                    zipper

                            modelWithSyncStatus =
                                SyncManager.Utils.determineSyncStatus activePage
                                    { model
                                        | syncStatus = SyncDownloadAuthority model.downloadAuthorityResponse
                                        , syncInfoAuthorities = Just syncInfoAuthorities
                                    }
                        in
                        SubModelReturn
                            modelWithSyncStatus
                            (Cmd.batch
                                [ deferredPhotosCmd

                                -- Send to JS the updated revision ID. We send the entire list.
                                , sendSyncInfoAuthoritiesCmd syncInfoAuthorities
                                ]
                            )
                            (maybeHttpError model.downloadAuthorityResponse "Backend.SyncManager.Update" "BackendAuthorityFetchedDataSavedHandle")
                            appMsgs
                    )
                    model.syncInfoAuthorities
                    (RemoteData.toMaybe model.downloadAuthorityResponse)
                    |> Maybe.withDefault noChange

        BackendAuthorityDashboardStatsFetch ->
            case model.syncStatus of
                SyncDownloadAuthorityDashboardStats webData ->
                    if RemoteData.isLoading webData then
                        -- We are already loading.
                        noChange

                    else
                        case model.syncInfoAuthorities of
                            Nothing ->
                                -- No zipper, means not subscribed yet to any
                                -- authority. `determineSyncStatus` will take care of
                                -- rotating if we're not on automatic sync.
                                determineSyncStatus

                            Just zipper ->
                                let
                                    currentZipper =
                                        Zipper.current zipper

                                    zipperUpdated =
                                        if currentZipper.status == Downloading then
                                            zipper

                                        else
                                            Zipper.mapCurrent
                                                (\old -> { old | status = Downloading })
                                                zipper

                                    ( syncInfoAuthorities, setSyncInfoAurhoritiesCmd ) =
                                        if currentZipper.status == Downloading then
                                            ( model.syncInfoAuthorities, Cmd.none )

                                        else
                                            ( Just zipperUpdated, sendSyncInfoAuthoritiesCmd zipperUpdated )

                                    cmd =
                                        HttpBuilder.get (device.backendUrl ++ "/api/sync/" ++ currentZipper.uuid)
                                            |> withQueryParams
                                                [ ( "access_token", device.accessToken )
                                                , ( "db_version", String.fromInt dbVersion )
                                                , ( "stats_cache_hash", currentZipper.statsCacheHash )
                                                , ( "statistics", "1" )
                                                ]
                                            |> withExpectJson decodeDownloadSyncResponseAuthorityStats
                                            |> HttpBuilder.send (RemoteData.fromResult >> BackendAuthorityDashboardStatsFetchHandle zipperUpdated)
                                in
                                SubModelReturn
                                    { model | syncStatus = SyncDownloadAuthorityDashboardStats RemoteData.Loading, syncInfoAuthorities = syncInfoAuthorities }
                                    (Cmd.batch
                                        [ cmd
                                        , setSyncInfoAurhoritiesCmd

                                        -- We may get to situation where sync unbinds the DropZone.
                                        -- For example, when at group session - photo measurement form,
                                        -- and sync downloads a new revision of some entity.
                                        -- This causes a rebuild of editable session, which, when rebuilt,
                                        -- causes the View of the page to be rebuilt as well.
                                        -- As a result, we end up with unbound DropZone container.
                                        -- Therefore, we try to bind DropZone when sync ends.
                                        -- This is ok, as the command will not proceed, unless it
                                        -- detects DropZone contaier element on the page.
                                        , bindDropZone ()
                                        ]
                                    )
                                    noError
                                    []

                _ ->
                    determineSyncStatus

        BackendAuthorityDashboardStatsFetchHandle zipper webData ->
            let
                currentZipper =
                    Zipper.current zipper

                ( cmd, statsCacheHash, appMsgs ) =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            if List.isEmpty data.entities then
                                ( Cmd.none, currentZipper.statsCacheHash, [] )

                            else
                                let
                                    dataToSend =
                                        data.entities
                                            |> List.foldl
                                                (\entity accum -> SyncManager.Utils.getDataToSendAuthority entity accum)
                                                []
                                            |> List.reverse

                                    -- Grab the updated cache hash.
                                    cacheHash =
                                        data.entities
                                            |> List.head
                                            |> Maybe.map
                                                (\backendAuthorityEntity ->
                                                    case backendAuthorityEntity of
                                                        BackendAuthorityDashboardStats statsEntity ->
                                                            statsEntity.entity.cacheHash

                                                        _ ->
                                                            currentZipper.statsCacheHash
                                                )
                                            |> Maybe.withDefault currentZipper.statsCacheHash
                                in
                                ( sendSyncedDataToIndexDb { table = "AuthorityStats", data = dataToSend, shard = currentZipper.uuid, timestamp = "" }
                                , cacheHash
                                , [ handleNewRevisionsMsg backendAuthorityEntityToRevision data.entities ]
                                )

                        Nothing ->
                            ( Cmd.none, currentZipper.statsCacheHash, [] )

                currentTimeMillis =
                    Time.posixToMillis currentTime

                syncInfoAuthorities =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            Zipper.mapCurrent
                                (\old ->
                                    { old
                                        | lastSuccesfulContact = currentTimeMillis
                                        , remainingToDownload = data.revisionCount
                                        , status = Success
                                        , statsCacheHash = statsCacheHash
                                    }
                                )
                                zipper

                        Nothing ->
                            zipper

                modelWithSyncStatus =
                    SyncManager.Utils.determineSyncStatus activePage
                        { model
                            | syncStatus = SyncDownloadAuthorityDashboardStats webData
                            , syncInfoAuthorities = Just syncInfoAuthorities
                        }

                -- Calculating the time it took authorities to sync.
                -- When sync is completed (status is about to change to Idle), we need to decide on
                -- additional actions:
                -- If sync lasted  more than 45 seconds (initial sync, for example), we refresh the page.
                -- Otherwise, we trigger photos download.
                extraMsgs =
                    if modelWithSyncStatus.syncStatus == SyncIdle then
                        let
                            authoritiesSyncTime =
                                currentTimeMillis - model.syncInfoGeneral.lastSuccesfulContact
                        in
                        if authoritiesSyncTime > 45000 then
                            [ SchedulePageRefresh ]

                        else
                            [ SchedulePhotosDownload, QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]

                    else
                        -- Sync is not completed yet - no additional actions.
                        []
            in
            SubModelReturn
                modelWithSyncStatus
                (Cmd.batch
                    [ cmd
                    , -- Send to JS the updated revision ID. We send the entire list.
                      sendSyncInfoAuthoritiesCmd syncInfoAuthorities
                    ]
                )
                (maybeHttpError webData "Backend.SyncManager.Update" "BackendAuthorityDashboardStatsFetchHandle")
                appMsgs
                |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device) extraMsgs

        BackendFetchMain ->
            case model.syncStatus of
                SyncIdle ->
                    determineSyncStatus
                        -- We send state report when we begin the sync.
                        |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device) [ QueryIndexDb IndexDbQueryGetTotalEntriesToUpload ]

                SyncUploadPhoto _ _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendPhotoUpload
                        model

                SyncUploadScreenshot _ _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendScreenshotUpload
                        model

                SyncUploadGeneral _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        FetchFromIndexDbUploadGeneral
                        model

                SyncUploadWhatsApp _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        FetchFromIndexDbUploadWhatsApp
                        model

                SyncUploadAuthority _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        FetchFromIndexDbUploadAuthority
                        model

                SyncDownloadGeneral _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendGeneralFetch
                        model

                SyncDownloadAuthority _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendAuthorityFetch
                        model

                SyncDownloadAuthorityDashboardStats _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendAuthorityDashboardStatsFetch
                        model

                SyncReportIncident incidentType ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        (BackendReportSyncIncident incidentType)
                        model

        BackendFetchPhotos ->
            case model.downloadPhotosStatus of
                DownloadPhotosIdle ->
                    SubModelReturn
                        (SyncManager.Utils.determineDownloadPhotosStatus model)
                        Cmd.none
                        noError
                        []

                DownloadPhotosInProcess _ ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        FetchFromIndexDbDeferredPhoto
                        model

        RevisionIdAuthorityAdd uuid ->
            -- Add a new authority to Local storage.
            let
                uuidAsString =
                    fromEntityUuid uuid

                syncInfoAuthorities =
                    case model.syncInfoAuthorities of
                        Just zipper ->
                            zipper
                                |> Zipper.toList
                                -- Before adding, lets remove the same UUID, so in case it's already
                                -- we won't have duplicates.
                                |> List.filter (\row -> row.uuid /= uuidAsString)
                                |> (\list -> emptySyncInfoAuthority uuidAsString :: list)
                                |> Zipper.fromList

                        Nothing ->
                            [ emptySyncInfoAuthority uuidAsString ]
                                |> Zipper.fromList

                cmd =
                    case syncInfoAuthorities of
                        Just zipper ->
                            sendSyncInfoAuthoritiesCmd zipper

                        Nothing ->
                            Cmd.none
            in
            SubModelReturn
                { model | syncInfoAuthorities = syncInfoAuthorities }
                cmd
                noError
                []
                |> sequenceSubModelReturn (update currentDate currentTime activePage dbVersion device) [ TrySyncing ]

        RevisionIdAuthorityRemove uuid ->
            -- Remove authority from Local storage.
            let
                uuidAsString =
                    fromEntityUuid uuid

                syncInfoAuthorities =
                    model.syncInfoAuthorities
                        |> Maybe.andThen (Zipper.toList >> List.filter (\row -> row.uuid /= uuidAsString) >> Zipper.fromList)

                cmd =
                    case syncInfoAuthorities of
                        Just zipper ->
                            sendSyncInfoAuthoritiesCmd zipper

                        Nothing ->
                            -- There're no authorities to sync, so we set an empty list.
                            sendSyncInfoAuthorities []
            in
            SubModelReturn
                { model | syncInfoAuthorities = syncInfoAuthorities }
                cmd
                noError
                []

        BackendGeneralFetch ->
            case model.syncStatus of
                SyncDownloadGeneral webData ->
                    if
                        RemoteData.isLoading webData
                            && (Time.posixToMillis currentTime - Time.posixToMillis model.downloadRequestTime < downloadRequestTimeout)
                    then
                        -- We are already loading, and request did not timed out.
                        noChange

                    else
                        let
                            syncInfoGeneral =
                                if model.syncInfoGeneral.status == Downloading then
                                    model.syncInfoGeneral

                                else
                                    model.syncInfoGeneral
                                        |> (\info -> { info | status = Downloading })

                            setSyncInfoGeneralCmd =
                                if model.syncInfoGeneral.status == Downloading then
                                    Cmd.none

                                else
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                            cmd =
                                HttpBuilder.get (device.backendUrl ++ "/ShowSyncManager")
                                    |> withQueryParams
                                        [ ( "accessToken", device.accessToken )
                                        , ( "db_version", String.fromInt dbVersion )
                                        , ( "serial", String.fromInt model.syncInfoGeneral.lastFetchedRevisionId )
                                        ]
                                    |> withExpectJson decodeDownloadSyncResponseGeneral
                                    |> HttpBuilder.send (RemoteData.fromResult >> BackendGeneralFetchHandle)
                        in
                        SubModelReturn
                            { model
                                | syncStatus = SyncDownloadGeneral RemoteData.Loading
                                , syncInfoGeneral = syncInfoGeneral
                                , downloadRequestTime = currentTime
                            }
                            (Cmd.batch [ cmd, setSyncInfoGeneralCmd ])
                            noError
                            []

                _ ->
                    SubModelReturn
                        (SyncManager.Utils.determineSyncStatus activePage model)
                        Cmd.none
                        noError
                        []

        BackendGeneralFetchHandle webData ->
            let
                ( cmd, modelUpdated ) =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            let
                                dataToSend =
                                    data.entities
                                        |> List.foldl (\entity accum -> SyncManager.Utils.getDataToSendGeneral entity accum) []
                                        |> List.reverse

                                saveFetchedDataCmd =
                                    sendSyncedDataToIndexDb { table = "General", data = dataToSend, shard = "", timestamp = requestTimestamp }

                                -- On last iteration of Download general (batch size during download is 500),
                                -- we also init Rollbar. The logic - Rollbar token is passed at General Download,
                                -- and it may have changed. So, we need to init Rollbar with new token.
                                -- Init also sends all unsent items from dbErrors table.
                                initRollbarCmd =
                                    if List.length data.entities < 500 && (not <| String.isEmpty data.rollbarToken) then
                                        initRollbar { device = data.deviceName, token = data.rollbarToken }

                                    else
                                        Cmd.none
                            in
                            ( Cmd.batch [ saveFetchedDataCmd, initRollbarCmd ]
                            , { model | downloadGeneralResponse = webData }
                            )

                        Nothing ->
                            ( Cmd.none
                            , SyncManager.Utils.determineSyncStatus activePage
                                { model | syncStatus = SyncDownloadGeneral webData }
                            )
            in
            SubModelReturn
                modelUpdated
                cmd
                (maybeHttpError webData "Backend.SyncManager.Update" "BackendGeneralFetchHandle")
                []

        BackendGeneralFetchedDataSavedHandle timestamp ->
            if requestTimestamp /= timestamp then
                -- Request timestamp does not match. This indicates that timeout
                -- has occured, and another request was issued instead.
                -- Therefore, we drop this request.
                noChange

            else
                Maybe.map
                    (\data ->
                        let
                            -- We have successfully saved the entities, so
                            -- we can delete them fom the `nodeChanges` table.
                            -- We will do it, by their localId.
                            deleteLocalIdsCmd =
                                let
                                    localIds =
                                        List.map (SyncManager.Utils.getBackendGeneralEntityIdentifier >> .uuid)
                                            data.entities
                                in
                                if List.isEmpty localIds then
                                    Cmd.none

                                else
                                    sendLocalIdsForDelete { type_ = "General", uuid = localIds }

                            appMsgs =
                                [ Backend.Model.ResetFailedToFetchAuthorities |> App.Model.MsgIndexedDb
                                , handleNewRevisionsMsg backendGeneralEntityToRevision data.entities
                                ]

                            syncInfoGeneral =
                                let
                                    status =
                                        if data.revisionCount == 0 then
                                            Success

                                        else
                                            model.syncInfoGeneral.status

                                    lastFetchedRevisionId =
                                        data.entities
                                            |> List.sortBy (SyncManager.Utils.getBackendGeneralEntityIdentifier >> .revision)
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.map (SyncManager.Utils.getBackendGeneralEntityIdentifier >> .revision)
                                            |> Maybe.withDefault model.syncInfoGeneral.lastFetchedRevisionId
                                in
                                model.syncInfoGeneral
                                    |> (\info ->
                                            { info
                                                | lastFetchedRevisionId = lastFetchedRevisionId
                                                , lastSuccesfulContact = Time.posixToMillis currentTime
                                                , remainingToDownload = data.revisionCount
                                                , deviceName = data.deviceName
                                                , status = status
                                                , rollbarToken = data.rollbarToken
                                                , site = data.site
                                                , features = data.features
                                            }
                                       )

                            modelWithSyncStatus =
                                let
                                    -- At general sync we get site param,
                                    -- Theoretically, it may change, and therefore,
                                    -- we recalculate geo info data.
                                    geoInfo =
                                        getGeoInfo syncInfoGeneral.site
                                in
                                SyncManager.Utils.determineSyncStatus activePage
                                    { model
                                        | syncStatus = SyncDownloadGeneral model.downloadGeneralResponse
                                        , syncInfoGeneral = syncInfoGeneral
                                        , geoInfo = geoInfo
                                        , reverseGeoInfo = getReverseGeoInfo geoInfo
                                    }
                        in
                        SubModelReturn
                            modelWithSyncStatus
                            (Cmd.batch
                                [ deleteLocalIdsCmd
                                , sendSyncInfoGeneralCmd syncInfoGeneral
                                ]
                            )
                            (maybeHttpError model.downloadAuthorityResponse "Backend.SyncManager.Update" "BackendGeneralFetchedDataSavedHandle")
                            appMsgs
                    )
                    (RemoteData.toMaybe model.downloadGeneralResponse)
                    |> Maybe.withDefault noChange

        SetLastFetchedRevisionIdAuthority zipper revisionId ->
            let
                zipperUpdated =
                    Zipper.mapCurrent (\old -> { old | lastFetchedRevisionId = revisionId }) zipper
            in
            SubModelReturn
                { model | syncInfoAuthorities = Just zipperUpdated }
                Cmd.none
                noError
                []

        SetLastFetchedRevisionIdGeneral revisionId ->
            let
                infoUpdated =
                    model.syncInfoGeneral
                        |> (\info -> { info | lastFetchedRevisionId = revisionId })
            in
            SubModelReturn
                { model | syncInfoGeneral = infoUpdated }
                Cmd.none
                noError
                []

        BackendReportState totalToUpload ->
            let
                version =
                    Version.version.build

                phase =
                    if model.syncStatus == SyncIdle then
                        "sync-end"

                    else
                        "sync-start"

                syncedAutorities =
                    model.syncInfoAuthorities
                        |> Maybe.map (Zipper.toList >> List.map .uuid)
                        |> Maybe.withDefault []

                cmd =
                    HttpBuilder.post (device.backendUrl ++ "/api/report-state")
                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeDeviceStateReport version phase totalToUpload syncedAutorities)
                        |> HttpBuilder.send (always NoOp)
            in
            SubModelReturn
                model
                cmd
                noError
                []

        BackendReportIncidentDetails details ->
            let
                cmd =
                    HttpBuilder.post (device.backendUrl ++ "/api/report-incident-details")
                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIncidentDetails details)
                        |> HttpBuilder.send (always NoOp)
            in
            SubModelReturn
                model
                cmd
                noError
                []

        BackendReportSyncIncident incidentType ->
            let
                cmd =
                    HttpBuilder.post (device.backendUrl ++ "/api/report-sync-incident")
                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeSyncIncident incidentType)
                        |> HttpBuilder.send (always NoOp)
            in
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus activePage model)
                cmd
                noError
                []

        FetchFromIndexDbDeferredPhoto ->
            -- Get a deferred photo from IndexDB.
            case model.downloadPhotosStatus of
                DownloadPhotosInProcess DownloadPhotosNone ->
                    noChange

                DownloadPhotosInProcess (DownloadPhotosBatch record) ->
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
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryDeferredPhoto)
                            { model | downloadPhotosStatus = DownloadPhotosInProcess (DownloadPhotosBatch recordUpdated) }

                DownloadPhotosInProcess (DownloadPhotosAll record) ->
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
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryDeferredPhoto)
                            { model | downloadPhotosStatus = DownloadPhotosInProcess (DownloadPhotosAll recordUpdated) }

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
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadGeneral)
                            { model | syncStatus = SyncUploadGeneral recordUpdated }

                _ ->
                    noChange

        FetchFromIndexDbUploadWhatsApp ->
            -- Get a entities for upload from IndexDB.
            case model.syncStatus of
                SyncUploadWhatsApp record ->
                    if
                        RemoteData.isLoading record.indexDbRemoteData
                            || RemoteData.isLoading record.backendRemoteData
                    then
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
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadWhatsApp)
                            { model | syncStatus = SyncUploadWhatsApp recordUpdated }

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
                        case model.syncInfoAuthorities of
                            Nothing ->
                                -- No zipper, means not subscribed yet to any
                                -- authority. `determineSyncStatus` will take care of
                                -- rotating if we're not on automatic sync.
                                determineSyncStatus

                            Just zipper ->
                                let
                                    currentZipper =
                                        Zipper.current zipper

                                    recordUpdated =
                                        { record
                                            | indexDbRemoteData = RemoteData.Loading
                                            , backendRemoteData = RemoteData.NotAsked
                                        }
                                in
                                update
                                    currentDate
                                    currentTime
                                    activePage
                                    dbVersion
                                    device
                                    (QueryIndexDb <| IndexDbQueryUploadAuthority currentZipper.uuid)
                                    { model | syncStatus = SyncUploadAuthority recordUpdated }

                _ ->
                    noChange

        BackendPhotoUpload ->
            case model.syncStatus of
                SyncUploadPhoto errorsCount webData ->
                    if RemoteData.isLoading webData then
                        noChange

                    else
                        let
                            -- This is first step of sync.
                            -- When device is offline, we may not get any further,
                            -- therefore, we need to generate and store geo info data here.
                            geoInfo =
                                getGeoInfo model.syncInfoGeneral.site
                        in
                        update
                            currentDate
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadPhoto)
                            { model
                                | syncStatus = SyncUploadPhoto errorsCount RemoteData.Loading
                                , geoInfo = geoInfo
                                , reverseGeoInfo = getReverseGeoInfo geoInfo
                            }

                _ ->
                    noChange

        BackendUploadPhotoHandle remoteData ->
            -- Uploading of photos happened through JS, since it involves working
            -- with file blobs. This handler however is for post upload attempt
            -- (success or not), to set RemoteData accordingly.
            case model.syncStatus of
                SyncUploadPhoto errorsCount _ ->
                    SubModelReturn
                        (SyncManager.Utils.determineSyncStatus activePage
                            { model | syncStatus = SyncUploadPhoto errorsCount remoteData }
                        )
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange

        BackendScreenshotUpload ->
            case model.syncStatus of
                SyncUploadScreenshot errorsCount webData ->
                    if RemoteData.isLoading webData then
                        noChange

                    else
                        update
                            currentDate
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb IndexDbQueryUploadScreenshot)
                            { model | syncStatus = SyncUploadScreenshot errorsCount RemoteData.Loading }

                _ ->
                    noChange

        BackendUploadScreenshotHandle remoteData ->
            -- Uploading of screenshots happened through JS, since it involves working
            -- with file blobs. This handler however is for post upload attempt
            -- (success or not), to set RemoteData accordingly.
            case model.syncStatus of
                SyncUploadScreenshot errorsCount _ ->
                    SubModelReturn
                        (SyncManager.Utils.determineSyncStatus activePage
                            { model | syncStatus = SyncUploadScreenshot errorsCount remoteData }
                        )
                        Cmd.none
                        noError
                        []

                _ ->
                    noChange

        -- We should never get here.
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
                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus })
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
                            ( backendRemoteData, indexDbRemoteData, uploadCmd ) =
                                if List.isEmpty result.entities then
                                    ( RemoteData.NotAsked
                                    , RemoteData.Success Nothing
                                    , Cmd.none
                                    )

                                else
                                    ( RemoteData.Loading
                                    , RemoteData.Success (Just result)
                                    , HttpBuilder.post (device.backendUrl ++ "/CreateSyncManagerHealthCenter")
                                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIndexDbQueryUploadAuthorityResultRecord dbVersion result)
                                        -- We don't need to decode anything, as we just want to have
                                        -- the browser download it.
                                        |> HttpBuilder.send (RemoteData.fromResult >> BackendUploadAuthorityHandle result)
                                    )

                            recordUpdated =
                                { record | backendRemoteData = backendRemoteData, indexDbRemoteData = indexDbRemoteData }

                            ( syncInfoAuthorities, setSyncInfoAurhoritiesCmd ) =
                                model.syncInfoAuthorities
                                    |> Maybe.map
                                        (\zipper ->
                                            let
                                                currentZipper =
                                                    Zipper.current zipper
                                            in
                                            if currentZipper.status == Uploading then
                                                ( model.syncInfoAuthorities, Cmd.none )

                                            else
                                                let
                                                    zipperUpdated =
                                                        Zipper.mapCurrent
                                                            (\old -> { old | status = Uploading })
                                                            zipper
                                                in
                                                ( Just zipperUpdated, sendSyncInfoAuthoritiesCmd zipper )
                                        )
                                    |> Maybe.withDefault ( model.syncInfoAuthorities, Cmd.none )

                            modelUpdated =
                                { model | syncStatus = SyncUploadAuthority recordUpdated, syncInfoAuthorities = syncInfoAuthorities }
                        in
                        SubModelReturn
                            (SyncManager.Utils.determineSyncStatus activePage modelUpdated)
                            (Cmd.batch [ uploadCmd, setSyncInfoAurhoritiesCmd ])
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

                                ( syncInfoAuthorities, setSyncInfoAurhoritiesCmd ) =
                                    model.syncInfoAuthorities
                                        |> Maybe.map
                                            (\zipper ->
                                                let
                                                    zipperUpdated =
                                                        Zipper.mapCurrent
                                                            (\old -> { old | status = Error })
                                                            zipper
                                                in
                                                ( Just zipperUpdated, sendSyncInfoAuthoritiesCmd zipperUpdated )
                                            )
                                        |> Maybe.withDefault ( model.syncInfoAuthorities, Cmd.none )

                                incidentDetailsMsg =
                                    resolveIncidentDetailsMsg error
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus activePage
                                    { model | syncStatus = syncStatus, syncInfoAuthorities = syncInfoAuthorities }
                                )
                                setSyncInfoAurhoritiesCmd
                                (maybeHttpError webData "Backend.SyncManager.Update" "BackendUploadAuthorityHandle")
                                []
                                |> sequenceSubModelReturn
                                    (update currentDate currentTime activePage dbVersion device)
                                    incidentDetailsMsg

                        RemoteData.Success _ ->
                            let
                                ( syncInfoAuthorities, setSyncInfoAurhoritiesCmd ) =
                                    model.syncInfoAuthorities
                                        |> Maybe.map
                                            (\zipper ->
                                                let
                                                    status =
                                                        if result.remaining == 0 then
                                                            Success

                                                        else
                                                            let
                                                                currentZipper =
                                                                    Zipper.current zipper
                                                            in
                                                            currentZipper.status

                                                    zipperUpdated =
                                                        Zipper.mapCurrent
                                                            (\old -> { old | status = status, remainingToUpload = result.remaining })
                                                            zipper
                                                in
                                                ( Just zipperUpdated, sendSyncInfoAuthoritiesCmd zipperUpdated )
                                            )
                                        |> Maybe.withDefault ( model.syncInfoAuthorities, Cmd.none )

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

                                uploadPhotosToDelete =
                                    Dict.keys result.uploadPhotos

                                subModelReturn =
                                    SubModelReturn
                                        (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus, syncInfoAuthorities = syncInfoAuthorities })
                                        (Cmd.batch [ cmd, setSyncInfoAurhoritiesCmd ])
                                        noError
                                        []
                            in
                            if List.isEmpty uploadPhotosToDelete then
                                subModelReturn

                            else
                                subModelReturn
                                    |> sequenceSubModelReturn
                                        (update currentDate currentTime activePage dbVersion device)
                                        [ QueryIndexDb <| IndexDbQueryRemoveUploadPhotos uploadPhotosToDelete ]

                        _ ->
                            -- Satisfy the compiler.
                            noChange

                _ ->
                    noChange

        -- We should never get here.
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
                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus })
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
                            ( backendRemoteData, indexDbRemoteData, uploadCmd ) =
                                if List.isEmpty result.entities then
                                    ( RemoteData.NotAsked
                                    , RemoteData.Success Nothing
                                    , Cmd.none
                                    )

                                else
                                    ( RemoteData.Loading
                                    , RemoteData.Success (Just result)
                                    , HttpBuilder.post (device.backendUrl ++ "/CreateSyncManager")
                                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIndexDbQueryUploadGeneralResultRecord dbVersion result)
                                        -- We don't need to decode anything, as we just want to have
                                        -- the browser download it.
                                        |> HttpBuilder.send (RemoteData.fromResult >> BackendUploadGeneralHandle result)
                                    )

                            recordUpdated =
                                { record | backendRemoteData = backendRemoteData, indexDbRemoteData = indexDbRemoteData }

                            syncInfoGeneral =
                                if model.syncInfoGeneral.status == Uploading then
                                    model.syncInfoGeneral

                                else
                                    model.syncInfoGeneral
                                        |> (\info -> { info | status = Uploading })

                            setSyncInfoGeneralCmd =
                                if model.syncInfoGeneral.status == Uploading then
                                    Cmd.none

                                else
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                            modelUpdated =
                                { model | syncStatus = SyncUploadGeneral recordUpdated, syncInfoGeneral = syncInfoGeneral }
                        in
                        SubModelReturn
                            (SyncManager.Utils.determineSyncStatus activePage modelUpdated)
                            (Cmd.batch [ uploadCmd, setSyncInfoGeneralCmd ])
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

                                syncInfoGeneral =
                                    model.syncInfoGeneral
                                        |> (\info -> { info | status = Error })

                                setSyncInfoGeneralCmd =
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                                incidentDetailsMsg =
                                    resolveIncidentDetailsMsg error
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus, syncInfoGeneral = syncInfoGeneral })
                                setSyncInfoGeneralCmd
                                (maybeHttpError webData "Backend.SyncManager.Update" "BackendUploadGeneralHandle")
                                []
                                |> sequenceSubModelReturn
                                    (update currentDate currentTime activePage dbVersion device)
                                    incidentDetailsMsg

                        RemoteData.Success _ ->
                            let
                                syncStatus =
                                    SyncUploadGeneral { record | backendRemoteData = RemoteData.Success () }

                                syncInfoGeneral =
                                    let
                                        status =
                                            if result.remaining == 0 then
                                                Success

                                            else
                                                model.syncInfoGeneral.status
                                    in
                                    model.syncInfoGeneral
                                        |> (\info -> { info | remainingToUpload = result.remaining, status = status })

                                setSyncInfoGeneralCmd =
                                    sendSyncInfoGeneralCmd syncInfoGeneral

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
                                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus, syncInfoGeneral = syncInfoGeneral })
                                (Cmd.batch [ cmd, setSyncInfoGeneralCmd ])
                                noError
                                []

                        _ ->
                            -- Satisfy the compiler.
                            noChange

                _ ->
                    noChange

        -- We should never get here.
        BackendUploadWhatsApp Nothing ->
            let
                syncStatus =
                    -- There are no entities for upload.
                    case model.syncStatus of
                        SyncUploadWhatsApp record ->
                            SyncUploadWhatsApp { record | indexDbRemoteData = RemoteData.Success Nothing }

                        _ ->
                            model.syncStatus
            in
            SubModelReturn
                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus })
                Cmd.none
                noError
                []

        BackendUploadWhatsApp (Just result) ->
            case model.syncStatus of
                SyncUploadWhatsApp record ->
                    if RemoteData.isLoading record.backendRemoteData then
                        -- We are already POSTing to the backend.
                        noChange

                    else
                        let
                            ( backendRemoteData, indexDbRemoteData, uploadCmd ) =
                                if List.isEmpty result.entities then
                                    ( RemoteData.NotAsked
                                    , RemoteData.Success Nothing
                                    , Cmd.none
                                    )

                                else
                                    ( RemoteData.Loading
                                    , RemoteData.Success (Just result)
                                    , HttpBuilder.post (device.backendUrl ++ "/api/sync")
                                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                                        |> withJsonBody (Json.Encode.object <| SyncManager.Encoder.encodeIndexDbQueryUploadWhatsAppResultRecord dbVersion result)
                                        |> HttpBuilder.send (RemoteData.fromResult >> BackendUploadWhatsAppHandle result)
                                    )

                            recordUpdated =
                                { record | backendRemoteData = backendRemoteData, indexDbRemoteData = indexDbRemoteData }

                            syncInfoGeneral =
                                if model.syncInfoGeneral.status == Uploading then
                                    model.syncInfoGeneral

                                else
                                    model.syncInfoGeneral
                                        |> (\info -> { info | status = Uploading })

                            setSyncInfoGeneralCmd =
                                if model.syncInfoGeneral.status == Uploading then
                                    Cmd.none

                                else
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                            modelUpdated =
                                { model | syncStatus = SyncUploadWhatsApp recordUpdated, syncInfoGeneral = syncInfoGeneral }
                        in
                        SubModelReturn
                            (SyncManager.Utils.determineSyncStatus activePage modelUpdated)
                            (Cmd.batch [ uploadCmd, setSyncInfoGeneralCmd ])
                            noError
                            []

                _ ->
                    noChange

        BackendUploadWhatsAppHandle result webData ->
            case model.syncStatus of
                SyncUploadWhatsApp record ->
                    case webData of
                        RemoteData.Failure error ->
                            let
                                syncStatus =
                                    SyncUploadWhatsApp { record | backendRemoteData = RemoteData.Failure error }

                                syncInfoGeneral =
                                    model.syncInfoGeneral
                                        |> (\info -> { info | status = Error })

                                setSyncInfoGeneralCmd =
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                                incidentDetailsMsg =
                                    resolveIncidentDetailsMsg error
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus activePage
                                    { model | syncStatus = syncStatus, syncInfoGeneral = syncInfoGeneral }
                                )
                                setSyncInfoGeneralCmd
                                (maybeHttpError webData "Backend.SyncManager.Update" "BackendUploadWhatsAppHandle")
                                []
                                |> sequenceSubModelReturn
                                    (update currentDate currentTime activePage dbVersion device)
                                    incidentDetailsMsg

                        RemoteData.Success _ ->
                            let
                                syncStatus =
                                    SyncUploadWhatsApp { record | backendRemoteData = RemoteData.Success () }

                                syncInfoGeneral =
                                    let
                                        status =
                                            if result.remaining == 0 then
                                                Success

                                            else
                                                model.syncInfoGeneral.status
                                    in
                                    model.syncInfoGeneral
                                        |> (\info -> { info | remainingToUpload = result.remaining, status = status })

                                setSyncInfoGeneralCmd =
                                    sendSyncInfoGeneralCmd syncInfoGeneral

                                -- We have successfully uploaded the entities, so
                                -- we can mark them as `isSynced`.
                                cmd =
                                    deleteEntitiesThatWereUploaded { type_ = "WhatsApp", localId = List.map .localId result.entities }
                            in
                            SubModelReturn
                                (SyncManager.Utils.determineSyncStatus activePage { model | syncStatus = syncStatus, syncInfoGeneral = syncInfoGeneral })
                                (Cmd.batch [ cmd, setSyncInfoGeneralCmd ])
                                noError
                                []

                        _ ->
                            -- Satisfy the compiler.
                            noChange

                _ ->
                    noChange

        BackendDeferredPhotoFetch Nothing ->
            let
                downloadPhotosStatus =
                    -- There are no deferred photos matching the query.
                    case model.downloadPhotosStatus of
                        DownloadPhotosInProcess (DownloadPhotosBatch record) ->
                            DownloadPhotosInProcess (DownloadPhotosBatch { record | indexDbRemoteData = RemoteData.Success Nothing })

                        DownloadPhotosInProcess (DownloadPhotosAll record) ->
                            DownloadPhotosInProcess (DownloadPhotosAll { record | indexDbRemoteData = RemoteData.Success Nothing })

                        _ ->
                            model.downloadPhotosStatus
            in
            SubModelReturn
                (SyncManager.Utils.determineDownloadPhotosStatus { model | downloadPhotosStatus = downloadPhotosStatus })
                Cmd.none
                noError
                []

        BackendDeferredPhotoFetch (Just result) ->
            let
                isLoading =
                    case model.downloadPhotosStatus of
                        DownloadPhotosInProcess (DownloadPhotosBatch record) ->
                            RemoteData.isLoading record.backendRemoteData

                        DownloadPhotosInProcess (DownloadPhotosAll record) ->
                            RemoteData.isLoading record.backendRemoteData

                        _ ->
                            False
            in
            if isLoading then
                noChange

            else
                let
                    downloadPhotosStatus =
                        case model.downloadPhotosStatus of
                            DownloadPhotosInProcess (DownloadPhotosBatch record) ->
                                let
                                    recordUpdated =
                                        { record
                                            | backendRemoteData = RemoteData.Loading
                                            , indexDbRemoteData = RemoteData.Success (Just result)
                                        }
                                in
                                DownloadPhotosInProcess (DownloadPhotosBatch recordUpdated)

                            DownloadPhotosInProcess (DownloadPhotosAll record) ->
                                let
                                    recordUpdated =
                                        { record
                                            | backendRemoteData = RemoteData.Loading
                                            , indexDbRemoteData = RemoteData.Success (Just result)
                                        }
                                in
                                DownloadPhotosInProcess (DownloadPhotosAll recordUpdated)

                            _ ->
                                model.downloadPhotosStatus

                    modelUpdated =
                        { model | downloadPhotosStatus = downloadPhotosStatus }

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
                    (SyncManager.Utils.determineDownloadPhotosStatus modelUpdated)
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
                            downloadPhotosStatus =
                                case model.downloadPhotosStatus of
                                    DownloadPhotosInProcess (DownloadPhotosBatch deferredPhoto) ->
                                        let
                                            deferredPhotoUpdated =
                                                { deferredPhoto
                                                  -- Reduce the batch counter.
                                                    | batchCounter = deferredPhoto.batchCounter - 1
                                                    , backendRemoteData = RemoteData.Failure error
                                                }
                                        in
                                        DownloadPhotosInProcess (DownloadPhotosBatch deferredPhotoUpdated)

                                    DownloadPhotosInProcess (DownloadPhotosAll deferredPhoto) ->
                                        let
                                            deferredPhotoUpdated =
                                                { deferredPhoto | backendRemoteData = RemoteData.Failure error }
                                        in
                                        DownloadPhotosInProcess (DownloadPhotosAll deferredPhotoUpdated)

                                    _ ->
                                        model.downloadPhotosStatus
                        in
                        update
                            currentDate
                            currentTime
                            activePage
                            dbVersion
                            device
                            (QueryIndexDb <| IndexDbQueryUpdateDeferredPhotoAttempts result)
                            { model | downloadPhotosStatus = downloadPhotosStatus }

                RemoteData.Success queryResult ->
                    let
                        downloadPhotosStatus =
                            case model.downloadPhotosStatus of
                                DownloadPhotosInProcess (DownloadPhotosBatch deferredPhoto) ->
                                    let
                                        deferredPhotoUpdated =
                                            { deferredPhoto
                                              -- Reduce the batch counter.
                                                | batchCounter = deferredPhoto.batchCounter - 1
                                                , backendRemoteData = RemoteData.Success queryResult
                                            }
                                    in
                                    DownloadPhotosInProcess (DownloadPhotosBatch deferredPhotoUpdated)

                                DownloadPhotosInProcess (DownloadPhotosAll deferredPhoto) ->
                                    let
                                        deferredPhotoUpdated =
                                            { deferredPhoto | backendRemoteData = RemoteData.Success queryResult }
                                    in
                                    DownloadPhotosInProcess (DownloadPhotosAll deferredPhotoUpdated)

                                _ ->
                                    model.downloadPhotosStatus
                    in
                    -- We've fetched the image, so we can remove the record from
                    -- `deferredPhotos` table.
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        (QueryIndexDb <| IndexDbQueryRemoveDeferredPhoto result.uuid)
                        { model | downloadPhotosStatus = downloadPhotosStatus }

                _ ->
                    -- Satisfy the compiler.
                    noChange

        QueryIndexDb indexDbQueryType ->
            let
                record =
                    case indexDbQueryType of
                        IndexDbQueryUploadPhoto ->
                            let
                                -- Send the device info so on JS, we'd know how
                                -- to contact the backend.
                                encodedData =
                                    Device.Encoder.encode device
                                        |> Json.Encode.encode 0
                            in
                            { queryType = "IndexDbQueryUploadPhoto"
                            , data = Just encodedData
                            }

                        IndexDbQueryUploadScreenshot ->
                            let
                                -- Send the device info so on JS, we'd know how
                                -- to contact the backend.
                                encodedData =
                                    Device.Encoder.encode device
                                        |> Json.Encode.encode 0
                            in
                            { queryType = "IndexDbQueryUploadScreenshot"
                            , data = Just encodedData
                            }

                        IndexDbQueryUploadGeneral ->
                            { queryType = "IndexDbQueryUploadGeneral"
                            , data = Nothing
                            }

                        IndexDbQueryUploadWhatsApp ->
                            { queryType = "IndexDbQueryUploadWhatsApp"
                            , data = Nothing
                            }

                        IndexDbQueryUploadAuthority uuid ->
                            { queryType = "IndexDbQueryUploadAuthority"
                            , data = Just uuid
                            }

                        IndexDbQueryDeferredPhoto ->
                            { queryType = "IndexDbQueryDeferredPhoto"
                            , data = Nothing
                            }

                        IndexDbQueryRemoveDeferredPhoto uuid ->
                            { queryType = "IndexDbQueryRemoveDeferredPhoto"
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

                        IndexDbQueryRemoveUploadPhotos uuids ->
                            let
                                uuidsAsString =
                                    uuids
                                        |> List.map String.fromInt
                                        |> List.intersperse ","
                                        |> String.concat
                            in
                            { queryType = "IndexDbQueryRemoveUploadPhotos"
                            , data = Just uuidsAsString
                            }

                        IndexDbQueryGetTotalEntriesToUpload ->
                            { queryType = "IndexDbQueryGetTotalEntriesToUpload"
                            , data = Nothing
                            }

                        IndexDbQueryGetShardsEntityByUuid uuidsAsString ->
                            { queryType = "IndexDbQueryGetShardsEntityByUuid"
                            , data = Just uuidsAsString
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
                        IndexDbQueryUploadPhotoResult remoteData ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendUploadPhotoHandle remoteData)
                                model

                        IndexDbQueryUploadScreenshotResult remoteData ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendUploadScreenshotHandle remoteData)
                                model

                        IndexDbQueryUploadAuthorityResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendUploadAuthority result)
                                model

                        IndexDbQueryUploadGeneralResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendUploadGeneral result)
                                model

                        IndexDbQueryUploadWhatsAppResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendUploadWhatsApp result)
                                model

                        IndexDbQueryDeferredPhotoResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendDeferredPhotoFetch result)
                                model

                        IndexDbQueryGetTotalEntriesToUploadResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendReportState result)
                                model

                        IndexDbQueryGetShardsEntityByUuidResult result ->
                            update
                                currentDate
                                currentTime
                                activePage
                                dbVersion
                                device
                                (BackendReportIncidentDetails result)
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
                        (decoderError "Backend.SyncManager.Update" location error)
                        []

        SavedAtIndexDbHandle val ->
            case decodeValue SyncManager.Decoder.decodeIndexDbSaveResult val of
                Ok indexDbSaveResult ->
                    case indexDbSaveResult.status of
                        IndexDbSaveSuccess ->
                            case indexDbSaveResult.table of
                                IndexDbSaveResultTableAutority ->
                                    update
                                        currentDate
                                        currentTime
                                        activePage
                                        dbVersion
                                        device
                                        (BackendAuthorityFetchedDataSavedHandle indexDbSaveResult.timestamp)
                                        model

                                IndexDbSaveResultTableGeneral ->
                                    update
                                        currentDate
                                        currentTime
                                        activePage
                                        dbVersion
                                        device
                                        (BackendGeneralFetchedDataSavedHandle indexDbSaveResult.timestamp)
                                        model

                                _ ->
                                    noChange

                        IndexDbSaveFailure ->
                            -- For now, we don't make any special handling,
                            -- so when request times out, we will retry.
                            noChange

                Err error ->
                    SubModelReturn
                        model
                        Cmd.none
                        (decoderError "Backend.SyncManager.Update" "SavedAtIndexDbHandle" error)
                        []

        ResetSettings ->
            let
                syncSpeed =
                    { idle = 5 * 60 * 1000
                    , cycle = 50
                    , offline = 30 * 1000
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

                -- Safe guard against too low values.
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

        TrySyncing ->
            case model.syncStatus of
                SyncIdle ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendFetchMain
                        model

                _ ->
                    -- Sync is already in progress.
                    noChange

        TryDownloadingPhotos ->
            case model.downloadPhotosStatus of
                DownloadPhotosIdle ->
                    update
                        currentDate
                        currentTime
                        activePage
                        dbVersion
                        device
                        BackendFetchPhotos
                        model

                _ ->
                    -- Sync is already in progress.
                    noChange


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        backendFetchCmds =
            case model.syncCycle of
                SyncManager.Model.SyncCyclePause ->
                    []

                _ ->
                    [ Time.every (getSyncSpeedForSubscriptions model) (always BackendFetchMain)
                    , Time.every (getDownloadPhotosSpeedForSubscriptions model) (always BackendFetchPhotos)
                    ]
    in
    Sub.batch <|
        [ getFromIndexDb QueryIndexDbHandle
        , savedAtIndexedDb SavedAtIndexDbHandle
        ]
            ++ backendFetchCmds


{-| Send to JS data we have synced, e.g. `person`, `health center`, etc.
-}
port sendSyncedDataToIndexDb : { table : String, data : List String, shard : String, timestamp : String } -> Cmd msg


{-| Send to JS the information about General sync.
-}
port sendSyncInfoGeneral : SyncInfoGeneralForPort -> Cmd msg


{-| Send to JS the information about Autohorities sync.
-}
port sendSyncInfoAuthorities : List SyncInfoAuthorityForPort -> Cmd msg


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


port refreshPage : () -> Cmd msg


{-| Get data requested from IndexDB.

For now we don't care who asked for the data, we just fill it in where
needed.

-}
port getFromIndexDb : (Value -> msg) -> Sub msg


{-| Reports that save to IndexDB operation was successful.
-}
port savedAtIndexedDb : (Value -> msg) -> Sub msg
