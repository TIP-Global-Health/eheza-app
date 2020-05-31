module DataManager.Utils exposing
    ( determineSyncStatus
    , getBackendAuthorityEntityIdentifier
    , getBackendGeneralEntityIdentifier
    , getPhotoFromBackendAuthorityEntity
    , getPhotoFromBackendGeneralEntity
    , getSyncSpeedForSubscriptions
    )

import Backend.Measurement.Model exposing (PhotoUrl(..))
import DataManager.Model exposing (BackendAuthorityEntity(..), BackendEntityIdentifier, BackendGeneralEntity(..), DownloadPhotos(..), Model, SyncStatus(..), emptyDownloadPhotosBatchRec, emptyUploadRec)
import Editable
import List.Zipper as Zipper
import RemoteData
import Utils.WebData


{-| Decide on the Sync status. Either keep the exiting one, or set the next one,
according to the order `SyncStatus` is defined.
-}
determineSyncStatus : Model -> Model
determineSyncStatus model =
    if model.syncCycle then
        let
            syncStatus =
                model.syncStatus

            revisionIdPerAuthorityZipper =
                model.revisionIdPerAuthorityZipper

            noChange =
                ( syncStatus, revisionIdPerAuthorityZipper )

            ( syncStatusUpdated, revisionIdPerAuthorityZipperUpdated ) =
                case syncStatus of
                    SyncIdle ->
                        ( SyncUploadPhotoGeneral RemoteData.NotAsked, revisionIdPerAuthorityZipper )

                    SyncUploadPhotoGeneral webData ->
                        case webData of
                            RemoteData.Success maybeData ->
                                case maybeData of
                                    Just data ->
                                        -- We still have date.
                                        noChange

                                    Nothing ->
                                        -- No more photos to upload.
                                        ( SyncUploadGeneral emptyUploadRec, revisionIdPerAuthorityZipper )

                            _ ->
                                noChange

                    SyncUploadGeneral record ->
                        if record.indexDbRemoteData == RemoteData.Success Nothing then
                            -- We tried to fetch entities for upload from IndexDB,
                            -- but there we non matching the query.
                            ( SyncDownloadGeneral RemoteData.NotAsked, revisionIdPerAuthorityZipper )

                        else
                            noChange

                    SyncDownloadGeneral webData ->
                        case webData of
                            RemoteData.Success data ->
                                if List.isEmpty data.entities then
                                    -- We tried to fetch, but there was no more data.
                                    -- Next we try authorities.
                                    ( SyncDownloadAuthority RemoteData.NotAsked
                                    , revisionIdPerAuthorityZipper
                                    )

                                else
                                    -- Still have data to download.
                                    noChange

                            _ ->
                                noChange

                    SyncDownloadAuthority webData ->
                        case ( model.revisionIdPerAuthorityZipper, webData ) of
                            ( Nothing, _ ) ->
                                -- There are no authorities, so we can set the next
                                -- status.
                                ( SyncDownloadPhotos model.downloadPhotos
                                , revisionIdPerAuthorityZipper
                                )

                            ( Just zipper, RemoteData.Success data ) ->
                                let
                                    syncDownloadPhotos =
                                        resetDownloadPhotosBatchCounter model
                                in
                                if List.isEmpty data.entities then
                                    -- We tried to fetch, but there was no more data.
                                    -- Check if this is the last element.
                                    if Zipper.isLast zipper then
                                        ( syncDownloadPhotos
                                        , Just (Zipper.first zipper)
                                        )

                                    else
                                        -- Go to the next authority if there is
                                        -- otherwise, to the next status
                                        case Zipper.next zipper of
                                            Just nextZipper ->
                                                ( SyncDownloadAuthority RemoteData.NotAsked
                                                , Just nextZipper
                                                )

                                            Nothing ->
                                                -- We've reached the last element
                                                -- so reset it back, and rotate
                                                -- to the next status.
                                                ( syncDownloadPhotos
                                                , Just (Zipper.first zipper)
                                                )

                                else
                                    -- Still have data to download.
                                    noChange

                            _ ->
                                noChange

                    SyncDownloadPhotos record ->
                        case record of
                            DownloadPhotosNone ->
                                ( SyncIdle, revisionIdPerAuthorityZipper )

                            DownloadPhotosBatch deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    ( SyncIdle, revisionIdPerAuthorityZipper )

                                else if deferredPhoto.batchCounter < 1 then
                                    -- We've reached the end of the batch, so we
                                    -- need to rotate.
                                    ( SyncIdle, revisionIdPerAuthorityZipper )

                                else
                                    noChange

                            DownloadPhotosAll deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    ( SyncIdle, revisionIdPerAuthorityZipper )

                                else
                                    -- There are still deferred photos in IndexDB
                                    -- that match out query.
                                    noChange
        in
        { model
            | syncStatus = syncStatusUpdated
            , revisionIdPerAuthorityZipper = revisionIdPerAuthorityZipperUpdated
        }

    else
        -- No change.
        model


resetDownloadPhotosBatchCounter : Model -> SyncStatus
resetDownloadPhotosBatchCounter model =
    case model.downloadPhotos of
        DownloadPhotosBatch deferredPhoto ->
            let
                deferredPhotoUpdated =
                    { deferredPhoto | batchCounter = deferredPhoto.batchSize }
            in
            SyncDownloadPhotos (DownloadPhotosBatch deferredPhotoUpdated)

        _ ->
            SyncDownloadPhotos model.downloadPhotos


{-| Get info about an entity. `revision` would be the Drupal revision
in case of download, or the `localId` in case of upload.
-}
getBackendGeneralEntityIdentifier : BackendGeneralEntity -> BackendEntityIdentifier
getBackendGeneralEntityIdentifier backendGeneralEntity =
    case backendGeneralEntity of
        BackendGeneralCatchmentArea uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "catchment_area"
            }

        BackendGeneralHealthCenter uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "health_center"
            }

        BackendGeneralNurse uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "nurse"
            }

        BackendGeneralPerson uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "person"
            }

        BackendGeneralPmtctParticipant uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "pmtct_participant"
            }

        BackendGeneralRelationship uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "relationship"
            }

        BackendGeneralEntityUnknown uuid revision ->
            { uuid = uuid
            , revision = revision
            , type_ = "unknown"
            }


{-| Get info about an "Authority" entity. `revision` would be the Drupal revision
in case of download, or the `localId` in case of upload.
-}
getBackendAuthorityEntityIdentifier : BackendAuthorityEntity -> BackendEntityIdentifier
getBackendAuthorityEntityIdentifier backendAuthorityEntity =
    case backendAuthorityEntity of
        BackendAuthorityAttendance uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "attendance"
            }

        BackendAuthorityBreastExam uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "breast_exam"
            }

        BackendAuthorityChildFbf uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "child_fbf"
            }

        BackendAuthorityNutritionPhoto uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "nutrition_photo"
            }

        BackendAuthorityPhoto uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "photo"
            }

        BackendAuthorityWeight uuid revision _ ->
            { uuid = uuid
            , revision = revision
            , type_ = "weight"
            }

        BackendAuthorityEntityUnknown uuid revision ->
            { uuid = uuid
            , revision = revision
            , type_ = "unknown"
            }


{-| Return a photo from a "General" entity.

Not all entities have a photo, and even if they do, it might be a Maybe value
(for example the `avatar` of a `Person` entity).

-}
getPhotoFromBackendGeneralEntity : BackendGeneralEntity -> Maybe String
getPhotoFromBackendGeneralEntity backendGeneralEntity =
    case backendGeneralEntity of
        BackendGeneralPerson _ _ person ->
            person.avatarUrl

        BackendGeneralCatchmentArea string int catchmentArea ->
            Nothing

        BackendGeneralHealthCenter string int healthCenter ->
            Nothing

        BackendGeneralNurse string int nurse ->
            Nothing

        BackendGeneralPmtctParticipant string int pmtctParticipant ->
            Nothing

        BackendGeneralRelationship string int relationship ->
            Nothing

        BackendGeneralEntityUnknown string int ->
            Nothing


{-| Return a photo from a "Authority" entity.
-}
getPhotoFromBackendAuthorityEntity : BackendAuthorityEntity -> Maybe String
getPhotoFromBackendAuthorityEntity backendAuthorityEntity =
    let
        getPhotoFromMeasurement entity_ =
            let
                (PhotoUrl url) =
                    entity_.value
            in
            Just url
    in
    case backendAuthorityEntity of
        BackendAuthorityPhoto _ _ entity ->
            getPhotoFromMeasurement entity

        BackendAuthorityNutritionPhoto _ _ entity ->
            getPhotoFromMeasurement entity

        BackendAuthorityAttendance string int attendance ->
            Nothing

        BackendAuthorityChildFbf string int fbf ->
            Nothing

        BackendAuthorityWeight string int weight ->
            Nothing

        BackendAuthorityEntityUnknown string int ->
            Nothing

        BackendAuthorityBreastExam string int breastExam ->
            Nothing


getSyncSpeedForSubscriptions : Model -> Float
getSyncSpeedForSubscriptions model =
    let
        syncSpeed =
            model.syncSpeed
                -- Take the original values.
                |> Editable.cancel
                |> Editable.value

        syncCycle =
            if syncSpeed.cycle < 50 then
                -- Safeguard against too quick iterations, in case someone
                -- changed values directly on localStorage.
                50

            else
                toFloat syncSpeed.cycle

        checkWebData webData =
            case webData of
                RemoteData.Failure error ->
                    if Utils.WebData.isNetworkError error then
                        if syncSpeed.offline < 1000 then
                            1000

                        else
                            toFloat syncSpeed.offline

                    else
                        syncCycle

                _ ->
                    syncCycle
    in
    case model.syncStatus of
        SyncIdle ->
            -- Rest until the next sync loop.
            if syncSpeed.idle < 3000 then
                -- Safeguard against too quick iterations.
                3000

            else
                toFloat syncSpeed.idle

        SyncUploadGeneral record ->
            checkWebData record.backendRemoteData

        SyncDownloadGeneral webData ->
            checkWebData webData

        SyncDownloadAuthority webData ->
            checkWebData webData

        SyncDownloadPhotos downloadPhotos ->
            case downloadPhotos of
                DownloadPhotosNone ->
                    syncCycle

                DownloadPhotosBatch record ->
                    checkWebData record.backendRemoteData

                DownloadPhotosAll record ->
                    checkWebData record.backendRemoteData

        _ ->
            syncCycle
