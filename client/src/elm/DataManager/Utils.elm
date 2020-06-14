module DataManager.Utils exposing
    ( determineSyncStatus
    , encodeBackendGeneralEntityToSend
    , getBackendAuthorityEntityIdentifier
    , getBackendGeneralEntityIdentifier
    , getDataToSendAuthority
    , getDataToSendGeneral
    , getPhotoFromBackendAuthorityEntity
    , getSyncSpeedForSubscriptions
    )

import Backend.Clinic.Encoder
import Backend.Counseling.Encoder
import Backend.HealthCenter.Encoder
import Backend.IndividualEncounterParticipant.Encoder
import Backend.Measurement.Encoder
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Nurse.Encoder
import Backend.NutritionEncounter.Encoder
import Backend.ParticipantConsent.Encoder
import Backend.Person.Encoder
import Backend.PmtctParticipant.Encoder
import Backend.Relationship.Encoder
import DataManager.Encoder
import DataManager.Model exposing (BackendAuthorityEntity(..), BackendEntity, BackendEntityIdentifier, BackendGeneralEntity(..), DownloadPhotos(..), Model, SyncStatus(..), emptyDownloadPhotosBatchRec, emptyUploadRec)
import Editable
import Json.Encode exposing (Value, object)
import List.Zipper as Zipper
import RemoteData
import Utils.WebData


{-| Decide on the Sync status. Either keep the exiting one, or set the next one,
according to the order `SyncStatus` is defined.
-}
determineSyncStatus : Model -> Model
determineSyncStatus model =
    let
        syncCycleRotate =
            case model.syncCycle of
                DataManager.Model.SyncCycleOn ->
                    True

                _ ->
                    False
    in
    if syncCycleRotate then
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
    let
        getIdentifier identifier type_ =
            { uuid = identifier.uuid
            , revision = identifier.revision
            , type_ = type_
            }
    in
    case backendGeneralEntity of
        BackendGeneralCatchmentArea identifier ->
            getIdentifier identifier "catchment_area"

        BackendGeneralCounselingSchedule identifier ->
            getIdentifier identifier "counseling_schedule"

        BackendGeneralCounselingTopic identifier ->
            getIdentifier identifier "counseling_topic"

        BackendGeneralHealthCenter identifier ->
            getIdentifier identifier "health_center"

        BackendGeneralNurse identifier ->
            getIdentifier identifier "nurse"

        BackendGeneralParticipantForm identifier ->
            getIdentifier identifier "participant_form"

        BackendGeneralPmtctParticipant identifier ->
            getIdentifier identifier "pmtct_participant"

        BackendGeneralRelationship identifier ->
            getIdentifier identifier "relationship"

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
    let
        getIdentifier identifier type_ =
            { uuid = identifier.uuid
            , revision = identifier.revision
            , type_ = type_
            }
    in
    case backendAuthorityEntity of
        BackendAuthorityAttendance identifier ->
            getIdentifier identifier "attendance"

        BackendAuthorityBreastExam identifier ->
            getIdentifier identifier "breast_exam"

        BackendAuthorityClinic identifier ->
            getIdentifier identifier "clinic"

        BackendAuthorityChildFbf identifier ->
            getIdentifier identifier "child_fbf"

        BackendAuthorityCounselingSession identifier ->
            getIdentifier identifier "counseling_session"

        BackendAuthorityCorePhysicalExam identifier ->
            getIdentifier identifier "counseling_session"

        BackendAuthorityDangerSigns identifier ->
            getIdentifier identifier "danger_signs"

        BackendAuthorityFamilyPlanning identifier ->
            getIdentifier identifier "family_planning"

        BackendAuthorityHeight identifier ->
            getIdentifier identifier "height"

        BackendAuthorityIndividualParticipant identifier ->
            getIdentifier identifier "individual_participant"

        BackendAuthorityLactation identifier ->
            getIdentifier identifier "lactation"

        BackendAuthorityLastMenstrualPeriod identifier ->
            getIdentifier identifier "last_menstrual_period"

        BackendAuthorityMedicalHistory identifier ->
            getIdentifier identifier "medical_history"

        BackendAuthorityMedication identifier ->
            getIdentifier identifier "medication"

        BackendAuthorityMotherFbf identifier ->
            getIdentifier identifier "mother_fbf"

        BackendAuthorityMuac identifier ->
            getIdentifier identifier "muac"

        BackendAuthorityNutrition identifier ->
            getIdentifier identifier "nutrition"

        BackendAuthorityNutritionEncounter identifier ->
            getIdentifier identifier "nutrition_encounter"

        BackendAuthorityNutritionHeight identifier ->
            getIdentifier identifier "nutrition_height"

        BackendAuthorityNutritionMuac identifier ->
            getIdentifier identifier "nutrition_muac"

        BackendAuthorityNutritionNutrition identifier ->
            getIdentifier identifier "nutrition_nutrition"

        BackendAuthorityNutritionPhoto identifier ->
            getIdentifier identifier "nutrition_photo"

        BackendAuthorityNutritionWeight identifier ->
            getIdentifier identifier "nutrition_weight"

        BackendAuthorityObstetricHistory identifier ->
            getIdentifier identifier "obstetric_history"

        BackendAuthorityObstetricHistoryStep2 identifier ->
            getIdentifier identifier "obstetric_history_step2"

        BackendAuthorityObstetricalExam identifier ->
            getIdentifier identifier "obstetrical_exam"

        BackendAuthorityParticipantConsent identifier ->
            getIdentifier identifier "participant_consent"

        BackendAuthorityPerson identifier ->
            getIdentifier identifier "person"

        BackendAuthorityPhoto identifier ->
            getIdentifier identifier "photo"

        BackendAuthorityWeight identifier ->
            getIdentifier identifier "weight"

        BackendAuthorityEntityUnknown uuid revision ->
            { uuid = uuid
            , revision = revision
            , type_ = "unknown"
            }


{-| Return a photo from a "Authority" entity.
-}
getPhotoFromBackendAuthorityEntity : BackendAuthorityEntity -> Maybe String
getPhotoFromBackendAuthorityEntity backendAuthorityEntity =
    let
        getPhotoFromMeasurement identifier =
            let
                (PhotoUrl url) =
                    identifier.entity.value
            in
            Just url
    in
    case backendAuthorityEntity of
        BackendAuthorityPerson identifier ->
            identifier.entity.avatarUrl

        BackendAuthorityPhoto identifier ->
            getPhotoFromMeasurement identifier

        BackendAuthorityNutritionPhoto identifier ->
            getPhotoFromMeasurement identifier

        _ ->
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


getDataToSendGeneral : BackendGeneralEntity -> List String -> List String
getDataToSendGeneral entity accum =
    case entity of
        BackendGeneralCatchmentArea identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                Backend.HealthCenter.Encoder.encodeCatchmentArea

        BackendGeneralCounselingSchedule identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                Backend.Counseling.Encoder.encodeCounselingSchedule

        BackendGeneralHealthCenter identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                Backend.HealthCenter.Encoder.encodeHealthCenter

        BackendGeneralCounselingTopic identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                (Json.Encode.object << Backend.Counseling.Encoder.encodeCounselingTopic)

        BackendGeneralNurse identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                (Json.Encode.object << Backend.Nurse.Encoder.encodeNurse)

        BackendGeneralParticipantForm identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                (Json.Encode.object << Backend.ParticipantConsent.Encoder.encodeParticipantForm)

        BackendGeneralPmtctParticipant identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                Backend.PmtctParticipant.Encoder.encodePmtctParticipant

        BackendGeneralRelationship identifier ->
            encodeBackendGeneralEntityToSend
                identifier
                accum
                Backend.Relationship.Encoder.encodeRelationship

        BackendGeneralEntityUnknown _ _ ->
            -- Filter out the unknown entities.
            accum


getDataToSendAuthority : BackendAuthorityEntity -> List String -> List String
getDataToSendAuthority entity accum =
    case entity of
        BackendAuthorityAttendance identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityBreastExam identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityClinic identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityChildFbf identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityCounselingSession identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityCorePhysicalExam identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityDangerSigns identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityFamilyPlanning identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityHeight identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityIndividualParticipant identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityLactation identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityLastMenstrualPeriod identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityMedicalHistory identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityMedication identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityMotherFbf identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityMuac identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutrition identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionEncounter identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionHeight identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionMuac identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionNutrition identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionPhoto identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityNutritionWeight identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityObstetricHistory identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityObstetricHistoryStep2 identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityObstetricalExam identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityParticipantConsent identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityPerson identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityPhoto identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityWeight identifier ->
            encodeBackendAuthorityEntityToSend
                identifier
                accum

        BackendAuthorityEntityUnknown _ _ ->
            -- Filter out the unknown entities.
            accum


encodeBackendGeneralEntity : BackendGeneralEntity -> Value
encodeBackendGeneralEntity backendGeneralEntity =
    case backendGeneralEntity of
        BackendGeneralCatchmentArea identifier ->
            Backend.HealthCenter.Encoder.encodeCatchmentArea identifier.entity

        BackendGeneralCounselingSchedule identifier ->
            Backend.Counseling.Encoder.encodeCounselingSchedule identifier.entity

        BackendGeneralCounselingTopic identifier ->
            (object << Backend.Counseling.Encoder.encodeCounselingTopic) identifier.entity

        BackendGeneralHealthCenter identifier ->
            Backend.HealthCenter.Encoder.encodeHealthCenter identifier.entity

        BackendGeneralNurse identifier ->
            (object << Backend.Nurse.Encoder.encodeNurse) identifier.entity

        BackendGeneralParticipantForm identifier ->
            (object << Backend.ParticipantConsent.Encoder.encodeParticipantForm) identifier.entity

        BackendGeneralPmtctParticipant identifier ->
            Backend.PmtctParticipant.Encoder.encodePmtctParticipant identifier.entity

        BackendGeneralRelationship identifier ->
            Backend.Relationship.Encoder.encodeRelationship identifier.entity

        BackendGeneralEntityUnknown string int ->
            object []


encodeBackendAuthorityEntity : BackendAuthorityEntity -> Value
encodeBackendAuthorityEntity entity =
    case entity of
        BackendAuthorityAttendance identifier ->
            (object << Backend.Measurement.Encoder.encodeAttendance) identifier.entity

        BackendAuthorityBreastExam identifier ->
            (object << Backend.Measurement.Encoder.encodeBreastExam) identifier.entity

        BackendAuthorityClinic identifier ->
            (object << Backend.Clinic.Encoder.encodeClinic) identifier.entity

        BackendAuthorityChildFbf identifier ->
            (object << Backend.Measurement.Encoder.encodeFbf) identifier.entity

        BackendAuthorityCounselingSession identifier ->
            (object << Backend.Measurement.Encoder.encodeCounselingSession) identifier.entity

        BackendAuthorityCorePhysicalExam identifier ->
            (object << Backend.Measurement.Encoder.encodeCorePhysicalExam) identifier.entity

        BackendAuthorityDangerSigns identifier ->
            (object << Backend.Measurement.Encoder.encodeDangerSigns) identifier.entity

        BackendAuthorityFamilyPlanning identifier ->
            (object << Backend.Measurement.Encoder.encodeFamilyPlanning) identifier.entity

        BackendAuthorityHeight identifier ->
            (object << Backend.Measurement.Encoder.encodeHeight) identifier.entity

        BackendAuthorityIndividualParticipant identifier ->
            Backend.IndividualEncounterParticipant.Encoder.encodeIndividualEncounterParticipant identifier.entity

        BackendAuthorityLactation identifier ->
            (object << Backend.Measurement.Encoder.encodeLactation) identifier.entity

        BackendAuthorityLastMenstrualPeriod identifier ->
            (object << Backend.Measurement.Encoder.encodeLastMenstrualPeriod) identifier.entity

        BackendAuthorityMedicalHistory identifier ->
            (object << Backend.Measurement.Encoder.encodeMedicalHistory) identifier.entity

        BackendAuthorityMedication identifier ->
            (object << Backend.Measurement.Encoder.encodeMedication) identifier.entity

        BackendAuthorityMotherFbf identifier ->
            (object << Backend.Measurement.Encoder.encodeFbf) identifier.entity

        BackendAuthorityMuac identifier ->
            (object << Backend.Measurement.Encoder.encodeMuac) identifier.entity

        BackendAuthorityNutrition identifier ->
            (object << Backend.Measurement.Encoder.encodeNutrition) identifier.entity

        BackendAuthorityNutritionEncounter identifier ->
            (object << Backend.NutritionEncounter.Encoder.encodeNutritionEncounter) identifier.entity

        BackendAuthorityNutritionHeight identifier ->
            (object << Backend.Measurement.Encoder.encodeNutritionHeight) identifier.entity

        BackendAuthorityNutritionMuac identifier ->
            (object << Backend.Measurement.Encoder.encodeNutritionMuac) identifier.entity

        BackendAuthorityNutritionNutrition identifier ->
            (object << Backend.Measurement.Encoder.encodeNutritionNutrition) identifier.entity

        BackendAuthorityNutritionPhoto identifier ->
            (object << Backend.Measurement.Encoder.encodeNutritionPhoto) identifier.entity

        BackendAuthorityNutritionWeight identifier ->
            (object << Backend.Measurement.Encoder.encodeNutritionWeight) identifier.entity

        BackendAuthorityObstetricHistory identifier ->
            (object << Backend.Measurement.Encoder.encodeObstetricHistory) identifier.entity

        BackendAuthorityObstetricHistoryStep2 identifier ->
            (object << Backend.Measurement.Encoder.encodeObstetricHistoryStep2) identifier.entity

        BackendAuthorityObstetricalExam identifier ->
            (object << Backend.Measurement.Encoder.encodeObstetricalExam) identifier.entity

        BackendAuthorityParticipantConsent identifier ->
            (object << Backend.Measurement.Encoder.encodeParticipantConsent) identifier.entity

        BackendAuthorityPerson identifier ->
            (object << Backend.Person.Encoder.encodePerson) identifier.entity

        BackendAuthorityPhoto identifier ->
            (object << Backend.Measurement.Encoder.encodePhoto) identifier.entity

        BackendAuthorityWeight identifier ->
            (object << Backend.Measurement.Encoder.encodeWeight) identifier.entity

        BackendAuthorityEntityUnknown _ _ ->
            -- Filter out the unknown entities.
            object []


encodeBackendGeneralEntityToSend : BackendGeneralEntity -> List String -> List String
encodeBackendGeneralEntityToSend entity accum =
    let
        identifier =
            getBackendGeneralEntityIdentifier entity
    in
    (Json.Encode.object
        [ ( "uuid", Json.Encode.string identifier.uuid )
        , ( "vid", Json.Encode.int identifier.revision )
        , ( "entity", encodeBackendGeneralEntity entity )
        ]
        |> Json.Encode.encode 0
    )
        :: accum


encodeBackendAuthorityEntityToSend : BackendAuthorityEntity -> List String -> List String
encodeBackendAuthorityEntityToSend entity accum =
    let
        identifier =
            getBackendAuthorityEntityIdentifier entity
    in
    (Json.Encode.object
        [ ( "uuid", Json.Encode.string identifier.uuid )
        , ( "vid", Json.Encode.int identifier.revision )
        , ( "entity", encodeBackendAuthorityEntity entity )
        ]
        |> Json.Encode.encode 0
    )
        :: accum
