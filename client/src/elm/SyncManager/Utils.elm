module SyncManager.Utils exposing
    ( determineSyncStatus
    , encodeBackendAuthorityEntity
    , encodeBackendGeneralEntity
    , getBackendAuthorityEntityIdentifier
    , getBackendGeneralEntityIdentifier
    , getDataToSendAuthority
    , getDataToSendGeneral
    , getPhotoFromBackendAuthorityEntity
    , getSyncSpeedForSubscriptions
    , getSyncedHealthCenters
    )

import Backend.AcuteIllnessEncounter.Encoder
import Backend.Clinic.Encoder
import Backend.Counseling.Encoder
import Backend.Dashboard.Encoder
import Backend.HealthCenter.Encoder
import Backend.IndividualEncounterParticipant.Encoder
import Backend.Measurement.Encoder
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Nurse.Encoder
import Backend.NutritionEncounter.Encoder
import Backend.ParticipantConsent.Encoder
import Backend.Person.Encoder
import Backend.PmtctParticipant.Encoder
import Backend.PrenatalEncounter.Encoder
import Backend.Relationship.Encoder
import Backend.Session.Encoder
import Backend.Village.Encoder
import Editable
import Json.Encode exposing (Value, object)
import List.Zipper as Zipper
import RemoteData
import SyncManager.Model exposing (BackendAuthorityEntity(..), BackendEntity, BackendEntityIdentifier, BackendGeneralEntity(..), DownloadPhotos(..), Model, SyncStatus(..), emptyDownloadPhotosBatchRec, emptyUploadRec)
import Utils.WebData


{-| Decide on the Sync status. Either keep the exiting one, or set the next one,
according to the order `SyncStatus` is defined.
-}
determineSyncStatus : Model -> Model
determineSyncStatus model =
    let
        syncCycleRotate =
            case model.syncCycle of
                SyncManager.Model.SyncCycleOn ->
                    True

                _ ->
                    False
    in
    if syncCycleRotate then
        let
            syncStatus =
                model.syncStatus

            syncInfoAuthorities =
                model.syncInfoAuthorities

            noChange =
                ( syncStatus, syncInfoAuthorities )

            ( syncStatusUpdated, syncInfoAuthoritiesUpdated ) =
                -- Cases are ordered by the cycle order.
                case syncStatus of
                    SyncIdle ->
                        ( SyncUploadPhotoAuthority RemoteData.NotAsked, syncInfoAuthorities )

                    SyncUploadPhotoAuthority webData ->
                        case webData of
                            RemoteData.Success maybeData ->
                                case maybeData of
                                    Just data ->
                                        -- We still have date.
                                        noChange

                                    Nothing ->
                                        -- No more photos to upload.
                                        ( SyncUploadGeneral emptyUploadRec, syncInfoAuthorities )

                            _ ->
                                noChange

                    SyncUploadGeneral record ->
                        if record.indexDbRemoteData == RemoteData.Success Nothing then
                            -- We tried to fetch entities for upload from IndexDB,
                            -- but there we non matching the query.
                            ( SyncUploadAuthority emptyUploadRec, syncInfoAuthorities )

                        else
                            noChange

                    SyncDownloadGeneral webData ->
                        case webData of
                            RemoteData.Success data ->
                                if List.isEmpty data.entities then
                                    -- We tried to fetch, but there was no more data.
                                    -- Next we try authorities.
                                    ( SyncDownloadAuthority RemoteData.NotAsked
                                    , syncInfoAuthorities
                                    )

                                else
                                    -- Still have data to download.
                                    noChange

                            _ ->
                                noChange

                    SyncUploadAuthority record ->
                        if record.indexDbRemoteData == RemoteData.Success Nothing then
                            -- We tried to fetch entities for upload from IndexDB,
                            -- but there we non matching the query.
                            ( SyncDownloadGeneral RemoteData.NotAsked, syncInfoAuthorities )

                        else
                            noChange

                    SyncDownloadAuthority webData ->
                        case ( model.syncInfoAuthorities, webData ) of
                            ( Nothing, _ ) ->
                                -- There are no authorities, so we can set the next
                                -- status.
                                ( SyncDownloadPhotos model.downloadPhotos
                                , syncInfoAuthorities
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
                                ( SyncIdle, syncInfoAuthorities )

                            DownloadPhotosBatch deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    ( SyncIdle, syncInfoAuthorities )

                                else if deferredPhoto.batchCounter < 1 then
                                    -- We've reached the end of the batch, so we
                                    -- need to rotate.
                                    ( SyncIdle, syncInfoAuthorities )

                                else
                                    noChange

                            DownloadPhotosAll deferredPhoto ->
                                if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                    -- We tried to fetch deferred photos from IndexDB,
                                    -- but there we non matching the query.
                                    ( SyncIdle, syncInfoAuthorities )

                                else
                                    -- There are still deferred photos in IndexDB
                                    -- that match out query.
                                    noChange
        in
        { model
            | syncStatus = syncStatusUpdated
            , syncInfoAuthorities = syncInfoAuthoritiesUpdated
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

        BackendGeneralVillage identifier ->
            getIdentifier identifier "village"


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
        BackendAuthorityAcuteFindings identifier ->
            getIdentifier identifier "acute_findings"

        BackendAuthorityAcuteIllnessEncounter identifier ->
            getIdentifier identifier "acute_illness_encounter"

        BackendAuthorityAcuteIllnessVitals identifier ->
            getIdentifier identifier "acute_illness_vitals"

        BackendAuthorityAttendance identifier ->
            getIdentifier identifier "attendance"

        BackendAuthorityBreastExam identifier ->
            getIdentifier identifier "breast_exam"

        BackendAuthorityCall114 identifier ->
            getIdentifier identifier "call_114"

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

        BackendAuthorityExposure identifier ->
            getIdentifier identifier "exposure"

        BackendAuthorityFamilyPlanning identifier ->
            getIdentifier identifier "family_planning"

        BackendAuthorityHCContact identifier ->
            getIdentifier identifier "hc_contact"

        BackendAuthorityHeight identifier ->
            getIdentifier identifier "height"

        BackendAuthorityIndividualParticipant identifier ->
            getIdentifier identifier "individual_participant"

        BackendAuthorityIsolation identifier ->
            getIdentifier identifier "isolation"

        BackendAuthorityLactation identifier ->
            getIdentifier identifier "lactation"

        BackendAuthorityLastMenstrualPeriod identifier ->
            getIdentifier identifier "last_menstrual_period"

        BackendAuthorityMalariaTesting identifier ->
            getIdentifier identifier "malaria_testing"

        BackendAuthorityMedicalHistory identifier ->
            getIdentifier identifier "medical_history"

        BackendAuthorityMedication identifier ->
            getIdentifier identifier "medication"

        BackendAuthorityMedicationDistribution identifier ->
            getIdentifier identifier "medication_distribution"

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

        BackendAuthorityPrenatalPhoto identifier ->
            getIdentifier identifier "prenatal_photo"

        BackendAuthorityPmtctParticipant identifier ->
            getIdentifier identifier "pmtct_participant"

        BackendAuthorityPrenatalFamilyPlanning identifier ->
            getIdentifier identifier "prenatal_family_planning"

        BackendAuthorityPrenatalNutrition identifier ->
            getIdentifier identifier "prenatal_nutrition"

        BackendAuthorityPrenatalEncounter identifier ->
            getIdentifier identifier "prenatal_encounter"

        BackendAuthorityRelationship identifier ->
            getIdentifier identifier "relationship"

        BackendAuthorityResource identifier ->
            getIdentifier identifier "resource"

        BackendAuthoritySendToHC identifier ->
            getIdentifier identifier "send_to_hc"

        BackendAuthoritySession identifier ->
            getIdentifier identifier "session"

        BackendAuthoritySocialHistory identifier ->
            getIdentifier identifier "social_history"

        BackendAuthoritySymptomsGeneral identifier ->
            getIdentifier identifier "symptoms_general"

        BackendAuthoritySymptomsGI identifier ->
            getIdentifier identifier "symptoms_gi"

        BackendAuthoritySymptomsRespiratory identifier ->
            getIdentifier identifier "symptoms_respiratory"

        BackendAuthorityTravelHistory identifier ->
            getIdentifier identifier "travel_history"

        BackendAuthorityTreatmentReview identifier ->
            getIdentifier identifier "treatment_history"

        BackendAuthorityWeight identifier ->
            getIdentifier identifier "weight"

        BackendAuthorityVitals identifier ->
            getIdentifier identifier "vitals"


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
            if RemoteData.isFailure webData then
                -- We got an error, so don't hammer the server.
                if syncSpeed.offline < 1000 then
                    1000

                else
                    toFloat syncSpeed.offline

            else
                syncCycle

        checkWebDataForPhotos webData =
            case webData of
                RemoteData.Failure error ->
                    if Utils.WebData.isNetworkError error then
                        -- It's a network error, so slow things down.
                        checkWebData webData

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

        SyncUploadAuthority record ->
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
                    checkWebDataForPhotos record.backendRemoteData

                DownloadPhotosAll record ->
                    checkWebDataForPhotos record.backendRemoteData

        _ ->
            syncCycle


encode : (a -> List ( String, Value )) -> BackendEntity a -> Value
encode encoder identifier =
    object <| encoder identifier.entity ++ [ ( "uuid", Json.Encode.string identifier.uuid ) ]


encodeBackendGeneralEntity : BackendGeneralEntity -> Value
encodeBackendGeneralEntity backendGeneralEntity =
    case backendGeneralEntity of
        BackendGeneralCatchmentArea identifier ->
            encode Backend.HealthCenter.Encoder.encodeCatchmentArea identifier

        BackendGeneralCounselingSchedule identifier ->
            encode Backend.Counseling.Encoder.encodeCounselingSchedule identifier

        BackendGeneralCounselingTopic identifier ->
            encode Backend.Counseling.Encoder.encodeCounselingTopic identifier

        BackendGeneralHealthCenter identifier ->
            encode Backend.HealthCenter.Encoder.encodeHealthCenter identifier

        BackendGeneralNurse identifier ->
            encode Backend.Nurse.Encoder.encodeNurse identifier

        BackendGeneralParticipantForm identifier ->
            encode Backend.ParticipantConsent.Encoder.encodeParticipantForm identifier

        BackendGeneralVillage identifier ->
            encode Backend.Village.Encoder.encodeVillage identifier


encodeBackendAuthorityEntity : BackendAuthorityEntity -> Value
encodeBackendAuthorityEntity entity =
    case entity of
        BackendAuthorityAcuteFindings identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteFindings identifier

        BackendAuthorityAcuteIllnessEncounter identifier ->
            encode Backend.AcuteIllnessEncounter.Encoder.encodeAcuteIllnessEncounter identifier

        BackendAuthorityAcuteIllnessVitals identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessVitals identifier

        BackendAuthorityAttendance identifier ->
            encode Backend.Measurement.Encoder.encodeAttendance identifier

        BackendAuthorityBreastExam identifier ->
            encode Backend.Measurement.Encoder.encodeBreastExam identifier

        BackendAuthorityCall114 identifier ->
            encode Backend.Measurement.Encoder.encodeCall114 identifier

        BackendAuthorityClinic identifier ->
            encode Backend.Clinic.Encoder.encodeClinic identifier

        BackendAuthorityChildFbf identifier ->
            encode Backend.Measurement.Encoder.encodeFbf identifier

        BackendAuthorityCounselingSession identifier ->
            encode Backend.Measurement.Encoder.encodeCounselingSession identifier

        BackendAuthorityCorePhysicalExam identifier ->
            encode Backend.Measurement.Encoder.encodeCorePhysicalExam identifier

        BackendAuthorityDangerSigns identifier ->
            encode Backend.Measurement.Encoder.encodeDangerSigns identifier

        BackendAuthorityExposure identifier ->
            encode Backend.Measurement.Encoder.encodeExposure identifier

        BackendAuthorityFamilyPlanning identifier ->
            encode Backend.Measurement.Encoder.encodeFamilyPlanning identifier

        BackendAuthorityHCContact identifier ->
            encode Backend.Measurement.Encoder.encodeHCContact identifier

        BackendAuthorityHeight identifier ->
            encode Backend.Measurement.Encoder.encodeHeight identifier

        BackendAuthorityIndividualParticipant identifier ->
            encode Backend.IndividualEncounterParticipant.Encoder.encodeIndividualEncounterParticipant identifier

        BackendAuthorityIsolation identifier ->
            encode Backend.Measurement.Encoder.encodeIsolation identifier

        BackendAuthorityLactation identifier ->
            encode Backend.Measurement.Encoder.encodeLactation identifier

        BackendAuthorityMalariaTesting identifier ->
            encode Backend.Measurement.Encoder.encodeMalariaTesting identifier

        BackendAuthorityLastMenstrualPeriod identifier ->
            encode Backend.Measurement.Encoder.encodeLastMenstrualPeriod identifier

        BackendAuthorityMedicalHistory identifier ->
            encode Backend.Measurement.Encoder.encodeMedicalHistory identifier

        BackendAuthorityMedication identifier ->
            encode Backend.Measurement.Encoder.encodeMedication identifier

        BackendAuthorityMedicationDistribution identifier ->
            encode Backend.Measurement.Encoder.encodeMedicationDistribution identifier

        BackendAuthorityMotherFbf identifier ->
            encode Backend.Measurement.Encoder.encodeFbf identifier

        BackendAuthorityMuac identifier ->
            encode Backend.Measurement.Encoder.encodeMuac identifier

        BackendAuthorityNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeNutrition identifier

        BackendAuthorityNutritionEncounter identifier ->
            encode Backend.NutritionEncounter.Encoder.encodeNutritionEncounter identifier

        BackendAuthorityNutritionHeight identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionHeight identifier

        BackendAuthorityNutritionMuac identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionMuac identifier

        BackendAuthorityNutritionNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionNutrition identifier

        BackendAuthorityNutritionPhoto identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionPhoto identifier

        BackendAuthorityNutritionWeight identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionWeight identifier

        BackendAuthorityObstetricHistory identifier ->
            encode Backend.Measurement.Encoder.encodeObstetricHistory identifier

        BackendAuthorityObstetricHistoryStep2 identifier ->
            encode Backend.Measurement.Encoder.encodeObstetricHistoryStep2 identifier

        BackendAuthorityObstetricalExam identifier ->
            encode Backend.Measurement.Encoder.encodeObstetricalExam identifier

        BackendAuthorityParticipantConsent identifier ->
            encode Backend.Measurement.Encoder.encodeParticipantConsent identifier

        BackendAuthorityPerson identifier ->
            encode Backend.Person.Encoder.encodePerson identifier

        BackendAuthorityPhoto identifier ->
            encode Backend.Measurement.Encoder.encodePhoto identifier

        BackendAuthorityPrenatalPhoto identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalPhoto identifier

        BackendAuthorityPmtctParticipant identifier ->
            encode Backend.PmtctParticipant.Encoder.encodePmtctParticipant identifier

        BackendAuthorityPrenatalFamilyPlanning identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalFamilyPlanning identifier

        BackendAuthorityPrenatalNutrition identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalNutrition identifier

        BackendAuthorityPrenatalEncounter identifier ->
            encode Backend.PrenatalEncounter.Encoder.encodePrenatalEncounter identifier

        BackendAuthorityRelationship identifier ->
            encode Backend.Relationship.Encoder.encodeRelationship identifier

        BackendAuthorityResource identifier ->
            encode Backend.Measurement.Encoder.encodeResource identifier

        BackendAuthoritySession identifier ->
            encode Backend.Session.Encoder.encodeSession identifier

        BackendAuthoritySendToHC identifier ->
            encode Backend.Measurement.Encoder.encodeSendToHC identifier

        BackendAuthoritySocialHistory identifier ->
            encode Backend.Measurement.Encoder.encodeSocialHistory identifier

        BackendAuthoritySymptomsGeneral identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsGeneral identifier

        BackendAuthoritySymptomsGI identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsGI identifier

        BackendAuthoritySymptomsRespiratory identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsRespiratory identifier

        BackendAuthorityTravelHistory identifier ->
            encode Backend.Measurement.Encoder.encodeTravelHistory identifier

        BackendAuthorityTreatmentReview identifier ->
            encode Backend.Measurement.Encoder.encodeTreatmentReview identifier

        BackendAuthorityVitals identifier ->
            encode Backend.Measurement.Encoder.encodeVitals identifier

        BackendAuthorityWeight identifier ->
            encode Backend.Measurement.Encoder.encodeWeight identifier


getDataToSendGeneral : BackendGeneralEntity -> List String -> List String
getDataToSendGeneral entity accum =
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


getDataToSendAuthority : BackendAuthorityEntity -> List String -> List String
getDataToSendAuthority entity accum =
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


getSyncedHealthCenters : Model -> List String
getSyncedHealthCenters model =
    model.syncInfoAuthorities
        |> Maybe.map (Zipper.toList >> List.map .uuid)
        |> Maybe.withDefault []
