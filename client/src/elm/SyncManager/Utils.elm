module SyncManager.Utils exposing (..)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import Backend.AcuteIllnessEncounter.Encoder
import Backend.ChildScoreboardEncounter.Encoder
import Backend.Clinic.Encoder
import Backend.Counseling.Encoder
import Backend.Dashboard.Encoder
import Backend.EducationSession.Encoder
import Backend.HIVEncounter.Encoder
import Backend.HealthCenter.Encoder
import Backend.HomeVisitEncounter.Encoder
import Backend.IndividualEncounterParticipant.Encoder
import Backend.Measurement.Encoder
import Backend.Measurement.Model exposing (ImageUrl(..))
import Backend.Model exposing (Revision(..))
import Backend.NCDEncounter.Encoder
import Backend.Nurse.Encoder
import Backend.NutritionEncounter.Encoder
import Backend.ParticipantConsent.Encoder
import Backend.Person.Encoder
import Backend.PmtctParticipant.Encoder
import Backend.PrenatalEncounter.Encoder
import Backend.Relationship.Encoder
import Backend.ResilienceMessage.Encoder
import Backend.ResilienceSurvey.Encoder
import Backend.Session.Encoder
import Backend.StockUpdate.Encoder
import Backend.TuberculosisEncounter.Encoder
import Backend.Village.Encoder
import Backend.WellChildEncounter.Encoder
import Editable
import EverySet exposing (EverySet)
import Http
import Json.Decode
import Json.Encode exposing (Value, object)
import List.Zipper as Zipper
import Maybe.Extra
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (..)
import Utils.WebData


{-| Decide on the Sync status. Either keep the exiting one, or set the next one,
according to the order `SyncStatus` is defined.
-}
determineSyncStatus : Page -> Model -> Model
determineSyncStatus activePage model =
    let
        syncCycleOn =
            model.syncCycle == SyncManager.Model.SyncCycleOn

        -- Determine if we're on session page where it's possible to
        -- take a photo. If so, we'll halt sync, since it may
        -- create problems with Dropzone binding / file saving at local cache.
        photoPage =
            case activePage of
                -- Activity page where pictures are taken in bulk.
                UserPage (SessionPage _ (ActivityPage (ChildActivity ChildPicture))) ->
                    True

                -- Participant page, where activites are taken for a child.
                UserPage (SessionPage _ (ChildPage _)) ->
                    True

                _ ->
                    False
    in
    if syncCycleOn && not photoPage then
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
                    SyncReportIncident _ ->
                        ( SyncIdle, syncInfoAuthorities )

                    SyncIdle ->
                        ( SyncUploadPhoto 0 RemoteData.NotAsked, syncInfoAuthorities )

                    SyncUploadPhoto errorsCount webData ->
                        case webData of
                            RemoteData.Success maybeData ->
                                case maybeData of
                                    Just _ ->
                                        -- We still have data. Reset errors counter to 0, since last upload was succesfull.
                                        ( SyncUploadPhoto 0 webData, syncInfoAuthorities )

                                    Nothing ->
                                        -- No more photos to upload. Move on to uploading screenshots.
                                        ( SyncUploadScreenshot 0 RemoteData.NotAsked, syncInfoAuthorities )

                            RemoteData.Failure error ->
                                let
                                    handleNonNetworkError reason =
                                        if errorsCount > fileUploadFailureThreshold then
                                            -- Threshold exceeded - report an incident and stop current sync cycle.
                                            SyncReportIncident (FileUploadIncident reason)

                                        else
                                            -- Threshold not exceeded - increase counter and try uploading again.
                                            SyncUploadPhoto (errorsCount + 1) webData
                                in
                                case error of
                                    NetworkError _ ->
                                        let
                                            handleNetworkError =
                                                if errorsCount > fileUploadFailureThreshold then
                                                    -- Threshold exceeded, as there's no internet connection - stop current sync cycle.
                                                    SyncIdle

                                                else
                                                    -- Threshold not exceeded - increase counter and try uploading again.
                                                    SyncUploadPhoto (errorsCount + 1) webData
                                        in
                                        ( handleNetworkError, syncInfoAuthorities )

                                    BadJson reason ->
                                        ( handleNonNetworkError reason, syncInfoAuthorities )

                                    UploadError reason ->
                                        ( handleNonNetworkError reason, syncInfoAuthorities )

                            _ ->
                                noChange

                    SyncUploadScreenshot errorsCount webData ->
                        case webData of
                            RemoteData.Success maybeData ->
                                case maybeData of
                                    Just _ ->
                                        -- We still have data. Reset errors counter to 0, since last upload was succesfull.
                                        ( SyncUploadScreenshot 0 webData, syncInfoAuthorities )

                                    Nothing ->
                                        -- No more photos to upload.
                                        ( SyncUploadGeneral emptyUploadRec, syncInfoAuthorities )

                            RemoteData.Failure error ->
                                let
                                    handleNonNetworkError reason =
                                        if errorsCount > fileUploadFailureThreshold then
                                            -- Threshold exceeded - report an incident and stop current sync cycle.
                                            SyncReportIncident (FileUploadIncident reason)

                                        else
                                            -- Threshold not exceeded - increase counter and try uploading again.
                                            SyncUploadScreenshot (errorsCount + 1) webData
                                in
                                case error of
                                    NetworkError _ ->
                                        let
                                            handleNetworkError =
                                                if errorsCount > fileUploadFailureThreshold then
                                                    -- Threshold exceeded, as there's no internet connection - stop current sync cycle.
                                                    SyncIdle

                                                else
                                                    -- Threshold not exceeded - increase counter and try uploading again.
                                                    SyncUploadScreenshot (errorsCount + 1) webData
                                        in
                                        ( handleNetworkError, syncInfoAuthorities )

                                    BadJson reason ->
                                        ( handleNonNetworkError reason, syncInfoAuthorities )

                                    UploadError reason ->
                                        ( handleNonNetworkError reason, syncInfoAuthorities )

                            _ ->
                                noChange

                    SyncUploadGeneral record ->
                        if record.indexDbRemoteData == RemoteData.Success Nothing then
                            -- We tried to fetch entities for upload from IndexDB,
                            -- but there we non matching the query.
                            ( SyncUploadAuthority emptyUploadRec, syncInfoAuthorities )

                        else
                            noChange

                    SyncUploadAuthority record ->
                        case ( syncInfoAuthorities, record.indexDbRemoteData ) of
                            ( Nothing, _ ) ->
                                -- There are no authorities, so we can set the next status.
                                ( SyncUploadWhatsApp emptyUploadRec
                                , syncInfoAuthorities
                                )

                            ( Just zipper, RemoteData.Success Nothing ) ->
                                -- We tried to fetch, but there was no more data.
                                -- Go to the next authority if there is one,
                                -- otherwise, to the next status.
                                case Zipper.next zipper of
                                    Just nextZipper ->
                                        ( SyncUploadAuthority emptyUploadRec
                                        , Just nextZipper
                                        )

                                    Nothing ->
                                        -- We've reached the last element,
                                        -- so reset authorities zipper to first element,
                                        -- and rotate to the next status.
                                        ( SyncUploadWhatsApp emptyUploadRec
                                        , Just (Zipper.first zipper)
                                        )

                            _ ->
                                -- Still have data to upload.
                                noChange

                    -- It's important to have Whatsapp uploaded after Authority upload
                    -- has completed, because Whatsapp record may refer to person
                    -- that's pending upload at Authority.
                    SyncUploadWhatsApp record ->
                        if record.indexDbRemoteData == RemoteData.Success Nothing then
                            -- We tried to fetch entities for upload from IndexDB,
                            -- but there we non matching the query.
                            ( SyncDownloadGeneral RemoteData.NotAsked
                            , syncInfoAuthorities
                            )

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

                    SyncDownloadAuthority webData ->
                        case ( syncInfoAuthorities, webData ) of
                            ( Nothing, _ ) ->
                                -- There are no authorities, so we can set the next
                                -- status, skipping statistics download.
                                ( SyncIdle
                                , syncInfoAuthorities
                                )

                            ( Just zipper, RemoteData.Success data ) ->
                                if List.isEmpty data.entities then
                                    -- We tried to fetch, but there was no more data.
                                    -- Go to the next authority if there is one,
                                    -- otherwise, to the next status.
                                    case Zipper.next zipper of
                                        Just nextZipper ->
                                            ( SyncDownloadAuthority RemoteData.NotAsked
                                            , Just nextZipper
                                            )

                                        Nothing ->
                                            -- We've reached the last element,
                                            -- so reset authorities zipper to first element,
                                            -- and rotate to the next status.
                                            ( SyncDownloadAuthorityDashboardStats RemoteData.NotAsked
                                            , Just (Zipper.first zipper)
                                            )

                                else
                                    -- Still have data to download.
                                    noChange

                            _ ->
                                noChange

                    SyncDownloadAuthorityDashboardStats webData ->
                        case ( syncInfoAuthorities, webData ) of
                            ( Nothing, _ ) ->
                                -- There are no authorities, so we can set the next
                                -- status.
                                ( SyncIdle
                                , syncInfoAuthorities
                                )

                            ( Just zipper, RemoteData.Success _ ) ->
                                -- Go to the next authority if there is
                                -- otherwise, to the next status
                                case Zipper.next zipper of
                                    Just nextZipper ->
                                        ( SyncDownloadAuthorityDashboardStats RemoteData.NotAsked
                                        , Just nextZipper
                                        )

                                    Nothing ->
                                        -- We've reached the last element,
                                        -- so reset authorities zipper to first element,
                                        -- and rotate to the next status.
                                        ( SyncIdle
                                        , Just (Zipper.first zipper)
                                        )

                            _ ->
                                noChange
        in
        { model
            | syncStatus = syncStatusUpdated
            , syncInfoAuthorities = syncInfoAuthoritiesUpdated
        }

    else
        -- No change.
        model


determineDownloadPhotosStatus : Model -> Model
determineDownloadPhotosStatus model =
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
            statusUpdated =
                case model.syncStatus of
                    SyncIdle ->
                        -- Cases are ordered by the cycle order.
                        let
                            currentStatus =
                                model.downloadPhotosStatus
                        in
                        case currentStatus of
                            DownloadPhotosIdle ->
                                DownloadPhotosInProcess model.downloadPhotosMode

                            DownloadPhotosInProcess record ->
                                case record of
                                    DownloadPhotosNone ->
                                        DownloadPhotosIdle

                                    DownloadPhotosBatch deferredPhoto ->
                                        if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                            -- We tried to fetch deferred photos from IndexDB,
                                            -- but there we non matching the query.
                                            DownloadPhotosIdle

                                        else if deferredPhoto.batchCounter < 1 then
                                            -- We've reached the end of the batch, so we
                                            -- need to rotate.
                                            DownloadPhotosIdle

                                        else
                                            currentStatus

                                    DownloadPhotosAll deferredPhoto ->
                                        if deferredPhoto.indexDbRemoteData == RemoteData.Success Nothing then
                                            -- We tried to fetch deferred photos from IndexDB,
                                            -- but there we non matching the query.
                                            DownloadPhotosIdle

                                        else
                                            -- There are still deferred photos in IndexDB
                                            -- that match out query.
                                            currentStatus

                    -- When sync is active, we stop photos download.
                    _ ->
                        DownloadPhotosIdle
        in
        { model | downloadPhotosStatus = statusUpdated }

    else
        -- No change.
        model


resetDownloadPhotosBatchCounter : Model -> DownloadPhotosStatus
resetDownloadPhotosBatchCounter model =
    case model.downloadPhotosMode of
        DownloadPhotosBatch deferredPhoto ->
            let
                deferredPhotoUpdated =
                    { deferredPhoto | batchCounter = deferredPhoto.batchSize }
            in
            DownloadPhotosInProcess (DownloadPhotosBatch deferredPhotoUpdated)

        _ ->
            DownloadPhotosInProcess model.downloadPhotosMode


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

        BackendGeneralResilienceSurvey identifier ->
            getIdentifier identifier "resilience_survey"


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

        BackendAuthorityAcuteIllnessContactsTracing identifier ->
            getIdentifier identifier "acute_illness_contacts_tracing"

        BackendAuthorityAcuteIllnessCoreExam identifier ->
            getIdentifier identifier "acute_illness_core_exam"

        BackendAuthorityAcuteIllnessDangerSigns identifier ->
            getIdentifier identifier "acute_illness_danger_signs"

        BackendAuthorityAcuteIllnessEncounter identifier ->
            getIdentifier identifier "acute_illness_encounter"

        BackendAuthorityAcuteIllnessFollowUp identifier ->
            getIdentifier identifier "acute_illness_follow_up"

        BackendAuthorityAcuteIllnessMuac identifier ->
            getIdentifier identifier "acute_illness_muac"

        BackendAuthorityAcuteIllnessNutrition identifier ->
            getIdentifier identifier "acute_illness_nutrition"

        BackendAuthorityAcuteIllnessTraceContact identifier ->
            getIdentifier identifier "acute_illness_trace_contact"

        BackendAuthorityAcuteIllnessVitals identifier ->
            getIdentifier identifier "acute_illness_vitals"

        BackendAuthorityAppointmentConfirmation identifier ->
            getIdentifier identifier "appointment_confirmation"

        BackendAuthorityAttendance identifier ->
            getIdentifier identifier "attendance"

        BackendAuthorityBreastExam identifier ->
            getIdentifier identifier "breast_exam"

        BackendAuthorityBirthPlan identifier ->
            getIdentifier identifier "birth_plan"

        BackendAuthorityCall114 identifier ->
            getIdentifier identifier "call_114"

        BackendAuthorityChildFbf identifier ->
            getIdentifier identifier "child_fbf"

        BackendAuthorityChildScoreboardEncounter identifier ->
            getIdentifier identifier "child_scoreboard_encounter"

        BackendAuthorityChildScoreboardBCGImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_bcg_iz"

        BackendAuthorityChildScoreboardDTPImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_dtp_iz"

        BackendAuthorityChildScoreboardDTPStandaloneImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_dtp_sa_iz"

        BackendAuthorityChildScoreboardIPVImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_ipv_iz"

        BackendAuthorityChildScoreboardMRImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_mr_iz"

        BackendAuthorityChildScoreboardNCDA identifier ->
            getIdentifier identifier "child_scoreboard_ncda"

        BackendAuthorityChildScoreboardOPVImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_opv_iz"

        BackendAuthorityChildScoreboardPCV13Immunisation identifier ->
            getIdentifier identifier "child_scoreboard_pcv13_iz"

        BackendAuthorityChildScoreboardRotarixImmunisation identifier ->
            getIdentifier identifier "child_scoreboard_rotarix_iz"

        BackendAuthorityClinic identifier ->
            getIdentifier identifier "clinic"

        BackendAuthorityContributingFactors identifier ->
            getIdentifier identifier "contributing_factors"

        BackendAuthorityCounselingSession identifier ->
            getIdentifier identifier "counseling_session"

        BackendAuthorityCorePhysicalExam identifier ->
            getIdentifier identifier "core_physical_exam"

        BackendAuthorityCovidTesting identifier ->
            getIdentifier identifier "covid_testing"

        BackendAuthorityDangerSigns identifier ->
            getIdentifier identifier "danger_signs"

        BackendAuthorityDashboardStats identifier ->
            getIdentifier identifier "statistics"

        BackendAuthorityEducationSession identifier ->
            getIdentifier identifier "education_session"

        BackendAuthorityExposure identifier ->
            getIdentifier identifier "exposure"

        BackendAuthorityFamilyPlanning identifier ->
            getIdentifier identifier "family_planning"

        BackendAuthorityFollowUp identifier ->
            getIdentifier identifier "follow_up"

        BackendAuthorityGroupHealthEducation identifier ->
            getIdentifier identifier "group_health_education"

        BackendAuthorityGroupNCDA identifier ->
            getIdentifier identifier "group_ncda"

        BackendAuthorityGroupSendToHC identifier ->
            getIdentifier identifier "group_send_to_hc"

        BackendAuthorityHealthEducation identifier ->
            getIdentifier identifier "health_education"

        BackendAuthorityHCContact identifier ->
            getIdentifier identifier "hc_contact"

        BackendAuthorityHeight identifier ->
            getIdentifier identifier "height"

        BackendAuthorityHIVDiagnostics identifier ->
            getIdentifier identifier "hiv_diagnostics"

        BackendAuthorityHIVEncounter identifier ->
            getIdentifier identifier "hiv_encounter"

        BackendAuthorityHIVFollowUp identifier ->
            getIdentifier identifier "hiv_follow_up"

        BackendAuthorityHIVHealthEducation identifier ->
            getIdentifier identifier "hiv_health_education"

        BackendAuthorityHIVMedication identifier ->
            getIdentifier identifier "hiv_medication"

        BackendAuthorityHIVReferral identifier ->
            getIdentifier identifier "hiv_referral"

        BackendAuthorityHIVSymptomReview identifier ->
            getIdentifier identifier "hiv_symptom_review"

        BackendAuthorityHIVTreatmentReview identifier ->
            getIdentifier identifier "hiv_treatment_review"

        BackendAuthorityHomeVisitEncounter identifier ->
            getIdentifier identifier "home_visit_encounter"

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

        BackendAuthorityNCDCoMorbidities identifier ->
            getIdentifier identifier "ncd_co_morbidities"

        BackendAuthorityNCDCoreExam identifier ->
            getIdentifier identifier "ncd_core_exam"

        BackendAuthorityNCDCreatinineTest identifier ->
            getIdentifier identifier "ncd_creatinine_test"

        BackendAuthorityNCDDangerSigns identifier ->
            getIdentifier identifier "ncd_danger_signs"

        BackendAuthorityNCDEncounter identifier ->
            getIdentifier identifier "ncd_encounter"

        BackendAuthorityNCDFamilyHistory identifier ->
            getIdentifier identifier "ncd_family_history"

        BackendAuthorityNCDFamilyPlanning identifier ->
            getIdentifier identifier "ncd_family_planning"

        BackendAuthorityNCDHbA1cTest identifier ->
            getIdentifier identifier "ncd_hba1c_test"

        BackendAuthorityNCDHealthEducation identifier ->
            getIdentifier identifier "ncd_health_education"

        BackendAuthorityNCDHIVTest identifier ->
            getIdentifier identifier "ncd_hiv_test"

        BackendAuthorityNCDLabsResults identifier ->
            getIdentifier identifier "ncd_labs_results"

        BackendAuthorityNCDLipidPanelTest identifier ->
            getIdentifier identifier "ncd_lipid_panel_test"

        BackendAuthorityNCDLiverFunctionTest identifier ->
            getIdentifier identifier "ncd_liver_function_test"

        BackendAuthorityNCDMedicationDistribution identifier ->
            getIdentifier identifier "ncd_medication_distribution"

        BackendAuthorityNCDMedicationHistory identifier ->
            getIdentifier identifier "ncd_medication_history"

        BackendAuthorityNCDOutsideCare identifier ->
            getIdentifier identifier "ncd_outside_care"

        BackendAuthorityNCDPregnancyTest identifier ->
            getIdentifier identifier "ncd_pregnancy_test"

        BackendAuthorityNCDRandomBloodSugarTest identifier ->
            getIdentifier identifier "ncd_random_blood_sugar_test"

        BackendAuthorityNCDReferral identifier ->
            getIdentifier identifier "ncd_referral"

        BackendAuthorityNCDSocialHistory identifier ->
            getIdentifier identifier "ncd_social_history"

        BackendAuthorityNCDSymptomReview identifier ->
            getIdentifier identifier "ncd_symptom_review"

        BackendAuthorityNCDUrineDipstickTest identifier ->
            getIdentifier identifier "ncd_urine_dipstick_test"

        BackendAuthorityNCDVitals identifier ->
            getIdentifier identifier "ncd_vitals"

        BackendAuthorityNutrition identifier ->
            getIdentifier identifier "nutrition"

        BackendAuthorityNutritionCaring identifier ->
            getIdentifier identifier "nutrition_caring"

        BackendAuthorityNutritionContributingFactors identifier ->
            getIdentifier identifier "nutrition_contributing_factors"

        BackendAuthorityNutritionEncounter identifier ->
            getIdentifier identifier "nutrition_encounter"

        BackendAuthorityNutritionFeeding identifier ->
            getIdentifier identifier "nutrition_feeding"

        BackendAuthorityNutritionFollowUp identifier ->
            getIdentifier identifier "nutrition_follow_up"

        BackendAuthorityNutritionFoodSecurity identifier ->
            getIdentifier identifier "nutrition_food_security"

        BackendAuthorityNutritionHealthEducation identifier ->
            getIdentifier identifier "nutrition_health_education"

        BackendAuthorityNutritionHeight identifier ->
            getIdentifier identifier "nutrition_height"

        BackendAuthorityNutritionHygiene identifier ->
            getIdentifier identifier "nutrition_hygiene"

        BackendAuthorityNutritionMuac identifier ->
            getIdentifier identifier "nutrition_muac"

        BackendAuthorityNutritionNCDA identifier ->
            getIdentifier identifier "nutrition_ncda"

        BackendAuthorityNutritionNutrition identifier ->
            getIdentifier identifier "nutrition_nutrition"

        BackendAuthorityNutritionPhoto identifier ->
            getIdentifier identifier "nutrition_photo"

        BackendAuthorityNutritionSendToHC identifier ->
            getIdentifier identifier "nutrition_send_to_hc"

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

        BackendAuthorityPmtctParticipant identifier ->
            getIdentifier identifier "pmtct_participant"

        BackendAuthorityPregnancyTest identifier ->
            getIdentifier identifier "pregnancy_testing"

        BackendAuthorityPrenatalBloodGpRsTest identifier ->
            getIdentifier identifier "prenatal_blood_gprs_test"

        BackendAuthorityPrenatalBreastfeeding identifier ->
            getIdentifier identifier "prenatal_breastfeeding"

        BackendAuthorityPrenatalEncounter identifier ->
            getIdentifier identifier "prenatal_encounter"

        BackendAuthorityPrenatalFamilyPlanning identifier ->
            getIdentifier identifier "prenatal_family_planning"

        BackendAuthorityPrenatalFollowUp identifier ->
            getIdentifier identifier "prenatal_follow_up"

        BackendAuthorityPrenatalGUExam identifier ->
            getIdentifier identifier "prenatal_gu_exam"

        BackendAuthorityPrenatalHealthEducation identifier ->
            getIdentifier identifier "prenatal_health_education"

        BackendAuthorityPrenatalHemoglobinTest identifier ->
            getIdentifier identifier "prenatal_hemoglobin_test"

        BackendAuthorityPrenatalHepatitisBTest identifier ->
            getIdentifier identifier "prenatal_hepatitis_b_test"

        BackendAuthorityPrenatalHIVTest identifier ->
            getIdentifier identifier "prenatal_hiv_test"

        BackendAuthorityPrenatalHIVPCRTest identifier ->
            getIdentifier identifier "prenatal_hiv_pcr_test"

        BackendAuthorityPrenatalLabsResults identifier ->
            getIdentifier identifier "prenatal_labs_results"

        BackendAuthorityPrenatalMalariaTest identifier ->
            getIdentifier identifier "prenatal_malaria_test"

        BackendAuthorityPrenatalMedicationDistribution identifier ->
            getIdentifier identifier "prenatal_medication_distribution"

        BackendAuthorityPrenatalMentalHealth identifier ->
            getIdentifier identifier "prenatal_mental_health"

        BackendAuthorityPrenatalNutrition identifier ->
            getIdentifier identifier "prenatal_nutrition"

        BackendAuthorityPrenatalOutsideCare identifier ->
            getIdentifier identifier "prenatal_outside_care"

        BackendAuthorityPrenatalPartnerHIVTest identifier ->
            getIdentifier identifier "prenatal_partner_hiv_test"

        BackendAuthorityPrenatalPhoto identifier ->
            getIdentifier identifier "prenatal_photo"

        BackendAuthorityPrenatalRandomBloodSugarTest identifier ->
            getIdentifier identifier "prenatal_random_blood_sugar_test"

        BackendAuthorityPrenatalSendToHC identifier ->
            getIdentifier identifier "prenatal_send_to_hc"

        BackendAuthorityPrenatalSpecialityCare identifier ->
            getIdentifier identifier "prenatal_speciality_care"

        BackendAuthorityPrenatalSymptomReview identifier ->
            getIdentifier identifier "prenatal_symptom_review"

        BackendAuthorityPrenatalSyphilisTest identifier ->
            getIdentifier identifier "prenatal_syphilis_test"

        BackendAuthorityPrenatalTetanusImmunisation identifier ->
            getIdentifier identifier "prenatal_tetanus_immunisation"

        BackendAuthorityPrenatalUrineDipstickTest identifier ->
            getIdentifier identifier "prenatal_urine_dipstick_test"

        BackendAuthorityRelationship identifier ->
            getIdentifier identifier "relationship"

        BackendAuthorityMalariaPrevention identifier ->
            getIdentifier identifier "resource"

        BackendAuthoritySendToHC identifier ->
            getIdentifier identifier "send_to_hc"

        BackendAuthoritySession identifier ->
            getIdentifier identifier "session"

        BackendAuthoritySocialHistory identifier ->
            getIdentifier identifier "social_history"

        BackendAuthorityStockUpdate identifier ->
            getIdentifier identifier "stock_update"

        BackendAuthoritySymptomsGeneral identifier ->
            getIdentifier identifier "symptoms_general"

        BackendAuthoritySymptomsGI identifier ->
            getIdentifier identifier "symptoms_gi"

        BackendAuthoritySymptomsRespiratory identifier ->
            getIdentifier identifier "symptoms_respiratory"

        BackendAuthorityTravelHistory identifier ->
            getIdentifier identifier "travel_history"

        BackendAuthorityTreatmentOngoing identifier ->
            getIdentifier identifier "treatment_ongoing"

        BackendAuthorityTreatmentReview identifier ->
            getIdentifier identifier "treatment_history"

        BackendAuthorityTuberculosisDiagnostics identifier ->
            getIdentifier identifier "tuberculosis_diagnostics"

        BackendAuthorityTuberculosisDOT identifier ->
            getIdentifier identifier "tuberculosis_dot"

        BackendAuthorityTuberculosisEncounter identifier ->
            getIdentifier identifier "tuberculosis_encounter"

        BackendAuthorityTuberculosisFollowUp identifier ->
            getIdentifier identifier "tuberculosis_follow_up"

        BackendAuthorityTuberculosisHealthEducation identifier ->
            getIdentifier identifier "tuberculosis_health_education"

        BackendAuthorityTuberculosisMedication identifier ->
            getIdentifier identifier "tuberculosis_medication"

        BackendAuthorityTuberculosisReferral identifier ->
            getIdentifier identifier "tuberculosis_referral"

        BackendAuthorityTuberculosisSymptomReview identifier ->
            getIdentifier identifier "tuberculosis_symptom_review"

        BackendAuthorityTuberculosisTreatmentReview identifier ->
            getIdentifier identifier "tuberculosis_treatment_review"

        BackendAuthorityVitals identifier ->
            getIdentifier identifier "vitals"

        BackendAuthorityWeight identifier ->
            getIdentifier identifier "weight"

        BackendAuthorityWellChildAlbendazole identifier ->
            getIdentifier identifier "well_child_albendazole"

        BackendAuthorityWellChildBCGImmunisation identifier ->
            getIdentifier identifier "well_child_bcg_immunisation"

        BackendAuthorityWellChildCaring identifier ->
            getIdentifier identifier "well_child_caring"

        BackendAuthorityWellChildContributingFactors identifier ->
            getIdentifier identifier "well_child_contributing_factors"

        BackendAuthorityWellChildDTPImmunisation identifier ->
            getIdentifier identifier "well_child_dtp_immunisation"

        BackendAuthorityWellChildDTPStandaloneImmunisation identifier ->
            getIdentifier identifier "well_child_dtp_sa_immunisation"

        BackendAuthorityWellChildECD identifier ->
            getIdentifier identifier "well_child_ecd"

        BackendAuthorityWellChildEncounter identifier ->
            getIdentifier identifier "well_child_encounter"

        BackendAuthorityWellChildFeeding identifier ->
            getIdentifier identifier "well_child_feeding"

        BackendAuthorityWellChildFollowUp identifier ->
            getIdentifier identifier "well_child_follow_up"

        BackendAuthorityWellChildFoodSecurity identifier ->
            getIdentifier identifier "well_child_food_security"

        BackendAuthorityWellChildHeadCircumference identifier ->
            getIdentifier identifier "well_child_head_circumference"

        BackendAuthorityWellChildHealthEducation identifier ->
            getIdentifier identifier "well_child_health_education"

        BackendAuthorityWellChildHeight identifier ->
            getIdentifier identifier "well_child_height"

        BackendAuthorityWellChildHygiene identifier ->
            getIdentifier identifier "well_child_hygiene"

        BackendAuthorityWellChildHPVImmunisation identifier ->
            getIdentifier identifier "well_child_hpv_immunisation"

        BackendAuthorityWellChildIPVImmunisation identifier ->
            getIdentifier identifier "well_child_ipv_immunisation"

        BackendAuthorityWellChildMebendezole identifier ->
            getIdentifier identifier "well_child_mebendezole"

        BackendAuthorityWellChildMRImmunisation identifier ->
            getIdentifier identifier "well_child_mr_immunisation"

        BackendAuthorityWellChildMuac identifier ->
            getIdentifier identifier "well_child_muac"

        BackendAuthorityWellChildNCDA identifier ->
            getIdentifier identifier "well_child_ncda"

        BackendAuthorityWellChildNextVisit identifier ->
            getIdentifier identifier "well_child_next_visit"

        BackendAuthorityWellChildNutrition identifier ->
            getIdentifier identifier "well_child_nutrition"

        BackendAuthorityWellChildOPVImmunisation identifier ->
            getIdentifier identifier "well_child_opv_immunisation"

        BackendAuthorityWellChildPCV13Immunisation identifier ->
            getIdentifier identifier "well_child_pcv13_immunisation"

        BackendAuthorityWellChildPhoto identifier ->
            getIdentifier identifier "well_child_photo"

        BackendAuthorityWellChildPregnancySummary identifier ->
            getIdentifier identifier "well_child_pregnancy_summary"

        BackendAuthorityWellChildRotarixImmunisation identifier ->
            getIdentifier identifier "well_child_rotarix_immunisation"

        BackendAuthorityWellChildSendToHC identifier ->
            getIdentifier identifier "well_child_send_to_hc"

        BackendAuthorityWellChildSymptomsReview identifier ->
            getIdentifier identifier "well_child_symptoms_review"

        BackendAuthorityWellChildVitals identifier ->
            getIdentifier identifier "well_child_vitals"

        BackendAuthorityWellChildVitaminA identifier ->
            getIdentifier identifier "well_child_vitamin_a"

        BackendAuthorityWellChildWeight identifier ->
            getIdentifier identifier "well_child_weight"


{-| Return a photo from a "Authority" entity.
-}
getImageFromBackendAuthorityEntity : BackendAuthorityEntity -> Maybe String
getImageFromBackendAuthorityEntity backendAuthorityEntity =
    let
        getImageFromMeasurement identifier =
            let
                (ImageUrl url) =
                    identifier.entity.value
            in
            Just url
    in
    case backendAuthorityEntity of
        BackendAuthorityPerson identifier ->
            identifier.entity.avatarUrl

        BackendAuthorityPhoto identifier ->
            getImageFromMeasurement identifier

        BackendAuthorityNutritionPhoto identifier ->
            getImageFromMeasurement identifier

        BackendAuthorityPrenatalPhoto identifier ->
            getImageFromMeasurement identifier

        BackendAuthorityStockUpdate identifier ->
            let
                (ImageUrl url) =
                    identifier.entity.signature
            in
            Just url

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
                if syncSpeed.offline < 10000 then
                    10000

                else
                    toFloat syncSpeed.offline

            else
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

        SyncUploadWhatsApp record ->
            checkWebData record.backendRemoteData

        SyncUploadAuthority record ->
            checkWebData record.backendRemoteData

        SyncDownloadGeneral webData ->
            checkWebData webData

        SyncDownloadAuthority webData ->
            checkWebData webData

        SyncDownloadAuthorityDashboardStats webData ->
            checkWebData webData

        _ ->
            syncCycle


getDownloadPhotosSpeedForSubscriptions : Model -> Float
getDownloadPhotosSpeedForSubscriptions model =
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
                        -- It's a network error, so slow things down.
                        if syncSpeed.offline < 10000 then
                            10000

                        else
                            toFloat syncSpeed.offline

                    else
                        syncCycle

                _ ->
                    syncCycle
    in
    case model.downloadPhotosStatus of
        DownloadPhotosIdle ->
            -- Rest until the next sync loop.
            if syncSpeed.idle < 3000 then
                -- Safeguard against too quick iterations.
                3000

            else
                toFloat syncSpeed.idle

        DownloadPhotosInProcess downloadPhotos ->
            case downloadPhotos of
                DownloadPhotosNone ->
                    syncCycle

                DownloadPhotosBatch record ->
                    checkWebData record.backendRemoteData

                DownloadPhotosAll record ->
                    checkWebData record.backendRemoteData


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

        BackendGeneralResilienceSurvey identifier ->
            encode Backend.ResilienceSurvey.Encoder.encodeResilienceSurvey identifier


encodeBackendAuthorityEntity : BackendAuthorityEntity -> Value
encodeBackendAuthorityEntity entity =
    case entity of
        BackendAuthorityAcuteFindings identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteFindings identifier

        BackendAuthorityAcuteIllnessContactsTracing identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessContactsTracing identifier

        BackendAuthorityAcuteIllnessCoreExam identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessCoreExam identifier

        BackendAuthorityAcuteIllnessDangerSigns identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessDangerSigns identifier

        BackendAuthorityAcuteIllnessEncounter identifier ->
            encode Backend.AcuteIllnessEncounter.Encoder.encodeAcuteIllnessEncounter identifier

        BackendAuthorityAcuteIllnessFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessFollowUp identifier

        BackendAuthorityAcuteIllnessMuac identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessMuac identifier

        BackendAuthorityAcuteIllnessNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessNutrition identifier

        BackendAuthorityAcuteIllnessTraceContact identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessTraceContact identifier

        BackendAuthorityAcuteIllnessVitals identifier ->
            encode Backend.Measurement.Encoder.encodeAcuteIllnessVitals identifier

        BackendAuthorityAppointmentConfirmation identifier ->
            encode Backend.Measurement.Encoder.encodeAppointmentConfirmation identifier

        BackendAuthorityAttendance identifier ->
            encode Backend.Measurement.Encoder.encodeAttendance identifier

        BackendAuthorityBreastExam identifier ->
            encode Backend.Measurement.Encoder.encodeBreastExam identifier

        BackendAuthorityBirthPlan identifier ->
            encode Backend.Measurement.Encoder.encodeBirthPlan identifier

        BackendAuthorityCall114 identifier ->
            encode Backend.Measurement.Encoder.encodeCall114 identifier

        BackendAuthorityChildFbf identifier ->
            encode Backend.Measurement.Encoder.encodeChildFbf identifier

        BackendAuthorityChildScoreboardEncounter identifier ->
            encode Backend.ChildScoreboardEncounter.Encoder.encodeChildScoreboardEncounter identifier

        BackendAuthorityChildScoreboardBCGImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardBCGImmunisation identifier

        BackendAuthorityChildScoreboardDTPImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardDTPImmunisation identifier

        BackendAuthorityChildScoreboardDTPStandaloneImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardDTPStandaloneImmunisation identifier

        BackendAuthorityChildScoreboardIPVImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardIPVImmunisation identifier

        BackendAuthorityChildScoreboardMRImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardMRImmunisation identifier

        BackendAuthorityChildScoreboardNCDA identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardNCDA identifier

        BackendAuthorityChildScoreboardOPVImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardOPVImmunisation identifier

        BackendAuthorityChildScoreboardPCV13Immunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardPCV13Immunisation identifier

        BackendAuthorityChildScoreboardRotarixImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeChildScoreboardRotarixImmunisation identifier

        BackendAuthorityClinic identifier ->
            encode Backend.Clinic.Encoder.encodeClinic identifier

        BackendAuthorityContributingFactors identifier ->
            encode Backend.Measurement.Encoder.encodeContributingFactors identifier

        BackendAuthorityCounselingSession identifier ->
            encode Backend.Measurement.Encoder.encodeCounselingSession identifier

        BackendAuthorityCorePhysicalExam identifier ->
            encode Backend.Measurement.Encoder.encodeCorePhysicalExam identifier

        BackendAuthorityCovidTesting identifier ->
            encode Backend.Measurement.Encoder.encodeCovidTesting identifier

        BackendAuthorityDangerSigns identifier ->
            encode Backend.Measurement.Encoder.encodeDangerSigns identifier

        BackendAuthorityDashboardStats identifier ->
            encode Backend.Dashboard.Encoder.encodeDashboardStatsRaw identifier

        BackendAuthorityEducationSession identifier ->
            encode Backend.EducationSession.Encoder.encodeEducationSession identifier

        BackendAuthorityExposure identifier ->
            encode Backend.Measurement.Encoder.encodeExposure identifier

        BackendAuthorityFamilyPlanning identifier ->
            encode Backend.Measurement.Encoder.encodeFamilyPlanning identifier

        BackendAuthorityFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeFollowUp identifier

        BackendAuthorityGroupHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeGroupHealthEducation identifier

        BackendAuthorityGroupNCDA identifier ->
            encode Backend.Measurement.Encoder.encodeGroupNCDA identifier

        BackendAuthorityGroupSendToHC identifier ->
            encode Backend.Measurement.Encoder.encodeGroupSendToHC identifier

        BackendAuthorityHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeHealthEducation identifier

        BackendAuthorityHCContact identifier ->
            encode Backend.Measurement.Encoder.encodeHCContact identifier

        BackendAuthorityHeight identifier ->
            encode Backend.Measurement.Encoder.encodeHeight identifier

        BackendAuthorityHIVDiagnostics identifier ->
            encode Backend.Measurement.Encoder.encodeHIVDiagnostics identifier

        BackendAuthorityHIVEncounter identifier ->
            encode Backend.HIVEncounter.Encoder.encodeHIVEncounter identifier

        BackendAuthorityHIVFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeHIVFollowUp identifier

        BackendAuthorityHIVHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeHIVHealthEducation identifier

        BackendAuthorityHIVMedication identifier ->
            encode Backend.Measurement.Encoder.encodeHIVMedication identifier

        BackendAuthorityHIVReferral identifier ->
            encode Backend.Measurement.Encoder.encodeHIVReferral identifier

        BackendAuthorityHIVSymptomReview identifier ->
            encode Backend.Measurement.Encoder.encodeHIVSymptomReview identifier

        BackendAuthorityHIVTreatmentReview identifier ->
            encode Backend.Measurement.Encoder.encodeHIVTreatmentReview identifier

        BackendAuthorityHomeVisitEncounter identifier ->
            encode Backend.HomeVisitEncounter.Encoder.encodeHomeVisitEncounter identifier

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
            encode Backend.Measurement.Encoder.encodeMotherFbf identifier

        BackendAuthorityMuac identifier ->
            encode Backend.Measurement.Encoder.encodeMuac identifier

        BackendAuthorityNCDCoMorbidities identifier ->
            encode Backend.Measurement.Encoder.encodeNCDCoMorbidities identifier

        BackendAuthorityNCDCoreExam identifier ->
            encode Backend.Measurement.Encoder.encodeNCDCoreExam identifier

        BackendAuthorityNCDCreatinineTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDCreatinineTest identifier

        BackendAuthorityNCDDangerSigns identifier ->
            encode Backend.Measurement.Encoder.encodeNCDDangerSigns identifier

        BackendAuthorityNCDEncounter identifier ->
            encode Backend.NCDEncounter.Encoder.encodeNCDEncounter identifier

        BackendAuthorityNCDFamilyHistory identifier ->
            encode Backend.Measurement.Encoder.encodeNCDFamilyHistory identifier

        BackendAuthorityNCDFamilyPlanning identifier ->
            encode Backend.Measurement.Encoder.encodeNCDFamilyPlanning identifier

        BackendAuthorityNCDHbA1cTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDHbA1cTest identifier

        BackendAuthorityNCDHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeNCDHealthEducation identifier

        BackendAuthorityNCDHIVTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDHIVTest identifier

        BackendAuthorityNCDLabsResults identifier ->
            encode Backend.Measurement.Encoder.encodeNCDLabsResults identifier

        BackendAuthorityNCDLipidPanelTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDLipidPanelTest identifier

        BackendAuthorityNCDLiverFunctionTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDLiverFunctionTest identifier

        BackendAuthorityNCDMedicationDistribution identifier ->
            encode Backend.Measurement.Encoder.encodeNCDMedicationDistribution identifier

        BackendAuthorityNCDMedicationHistory identifier ->
            encode Backend.Measurement.Encoder.encodeNCDMedicationHistory identifier

        BackendAuthorityNCDOutsideCare identifier ->
            encode Backend.Measurement.Encoder.encodeNCDOutsideCare identifier

        BackendAuthorityNCDPregnancyTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDPregnancyTest identifier

        BackendAuthorityNCDRandomBloodSugarTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDRandomBloodSugarTest identifier

        BackendAuthorityNCDReferral identifier ->
            encode Backend.Measurement.Encoder.encodeNCDReferral identifier

        BackendAuthorityNCDSocialHistory identifier ->
            encode Backend.Measurement.Encoder.encodeNCDSocialHistory identifier

        BackendAuthorityNCDSymptomReview identifier ->
            encode Backend.Measurement.Encoder.encodeNCDSymptomReview identifier

        BackendAuthorityNCDUrineDipstickTest identifier ->
            encode Backend.Measurement.Encoder.encodeNCDUrineDipstickTest identifier

        BackendAuthorityNCDVitals identifier ->
            encode Backend.Measurement.Encoder.encodeNCDVitals identifier

        BackendAuthorityNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeNutrition identifier

        BackendAuthorityNutritionCaring identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionCaring identifier

        BackendAuthorityNutritionContributingFactors identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionContributingFactors identifier

        BackendAuthorityNutritionEncounter identifier ->
            encode Backend.NutritionEncounter.Encoder.encodeNutritionEncounter identifier

        BackendAuthorityNutritionFeeding identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionFeeding identifier

        BackendAuthorityNutritionFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionFollowUp identifier

        BackendAuthorityNutritionFoodSecurity identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionFoodSecurity identifier

        BackendAuthorityNutritionHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionHealthEducation identifier

        BackendAuthorityNutritionHeight identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionHeight identifier

        BackendAuthorityNutritionHygiene identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionHygiene identifier

        BackendAuthorityNutritionMuac identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionMuac identifier

        BackendAuthorityNutritionNCDA identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionNCDA identifier

        BackendAuthorityNutritionNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionNutrition identifier

        BackendAuthorityNutritionPhoto identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionPhoto identifier

        BackendAuthorityNutritionSendToHC identifier ->
            encode Backend.Measurement.Encoder.encodeNutritionSendToHC identifier

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

        BackendAuthorityPmtctParticipant identifier ->
            encode Backend.PmtctParticipant.Encoder.encodePmtctParticipant identifier

        BackendAuthorityPregnancyTest identifier ->
            encode Backend.Measurement.Encoder.encodePregnancyTest identifier

        BackendAuthorityPrenatalBloodGpRsTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalBloodGpRsTest identifier

        BackendAuthorityPrenatalBreastfeeding identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalBreastfeeding identifier

        BackendAuthorityPrenatalEncounter identifier ->
            encode Backend.PrenatalEncounter.Encoder.encodePrenatalEncounter identifier

        BackendAuthorityPrenatalFamilyPlanning identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalFamilyPlanning identifier

        BackendAuthorityPrenatalFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalFollowUp identifier

        BackendAuthorityPrenatalGUExam identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalGUExam identifier

        BackendAuthorityPrenatalHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalHealthEducation identifier

        BackendAuthorityPrenatalHemoglobinTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalHemoglobinTest identifier

        BackendAuthorityPrenatalHepatitisBTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalHepatitisBTest identifier

        BackendAuthorityPrenatalHIVTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalHIVTest identifier

        BackendAuthorityPrenatalHIVPCRTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalHIVPCRTest identifier

        BackendAuthorityPrenatalLabsResults identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalLabsResults identifier

        BackendAuthorityPrenatalMalariaTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalMalariaTest identifier

        BackendAuthorityPrenatalMedicationDistribution identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalMedicationDistribution identifier

        BackendAuthorityPrenatalMentalHealth identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalMentalHealth identifier

        BackendAuthorityPrenatalNutrition identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalNutrition identifier

        BackendAuthorityPrenatalOutsideCare identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalOutsideCare identifier

        BackendAuthorityPrenatalPartnerHIVTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalPartnerHIVTest identifier

        BackendAuthorityPrenatalPhoto identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalPhoto identifier

        BackendAuthorityPrenatalRandomBloodSugarTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalRandomBloodSugarTest identifier

        BackendAuthorityPrenatalSendToHC identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalSendToHC identifier

        BackendAuthorityPrenatalSpecialityCare identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalSpecialityCare identifier

        BackendAuthorityPrenatalSymptomReview identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalSymptomReview identifier

        BackendAuthorityPrenatalSyphilisTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalSyphilisTest identifier

        BackendAuthorityPrenatalTetanusImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalTetanusImmunisation identifier

        BackendAuthorityPrenatalUrineDipstickTest identifier ->
            encode Backend.Measurement.Encoder.encodePrenatalUrineDipstickTest identifier

        BackendAuthorityRelationship identifier ->
            encode Backend.Relationship.Encoder.encodeRelationship identifier

        BackendAuthorityMalariaPrevention identifier ->
            encode Backend.Measurement.Encoder.encodeMalariaPrevention identifier

        BackendAuthoritySession identifier ->
            encode Backend.Session.Encoder.encodeSession identifier

        BackendAuthoritySendToHC identifier ->
            encode Backend.Measurement.Encoder.encodeSendToHC identifier

        BackendAuthoritySocialHistory identifier ->
            encode Backend.Measurement.Encoder.encodeSocialHistory identifier

        BackendAuthorityStockUpdate identifier ->
            encode Backend.StockUpdate.Encoder.encodeStockUpdate identifier

        BackendAuthoritySymptomsGeneral identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsGeneral identifier

        BackendAuthoritySymptomsGI identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsGI identifier

        BackendAuthoritySymptomsRespiratory identifier ->
            encode Backend.Measurement.Encoder.encodeSymptomsRespiratory identifier

        BackendAuthorityTravelHistory identifier ->
            encode Backend.Measurement.Encoder.encodeTravelHistory identifier

        BackendAuthorityTreatmentOngoing identifier ->
            encode Backend.Measurement.Encoder.encodeTreatmentOngoing identifier

        BackendAuthorityTreatmentReview identifier ->
            encode Backend.Measurement.Encoder.encodeTreatmentReview identifier

        BackendAuthorityTuberculosisDiagnostics identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisDiagnostics identifier

        BackendAuthorityTuberculosisDOT identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisDOT identifier

        BackendAuthorityTuberculosisEncounter identifier ->
            encode Backend.TuberculosisEncounter.Encoder.encodeTuberculosisEncounter identifier

        BackendAuthorityTuberculosisFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisFollowUp identifier

        BackendAuthorityTuberculosisHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisHealthEducation identifier

        BackendAuthorityTuberculosisMedication identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisMedication identifier

        BackendAuthorityTuberculosisReferral identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisReferral identifier

        BackendAuthorityTuberculosisSymptomReview identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisSymptomReview identifier

        BackendAuthorityTuberculosisTreatmentReview identifier ->
            encode Backend.Measurement.Encoder.encodeTuberculosisTreatmentReview identifier

        BackendAuthorityVitals identifier ->
            encode Backend.Measurement.Encoder.encodeVitals identifier

        BackendAuthorityWeight identifier ->
            encode Backend.Measurement.Encoder.encodeWeight identifier

        BackendAuthorityWellChildAlbendazole identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildAlbendazole identifier

        BackendAuthorityWellChildBCGImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildBCGImmunisation identifier

        BackendAuthorityWellChildCaring identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildCaring identifier

        BackendAuthorityWellChildContributingFactors identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildContributingFactors identifier

        BackendAuthorityWellChildDTPImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildDTPImmunisation identifier

        BackendAuthorityWellChildDTPStandaloneImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildDTPStandaloneImmunisation identifier

        BackendAuthorityWellChildECD identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildECD identifier

        BackendAuthorityWellChildEncounter identifier ->
            encode Backend.WellChildEncounter.Encoder.encodeWellChildEncounter identifier

        BackendAuthorityWellChildFeeding identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildFeeding identifier

        BackendAuthorityWellChildFollowUp identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildFollowUp identifier

        BackendAuthorityWellChildFoodSecurity identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildFoodSecurity identifier

        BackendAuthorityWellChildHeadCircumference identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildHeadCircumference identifier

        BackendAuthorityWellChildHealthEducation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildHealthEducation identifier

        BackendAuthorityWellChildHeight identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildHeight identifier

        BackendAuthorityWellChildHygiene identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildHygiene identifier

        BackendAuthorityWellChildHPVImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildHPVImmunisation identifier

        BackendAuthorityWellChildIPVImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildIPVImmunisation identifier

        BackendAuthorityWellChildMebendezole identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildMebendezole identifier

        BackendAuthorityWellChildMRImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildMRImmunisation identifier

        BackendAuthorityWellChildMuac identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildMuac identifier

        BackendAuthorityWellChildNCDA identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildNCDA identifier

        BackendAuthorityWellChildNextVisit identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildNextVisit identifier

        BackendAuthorityWellChildNutrition identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildNutrition identifier

        BackendAuthorityWellChildOPVImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildOPVImmunisation identifier

        BackendAuthorityWellChildPCV13Immunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildPCV13Immunisation identifier

        BackendAuthorityWellChildPhoto identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildPhoto identifier

        BackendAuthorityWellChildPregnancySummary identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildPregnancySummary identifier

        BackendAuthorityWellChildRotarixImmunisation identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildRotarixImmunisation identifier

        BackendAuthorityWellChildSendToHC identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildSendToHC identifier

        BackendAuthorityWellChildSymptomsReview identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildSymptomsReview identifier

        BackendAuthorityWellChildVitals identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildVitals identifier

        BackendAuthorityWellChildVitaminA identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildVitaminA identifier

        BackendAuthorityWellChildWeight identifier ->
            encode Backend.Measurement.Encoder.encodeWellChildWeight identifier


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


syncInfoStatusToString : SyncInfoStatus -> String
syncInfoStatusToString status =
    case status of
        Downloading ->
            "Downloading"

        Error ->
            "Error"

        NotAvailable ->
            "Not Available"

        Success ->
            "Success"

        Uploading ->
            "Uploading"


syncInfoStatusFromString : String -> Maybe SyncInfoStatus
syncInfoStatusFromString status =
    case status of
        "Downloading" ->
            Just Downloading

        "Error" ->
            Just Error

        "Not Available" ->
            Just NotAvailable

        "Success" ->
            Just Success

        "Uploading" ->
            Just Uploading

        _ ->
            Nothing


siteToString : Site -> String
siteToString site =
    case site of
        SiteRwanda ->
            "rwanda"

        SiteBurundi ->
            "burundi"

        SiteUnknown ->
            ""


siteFromString : String -> Site
siteFromString str =
    case String.toLower str of
        "rwanda" ->
            SiteRwanda

        "burundi" ->
            SiteBurundi

        _ ->
            SiteUnknown


siteFeatureFromString : String -> Maybe SiteFeature
siteFeatureFromString str =
    case String.toLower str of
        "ncda" ->
            Just FeatureNCDA

        "report_to_whatsapp" ->
            Just FeatureReportToWhatsApp

        "stock_management" ->
            Just FeatureStockManagement

        "tuberculosis_management" ->
            Just FeatureTuberculosisManagement

        "group_education" ->
            Just FeatureGroupEducation

        "hiv_management" ->
            Just FeatureHIVManagement

        "gps_coordinates" ->
            Just FeatureGPSCoordinates

        _ ->
            Nothing


siteFeatureToString : SiteFeature -> String
siteFeatureToString feature =
    case feature of
        FeatureNCDA ->
            "ncda"

        FeatureReportToWhatsApp ->
            "report_to_whatsapp"

        FeatureStockManagement ->
            "stock_management"

        FeatureTuberculosisManagement ->
            "tuberculosis_management"

        FeatureGroupEducation ->
            "group_education"

        FeatureHIVManagement ->
            "hiv_management"

        FeatureGPSCoordinates ->
            "gps_coordinates"


siteFeaturesFromString : String -> EverySet SiteFeature
siteFeaturesFromString str =
    String.words str
        |> List.map siteFeatureFromString
        |> Maybe.Extra.values
        |> EverySet.fromList


siteFeaturesToString : EverySet SiteFeature -> String
siteFeaturesToString =
    EverySet.toList
        >> List.map siteFeatureToString
        >> String.join " "


syncInfoGeneralForPort : SyncInfoGeneral -> SyncInfoGeneralForPort
syncInfoGeneralForPort info =
    SyncInfoGeneralForPort
        info.lastFetchedRevisionId
        info.lastSuccesfulContact
        info.remainingToUpload
        info.remainingToDownload
        info.deviceName
        (syncInfoStatusToString info.status)
        info.rollbarToken
        (siteToString info.site)
        (siteFeaturesToString info.features)


syncInfoAuthorityForPort : SyncInfoAuthority -> SyncInfoAuthorityForPort
syncInfoAuthorityForPort info =
    SyncInfoAuthorityForPort
        info.uuid
        info.lastFetchedRevisionId
        info.lastSuccesfulContact
        info.remainingToUpload
        info.remainingToDownload
        info.statsCacheHash
        (syncInfoStatusToString info.status)


syncInfoGeneralFromPort : SyncInfoGeneralForPort -> SyncInfoGeneral
syncInfoGeneralFromPort info =
    SyncInfoGeneral
        info.lastFetchedRevisionId
        info.lastSuccesfulContact
        info.remainingToUpload
        info.remainingToDownload
        info.deviceName
        (syncInfoStatusFromString info.status |> Maybe.withDefault NotAvailable)
        info.rollbarToken
        (siteFromString info.site)
        (siteFeaturesFromString info.features)


syncInfoAuthorityFromPort : SyncInfoAuthorityForPort -> SyncInfoAuthority
syncInfoAuthorityFromPort info =
    SyncInfoAuthority
        info.uuid
        info.lastFetchedRevisionId
        info.lastSuccesfulContact
        info.remainingToUpload
        info.remainingToDownload
        info.statsCacheHash
        (syncInfoStatusFromString info.status |> Maybe.withDefault NotAvailable)


backendGeneralEntityToRevision : BackendGeneralEntity -> Revision
backendGeneralEntityToRevision backendGeneralEntity =
    case backendGeneralEntity of
        BackendGeneralCatchmentArea identifier ->
            CatchmentAreaRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralCounselingSchedule identifier ->
            CounselingScheduleRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralCounselingTopic identifier ->
            CounselingTopicRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralHealthCenter identifier ->
            HealthCenterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralNurse identifier ->
            NurseRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralParticipantForm identifier ->
            ParticipantFormRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralVillage identifier ->
            VillageRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendGeneralResilienceSurvey identifier ->
            ResilienceSurveyRevision (toEntityUuid identifier.uuid) identifier.entity


backendAuthorityEntityToRevision : BackendAuthorityEntity -> Revision
backendAuthorityEntityToRevision backendAuthorityEntity =
    case backendAuthorityEntity of
        BackendAuthorityAcuteFindings identifier ->
            AcuteFindingsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessContactsTracing identifier ->
            AcuteIllnessContactsTracingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessCoreExam identifier ->
            AcuteIllnessCoreExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessDangerSigns identifier ->
            AcuteIllnessDangerSignsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessEncounter identifier ->
            AcuteIllnessEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessFollowUp identifier ->
            AcuteIllnessFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessMuac identifier ->
            AcuteIllnessMuacRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessNutrition identifier ->
            AcuteIllnessNutritionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessTraceContact identifier ->
            AcuteIllnessTraceContactRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAcuteIllnessVitals identifier ->
            AcuteIllnessVitalsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAppointmentConfirmation identifier ->
            AppointmentConfirmationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityAttendance identifier ->
            AttendanceRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityBreastExam identifier ->
            BreastExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityBirthPlan identifier ->
            BirthPlanRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityCall114 identifier ->
            Call114Revision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildFbf identifier ->
            ChildFbfRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardEncounter identifier ->
            ChildScoreboardEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardBCGImmunisation identifier ->
            ChildScoreboardBCGImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardDTPImmunisation identifier ->
            ChildScoreboardDTPImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardDTPStandaloneImmunisation identifier ->
            ChildScoreboardDTPStandaloneImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardIPVImmunisation identifier ->
            ChildScoreboardIPVImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardMRImmunisation identifier ->
            ChildScoreboardMRImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardNCDA identifier ->
            ChildScoreboardNCDARevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardOPVImmunisation identifier ->
            ChildScoreboardOPVImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardPCV13Immunisation identifier ->
            ChildScoreboardPCV13ImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityChildScoreboardRotarixImmunisation identifier ->
            ChildScoreboardRotarixImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityClinic identifier ->
            ClinicRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityContributingFactors identifier ->
            ContributingFactorsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityCounselingSession identifier ->
            CounselingSessionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityCorePhysicalExam identifier ->
            CorePhysicalExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityCovidTesting identifier ->
            CovidTestingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityDangerSigns identifier ->
            DangerSignsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityDashboardStats identifier ->
            DashboardStatsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityEducationSession identifier ->
            EducationSessionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityExposure identifier ->
            ExposureRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityFamilyPlanning identifier ->
            FamilyPlanningRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityFollowUp identifier ->
            FollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityGroupHealthEducation identifier ->
            GroupHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityGroupNCDA identifier ->
            GroupNCDARevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityGroupSendToHC identifier ->
            GroupSendToHCRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHealthEducation identifier ->
            HealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHCContact identifier ->
            HCContactRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHeight identifier ->
            HeightRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVDiagnostics identifier ->
            HIVDiagnosticsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVEncounter identifier ->
            HIVEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVFollowUp identifier ->
            HIVFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVHealthEducation identifier ->
            HIVHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVMedication identifier ->
            HIVMedicationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVReferral identifier ->
            HIVReferralRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVSymptomReview identifier ->
            HIVSymptomReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHIVTreatmentReview identifier ->
            HIVTreatmentReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityHomeVisitEncounter identifier ->
            HomeVisitEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityIndividualParticipant identifier ->
            IndividualEncounterParticipantRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityIsolation identifier ->
            IsolationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityLactation identifier ->
            LactationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityLastMenstrualPeriod identifier ->
            LastMenstrualPeriodRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMalariaTesting identifier ->
            MalariaTestingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMedicalHistory identifier ->
            MedicalHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMedication identifier ->
            MedicationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMedicationDistribution identifier ->
            MedicationDistributionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMotherFbf identifier ->
            MotherFbfRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMuac identifier ->
            MuacRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDCoMorbidities identifier ->
            NCDCoMorbiditiesRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDCoreExam identifier ->
            NCDCoreExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDCreatinineTest identifier ->
            NCDCreatinineTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDDangerSigns identifier ->
            NCDDangerSignsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDEncounter identifier ->
            NCDEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDFamilyHistory identifier ->
            NCDFamilyHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDFamilyPlanning identifier ->
            NCDFamilyPlanningRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDHbA1cTest identifier ->
            NCDHbA1cTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDHealthEducation identifier ->
            NCDHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDHIVTest identifier ->
            NCDHIVTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDLabsResults identifier ->
            NCDLabsResultsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDLipidPanelTest identifier ->
            NCDLipidPanelTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDLiverFunctionTest identifier ->
            NCDLiverFunctionTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDMedicationDistribution identifier ->
            NCDMedicationDistributionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDMedicationHistory identifier ->
            NCDMedicationHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDOutsideCare identifier ->
            NCDOutsideCareRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDPregnancyTest identifier ->
            NCDPregnancyTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDRandomBloodSugarTest identifier ->
            NCDRandomBloodSugarTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDReferral identifier ->
            NCDReferralRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDSocialHistory identifier ->
            NCDSocialHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDSymptomReview identifier ->
            NCDSymptomReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDUrineDipstickTest identifier ->
            NCDUrineDipstickTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNCDVitals identifier ->
            NCDVitalsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutrition identifier ->
            ChildNutritionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionCaring identifier ->
            NutritionCaringRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionContributingFactors identifier ->
            NutritionContributingFactorsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionEncounter identifier ->
            NutritionEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionFeeding identifier ->
            NutritionFeedingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionFollowUp identifier ->
            NutritionFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionFoodSecurity identifier ->
            NutritionFoodSecurityRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionHealthEducation identifier ->
            NutritionHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionHeight identifier ->
            NutritionHeightRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionHygiene identifier ->
            NutritionHygieneRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionMuac identifier ->
            NutritionMuacRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionNCDA identifier ->
            NutritionNCDARevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionNutrition identifier ->
            NutritionNutritionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionPhoto identifier ->
            NutritionPhotoRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionSendToHC identifier ->
            NutritionSendToHCRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityNutritionWeight identifier ->
            NutritionWeightRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityObstetricHistory identifier ->
            ObstetricHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityObstetricHistoryStep2 identifier ->
            ObstetricHistoryStep2Revision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityObstetricalExam identifier ->
            ObstetricalExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityParticipantConsent identifier ->
            ParticipantConsentRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPerson identifier ->
            PersonRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPhoto identifier ->
            PhotoRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPmtctParticipant identifier ->
            PmtctParticipantRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPregnancyTest identifier ->
            PregnancyTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalBloodGpRsTest identifier ->
            PrenatalBloodGpRsTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalBreastfeeding identifier ->
            PrenatalBreastfeedingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalEncounter identifier ->
            PrenatalEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalFamilyPlanning identifier ->
            PrenatalFamilyPlanningRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalFollowUp identifier ->
            PrenatalFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalGUExam identifier ->
            PrenatalGUExamRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalHealthEducation identifier ->
            PrenatalHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalHemoglobinTest identifier ->
            PrenatalHemoglobinTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalHepatitisBTest identifier ->
            PrenatalHepatitisBTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalHIVTest identifier ->
            PrenatalHIVTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalHIVPCRTest identifier ->
            PrenatalHIVPCRTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalLabsResults identifier ->
            PrenatalLabsResultsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalMalariaTest identifier ->
            PrenatalMalariaTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalMedicationDistribution identifier ->
            PrenatalMedicationDistributionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalMentalHealth identifier ->
            PrenatalMentalHealthRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalNutrition identifier ->
            PrenatalNutritionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalOutsideCare identifier ->
            PrenatalOutsideCareRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalPartnerHIVTest identifier ->
            PrenatalPartnerHIVTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalPhoto identifier ->
            PrenatalPhotoRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalRandomBloodSugarTest identifier ->
            PrenatalRandomBloodSugarTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalSendToHC identifier ->
            PrenatalSendToHCRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalSpecialityCare identifier ->
            PrenatalSpecialityCareRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalSymptomReview identifier ->
            PrenatalSymptomReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalSyphilisTest identifier ->
            PrenatalSyphilisTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalTetanusImmunisation identifier ->
            PrenatalTetanusImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityPrenatalUrineDipstickTest identifier ->
            PrenatalUrineDipstickTestRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityRelationship identifier ->
            RelationshipRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityMalariaPrevention identifier ->
            MalariaPreventionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySendToHC identifier ->
            SendToHCRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySession identifier ->
            SessionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySocialHistory identifier ->
            SocialHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityStockUpdate identifier ->
            StockUpdateRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySymptomsGeneral identifier ->
            SymptomsGeneralRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySymptomsGI identifier ->
            SymptomsGIRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthoritySymptomsRespiratory identifier ->
            SymptomsRespiratoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTravelHistory identifier ->
            TravelHistoryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTreatmentOngoing identifier ->
            TreatmentOngoingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTreatmentReview identifier ->
            TreatmentReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisDiagnostics identifier ->
            TuberculosisDiagnosticsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisDOT identifier ->
            TuberculosisDOTRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisEncounter identifier ->
            TuberculosisEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisFollowUp identifier ->
            TuberculosisFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisHealthEducation identifier ->
            TuberculosisHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisMedication identifier ->
            TuberculosisMedicationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisReferral identifier ->
            TuberculosisReferralRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisSymptomReview identifier ->
            TuberculosisSymptomReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityTuberculosisTreatmentReview identifier ->
            TuberculosisTreatmentReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityVitals identifier ->
            VitalsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWeight identifier ->
            WeightRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildAlbendazole identifier ->
            WellChildAlbendazoleRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildBCGImmunisation identifier ->
            WellChildBCGImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildCaring identifier ->
            WellChildCaringRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildContributingFactors identifier ->
            WellChildContributingFactorsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildDTPImmunisation identifier ->
            WellChildDTPImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildDTPStandaloneImmunisation identifier ->
            WellChildDTPStandaloneImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildECD identifier ->
            WellChildECDRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildEncounter identifier ->
            WellChildEncounterRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildFeeding identifier ->
            WellChildFeedingRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildFollowUp identifier ->
            WellChildFollowUpRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildFoodSecurity identifier ->
            WellChildFoodSecurityRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildHeadCircumference identifier ->
            WellChildHeadCircumferenceRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildHealthEducation identifier ->
            WellChildHealthEducationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildHeight identifier ->
            WellChildHeightRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildHygiene identifier ->
            WellChildHygieneRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildHPVImmunisation identifier ->
            WellChildHPVImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildIPVImmunisation identifier ->
            WellChildIPVImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildMebendezole identifier ->
            WellChildMebendezoleRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildMRImmunisation identifier ->
            WellChildMRImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildMuac identifier ->
            WellChildMuacRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildNCDA identifier ->
            WellChildNCDARevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildNextVisit identifier ->
            WellChildNextVisitRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildNutrition identifier ->
            WellChildNutritionRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildOPVImmunisation identifier ->
            WellChildOPVImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildPCV13Immunisation identifier ->
            WellChildPCV13ImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildPhoto identifier ->
            WellChildPhotoRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildPregnancySummary identifier ->
            WellChildPregnancySummaryRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildRotarixImmunisation identifier ->
            WellChildRotarixImmunisationRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildSendToHC identifier ->
            WellChildSendToHCRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildSymptomsReview identifier ->
            WellChildSymptomsReviewRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildVitals identifier ->
            WellChildVitalsRevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildVitaminA identifier ->
            WellChildVitaminARevision (toEntityUuid identifier.uuid) identifier.entity

        BackendAuthorityWellChildWeight identifier ->
            WellChildWeightRevision (toEntityUuid identifier.uuid) identifier.entity


resolveIncidentDetailsMsg : Http.Error -> List Msg
resolveIncidentDetailsMsg error =
    case error of
        Http.BadStatus response ->
            case Json.Decode.decodeString Utils.WebData.decodeDrupalError response.body of
                Ok decoded ->
                    if String.startsWith "Could not find UUID" decoded.title then
                        let
                            uuidAsString =
                                String.dropLeft 21 decoded.title
                        in
                        [ QueryIndexDb <| IndexDbQueryGetShardsEntityByUuid uuidAsString ]

                    else
                        []

                Err _ ->
                    []

        _ ->
            []


fileUploadFailureThreshold : Int
fileUploadFailureThreshold =
    5
