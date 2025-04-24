module SyncManager.View exposing (view)

import App.Model exposing (ConfiguredModel)
import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import Editable
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra exposing (isJust)
import RemoteData exposing (RemoteData, WebData)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , DownloadSyncResponse
        , Model
        , Msg(..)
        , SyncCycle(..)
        , SyncStatus(..)
        )
import SyncManager.Utils exposing (getSyncedHealthCenters)
import Translate exposing (Language)
import Utils.Html exposing (spinner)
import Utils.WebData


view : Language -> RemoteData String ConfiguredModel -> ModelIndexedDb -> Model -> Html Msg
view language configuredModel db model =
    case RemoteData.toMaybe configuredModel of
        Just configuration ->
            if not configuration.config.debug then
                emptyNode

            else
                details
                    [ property "open" (Json.Encode.bool False)
                    , class "sync-manager"
                    ]
                    [ summary [] [ text "Sync Manager" ]
                    , viewHealthCentersForSync language db model
                    , viewSyncSettings model
                    , viewSyncStatus language db model
                    ]

        Nothing ->
            emptyNode


viewSyncSettings : Model -> Html Msg
viewSyncSettings model =
    let
        currentStatus =
            case model.syncCycle of
                SyncCycleOn ->
                    "Cycle is currently on"

                SyncCycleStayOnCurrentSyncStatus ->
                    "Cycle is currently off, sync continues, but won't rotate to a new sync step"

                SyncCyclePause ->
                    "Sync is completely paused"

        getOption status =
            case status of
                SyncCycleOn ->
                    { name = "On"
                    , icon = "recycle"
                    , msg = SetSyncCycle SyncCycleOn
                    }

                SyncCycleStayOnCurrentSyncStatus ->
                    { name = "Stay on current sync step"
                    , icon = "repeat"
                    , msg = SetSyncCycle SyncCycleStayOnCurrentSyncStatus
                    }

                SyncCyclePause ->
                    { name = "Pause"
                    , icon = "pause"
                    , msg = SetSyncCycle SyncCyclePause
                    }

        syncCycleButtons =
            div []
                (List.map
                    (\status ->
                        let
                            record =
                                getOption status
                        in
                        button
                            [ classList
                                [ ( "ui labeled icon button", True )
                                , ( "active", model.syncCycle == status )
                                ]
                            , onClick record.msg
                            ]
                            [ i [ class <| "icon " ++ record.icon ] []
                            , text record.name
                            ]
                    )
                    [ SyncCycleOn, SyncCycleStayOnCurrentSyncStatus, SyncCyclePause ]
                )

        syncSpeed =
            Editable.value model.syncSpeed
    in
    details
        [ property "open" (Json.Encode.bool False)
        , class "segment ui"
        ]
        [ summary [] [ text "Sync Settings" ]
        , fieldset []
            [ div [] [ text "Sync Speed" ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Idle time" ]
                , input
                    [ type_ "number"

                    -- No less than every 3 second. On production it should be
                    -- no less than 10 seconds.
                    , Html.Attributes.min (String.fromInt <| 3 * 1000)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.idle
                    , onInput SetSyncSpeedIdle
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Cycle time" ]
                , input
                    [ type_ "number"

                    -- No less than every 50 ms.
                    , Html.Attributes.min (String.fromInt <| 50)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.cycle
                    , onInput SetSyncSpeedCycle
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Offline time" ]
                , input
                    [ type_ "number"

                    -- No less than every 1000 ms.
                    , Html.Attributes.min (String.fromInt <| 1000)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.offline
                    , onInput SetSyncSpeedOffline
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div []
                [ button
                    [ onClick SaveSettings
                    , class "ui primary button"
                    ]
                    [ text "Save" ]
                , button
                    [ onClick ResetSettings
                    , class "ui button"
                    ]
                    [ text "Reset Settings" ]
                ]
            ]
        , fieldset []
            [ div [] [ text "Sync Cycle" ]
            , div [] [ text <| "Current status: " ++ currentStatus ]
            , syncCycleButtons
            ]
        ]


viewSyncDownloadGeneral : Language -> Model -> WebData (DownloadSyncResponse BackendGeneralEntity) -> Html Msg
viewSyncDownloadGeneral language model webData =
    div []
        [ div [] [ text <| "Fetch from General from revision ID " ++ String.fromInt model.syncInfoGeneral.lastFetchedRevisionId ]
        , button [ onClick <| SyncManager.Model.SetLastFetchedRevisionIdGeneral 0 ] [ text "Reset revision ID to 0" ]
        , div [] [ text "HTTP requests:" ]
        , case webData of
            RemoteData.Success data ->
                div []
                    [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                    , if List.isEmpty data.entities then
                        div [] [ text "No content fetched in last HTTP request" ]

                      else
                        ol [] (List.map (viewGeneralEntity language) data.entities)
                    ]

            RemoteData.Failure error ->
                text <| Debug.toString error

            RemoteData.Loading ->
                spinner

            RemoteData.NotAsked ->
                emptyNode
        ]


viewGeneralEntity : Language -> BackendGeneralEntity -> Html msg
viewGeneralEntity language backendGeneralEntity =
    li []
        [ case backendGeneralEntity of
            BackendGeneralCatchmentArea identifier ->
                text <| "Catchment area " ++ identifier.entity.name

            BackendGeneralCounselingSchedule identifier ->
                text <| "Counseling Schedule " ++ identifier.uuid

            BackendGeneralCounselingTopic identifier ->
                text <| "Counseling Topic " ++ identifier.entity.english

            BackendGeneralHealthCenter identifier ->
                text <| "Health Center " ++ identifier.entity.name

            BackendGeneralNurse identifier ->
                text <| "Nurse " ++ identifier.entity.name

            BackendGeneralParticipantForm identifier ->
                text <| "Participant Form " ++ identifier.entity.title.english

            BackendGeneralVillage identifier ->
                text <| "Village " ++ identifier.entity.name

            BackendGeneralResilienceSurvey identifier ->
                text <| "Resilience Survey " ++ identifier.uuid
        ]


viewSyncDownloadAuthority : ModelIndexedDb -> Model -> WebData (DownloadSyncResponse BackendAuthorityEntity) -> Html Msg
viewSyncDownloadAuthority db model webData =
    case model.syncInfoAuthorities of
        Nothing ->
            emptyNode

        Just zipper ->
            let
                currentZipper =
                    Zipper.current zipper

                getAuthorityName uuid =
                    db.healthCenters
                        |> RemoteData.toMaybe
                        |> Maybe.andThen (\healthCenters -> Dict.get (toEntityUuid uuid) healthCenters)
                        |> Maybe.map (\healthCenter -> healthCenter.name)
                        |> Maybe.withDefault uuid

                authoritiesListHtml =
                    Zipper.toList zipper
                        |> List.map
                            (\row ->
                                if row.uuid == currentZipper.uuid then
                                    li [ class "active" ] [ text <| getAuthorityName row.uuid ++ " (from revision ID " ++ String.fromInt row.lastFetchedRevisionId ++ ")" ]

                                else
                                    li [] [ text <| getAuthorityName row.uuid ]
                            )
            in
            div []
                [ div [] [ text <| "Fetch from Authority" ]
                , ol [] authoritiesListHtml
                , button [ onClick <| SyncManager.Model.SetLastFetchedRevisionIdAuthority zipper 0 ] [ text "Reset revision ID to 0" ]
                , case webData of
                    RemoteData.Success data ->
                        div []
                            [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                            , if List.isEmpty data.entities then
                                div [] [ text "No content fetched in last HTTP request" ]

                              else
                                div []
                                    [ div [] [ text <| "Here is the content we've fetched in the last HTTP request:" ]
                                    , ol [] (List.map viewAuthorityEntity data.entities)
                                    ]
                            ]

                    RemoteData.Failure error ->
                        text <| Debug.toString error

                    RemoteData.Loading ->
                        spinner

                    RemoteData.NotAsked ->
                        emptyNode
                ]


viewAuthorityEntity : BackendAuthorityEntity -> Html msg
viewAuthorityEntity backendAuthorityEntity =
    let
        viewMeasurement identifier name =
            text (name ++ " for person ID " ++ fromEntityUuid identifier.entity.participantId)
    in
    li []
        [ case backendAuthorityEntity of
            BackendAuthorityAcuteFindings identifier ->
                viewMeasurement identifier "Acute Findings"

            BackendAuthorityAcuteIllnessContactsTracing identifier ->
                viewMeasurement identifier "Acute Illness Contacts Tracing"

            BackendAuthorityAcuteIllnessCoreExam identifier ->
                viewMeasurement identifier "AcuteI llness Core Exam"

            BackendAuthorityAcuteIllnessDangerSigns identifier ->
                viewMeasurement identifier "AcuteI llness Danger Signs"

            BackendAuthorityAcuteIllnessEncounter identifier ->
                text ("Acute Illness Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityAcuteIllnessFollowUp identifier ->
                viewMeasurement identifier "Acute Illness Follow Up"

            BackendAuthorityAcuteIllnessMuac identifier ->
                viewMeasurement identifier "Acute Illness Muac"

            BackendAuthorityAcuteIllnessNutrition identifier ->
                viewMeasurement identifier "Acute Illness Nutrition"

            BackendAuthorityAcuteIllnessTraceContact identifier ->
                viewMeasurement identifier "Acute Illness Trace Contact"

            BackendAuthorityAcuteIllnessVitals identifier ->
                viewMeasurement identifier "Acute Illness Vitals"

            BackendAuthorityAppointmentConfirmation identifier ->
                viewMeasurement identifier "Appointment Confirmation"

            BackendAuthorityAttendance identifier ->
                viewMeasurement identifier "Attendance"

            BackendAuthorityBreastExam identifier ->
                viewMeasurement identifier "Breast Exam"

            BackendAuthorityBirthPlan identifier ->
                viewMeasurement identifier "Birth Plan"

            BackendAuthorityCall114 identifier ->
                viewMeasurement identifier "Call 114"

            BackendAuthorityChildFbf identifier ->
                viewMeasurement identifier "Child Fbf"

            BackendAuthorityChildScoreboardEncounter identifier ->
                text ("Child Scoreboard Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityChildScoreboardBCGImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard BCG Immunisation"

            BackendAuthorityChildScoreboardDTPImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard DTP Immunisation"

            BackendAuthorityChildScoreboardDTPStandaloneImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard DTP standalone Immunisation"

            BackendAuthorityChildScoreboardIPVImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard IPV Immunisation"

            BackendAuthorityChildScoreboardMRImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard MR Immunisation"

            BackendAuthorityChildScoreboardNCDA identifier ->
                viewMeasurement identifier "Child Scoreboard NCDA"

            BackendAuthorityChildScoreboardOPVImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard OPV Immunisation"

            BackendAuthorityChildScoreboardPCV13Immunisation identifier ->
                viewMeasurement identifier "Child Scoreboard PCV13 Immunisation"

            BackendAuthorityChildScoreboardRotarixImmunisation identifier ->
                viewMeasurement identifier "Child Scoreboard Rotarix Immunisation"

            BackendAuthorityClinic identifier ->
                text <| "Clinic " ++ identifier.entity.name

            BackendAuthorityContributingFactors identifier ->
                viewMeasurement identifier "Contributing Factors"

            BackendAuthorityCounselingSession identifier ->
                viewMeasurement identifier "Counseling Session"

            BackendAuthorityCorePhysicalExam identifier ->
                viewMeasurement identifier "Core Physical Exam"

            BackendAuthorityCovidTesting identifier ->
                viewMeasurement identifier "Covid Testing"

            BackendAuthorityDangerSigns identifier ->
                viewMeasurement identifier "Danger Signs"

            BackendAuthorityDashboardStats _ ->
                text "Dashboard Statistics"

            BackendAuthorityEducationSession identifier ->
                text ("Education Session at village ID " ++ fromEntityUuid identifier.entity.village)

            BackendAuthorityExposure identifier ->
                viewMeasurement identifier "Exposure"

            BackendAuthorityFamilyPlanning identifier ->
                viewMeasurement identifier "Family Planning"

            BackendAuthorityFollowUp identifier ->
                viewMeasurement identifier "Follow Up"

            BackendAuthorityGroupHealthEducation identifier ->
                viewMeasurement identifier "Group Health Education"

            BackendAuthorityGroupNCDA identifier ->
                viewMeasurement identifier "Group NCDA"

            BackendAuthorityGroupSendToHC identifier ->
                viewMeasurement identifier "Group Send to HC"

            BackendAuthorityHealthEducation identifier ->
                viewMeasurement identifier "Health Education"

            BackendAuthorityHCContact identifier ->
                viewMeasurement identifier "HC Contact"

            BackendAuthorityHeight identifier ->
                viewMeasurement identifier "Height"

            BackendAuthorityHIVDiagnostics identifier ->
                viewMeasurement identifier "HIV Diagnostics"

            BackendAuthorityHIVEncounter identifier ->
                text ("HIV Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityHIVFollowUp identifier ->
                viewMeasurement identifier "HIV Follow Up"

            BackendAuthorityHIVHealthEducation identifier ->
                viewMeasurement identifier "HIV Health Education"

            BackendAuthorityHIVMedication identifier ->
                viewMeasurement identifier "HIV Medication"

            BackendAuthorityHIVReferral identifier ->
                viewMeasurement identifier "HIV Referral"

            BackendAuthorityHIVSymptomReview identifier ->
                viewMeasurement identifier "HIV Symptom Review"

            BackendAuthorityHIVTreatmentReview identifier ->
                viewMeasurement identifier "HIV Treatment Review"

            BackendAuthorityHomeVisitEncounter identifier ->
                text ("Home Visit Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityIndividualParticipant identifier ->
                text <| "Individual Participant for person ID " ++ fromEntityUuid identifier.entity.person

            BackendAuthorityIsolation identifier ->
                viewMeasurement identifier "Isolation"

            BackendAuthorityLactation identifier ->
                viewMeasurement identifier "Lactation"

            BackendAuthorityLastMenstrualPeriod identifier ->
                viewMeasurement identifier "Menstrual Period"

            BackendAuthorityMalariaTesting identifier ->
                viewMeasurement identifier "Malaria Testing"

            BackendAuthorityMedicalHistory identifier ->
                viewMeasurement identifier "Medical History"

            BackendAuthorityMedication identifier ->
                viewMeasurement identifier "Medication"

            BackendAuthorityMedicationDistribution identifier ->
                viewMeasurement identifier "Medication Distribution"

            BackendAuthorityMotherFbf identifier ->
                viewMeasurement identifier "Mother Fbf"

            BackendAuthorityMuac identifier ->
                viewMeasurement identifier "Muac"

            BackendAuthorityNCDCoMorbidities identifier ->
                viewMeasurement identifier "NCDCoMorbidities"

            BackendAuthorityNCDCoreExam identifier ->
                viewMeasurement identifier "NCDCoreExam"

            BackendAuthorityNCDCreatinineTest identifier ->
                viewMeasurement identifier "NCDCreatinineTest"

            BackendAuthorityNCDDangerSigns identifier ->
                viewMeasurement identifier "NCDDangerSigns"

            BackendAuthorityNCDEncounter identifier ->
                text ("NCDEncounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityNCDFamilyHistory identifier ->
                viewMeasurement identifier "NCDFamilyHistory"

            BackendAuthorityNCDFamilyPlanning identifier ->
                viewMeasurement identifier "NCDFamilyPlanning"

            BackendAuthorityNCDHbA1cTest identifier ->
                viewMeasurement identifier "NCDHbA1cTest"

            BackendAuthorityNCDHealthEducation identifier ->
                viewMeasurement identifier "NCDHealthEducation"

            BackendAuthorityNCDHIVTest identifier ->
                viewMeasurement identifier "NCDHIVTest"

            BackendAuthorityNCDLabsResults identifier ->
                viewMeasurement identifier "NCDLabsResults"

            BackendAuthorityNCDLipidPanelTest identifier ->
                viewMeasurement identifier "NCDLipidPanelTest"

            BackendAuthorityNCDLiverFunctionTest identifier ->
                viewMeasurement identifier "NCDLiverFunctionTest"

            BackendAuthorityNCDMedicationDistribution identifier ->
                viewMeasurement identifier "NCDMedicationDistribution"

            BackendAuthorityNCDMedicationHistory identifier ->
                viewMeasurement identifier "NCDMedicationHistory"

            BackendAuthorityNCDOutsideCare identifier ->
                viewMeasurement identifier "NCDOutsideCare"

            BackendAuthorityNCDPregnancyTest identifier ->
                viewMeasurement identifier "NCDPregnancyTest"

            BackendAuthorityNCDRandomBloodSugarTest identifier ->
                viewMeasurement identifier "NCDRandomBloodSugarTest"

            BackendAuthorityNCDReferral identifier ->
                viewMeasurement identifier "NCDReferral"

            BackendAuthorityNCDSocialHistory identifier ->
                viewMeasurement identifier "NCDSocialHistory"

            BackendAuthorityNCDSymptomReview identifier ->
                viewMeasurement identifier "NCDSymptomReview"

            BackendAuthorityNCDUrineDipstickTest identifier ->
                viewMeasurement identifier "NCDUrineDipstickTest"

            BackendAuthorityNCDVitals identifier ->
                viewMeasurement identifier "NCDVitals"

            BackendAuthorityNutrition identifier ->
                viewMeasurement identifier "Nutrition"

            BackendAuthorityNutritionCaring identifier ->
                viewMeasurement identifier "Nutrition Caring"

            BackendAuthorityNutritionContributingFactors identifier ->
                viewMeasurement identifier "Nutrition Contributing Factors"

            BackendAuthorityNutritionEncounter identifier ->
                text ("Nutrition Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityNutritionFeeding identifier ->
                viewMeasurement identifier "Nutrition Feeding"

            BackendAuthorityNutritionFollowUp identifier ->
                viewMeasurement identifier "Nutrition Follow Up"

            BackendAuthorityNutritionFoodSecurity identifier ->
                viewMeasurement identifier "Nutrition Food Security"

            BackendAuthorityNutritionHealthEducation identifier ->
                viewMeasurement identifier "Nutrition Health Education"

            BackendAuthorityNutritionHeight identifier ->
                viewMeasurement identifier "Nutrition Height"

            BackendAuthorityNutritionHygiene identifier ->
                viewMeasurement identifier "Nutrition Hygiene"

            BackendAuthorityNutritionMuac identifier ->
                viewMeasurement identifier "Nutrition Muac"

            BackendAuthorityNutritionNCDA identifier ->
                viewMeasurement identifier "Nutrition NCDA"

            BackendAuthorityNutritionNutrition identifier ->
                viewMeasurement identifier "Nutrition Nutrition"

            BackendAuthorityNutritionPhoto identifier ->
                viewMeasurement identifier "Nutrition Photo"

            BackendAuthorityNutritionSendToHC identifier ->
                viewMeasurement identifier "Nutrition Send to HC"

            BackendAuthorityNutritionWeight identifier ->
                viewMeasurement identifier "Nutrition Weight"

            BackendAuthorityObstetricHistory identifier ->
                viewMeasurement identifier "Obstetric History"

            BackendAuthorityObstetricHistoryStep2 identifier ->
                viewMeasurement identifier "Obstetric History Step 2"

            BackendAuthorityObstetricalExam identifier ->
                viewMeasurement identifier "Obstetrical Exam"

            BackendAuthorityParticipantConsent identifier ->
                viewMeasurement identifier "Participant Consent"

            BackendAuthorityPerson identifier ->
                text <| "Person " ++ identifier.entity.name

            BackendAuthorityPhoto identifier ->
                viewMeasurement identifier "Photo"

            BackendAuthorityPmtctParticipant identifier ->
                text <| "Pmtct Participant for child ID " ++ fromEntityUuid identifier.entity.child

            BackendAuthorityPregnancyTest identifier ->
                viewMeasurement identifier "Pregnancy Testing"

            BackendAuthorityPrenatalEncounter identifier ->
                text <| "Prenatal Encounter for person ID " ++ fromEntityUuid identifier.entity.participant

            BackendAuthorityPrenatalBloodGpRsTest identifier ->
                viewMeasurement identifier "Prenatal Blood GpRs Test"

            BackendAuthorityPrenatalBreastfeeding identifier ->
                viewMeasurement identifier "Prenatal Breastfeeding"

            BackendAuthorityPrenatalCalcium identifier ->
                viewMeasurement identifier "Prenatal Calcium"

            BackendAuthorityPrenatalFamilyPlanning identifier ->
                viewMeasurement identifier "Prenatal Family Planning"

            BackendAuthorityPrenatalFolate identifier ->
                viewMeasurement identifier "Prenatal Folate"

            BackendAuthorityPrenatalFollowUp identifier ->
                viewMeasurement identifier "Prenatal Follow Up"

            BackendAuthorityPrenatalGUExam identifier ->
                viewMeasurement identifier "Prenatal GU Exam"

            BackendAuthorityPrenatalHealthEducation identifier ->
                viewMeasurement identifier "Prenatal Health Education"

            BackendAuthorityPrenatalHemoglobinTest identifier ->
                viewMeasurement identifier "Prenatal Hemoglobin Test"

            BackendAuthorityPrenatalHepatitisBTest identifier ->
                viewMeasurement identifier "Prenatal Hepatitis B Test"

            BackendAuthorityPrenatalHIVTest identifier ->
                viewMeasurement identifier "Prenatal HIV Test"

            BackendAuthorityPrenatalHIVPCRTest identifier ->
                viewMeasurement identifier "Prenatal HIV PCR Test"

            BackendAuthorityPrenatalIron identifier ->
                viewMeasurement identifier "Prenatal Iron"

            BackendAuthorityPrenatalLabsResults identifier ->
                viewMeasurement identifier "Prenatal Labs Results"

            BackendAuthorityPrenatalMalariaTest identifier ->
                viewMeasurement identifier "Prenatal Malaria Test"

            BackendAuthorityPrenatalMedicationDistribution identifier ->
                viewMeasurement identifier "Prenatal Medication Distribution"

            BackendAuthorityPrenatalMentalHealth identifier ->
                viewMeasurement identifier "Prenatal Mental Health"

            BackendAuthorityPrenatalMMS identifier ->
                viewMeasurement identifier "Prenatal MMS"

            BackendAuthorityPrenatalNutrition identifier ->
                viewMeasurement identifier "Prenatal Nutrition"

            BackendAuthorityPrenatalOutsideCare identifier ->
                viewMeasurement identifier "Prenatal Outside Care"

            BackendAuthorityPrenatalPartnerHIVTest identifier ->
                viewMeasurement identifier "Prenatal Partner HIV Test"

            BackendAuthorityPrenatalPhoto identifier ->
                viewMeasurement identifier "Prenatal Photo"

            BackendAuthorityPrenatalRandomBloodSugarTest identifier ->
                viewMeasurement identifier "Prenatal Random Blood Sugar Test"

            BackendAuthorityPrenatalSendToHC identifier ->
                viewMeasurement identifier "Prenatal Send to HC"

            BackendAuthorityPrenatalSpecialityCare identifier ->
                viewMeasurement identifier "Prenatal Speciality Care"

            BackendAuthorityPrenatalSymptomReview identifier ->
                viewMeasurement identifier "Prenatal Symptom Review"

            BackendAuthorityPrenatalSyphilisTest identifier ->
                viewMeasurement identifier "Prenatal Syphilis Test"

            BackendAuthorityPrenatalTetanusImmunisation identifier ->
                viewMeasurement identifier "Prenatal Tetanus Immunisation"

            BackendAuthorityPrenatalUrineDipstickTest identifier ->
                viewMeasurement identifier "Prenatal Urine Dipstick Test"

            BackendAuthorityRelationship identifier ->
                text <| "Relationship for person ID " ++ fromEntityUuid identifier.entity.person

            BackendAuthorityMalariaPrevention identifier ->
                viewMeasurement identifier "Malaria Prevention"

            BackendAuthoritySendToHC identifier ->
                viewMeasurement identifier "Send to HC"

            BackendAuthoritySession identifier ->
                text <| "Session for Clinic ID " ++ fromEntityUuid identifier.entity.clinicId

            BackendAuthoritySocialHistory identifier ->
                viewMeasurement identifier "Social History"

            BackendAuthorityStockUpdate identifier ->
                text <| "Stock Update " ++ identifier.uuid

            BackendAuthoritySymptomsGeneral identifier ->
                viewMeasurement identifier "Symptoms General"

            BackendAuthoritySymptomsGI identifier ->
                viewMeasurement identifier "Symptoms GI"

            BackendAuthoritySymptomsRespiratory identifier ->
                viewMeasurement identifier "Symptoms Respiratory"

            BackendAuthorityTravelHistory identifier ->
                viewMeasurement identifier "Travel History"

            BackendAuthorityTreatmentOngoing identifier ->
                viewMeasurement identifier "Treatment Ongoing"

            BackendAuthorityTreatmentReview identifier ->
                viewMeasurement identifier "Treatment Review"

            BackendAuthorityTuberculosisDiagnostics identifier ->
                viewMeasurement identifier "Tuberculosis Diagnostics"

            BackendAuthorityTuberculosisDOT identifier ->
                viewMeasurement identifier "Tuberculosis DOT"

            BackendAuthorityTuberculosisEncounter identifier ->
                text ("Tuberculosis Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityTuberculosisFollowUp identifier ->
                viewMeasurement identifier "Tuberculosis Follow Up"

            BackendAuthorityTuberculosisHealthEducation identifier ->
                viewMeasurement identifier "Tuberculosis Health Education"

            BackendAuthorityTuberculosisMedication identifier ->
                viewMeasurement identifier "Tuberculosis Medication"

            BackendAuthorityTuberculosisReferral identifier ->
                viewMeasurement identifier "Tuberculosis Referral"

            BackendAuthorityTuberculosisSymptomReview identifier ->
                viewMeasurement identifier "Tuberculosis Symptom Review"

            BackendAuthorityTuberculosisTreatmentReview identifier ->
                viewMeasurement identifier "Tuberculosis Treatment Review"

            BackendAuthorityVitals identifier ->
                viewMeasurement identifier "Vitals"

            BackendAuthorityWeight identifier ->
                viewMeasurement identifier "Weight"

            BackendAuthorityWellChildAlbendazole identifier ->
                viewMeasurement identifier "Well Child Albendazole"

            BackendAuthorityWellChildBCGImmunisation identifier ->
                viewMeasurement identifier "Well Child BCG Immunisation"

            BackendAuthorityWellChildCaring identifier ->
                viewMeasurement identifier "Well Child Caring"

            BackendAuthorityWellChildContributingFactors identifier ->
                viewMeasurement identifier "Well Child Contributing Factors"

            BackendAuthorityWellChildDTPImmunisation identifier ->
                viewMeasurement identifier "Well Child DTP Immunisation"

            BackendAuthorityWellChildDTPStandaloneImmunisation identifier ->
                viewMeasurement identifier "Well Child DTP standalone Immunisation"

            BackendAuthorityWellChildECD identifier ->
                viewMeasurement identifier "Well Child ECD"

            BackendAuthorityWellChildEncounter identifier ->
                text ("Well Child Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityWellChildFeeding identifier ->
                viewMeasurement identifier "Well Child Feeding"

            BackendAuthorityWellChildFollowUp identifier ->
                viewMeasurement identifier "Well Child Follow Up"

            BackendAuthorityWellChildFoodSecurity identifier ->
                viewMeasurement identifier "Well Child Food Security"

            BackendAuthorityWellChildHeadCircumference identifier ->
                viewMeasurement identifier "Well Child Head Circumference"

            BackendAuthorityWellChildHealthEducation identifier ->
                viewMeasurement identifier "Well Child Health Education"

            BackendAuthorityWellChildHeight identifier ->
                viewMeasurement identifier "Well Child Height"

            BackendAuthorityWellChildHygiene identifier ->
                viewMeasurement identifier "Well Child Hygiene"

            BackendAuthorityWellChildHPVImmunisation identifier ->
                viewMeasurement identifier "Well Child HPV Immunisation"

            BackendAuthorityWellChildIPVImmunisation identifier ->
                viewMeasurement identifier "Well Child IPV Immunisation"

            BackendAuthorityWellChildMebendezole identifier ->
                viewMeasurement identifier "Well Child Mebendezole"

            BackendAuthorityWellChildMRImmunisation identifier ->
                viewMeasurement identifier "Well Child MR Immunisation"

            BackendAuthorityWellChildMuac identifier ->
                viewMeasurement identifier "Well Child Muac"

            BackendAuthorityWellChildNCDA identifier ->
                viewMeasurement identifier "Well Child NCDA"

            BackendAuthorityWellChildNextVisit identifier ->
                viewMeasurement identifier "Well Child Next Visit"

            BackendAuthorityWellChildNutrition identifier ->
                viewMeasurement identifier "Well Child Nutrition"

            BackendAuthorityWellChildOPVImmunisation identifier ->
                viewMeasurement identifier "Well Child OPV Immunisation"

            BackendAuthorityWellChildPCV13Immunisation identifier ->
                viewMeasurement identifier "Well Child PCV13 Immunisation"

            BackendAuthorityWellChildPhoto identifier ->
                viewMeasurement identifier "Well Child Photo"

            BackendAuthorityWellChildPregnancySummary identifier ->
                viewMeasurement identifier "Well Child Pregnancy Summary"

            BackendAuthorityWellChildRotarixImmunisation identifier ->
                viewMeasurement identifier "Well Child Rotarix Immunisation"

            BackendAuthorityWellChildSendToHC identifier ->
                viewMeasurement identifier "Well Child Send to HC"

            BackendAuthorityWellChildSymptomsReview identifier ->
                viewMeasurement identifier "Well Child Symptoms Review"

            BackendAuthorityWellChildVitals identifier ->
                viewMeasurement identifier "Well Child Vitals"

            BackendAuthorityWellChildVitaminA identifier ->
                viewMeasurement identifier "Well Child VitaminA"

            BackendAuthorityWellChildWeight identifier ->
                viewMeasurement identifier "Well Child Weight"
        ]


{-| Show a list of Authorities that allow syncing from.
-}
viewHealthCentersForSync : Language -> ModelIndexedDb -> Model -> Html Msg
viewHealthCentersForSync language db model =
    case db.healthCenters of
        RemoteData.Success healthCenters ->
            if Dict.isEmpty healthCenters then
                details [ class "segment ui" ]
                    [ summary [] [ text "No health centers synced yet" ] ]

            else
                let
                    selectedHealthCentersUuid =
                        getSyncedHealthCenters model
                in
                details
                    [ class "segment ui"
                    , property "open" (Json.Encode.bool False)
                    ]
                    [ summary [] [ text "Health Centers" ]
                    , ul []
                        (List.map
                            (\( healthCenterId, healthCenter ) ->
                                let
                                    isSynced =
                                        List.Extra.find (\selectedUuid -> selectedUuid == fromEntityUuid healthCenterId) selectedHealthCentersUuid
                                            |> isJust
                                in
                                viewHealthCenter ( healthCenterId, healthCenter ) isSynced
                            )
                            (Dict.toList healthCenters)
                        )
                    ]

        RemoteData.Failure error ->
            Utils.WebData.viewError language error

        RemoteData.Loading ->
            spinner

        RemoteData.NotAsked ->
            emptyNode


viewHealthCenter : ( HealthCenterId, HealthCenter ) -> Bool -> Html Msg
viewHealthCenter ( healthCenterId, healthCenter ) isSynced =
    let
        ( syncLabel, syncMsg ) =
            if isSynced then
                ( "Remove from Sync list", RevisionIdAuthorityRemove healthCenterId )

            else
                ( "Add to Sync list", RevisionIdAuthorityAdd healthCenterId )
    in
    li [ style "margin-bottom" "5px" ]
        [ text <| healthCenter.name
        , button
            [ onClick syncMsg
            , style "margin-left" "20px"
            ]
            [ text syncLabel ]
        ]


viewSyncStatus : Language -> ModelIndexedDb -> Model -> Html Msg
viewSyncStatus language db model =
    details
        [ property "open" (Json.Encode.bool False)
        , class "segment ui"
        ]
        [ summary [] [ text "Sync Status" ]
        , div [] [ text <| "Sync status: " ++ Debug.toString model.syncStatus ]
        , case model.syncStatus of
            SyncDownloadGeneral webData ->
                viewSyncDownloadGeneral language model webData

            SyncDownloadAuthority webData ->
                viewSyncDownloadAuthority db model webData

            _ ->
                emptyNode
        ]
