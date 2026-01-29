module Pages.Prenatal.RecurrentActivity.View exposing (view, viewLabsHistory)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isLabTechnician)
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Model exposing (ContentAndTasksLaboratoryResultConfig, LaboratoryTask(..), VitalsForm)
import Measurement.Utils
    exposing
        ( bloodGpRsResultFormAndTasks
        , bloodGpRsResultFormWithDefault
        , emptyContentAndTasksLaboratoryResultConfig
        , hemoglobinResultFormAndTasks
        , hemoglobinResultFormWithDefault
        , hepatitisBResultFormAndTasks
        , hepatitisBResultFormWithDefault
        , hivPCRResultFormAndTasks
        , hivPCRResultFormWithDefault
        , hivResultFollowUpsFormAndTasks
        , hivResultFormAndTasks
        , hivResultFormWithDefault
        , laboratoryTaskIconClass
        , malariaResultFormAndTasks
        , malariaResultFormWithDefault
        , partnerHIVResultFollowUpsFormAndTasks
        , partnerHIVResultFormAndTasks
        , partnerHIVResultFormWithDefault
        , randomBloodSugarResultFormAndTasks
        , randomBloodSugarResultFormWithDefault
        , syphilisResultFollowUpsFormAndTasks
        , syphilisResultFormAndTasks
        , syphilisResultFormWithDefault
        , urineDipstickResultFormAndTasks
        , urineDipstickResultFormWithDefault
        , vitalsFormWithDefault
        )
import Measurement.View
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.View exposing (warningPopup)
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (ExaminationData, LabResultsData, Model, Msg(..), NextStepsData)
import Pages.Prenatal.RecurrentActivity.Types exposing (ExaminationTask(..), NextStepsTask(..))
import Pages.Prenatal.RecurrentActivity.Utils exposing (examinationMeasurementTaken, examinationTasksCompletedFromTotal, generateVitalsFormConfig, healthEducationFormInputsAndTasks, healthEducationFormWithDefault, laboratoryResultFollowUpsTaskCompleted, laboratoryResultTaskCompleted, nextStepsTaskCompleted, nextStepsTasksCompletedFromTotal, resolveLaboratoryResultFollowUpsTasks, resolveLaboratoryResultTasks, resolveNextStepsTasks, resolveReferralInputsAndTasks)
import Pages.Prenatal.Utils exposing (medicationDistributionFormWithDefaultRecurrentPhase, referralFormWithDefault, resolvePartnerHIVTestResult)
import Pages.Prenatal.View exposing (viewMalariaPreventionContent, viewMedicationDistributionForm)
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , tasksBarId
        , viewSaveAction
        , viewTasksCount
        )
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Nurse -> PrenatalEncounterId -> PrenatalRecurrentActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurse id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate nurse id activity db model) identity assembled


viewHeaderAndContent :
    Language
    -> NominalDate
    -> Nurse
    -> PrenatalEncounterId
    -> PrenatalRecurrentActivity
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate nurse id activity db model assembled =
    let
        isLabTech =
            isLabTechnician nurse

        goBackPage =
            if isLabTech then
                GlobalCaseManagementPage

            else
                PrenatalRecurrentEncounterPage id
    in
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language goBackPage (Translate.PrenatalRecurrentActivitiesTitle activity) assembled
        , viewContent language currentDate isLabTech activity db model assembled
        , viewModal <|
            warningPopup language currentDate False assembled.encounter.diagnoses SetWarningPopupState model.warningPopupState
        ]


viewHeader : Language -> UserPage -> TranslationId -> AssembledData -> Html Msg
viewHeader language goBackPage labelTransId assembled =
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language labelTransId ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> Bool -> PrenatalRecurrentActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate isLabTech activity db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewActivity language currentDate isLabTech activity assembled db model


viewActivity : Language -> NominalDate -> Bool -> PrenatalRecurrentActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate isLabTech activity assembled db model =
    case activity of
        LabResults ->
            viewLabResultsContent language currentDate isLabTech assembled model

        RecurrentNextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData

        RecurrentExamination ->
            viewExaminationContent language currentDate assembled model.examinationData

        RecurrentMalariaPrevention ->
            viewMalariaPreventionContent language
                currentDate
                assembled
                SetMalariaPreventionBoolInput
                SaveMalariaPrevention
                model.malariaPreventionData

        LabsResultsFollowUps ->
            viewLabResultFollowUpsContent language currentDate isLabTech assembled model


viewLabResultsContent : Language -> NominalDate -> Bool -> AssembledData -> Model -> List (Html Msg)
viewLabResultsContent language currentDate isLabTech assembled model =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveLaboratoryResultTasks currentDate isLabTech assembled

        activeTask =
            resolveActiveTask tasks model.labResultsData.activeTask

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryResultTaskCompleted currentDate isLabTech assembled task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLabResultsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.LaboratoryTask task)
                    ]
                ]

        formHtmlAndTasks =
            List.map
                (\task ->
                    ( task
                    , case task of
                        TaskPartnerHIVTest ->
                            getMeasurementValueFunc measurements.partnerHIVTest
                                |> partnerHIVResultFormWithDefault model.labResultsData.partnerHIVTestForm
                                |> partnerHIVResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetPartnerHIVTestResult
                                    SetPartnerHIVTestFormBoolInput

                        TaskHIVTest ->
                            let
                                partnerHIVTestResult =
                                    resolvePartnerHIVTestResult assembled
                            in
                            getMeasurementValueFunc measurements.hivTest
                                |> hivResultFormWithDefault model.labResultsData.hivTestForm
                                |> hivResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetHIVTestResult
                                    SetHIVTestFormBoolInput
                                    partnerHIVTestResult

                        TaskSyphilisTest ->
                            getMeasurementValueFunc measurements.syphilisTest
                                |> syphilisResultFormWithDefault model.labResultsData.syphilisTestForm
                                |> syphilisResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetSyphilisTestResult
                                    SetIllnessSymptom

                        TaskHepatitisBTest ->
                            getMeasurementValueFunc measurements.hepatitisBTest
                                |> hepatitisBResultFormWithDefault model.labResultsData.hepatitisBTestForm
                                |> hepatitisBResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetHepatitisBTestResult

                        TaskMalariaTest ->
                            getMeasurementValueFunc measurements.malariaTest
                                |> malariaResultFormWithDefault model.labResultsData.malariaTestForm
                                |> malariaResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetMalariaTestResult
                                    SetBloodSmearResult

                        TaskBloodGpRsTest ->
                            getMeasurementValueFunc measurements.bloodGpRsTest
                                |> bloodGpRsResultFormWithDefault model.labResultsData.bloodGpRsTestForm
                                |> bloodGpRsResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetBloodGroup
                                    SetRhesus

                        TaskUrineDipstickTest ->
                            getMeasurementValueFunc measurements.urineDipstickTest
                                |> urineDipstickResultFormWithDefault model.labResultsData.urineDipstickTestForm
                                |> urineDipstickResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetProtein
                                    SetPH
                                    SetGlucose
                                    SetLeukocytes
                                    SetNitrite
                                    SetUrobilinogen
                                    SetHaemoglobin
                                    SetKetone
                                    SetBilirubin

                        TaskHemoglobinTest ->
                            getMeasurementValueFunc measurements.hemoglobinTest
                                |> hemoglobinResultFormWithDefault model.labResultsData.hemoglobinTestForm
                                |> hemoglobinResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetHemoglobin

                        TaskRandomBloodSugarTest ->
                            getMeasurementValueFunc measurements.randomBloodSugarTest
                                |> randomBloodSugarResultFormWithDefault model.labResultsData.randomBloodSugarTestForm
                                |> randomBloodSugarResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetRandomBloodSugar

                        TaskHIVPCRTest ->
                            getMeasurementValueFunc measurements.hivPCRTest
                                |> hivPCRResultFormWithDefault model.labResultsData.hivPCRTestForm
                                |> hivPCRResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetHIVViralLoad
                                    SetHIVViralLoadUndetectable

                        -- Only relevant for initial phase.
                        TaskCompletePreviousTests ->
                            ( emptyNode, 0, 0 )

                        -- Others do not participate at Prenatal.
                        _ ->
                            ( emptyNode, 0, 0 )
                    )
                )
                tasks
                |> Dict.fromList

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.andThen
                (\task -> Dict.get task formHtmlAndTasks)
                activeTask
                |> Maybe.withDefault ( emptyNode, 0, 0 )

        actions =
            Maybe.andThen
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        tasksCompletedFromTotalDict =
                            Dict.map (\_ ( _, completed, total ) -> ( completed, total ))
                                formHtmlAndTasks

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                TaskPartnerHIVTest ->
                                    SavePartnerHIVResult personId measurements.partnerHIVTest nextTask |> Just

                                TaskHIVTest ->
                                    SaveHIVResult personId measurements.hivTest nextTask |> Just

                                TaskSyphilisTest ->
                                    SaveSyphilisResult personId measurements.syphilisTest nextTask |> Just

                                TaskHepatitisBTest ->
                                    SaveHepatitisBResult personId measurements.hepatitisBTest nextTask |> Just

                                TaskMalariaTest ->
                                    SaveMalariaResult personId measurements.malariaTest nextTask |> Just

                                TaskBloodGpRsTest ->
                                    SaveBloodGpRsResult personId measurements.bloodGpRsTest nextTask |> Just

                                TaskUrineDipstickTest ->
                                    SaveUrineDipstickResult personId measurements.urineDipstickTest nextTask |> Just

                                TaskHemoglobinTest ->
                                    SaveHemoglobinResult personId measurements.hemoglobinTest nextTask |> Just

                                TaskRandomBloodSugarTest ->
                                    SaveRandomBloodSugarResult personId measurements.randomBloodSugarTest nextTask |> Just

                                TaskHIVPCRTest ->
                                    SaveHIVPCRResult personId measurements.hivPCRTest nextTask |> Just

                                TaskCompletePreviousTests ->
                                    Nothing

                                -- Others do not participate at Prenatal.
                                _ ->
                                    Nothing
                    in
                    Maybe.map
                        (\msg ->
                            viewSaveAction language msg (tasksCompleted /= totalTasks)
                        )
                        saveMsg
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm
            , actions
            ]
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveNextStepsTasks currentDate assembled

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                iconClass =
                    case task of
                        NextStepsSendToHC ->
                            "next-steps-send-to-hc"

                        NextStepsMedicationDistribution ->
                            "next-steps-medication-distribution"

                        NextStepsHealthEducation ->
                            "next-steps-health-education"

                isActive =
                    activeTask == Just task

                isCompleted =
                    nextStepsTaskCompleted currentDate assembled task

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveNextStepsTask task ]

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PrenatalRecurrentNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal language currentDate assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsSendToHC ->
                    getMeasurementValueFunc measurements.sendToHC
                        |> referralFormWithDefault data.referralForm
                        |> viewReferralForm language currentDate assembled

                Just NextStepsMedicationDistribution ->
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultRecurrentPhase data.medicationDistributionForm
                        |> viewMedicationDistributionForm language
                            currentDate
                            PrenatalEncounterPhaseRecurrent
                            assembled
                            SetMedicationDistributionBoolInput
                            SetMedicationDistributionAdministrationNote
                            SetRecommendedTreatmentSign
                            (always NoOp)

                Just NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate assembled

                Nothing ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                NextStepsSendToHC ->
                                    SaveSendToHC personId measurements.sendToHC nextTask

                                NextStepsMedicationDistribution ->
                                    SaveMedicationDistribution personId measurements.medicationDistribution nextTask

                                NextStepsHealthEducation ->
                                    SaveHealthEducation personId measurements.healthEducation nextTask
                    in
                    div [ class "actions next-steps" ]
                        [ button
                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                            , onClick saveMsg
                            ]
                            [ text <| translate language Translate.Save ]
                        ]
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewExaminationContent : Language -> NominalDate -> AssembledData -> ExaminationData -> List (Html Msg)
viewExaminationContent language currentDate assembled data =
    let
        tasks =
            [ ExaminationVitals ]

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                iconClass =
                    case task of
                        ExaminationVitals ->
                            "vitals"

                isActive =
                    activeTask == Just task

                isCompleted =
                    examinationMeasurementTaken
                        assembled
                        task

                attributes =
                    -- Currently, this is a single taske at Examinaiton activity.
                    -- Therefore, we do not need to react on click action.
                    [ classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                    ]
            in
            div [ class <| "column " ++ iconClass ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ExaminationTaskRecurrent task)
                    ]
                ]

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen
                (\task ->
                    let
                        tasksCompletedFromTotalDict =
                            List.map
                                (\task_ ->
                                    ( task_, examinationTasksCompletedFromTotal currentDate assembled data task_ )
                                )
                                tasks
                                |> Dict.fromList
                    in
                    Dict.get task tasksCompletedFromTotalDict
                )
                activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just ExaminationVitals ->
                    assembled.measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language currentDate assembled

                Nothing ->
                    emptyNode

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveAction =
                                case task of
                                    ExaminationVitals ->
                                        let
                                            personId =
                                                assembled.participant.person

                                            measurements =
                                                assembled.measurements
                                        in
                                        SaveVitals personId measurements.vitals
                        in
                        div [ class "actions examination" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                , onClick saveAction
                                ]
                                [ text <| translate language Translate.Save ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask <|
                tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        formConfig =
            generateVitalsFormConfig assembled form
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewHealthEducationForm : Language -> NominalDate -> AssembledData -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate assembled form =
    let
        ( inputs, _ ) =
            healthEducationFormInputsAndTasks language assembled form
    in
    div [ class "ui form health-education" ]
        inputs


viewReferralForm : Language -> NominalDate -> AssembledData -> ReferralForm -> Html Msg
viewReferralForm language currentDate assembled form =
    let
        ( inputs, _ ) =
            resolveReferralInputsAndTasks language
                currentDate
                assembled
                SetReferralBoolInput
                SetFacilityNonReferralReason
                form
    in
    div [ class "ui form referral" ]
        inputs


viewLabResultFollowUpsContent : Language -> NominalDate -> Bool -> AssembledData -> Model -> List (Html Msg)
viewLabResultFollowUpsContent language currentDate isLabTech assembled model =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveLaboratoryResultFollowUpsTasks currentDate assembled

        activeTask =
            resolveActiveTask tasks model.labResultsData.activeTask

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryResultFollowUpsTaskCompleted currentDate assembled task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLabResultsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.LaboratoryTask task)
                    ]
                ]

        formHtmlAndTasks =
            List.map
                (\task ->
                    ( task
                    , case task of
                        TaskHIVTest ->
                            let
                                partnerHIVTestResult =
                                    resolvePartnerHIVTestResult assembled
                            in
                            getMeasurementValueFunc measurements.hivTest
                                |> hivResultFormWithDefault model.labResultsData.hivTestForm
                                |> hivResultFollowUpsFormAndTasks language
                                    currentDate
                                    SetHIVTestFormBoolInput
                                    partnerHIVTestResult

                        TaskSyphilisTest ->
                            getMeasurementValueFunc measurements.syphilisTest
                                |> syphilisResultFormWithDefault model.labResultsData.syphilisTestForm
                                |> syphilisResultFollowUpsFormAndTasks language currentDate SetIllnessSymptom

                        TaskPartnerHIVTest ->
                            getMeasurementValueFunc measurements.partnerHIVTest
                                |> partnerHIVResultFormWithDefault model.labResultsData.partnerHIVTestForm
                                |> partnerHIVResultFollowUpsFormAndTasks language currentDate SetPartnerHIVTestFormBoolInput

                        -- Others do not have results follow ups section,
                        -- or, do not participate at Prenatal.
                        _ ->
                            ( emptyNode, 0, 0 )
                    )
                )
                tasks
                |> Dict.fromList

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.andThen
                (\task -> Dict.get task formHtmlAndTasks)
                activeTask
                |> Maybe.withDefault ( emptyNode, 0, 0 )

        actions =
            Maybe.andThen
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        tasksCompletedFromTotalDict =
                            Dict.map (\_ ( _, completed, total ) -> ( completed, total ))
                                formHtmlAndTasks

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                TaskHIVTest ->
                                    SaveHIVResult personId measurements.hivTest nextTask |> Just

                                TaskSyphilisTest ->
                                    SaveSyphilisResult personId measurements.syphilisTest nextTask |> Just

                                -- Others do not have results follow ups section,
                                -- or, do not participate at Prenatal.
                                _ ->
                                    Nothing
                    in
                    Maybe.map
                        (\msg ->
                            viewSaveAction language msg (tasksCompleted /= totalTasks)
                        )
                        saveMsg
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm
            , actions
            ]
        ]
    ]


contentAndTasksLaboratorResultsConfig : ContentAndTasksLaboratoryResultConfig Msg PrenatalEncounterId
contentAndTasksLaboratorResultsConfig =
    emptyContentAndTasksLaboratoryResultConfig NoOp
        |> (\config ->
                { config
                    | setHIVTestFormBoolInputMsg = SetHIVTestFormBoolInput
                    , setHIVTestExecutionNoteMsg = SetHIVTestExecutionNote
                    , setPartnerHIVTestFormBoolInputMsg = SetPartnerHIVTestFormBoolInput
                    , setPartnerHIVTestExecutionNoteMsg = SetPartnerHIVTestExecutionNote
                    , setMalariaTestFormBoolInputMsg = SetMalariaTestFormBoolInput
                    , setMalariaTestExecutionNoteMsg = SetMalariaTestExecutionNote
                    , setSyphilisTestFormBoolInputMsg = SetSyphilisTestFormBoolInput
                    , setSyphilisTestExecutionNoteMsg = SetSyphilisTestExecutionNote
                    , setHepatitisBTestFormBoolInputMsg = SetHepatitisBTestFormBoolInput
                    , setHepatitisBTestExecutionNoteMsg = SetHepatitisBTestExecutionNote
                    , setBloodGpRsTestFormBoolInputMsg = SetBloodGpRsTestFormBoolInput
                    , setBloodGpRsTestExecutionNoteMsg = SetBloodGpRsTestExecutionNote
                    , setRandomBloodSugarTestFormBoolInputMsg = SetRandomBloodSugarTestFormBoolInput
                    , setRandomBloodSugarTestExecutionNoteMsg = SetRandomBloodSugarTestExecutionNote
                    , setHemoglobinTestFormBoolInputMsg = SetHemoglobinTestFormBoolInput
                    , setHemoglobinTestExecutionNoteMsg = SetHemoglobinTestExecutionNote
                    , setHIVPCRTestFormBoolInputMsg = SetHIVPCRTestFormBoolInput
                    , setHIVPCRTestExecutionNoteMsg = SetHIVPCRTestExecutionNote
                    , setUrineDipstickTestFormBoolInputMsg = SetUrineDipstickTestFormBoolInput
                    , setUrineDipstickTestExecutionNoteMsg = SetUrineDipstickTestExecutionNote
                }
           )



-- LAB HISTORY


viewLabsHistory :
    Language
    -> NominalDate
    -> PrenatalEncounterId
    -> PrenatalEncounterId
    -> LaboratoryTest
    -> ModelIndexedDb
    -> LabResultsData
    -> Html Msg
viewLabsHistory language currentDate originatingEncounterId labEncounterId lab db data =
    let
        assembled =
            generateAssembledData labEncounterId db
    in
    viewWebData language (viewLabsHistoryHeaderAndContent language currentDate originatingEncounterId labEncounterId lab db data) identity assembled


viewLabsHistoryHeaderAndContent :
    Language
    -> NominalDate
    -> PrenatalEncounterId
    -> PrenatalEncounterId
    -> LaboratoryTest
    -> ModelIndexedDb
    -> LabResultsData
    -> AssembledData
    -> Html Msg
viewLabsHistoryHeaderAndContent language currentDate originatingEncounterId labEncounterId lab db data assembled =
    div [ class "page-activity prenatal labs-history" ] <|
        [ viewHeader language
            (PrenatalActivityPage originatingEncounterId Backend.PrenatalActivity.Model.Laboratory)
            (Translate.LaboratoryTest lab)
            assembled
        , viewLabsHistoryContent language currentDate lab db data assembled
        ]


viewLabsHistoryContent : Language -> NominalDate -> LaboratoryTest -> ModelIndexedDb -> LabResultsData -> AssembledData -> Html Msg
viewLabsHistoryContent language currentDate lab db data assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled Nothing
            ++ viewLab language currentDate lab assembled data


viewLab : Language -> NominalDate -> LaboratoryTest -> AssembledData -> LabResultsData -> List (Html Msg)
viewLab language currentDate lab assembled data =
    let
        isLabTech =
            -- Labs history can be filled only by nurses.
            False

        measurements =
            assembled.measurements

        ( viewForm, tasksCompleted, totalTasks ) =
            case lab of
                TestPartnerHIV ->
                    getMeasurementValueFunc measurements.partnerHIVTest
                        |> partnerHIVResultFormWithDefault data.partnerHIVTestForm
                        |> partnerHIVResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetPartnerHIVTestResult
                            SetPartnerHIVTestFormBoolInput

                TestHIV ->
                    let
                        partnerHIVTestResult =
                            resolvePartnerHIVTestResult assembled
                    in
                    getMeasurementValueFunc measurements.hivTest
                        |> hivResultFormWithDefault data.hivTestForm
                        |> hivResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetHIVTestResult
                            SetHIVTestFormBoolInput
                            partnerHIVTestResult

                TestSyphilis ->
                    getMeasurementValueFunc measurements.syphilisTest
                        |> syphilisResultFormWithDefault data.syphilisTestForm
                        |> syphilisResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetSyphilisTestResult
                            SetIllnessSymptom

                TestHepatitisB ->
                    getMeasurementValueFunc measurements.hepatitisBTest
                        |> hepatitisBResultFormWithDefault data.hepatitisBTestForm
                        |> hepatitisBResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetHepatitisBTestResult

                TestMalaria ->
                    getMeasurementValueFunc measurements.malariaTest
                        |> malariaResultFormWithDefault data.malariaTestForm
                        |> malariaResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetMalariaTestResult
                            SetBloodSmearResult

                TestBloodGpRs ->
                    getMeasurementValueFunc measurements.bloodGpRsTest
                        |> bloodGpRsResultFormWithDefault data.bloodGpRsTestForm
                        |> bloodGpRsResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetBloodGroup
                            SetRhesus

                TestUrineDipstick ->
                    getMeasurementValueFunc measurements.urineDipstickTest
                        |> urineDipstickResultFormWithDefault data.urineDipstickTestForm
                        |> urineDipstickResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetProtein
                            SetPH
                            SetGlucose
                            SetLeukocytes
                            SetNitrite
                            SetUrobilinogen
                            SetHaemoglobin
                            SetKetone
                            SetBilirubin

                TestHemoglobin ->
                    getMeasurementValueFunc measurements.hemoglobinTest
                        |> hemoglobinResultFormWithDefault data.hemoglobinTestForm
                        |> hemoglobinResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetHemoglobin

                TestRandomBloodSugar ->
                    getMeasurementValueFunc measurements.randomBloodSugarTest
                        |> randomBloodSugarResultFormWithDefault data.randomBloodSugarTestForm
                        |> randomBloodSugarResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetRandomBloodSugar

                TestHIVPCR ->
                    getMeasurementValueFunc measurements.hivPCRTest
                        |> hivPCRResultFormWithDefault data.hivPCRTestForm
                        |> hivPCRResultFormAndTasks language
                            currentDate
                            isLabTech
                            contentAndTasksLaboratorResultsConfig
                            SetHIVViralLoad
                            SetHIVViralLoadUndetectable

                TestVitalsRecheck ->
                    ( emptyNode, 0, 0 )

                -- Others do no participate at Prenatal.
                _ ->
                    ( emptyNode, 0, 0 )

        actions =
            let
                personId =
                    assembled.participant.person

                saveMsg =
                    case lab of
                        TestPartnerHIV ->
                            SavePartnerHIVResult personId measurements.partnerHIVTest Nothing

                        TestHIV ->
                            SaveHIVResult personId measurements.hivTest Nothing

                        TestSyphilis ->
                            SaveSyphilisResult personId measurements.syphilisTest Nothing

                        TestHepatitisB ->
                            SaveHepatitisBResult personId measurements.hepatitisBTest Nothing

                        TestMalaria ->
                            SaveMalariaResult personId measurements.malariaTest Nothing

                        TestBloodGpRs ->
                            SaveBloodGpRsResult personId measurements.bloodGpRsTest Nothing

                        TestUrineDipstick ->
                            SaveUrineDipstickResult personId measurements.urineDipstickTest Nothing

                        TestHemoglobin ->
                            SaveHemoglobinResult personId measurements.hemoglobinTest Nothing

                        TestRandomBloodSugar ->
                            SaveRandomBloodSugarResult personId measurements.randomBloodSugarTest Nothing

                        TestHIVPCR ->
                            SaveHIVPCRResult personId measurements.hivPCRTest Nothing

                        TestVitalsRecheck ->
                            NoOp

                        -- Others do no participate at Prenatal.
                        _ ->
                            NoOp
            in
            viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm
            , actions
            ]
        ]
    ]
