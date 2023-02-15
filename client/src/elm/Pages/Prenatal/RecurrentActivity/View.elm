module Pages.Prenatal.RecurrentActivity.View exposing (view, viewLabsHistory)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Backend.PrenatalActivity.Utils exposing (getActivityIcon)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Date exposing (Unit(..))
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (InvokationModule(..), LaboratoryTask(..), VitalsForm, VitalsFormMode(..))
import Measurement.Utils
    exposing
        ( bloodGpRsResultFormAndTasks
        , bloodGpRsResultFormWithDefault
        , hemoglobinResultFormAndTasks
        , hemoglobinResultFormWithDefault
        , hepatitisBResultFormAndTasks
        , hepatitisBResultFormWithDefault
        , hivPCRResultFormAndTasks
        , hivPCRResultFormWithDefault
        , laboratoryTaskIconClass
        , randomBloodSugarResultFormAndTasks
        , randomBloodSugarResultFormWithDefault
        , syphilisResultFormAndTasks
        , syphilisResultFormWithDefault
        , urineDipstickResultFormAndTasks
        , urineDipstickResultFormWithDefault
        , vitalsFormWithDefault
        )
import Measurement.View exposing (viewVitalsForm)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.View exposing (warningPopup)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)
import Pages.Prenatal.RecurrentActivity.Utils exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Prenatal.View exposing (viewMalariaPreventionContent, viewMedicationDistributionForm)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewInstructionsLabel
        , viewLabel
        , viewMeasurementInput
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


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
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        ( viewForm, tasksCompleted, totalTasks ) =
            case lab of
                TestSyphilis ->
                    measurements.syphilisTest
                        |> getMeasurementValueFunc
                        |> syphilisResultFormWithDefault data.syphilisTestForm
                        |> syphilisResultFormAndTasks language currentDate SetIllnessSymptom SetSyphilisTestResult

                TestHepatitisB ->
                    measurements.hepatitisBTest
                        |> getMeasurementValueFunc
                        |> hepatitisBResultFormWithDefault data.hepatitisBTestForm
                        |> hepatitisBResultFormAndTasks language currentDate SetHepatitisBTestResult

                TestBloodGpRs ->
                    measurements.bloodGpRsTest
                        |> getMeasurementValueFunc
                        |> bloodGpRsResultFormWithDefault data.bloodGpRsTestForm
                        |> bloodGpRsResultFormAndTasks language currentDate SetBloodGroup SetRhesus

                TestUrineDipstick ->
                    measurements.urineDipstickTest
                        |> getMeasurementValueFunc
                        |> urineDipstickResultFormWithDefault data.urineDipstickTestForm
                        |> urineDipstickResultFormAndTasks language
                            currentDate
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
                    measurements.hemoglobinTest
                        |> getMeasurementValueFunc
                        |> hemoglobinResultFormWithDefault data.hemoglobinTestForm
                        |> hemoglobinResultFormAndTasks language currentDate SetHemoglobin

                TestRandomBloodSugar ->
                    measurements.randomBloodSugarTest
                        |> getMeasurementValueFunc
                        |> randomBloodSugarResultFormWithDefault data.randomBloodSugarTestForm
                        |> randomBloodSugarResultFormAndTasks language currentDate SetRandomBloodSugar

                TestHIVPCR ->
                    measurements.hivPCRTest
                        |> getMeasurementValueFunc
                        |> hivPCRResultFormWithDefault data.hivPCRTestForm
                        |> hivPCRResultFormAndTasks language currentDate SetHIVViralLoad SetHIVViralLoadUndetectable

                TestVitalsRecheck ->
                    ( emptyNode, 0, 0 )

                -- Others do no participate at Prenatal.
                _ ->
                    ( emptyNode, 0, 0 )

        actions =
            let
                saveMsg =
                    case lab of
                        TestSyphilis ->
                            SaveSyphilisResult personId measurements.syphilisTest Nothing

                        TestHepatitisB ->
                            SaveHepatitisBResult personId measurements.hepatitisBTest Nothing

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
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm
            , actions
            ]
        ]
    ]


view : Language -> NominalDate -> PrenatalEncounterId -> PrenatalRecurrentActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> PrenatalRecurrentActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language (PrenatalRecurrentEncounterPage id) (Translate.PrenatalRecurrentActivitiesTitle activity) assembled
        , viewContent language currentDate activity db model assembled
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


viewContent : Language -> NominalDate -> PrenatalRecurrentActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewActivity language currentDate activity assembled db model


viewActivity : Language -> NominalDate -> PrenatalRecurrentActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        LabResults ->
            viewLabResultsContent language currentDate assembled model

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


viewLabResultsContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewLabResultsContent language currentDate assembled model =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            resolveLaboratoryResultTask currentDate assembled

        activeTask =
            Maybe.Extra.or model.labResultsData.activeTask (List.head tasks)

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryResultTaskCompleted currentDate assembled task

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
                            ( emptyNode, 0, 0 )

                        TaskSyphilisTest ->
                            measurements.syphilisTest
                                |> getMeasurementValueFunc
                                |> syphilisResultFormWithDefault model.labResultsData.syphilisTestForm
                                |> syphilisResultFormAndTasks language currentDate SetIllnessSymptom SetSyphilisTestResult

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> hepatitisBResultFormWithDefault model.labResultsData.hepatitisBTestForm
                                |> hepatitisBResultFormAndTasks language currentDate SetHepatitisBTestResult

                        TaskMalariaTest ->
                            ( emptyNode, 0, 0 )

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> bloodGpRsResultFormWithDefault model.labResultsData.bloodGpRsTestForm
                                |> bloodGpRsResultFormAndTasks language currentDate SetBloodGroup SetRhesus

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> urineDipstickResultFormWithDefault model.labResultsData.urineDipstickTestForm
                                |> urineDipstickResultFormAndTasks language
                                    currentDate
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
                            measurements.hemoglobinTest
                                |> getMeasurementValueFunc
                                |> hemoglobinResultFormWithDefault model.labResultsData.hemoglobinTestForm
                                |> hemoglobinResultFormAndTasks language currentDate SetHemoglobin

                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> randomBloodSugarResultFormWithDefault model.labResultsData.randomBloodSugarTestForm
                                |> randomBloodSugarResultFormAndTasks language currentDate SetRandomBloodSugar

                        TaskHIVPCRTest ->
                            measurements.hivPCRTest
                                |> getMeasurementValueFunc
                                |> hivPCRResultFormWithDefault model.labResultsData.hivPCRTestForm
                                |> hivPCRResultFormAndTasks language currentDate SetHIVViralLoad SetHIVViralLoadUndetectable

                        TaskPartnerHIVTest ->
                            ( emptyNode, 0, 0 )

                        TaskCompletePreviousTests ->
                            ( emptyNode, 0, 0 )

                        -- Others do not participate at Prenatal.
                        _ ->
                            ( emptyNode, 0, 0 )
                    )
                )
                tasks
                |> Dict.fromList

        tasksCompletedFromTotalDict =
            Dict.map (\_ ( _, completed, total ) -> ( completed, total ))
                formHtmlAndTasks

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.andThen
                (\task -> Dict.get task formHtmlAndTasks)
                activeTask
                |> Maybe.withDefault ( emptyNode, 0, 0 )

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            Maybe.andThen
                (\task ->
                    let
                        saveMsg =
                            case task of
                                TaskHIVTest ->
                                    Nothing

                                TaskSyphilisTest ->
                                    SaveSyphilisResult personId measurements.syphilisTest nextTask |> Just

                                TaskHepatitisBTest ->
                                    SaveHepatitisBResult personId measurements.hepatitisBTest nextTask |> Just

                                TaskMalariaTest ->
                                    Nothing

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

                                TaskPartnerHIVTest ->
                                    Nothing

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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
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
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            resolveNextStepsTasks currentDate assembled

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

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

        nextTask =
            tasks
                |> List.filter
                    (\task ->
                        (Just task /= activeTask)
                            && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                    )
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
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
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
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
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            [ ExaminationVitals ]

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

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

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, examinationTasksCompletedFromTotal assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
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
            { setIntInputMsg = \_ _ -> NoOp
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = form.sysBloodPressure
            , diaBloodPressurePreviousValue = form.diaBloodPressure
            , heartRatePreviousValue = Nothing
            , respiratoryRatePreviousValue = Nothing
            , bodyTemperaturePreviousValue = Nothing
            , birthDate = assembled.person.birthDate
            , formClass = "examination vitals"
            , mode = VitalsFormRepeated
            , invokationModule = InvokationModulePrenatal
            }
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
