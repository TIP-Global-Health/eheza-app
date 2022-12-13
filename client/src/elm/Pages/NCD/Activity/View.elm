module Pages.NCD.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model
    exposing
        ( ContentAndTasksForPerformedLaboratoryTestConfig
        , ContentAndTasksLaboratoryTestInitialConfig
        , CorePhysicalExamForm
        , InvokationModule(..)
        , LaboratoryTask(..)
        , OutsideCareForm
        , OutsideCareStep(..)
        , VitalsForm
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , emptyContentAndTasksForPerformedLaboratoryTestConfig
        , emptyContentAndTasksLaboratoryTestInitialConfig
        , familyPlanningFormWithDefault
        , hivTestFormWithDefault
        , laboratoryTaskIconClass
        , nonRDTFormWithDefault
        , outsideCareFormInputsAndTasks
        , outsideCareFormWithDefault
        , pregnancyTestFormWithDefault
        , randomBloodSugarFormWithDefault
        , urineDipstickFormWithDefault
        , viewHIVTestForm
        , viewNonRDTForm
        , viewPregnancyTestForm
        , viewRandomBloodSugarForm
        , viewUrineDipstickForm
        , vitalsFormWithDefault
        )
import Measurement.View exposing (viewCorePhysicalExamForm, viewFamilyPlanningForm, viewVitalsForm)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Types exposing (..)
import Pages.NCD.Activity.Utils exposing (..)
import Pages.NCD.Model exposing (..)
import Pages.NCD.Utils exposing (generateAssembledData, medicationDistributionFormWithDefault, referralFormWithDefault, resolveReferralInputsAndTasks)
import Pages.NCD.View exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( isTaskCompleted
        , saveButton
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewNumberInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> NCDEncounterId -> NCDActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity ncd" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> NCDEncounterId -> NCDActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.NCDActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NCDEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> NCDActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> NCDActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        DangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        Examination ->
            viewExaminationContent language currentDate assembled model.examinationData

        FamilyPlanning ->
            viewFamilyPlanningContent language currentDate assembled model.familyPlanningData

        Laboratory ->
            viewLaboratoryContent language currentDate assembled model.laboratoryData

        MedicalHistory ->
            viewMedicalHistoryContent language currentDate assembled model.medicalHistoryData

        SymptomReview ->
            viewSymptomReviewContent language currentDate assembled model.symptomReviewData

        OutsideCare ->
            viewOutsideCareContent language currentDate assembled model.medicalHistoryData.outsideCareForm

        NextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData


viewDangerSignsContent : Language -> NominalDate -> AssembledData -> DangerSignsData -> List (Html Msg)
viewDangerSignsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.dangerSigns
                |> getMeasurementValueFunc
                |> dangerSignsFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language Translate.PatientGotAnyDangerSigns
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ Dyspnea, VisionChanges, ChestPain, FlankPain, Hematuria, SevereHeadaches, LossOfConciousness ]
                    []
                    (form.signs |> Maybe.withDefault [])
                    (Just NoNCDDangerSigns)
                    SetDangerSign
                    Translate.NCDDangerSign
              ]
            , taskCompleted form.signs
            , 1
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveDangerSigns assembled.participant.person assembled.measurements.dangerSigns)
            ]
        ]
    ]


viewSymptomReviewContent : Language -> NominalDate -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language Translate.PatientGotAnySymptoms
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ SwellingInLegs, UrinaryFrequency, Anxiety, WeightLoss, Palpitations, Tremor ]
                    [ SwellingInFace, SwellingInAbdomen, DizzinessWithChangingPosition, MildHeadache ]
                    (form.group1Symptoms |> Maybe.withDefault [])
                    (Just NoNCDGroup1Symptoms)
                    SetGroup1Symptom
                    Translate.NCDGroup1Symptom
              , viewQuestionLabel language Translate.PatientGotPainAnywhewre
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ PainFlank, PainLowerBack, PainFeet ]
                    [ PainAbdomen, PainNeck ]
                    (form.painSymptoms |> Maybe.withDefault [])
                    (Just NoNCDPainSymptoms)
                    SetPainSymptom
                    Translate.NCDPainSymptom
              , viewQuestionLabel language Translate.PatientGotAnySymptoms
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ WeaknessOfOneSideOfTheBody
                    , ProblemsWithWalking
                    , ProblemsWithTalking
                    , DecreasedVision
                    , BlurryVision
                    , IncreasedFatigueWithDailyActivities
                    ]
                    [ ShortOfBreathWhenLayingDown
                    , ShortOfBreathAtNight
                    , KidneyProblems
                    , NCDIncreasedThirst
                    ]
                    (form.group2Symptoms |> Maybe.withDefault [])
                    (Just NoNCDGroup2Symptoms)
                    SetGroup2Symptom
                    Translate.NCDGroup2Symptom
              ]
            , taskCompleted form.group1Symptoms + taskCompleted form.painSymptoms + taskCompleted form.group2Symptoms
            , 3
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form symptom-review" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview)
            ]
        ]
    ]


viewFamilyPlanningContent : Language -> NominalDate -> AssembledData -> FamilyPlanningData -> List (Html Msg)
viewFamilyPlanningContent language currentDate assembled data =
    let
        form =
            assembled.measurements.familyPlanning
                |> getMeasurementValueFunc
                |> familyPlanningFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewFamilyPlanningForm language Translate.FamilyPlanningCurentlyQuestion SetFamilyPlanningSign form
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveFamilyPlanning assembled.participant.person assembled.measurements.familyPlanning)
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
            [ TaskVitals, TaskCoreExam ]

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskVitals ->
                            ( "vitals", isJust assembled.measurements.vitals )

                        TaskCoreExam ->
                            ( "core-physical-exam", isJust assembled.measurements.coreExam )

                isActive =
                    activeTask == Just task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveExaminationTask task ]
            in
            div [ class <| "column " ++ iconClass ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NCDExaminationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    ( task, examinationTasksCompletedFromTotal currentDate assembled data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskVitals ->
                    getMeasurementValueFunc assembled.measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language currentDate assembled

                Just TaskCoreExam ->
                    getMeasurementValueFunc assembled.measurements.coreExam
                        |> corePhysicalExamFormWithDefault data.coreExamForm
                        |> viewCoreExamForm language currentDate

                Nothing ->
                    emptyNode

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            Maybe.map
                (\task ->
                    let
                        saveAction =
                            case task of
                                TaskVitals ->
                                    SaveVitals personId measurements.vitals nextTask

                                TaskCoreExam ->
                                    SaveCoreExam personId measurements.coreExam nextTask
                    in
                    div [ class "actions" ]
                        [ saveButton language (tasksCompleted == totalTasks) saveAction ]
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui five column grid" ] <|
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


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        config =
            { setIntInputMsg = SetVitalsIntInput
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .sys
            , diaBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .dia
            , heartRatePreviousValue =
                resolvePreviousMaybeValue assembled .vitals .heartRate
                    |> Maybe.map toFloat
            , respiratoryRatePreviousValue =
                resolvePreviousValue assembled .vitals .respiratoryRate
                    |> Maybe.map toFloat
            , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
            , birthDate = assembled.person.birthDate
            , formClass = "vitals"
            , mode = VitalsFormFull
            , invokationModule = InvokationModuleNCD
            }
    in
    Measurement.View.viewVitalsForm language currentDate config form


viewCoreExamForm : Language -> NominalDate -> CorePhysicalExamForm -> Html Msg
viewCoreExamForm language currentDate form =
    let
        config =
            { setBoolInputMsg = SetCoreExamBoolInput
            , setNeckMsg = SetCoreExamNeck
            , setHeartMsg = SetCoreExamHeart
            , setLungsMsg = SetCoreExamLungs
            , setAbdomenMsg = SetCoreExamAbdomen
            , setHandsMsg = SetCoreExamHands
            , setLegsMsg = SetCoreExamLegs
            }
    in
    Measurement.View.viewCorePhysicalExamForm language currentDate config form


viewMedicalHistoryContent : Language -> NominalDate -> AssembledData -> MedicalHistoryData -> List (Html Msg)
viewMedicalHistoryContent language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            [ TaskCoMorbidities
            , TaskMedicationHistory
            , TaskSocialHistory
            , TaskFamilyHistory
            , TaskOutsideCare
            ]

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskCoMorbidities ->
                            ( "danger-signs", isJust assembled.measurements.coMorbidities )

                        TaskMedicationHistory ->
                            ( "medical", isJust assembled.measurements.medicationHistory )

                        TaskSocialHistory ->
                            ( "social", isJust assembled.measurements.socialHistory )

                        TaskFamilyHistory ->
                            ( "family", isJust assembled.measurements.familyHistory )

                        TaskOutsideCare ->
                            ( "outside-care", isJust assembled.measurements.outsideCare )

                isActive =
                    activeTask == Just task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveMedicalHistoryTask task ]
            in
            div [ class <| "column " ++ iconClass ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NCDMedicalHistoryTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    case task of
                        TaskOutsideCare ->
                            ( TaskOutsideCare
                            , ( Maybe.Extra.values outsideCareTasks
                                    |> List.length
                              , List.length outsideCareTasks
                              )
                            )

                        _ ->
                            ( task, medicalHistoryTasksCompletedFromTotal currentDate assembled data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        outsideCareForm =
            assembled.measurements.outsideCare
                |> getMeasurementValueFunc
                |> outsideCareFormWithDefault data.outsideCareForm

        ( outsideCareInputs, outsideCareTasks ) =
            case outsideCareForm.step of
                OutsideCareStepDiagnoses ->
                    ( outsideCareInputsStep1, outsideCareTasksStep1 )

                OutsideCareStepMedications ->
                    ( outsideCareInputsStep2, outsideCareTasksStep2 )

        ( outsideCareInputsStep1, outsideCareTasksStep1 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepDiagnoses outsideCareForm

        ( outsideCareInputsStep2, outsideCareTasksStep2 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepMedications outsideCareForm

        viewForm =
            case activeTask of
                Just TaskCoMorbidities ->
                    getMeasurementValueFunc assembled.measurements.coMorbidities
                        |> coMorbiditiesFormWithDefault data.coMorbiditiesForm
                        |> viewCoMorbiditiesForm language currentDate

                Just TaskMedicationHistory ->
                    getMeasurementValueFunc assembled.measurements.medicationHistory
                        |> medicationHistoryFormWithDefault data.medicationHistoryForm
                        |> viewMedicationHistoryForm language currentDate

                Just TaskSocialHistory ->
                    getMeasurementValueFunc assembled.measurements.socialHistory
                        |> socialHistoryFormWithDefault data.socialHistoryForm
                        |> viewSocialHistoryForm language currentDate

                Just TaskFamilyHistory ->
                    getMeasurementValueFunc assembled.measurements.familyHistory
                        |> familyHistoryFormWithDefault data.familyHistoryForm
                        |> viewFamilyHistoryForm language currentDate

                Just TaskOutsideCare ->
                    div [ class "ui form history outside-care" ]
                        outsideCareInputs

                Nothing ->
                    emptyNode

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            Maybe.map
                (\task ->
                    let
                        saveButtonActive =
                            tasksCompleted == totalTasks

                        buttons =
                            case task of
                                TaskCoMorbidities ->
                                    [ saveButton language
                                        saveButtonActive
                                        (SaveCoMorbidities personId measurements.coMorbidities nextTask)
                                    ]

                                TaskMedicationHistory ->
                                    [ saveButton language
                                        saveButtonActive
                                        (SaveMedicationHistory personId measurements.medicationHistory nextTask)
                                    ]

                                TaskSocialHistory ->
                                    [ saveButton language
                                        saveButtonActive
                                        (SaveSocialHistory personId measurements.socialHistory nextTask)
                                    ]

                                TaskFamilyHistory ->
                                    [ saveButton language
                                        saveButtonActive
                                        (SaveFamilyHistory personId measurements.familyHistory nextTask)
                                    ]

                                TaskOutsideCare ->
                                    let
                                        saveAction =
                                            SaveOutsideCare personId measurements.outsideCare nextTask
                                    in
                                    case outsideCareForm.step of
                                        OutsideCareStepDiagnoses ->
                                            let
                                                actionMsg =
                                                    if List.isEmpty outsideCareTasksStep2 then
                                                        saveAction

                                                    else
                                                        SetOutsideCareStep OutsideCareStepMedications
                                            in
                                            [ saveButton language saveButtonActive actionMsg ]

                                        OutsideCareStepMedications ->
                                            [ button
                                                [ class "ui fluid primary button"
                                                , onClick <| SetOutsideCareStep OutsideCareStepDiagnoses
                                                ]
                                                [ text <| ("< " ++ translate language Translate.Back) ]
                                            , saveButton language saveButtonActive saveAction
                                            ]
                    in
                    div
                        [ classList
                            [ ( "actions", True )
                            , ( "two", task == TaskOutsideCare && outsideCareForm.step == OutsideCareStepMedications )
                            ]
                        ]
                        buttons
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui five column grid" ] <|
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


viewOutsideCareContent : Language -> NominalDate -> AssembledData -> OutsideCareForm MedicalCondition -> List (Html Msg)
viewOutsideCareContent language currentDate assembled form =
    let
        outsideCareForm =
            assembled.measurements.outsideCare
                |> getMeasurementValueFunc
                |> outsideCareFormWithDefault form

        ( outsideCareInputs, outsideCareTasks ) =
            case outsideCareForm.step of
                OutsideCareStepDiagnoses ->
                    ( outsideCareInputsStep1, outsideCareTasksStep1 )

                OutsideCareStepMedications ->
                    ( outsideCareInputsStep2, outsideCareTasksStep2 )

        ( outsideCareInputsStep1, outsideCareTasksStep1 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepDiagnoses outsideCareForm

        ( outsideCareInputsStep2, outsideCareTasksStep2 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepMedications outsideCareForm

        totalTasks =
            List.length outsideCareTasks

        tasksCompleted =
            Maybe.Extra.values outsideCareTasks
                |> List.length

        actions =
            let
                saveAction =
                    SaveOutsideCare assembled.participant.person assembled.measurements.outsideCare Nothing

                saveButtonActive =
                    tasksCompleted == totalTasks
            in
            case form.step of
                OutsideCareStepDiagnoses ->
                    let
                        actionMsg =
                            if List.isEmpty outsideCareTasksStep2 then
                                saveAction

                            else
                                SetOutsideCareStep OutsideCareStepMedications
                    in
                    div [ class "actions" ]
                        [ saveButton language saveButtonActive actionMsg ]

                OutsideCareStepMedications ->
                    div [ class "actions two" ]
                        [ button
                            [ class "ui fluid primary button"
                            , onClick <| SetOutsideCareStep OutsideCareStepDiagnoses
                            ]
                            [ text <| ("< " ++ translate language Translate.Back) ]
                        , saveButton language saveButtonActive saveAction
                        ]
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form history outside-care" ]
                outsideCareInputs
            ]
        , actions
        ]
    ]


outsideCareConfig =
    { setBoolInputMsg = SetOutsideCareSignBoolInput
    , setDiagnosisMsg = SetOutsideCareDiagnosis
    , setMalariaMedicationMsg = SetOutsideCareMalariaMedication
    , setHypertensionMedicationMsg = SetOutsideCareHypertensionMedication
    , setSyphilisMedicationMsg = SetOutsideCareSyphilisMedication
    , setAnemiaMedicationMsg = SetOutsideCareAnemiaMedication
    , setHIVMedicationMsg = SetOutsideCareHIVMedication
    , malariaDiagnoses = [ MedicalConditionMalaria ]
    , hypertensionDiagnoses = [ MedicalConditionHypertension ]
    , syphilisDiagnoses = [ MedicalConditionSyphilis ]
    , anemiaDiagnoses = [ MedicalConditionAnemia ]
    , hivDiagnoses = [ MedicalConditionHIV ]
    , malariaHeaderTransId = Translate.MedicalCondition MedicalConditionMalaria
    , resolveHypertensionHeaderTransId = always (Translate.MedicalCondition MedicalConditionHypertension)
    , syphilisHeaderTransId = Translate.MedicalCondition MedicalConditionSyphilis
    , anemiaHeaderTransId = Translate.MedicalCondition MedicalConditionAnemia
    , hivHeaderTransId = Translate.MedicalCondition MedicalConditionHIV
    , diagnosesLeftColumn = outsideCareDiagnosesLeftColumn
    , diagnosesRightColumn = outsideCareDiagnosesRightColumn
    , otherDiagnosis = MedicalConditionOther
    , diagnosisTransId = Translate.MedicalCondition
    }


viewCoMorbiditiesForm : Language -> NominalDate -> CoMorbiditiesForm -> Html Msg
viewCoMorbiditiesForm language currentDate form =
    div [ class "ui form co-morbidities" ]
        [ viewQuestionLabel language Translate.MedicalConditionQuestion
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ MedicalConditionHIV
            , MedicalConditionDiabetes
            , MedicalConditionKidneyDisease
            , MedicalConditionPregnancy
            , MedicalConditionHypertension
            , MedicalConditionGestationalDiabetes
            , MedicalConditionPregnancyRelatedHypertension
            ]
            []
            (Maybe.withDefault [] form.conditions)
            (Just NoMedicalConditions)
            SetMedicalCondition
            Translate.MedicalCondition
        ]


viewMedicationHistoryForm : Language -> NominalDate -> MedicationHistoryForm -> Html Msg
viewMedicationHistoryForm language currentDate form =
    div [ class "ui form medication-history" ]
        [ viewQuestionLabel language Translate.MedicationCausingHypertensionQuestion
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ MedicationOestrogens
            , MedicationSteroids
            , MedicationAmitriptyline
            ]
            [ MedicationIbuprofen
            , NoMedicationCausingHypertension
            ]
            (Maybe.withDefault [] form.medicationsCausingHypertension)
            Nothing
            SetMedicationCausingHypertension
            Translate.MedicationCausingHypertension
        , viewQuestionLabel language Translate.MedicationTreatingHypertensionQuestion
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ MedicationAceInhibitors
            , MedicationARBs
            , MedicationHCTZ
            , MedicationCalciumChannelBlockers
            ]
            [ MedicationMethyldopa
            , MedicationBetaBlockers
            , MedicationHydralazine
            , NoMedicationTreatingHypertension
            ]
            (Maybe.withDefault [] form.medicationsTreatingHypertension)
            Nothing
            SetMedicationTreatingHypertension
            Translate.MedicationTreatingHypertension
        , viewQuestionLabel language Translate.MedicationTreatingDiabetesQuestion
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ MedicationMetformin
            , MedicationGlibenclamide
            ]
            [ MedicationInsulin, NoMedicationTreatingDiabetes ]
            (Maybe.withDefault [] form.medicationsTreatingDiabetes)
            Nothing
            SetMedicationTreatingDiabetes
            Translate.MedicationTreatingDiabetes
        ]


viewSocialHistoryForm : Language -> NominalDate -> SocialHistoryForm -> Html Msg
viewSocialHistoryForm language currentDate form =
    let
        ( inputs, _ ) =
            socialHistoryFormInputsAndTasks language currentDate form
    in
    div [ class "ui form social-history" ]
        inputs


viewFamilyHistoryForm : Language -> NominalDate -> FamilyHistoryForm -> Html Msg
viewFamilyHistoryForm language currentDate form =
    let
        ( inputs, _ ) =
            familyHistoryFormInputsAndTasks language currentDate form
    in
    div [ class "ui form family-history" ]
        inputs


viewLaboratoryContent : Language -> NominalDate -> AssembledData -> LaboratoryData -> List (Html Msg)
viewLaboratoryContent language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectLaboratoryTask currentDate assembled) laboratoryTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryTaskCompleted currentDate assembled task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLaboratoryTask task ]
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
                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> randomBloodSugarFormWithDefault data.randomBloodSugarTestForm
                                |> viewRandomBloodSugarForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> urineDipstickFormWithDefault data.urineDipstickTestForm
                                |> viewUrineDipstickForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskHIVTest ->
                            measurements.hivTest
                                |> getMeasurementValueFunc
                                |> hivTestFormWithDefault data.hivTestForm
                                |> viewHIVTestForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskPregnancyTest ->
                            measurements.pregnancyTest
                                |> getMeasurementValueFunc
                                |> pregnancyTestFormWithDefault data.pregnancyTestForm
                                |> viewPregnancyTestForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskCreatinineTest ->
                            measurements.creatinineTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.creatinineTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskCreatinineTest

                        TaskLiverFunctionTest ->
                            measurements.liverFunctionTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.liverFunctionTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskLiverFunctionTest

                        TaskLipidPanelTest ->
                            measurements.lipidPanelTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.lipidPanelTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskLipidPanelTest

                        -- Others do not participate at NCD.
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
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                TaskHIVTest ->
                                    SaveHIVTest personId measurements.hivTest nextTask

                                TaskUrineDipstickTest ->
                                    SaveUrineDipstickTest personId measurements.urineDipstickTest nextTask

                                TaskRandomBloodSugarTest ->
                                    SaveRandomBloodSugarTest personId measurements.randomBloodSugarTest nextTask

                                TaskPregnancyTest ->
                                    SavePregnancyTest personId measurements.pregnancyTest nextTask

                                TaskCreatinineTest ->
                                    SaveCreatinineTest personId measurements.creatinineTest nextTask

                                TaskLiverFunctionTest ->
                                    SaveLiverFunctionTest personId measurements.liverFunctionTest nextTask

                                TaskLipidPanelTest ->
                                    SaveLipidPanelTest personId measurements.lipidPanelTest nextTask

                                -- Others do not participate at NCD.
                                _ ->
                                    NoOp
                    in
                    viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
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
            [ viewForm, actions ]
        ]
    ]


contentAndTasksLaboratoryTestInitialConfig : ContentAndTasksLaboratoryTestInitialConfig Msg
contentAndTasksLaboratoryTestInitialConfig =
    emptyContentAndTasksLaboratoryTestInitialConfig NoOp
        |> (\config ->
                { config
                    | setHIVTestFormBoolInputMsg = SetHIVTestFormBoolInput
                    , setHIVTestExecutionNoteMsg = SetHIVTestExecutionNote
                    , setHIVTestResultMsg = SetHIVTestResult
                    , setUrineDipstickTestFormBoolInputMsg = SetUrineDipstickTestFormBoolInput
                    , setUrineDipstickTestExecutionNoteMsg = SetUrineDipstickTestExecutionNote
                    , setUrineDipstickTestVariantMsg = SetUrineDipstickTestVariant
                    , setRandomBloodSugarTestFormBoolInputMsg = SetRandomBloodSugarTestFormBoolInput
                    , setRandomBloodSugarTestExecutionNoteMsg = SetRandomBloodSugarTestExecutionNote
                    , setPregnancyTestFormBoolInputMsg = SetPregnancyTestFormBoolInput
                    , setPregnancyTestExecutionNoteMsg = SetPregnancyTestExecutionNote
                    , setPregnancyTestResultMsg = SetPregnancyTestResult
                    , setCreatinineTestFormBoolInputMsg = SetCreatinineTestFormBoolInput
                    , setCreatinineTestExecutionNoteMsg = SetCreatinineTestExecutionNote
                    , setLiverFunctionTestFormBoolInputMsg = SetLiverFunctionTestFormBoolInput
                    , setLiverFunctionTestExecutionNoteMsg = SetLiverFunctionTestExecutionNote
                    , setLipidPanelTestFormBoolInputMsg = SetLipidPanelTestFormBoolInput
                    , setLipidPanelTestExecutionNoteMsg = SetLipidPanelTestExecutionNote
                }
           )


contentAndTasksForPerformedLaboratoryTestConfig : ContentAndTasksForPerformedLaboratoryTestConfig Msg
contentAndTasksForPerformedLaboratoryTestConfig =
    emptyContentAndTasksForPerformedLaboratoryTestConfig NoOp
        |> (\config ->
                { config
                    | setHIVTestFormBoolInputMsg = SetHIVTestFormBoolInput
                    , setHIVTestExecutionDateMsg = SetHIVTestExecutionDate
                    , setHIVTestDateSelectorStateMsg = SetHIVTestDateSelectorState
                    , setUrineDipstickTestFormBoolInputMsg = SetUrineDipstickTestFormBoolInput
                    , setUrineDipstickTestExecutionDateMsg = SetUrineDipstickTestExecutionDate
                    , setUrineDipstickTestDateSelectorStateMsg = SetUrineDipstickTestDateSelectorState
                    , setRandomBloodSugarTestFormBoolInputMsg = SetRandomBloodSugarTestFormBoolInput
                    , setRandomBloodSugarTestExecutionDateMsg = SetRandomBloodSugarTestExecutionDate
                    , setRandomBloodSugarTestDateSelectorStateMsg = SetRandomBloodSugarTestDateSelectorState
                    , setPregnancyTestFormBoolInputMsg = SetPregnancyTestFormBoolInput
                    , setPregnancyTestExecutionDateMsg = SetPregnancyTestExecutionDate
                    , setPregnancyTestDateSelectorStateMsg = SetPregnancyTestDateSelectorState
                    , setCreatinineTestFormBoolInputMsg = SetCreatinineTestFormBoolInput
                    , setCreatinineTestExecutionDateMsg = SetCreatinineTestExecutionDate
                    , setCreatinineTestDateSelectorStateMsg = SetCreatinineTestDateSelectorState
                    , setLiverFunctionTestFormBoolInputMsg = SetLiverFunctionTestFormBoolInput
                    , setLiverFunctionTestExecutionDateMsg = SetLiverFunctionTestExecutionDate
                    , setLiverFunctionTestDateSelectorStateMsg = SetLiverFunctionTestDateSelectorState
                    , setLipidPanelTestFormBoolInputMsg = SetLipidPanelTestFormBoolInput
                    , setLipidPanelTestExecutionDateMsg = SetLipidPanelTestExecutionDate
                    , setLipidPanelTestDateSelectorStateMsg = SetLipidPanelTestDateSelectorState
                }
           )


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
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        viewTask task =
            let
                iconClass =
                    case task of
                        TaskHealthEducation ->
                            "next-steps-health-education"

                        TaskMedicationDistribution ->
                            "next-steps-treatment"

                        TaskReferral ->
                            "next-steps-referral"

                isActive =
                    activeTask == Just task

                isCompleted =
                    nextStepsTaskCompleted assembled task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveNextStepsTask task ]
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NCDNextStepsTask task)
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
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate assembled

                Just TaskMedicationDistribution ->
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> viewMedicationDistributionForm language
                            currentDate
                            NCDEncounterPhaseInitial
                            SetRecommendedTreatmentSignSingle
                            SetRecommendedTreatmentSignMultiple
                            SetMedicationDistributionBoolInput
                            assembled

                Just TaskReferral ->
                    getMeasurementValueFunc measurements.referral
                        |> referralFormWithDefault data.referralForm
                        |> viewReferralForm language
                            currentDate
                            NCDEncounterPhaseInitial
                            SetReferralBoolInput
                            SetFacilityNonReferralReason
                            assembled

                Nothing ->
                    emptyNode

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveMsg =
                                case task of
                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskMedicationDistribution ->
                                        SaveMedicationDistribution personId measurements.medicationDistribution nextTask

                                    TaskReferral ->
                                        SaveReferral personId measurements.referral nextTask
                        in
                        viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
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
            [ viewForm, actions ]
        ]
    ]


viewHealthEducationForm : Language -> NominalDate -> AssembledData -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate assembled form =
    div [ class "ui form health-education" ]
        [ viewCustomLabel language Translate.NCDHealthEducationHeader "" "label header"
        , viewCustomLabel language Translate.NCDHealthEducationInstructions "." "label paragraph"
        , viewQuestionLabel language Translate.NCDHealthEducationQuestion
        , viewBoolInput
            language
            form.hypertension
            (SetHealthEducationBoolInput (\value form_ -> { form_ | hypertension = Just value }))
            "hypertension"
            Nothing
        ]
