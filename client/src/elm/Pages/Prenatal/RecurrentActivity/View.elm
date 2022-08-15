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
import Measurement.Model exposing (InvokationModule(..), VitalsForm, VitalsFormMode(..))
import Measurement.Utils exposing (vitalsFormWithDefault)
import Measurement.View exposing (viewSendToHospitalForm, viewVitalsForm)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask(..))
import Pages.Prenatal.Activity.Utils exposing (laboratoryTaskIconClass)
import Pages.Prenatal.Activity.View exposing (warningPopup)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)
import Pages.Prenatal.RecurrentActivity.Utils exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Prenatal.View exposing (viewMedicationDistributionForm)
import Pages.Utils
    exposing
        ( emptySelectOption
        , isTaskCompleted
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
    -> PrenatalLaboratoryTest
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
    -> PrenatalLaboratoryTest
    -> ModelIndexedDb
    -> LabResultsData
    -> AssembledData
    -> Html Msg
viewLabsHistoryHeaderAndContent language currentDate originatingEncounterId labEncounterId lab db data assembled =
    div [ class "page-activity prenatal labs-history" ] <|
        [ viewHeader language
            (PrenatalActivityPage originatingEncounterId Backend.PrenatalActivity.Model.Laboratory)
            (Translate.PrenatalLaboratoryTest lab)
            assembled
        , viewLabsHistoryContent language currentDate lab db data assembled
        ]


viewLabsHistoryContent : Language -> NominalDate -> PrenatalLaboratoryTest -> ModelIndexedDb -> LabResultsData -> AssembledData -> Html Msg
viewLabsHistoryContent language currentDate lab db data assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled Nothing
            ++ viewLab language currentDate lab assembled data
            ++ []


viewLab : Language -> NominalDate -> PrenatalLaboratoryTest -> AssembledData -> LabResultsData -> List (Html Msg)
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
                        |> prenatalSyphilisResultFormAndTasks language currentDate TaskSyphilisTest

                TestHepatitisB ->
                    measurements.hepatitisBTest
                        |> getMeasurementValueFunc
                        |> hepatitisBFormWithDefault data.hepatitisBTestForm
                        |> prenatalHepatitisBResultFormAndTasks language currentDate TaskHepatitisBTest

                TestBloodGpRs ->
                    measurements.bloodGpRsTest
                        |> getMeasurementValueFunc
                        |> prenatalBloodGpRsResultFormWithDefault data.bloodGpRsTestForm
                        |> prenatalBloodGpRsResultFormAndTasks language currentDate

                TestUrineDipstick ->
                    measurements.urineDipstickTest
                        |> getMeasurementValueFunc
                        |> prenatalUrineDipstickResultFormWithDefault data.urineDipstickTestForm
                        |> prenatalUrineDipstickResultFormAndTasks language currentDate

                TestHemoglobin ->
                    measurements.hemoglobinTest
                        |> getMeasurementValueFunc
                        |> prenatalHemoglobinResultFormWithDefault data.hemoglobinTestForm
                        |> prenatalHemoglobinResultFormAndTasks language currentDate

                TestRandomBloodSugar ->
                    measurements.randomBloodSugarTest
                        |> getMeasurementValueFunc
                        |> prenatalRandomBloodSugarResultFormWithDefault data.randomBloodSugarTestForm
                        |> prenatalRandomBloodSugarResultFormAndTasks language currentDate

                TestHIVPCR ->
                    measurements.hivPCRTest
                        |> getMeasurementValueFunc
                        |> prenatalHIVPCRResultFormWithDefault data.hivPCRTestForm
                        |> prenatalHIVPCRResultFormAndTasks language currentDate

                TestVitalsRecheck ->
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
            warningPopup language currentDate False SetWarningPopupState model.warningPopupState
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
                    , text <| translate language (Translate.PrenatalLaboratoryTask task)
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
                                |> prenatalSyphilisResultFormAndTasks language currentDate TaskSyphilisTest

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> hepatitisBFormWithDefault model.labResultsData.hepatitisBTestForm
                                |> prenatalHepatitisBResultFormAndTasks language currentDate TaskHepatitisBTest

                        TaskMalariaTest ->
                            ( emptyNode, 0, 0 )

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> prenatalBloodGpRsResultFormWithDefault model.labResultsData.bloodGpRsTestForm
                                |> prenatalBloodGpRsResultFormAndTasks language currentDate

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> prenatalUrineDipstickResultFormWithDefault model.labResultsData.urineDipstickTestForm
                                |> prenatalUrineDipstickResultFormAndTasks language currentDate

                        TaskHemoglobinTest ->
                            measurements.hemoglobinTest
                                |> getMeasurementValueFunc
                                |> prenatalHemoglobinResultFormWithDefault model.labResultsData.hemoglobinTestForm
                                |> prenatalHemoglobinResultFormAndTasks language currentDate

                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> prenatalRandomBloodSugarResultFormWithDefault model.labResultsData.randomBloodSugarTestForm
                                |> prenatalRandomBloodSugarResultFormAndTasks language currentDate

                        TaskHIVPCRTest ->
                            measurements.hivPCRTest
                                |> getMeasurementValueFunc
                                |> prenatalHIVPCRResultFormWithDefault model.labResultsData.hivPCRTestForm
                                |> prenatalHIVPCRResultFormAndTasks language currentDate

                        TaskCompletePreviousTests ->
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

                                TaskCompletePreviousTests ->
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


prenatalSyphilisResultFormAndTasks : Language -> NominalDate -> LaboratoryTask -> SyphilisResultForm -> ( Html Msg, Int, Int )
prenatalSyphilisResultFormAndTasks language currentDate task form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                ( symptomsSection, symptomsTasksCompleted, symptomsTasksTotal ) =
                    if form.testResult == Just PrenatalTestPositive then
                        ( [ viewLabel language Translate.SelectIllnessSymptoms
                          , viewCheckBoxMultipleSelectInput language
                                [ IllnessSymptomHeadache
                                , IllnessSymptomVisionChanges
                                , IllnessSymptomRash
                                , IllnessSymptomPainlessUlcerMouth
                                , IllnessSymptomPainlessUlcerGenitals
                                ]
                                []
                                (form.symptoms |> Maybe.withDefault [])
                                (Just NoIllnessSymptoms)
                                SetIllnessSymptom
                                Translate.IllnessSymptom
                          ]
                        , taskCompleted form.symptoms
                        , 1
                        )

                    else
                        ( [], 0, 0 )
            in
            ( viewSelectInput language
                (Translate.PrenatalLaboratoryTaskResult task)
                form.testResult
                Translate.PrenatalTestResult
                prenatalTestResultToString
                [ PrenatalTestPositive, PrenatalTestNegative, PrenatalTestIndeterminate ]
                SetSyphilisTestResult
                ++ symptomsSection
            , taskCompleted form.testResult + symptomsTasksCompleted
            , 1 + symptomsTasksTotal
            )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate task
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalHepatitisBResultFormAndTasks : Language -> NominalDate -> LaboratoryTask -> HepatitisBResultForm -> ( Html Msg, Int, Int )
prenatalHepatitisBResultFormAndTasks language currentDate task form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                emptyOption =
                    if isNothing form.testResult then
                        emptySelectOption True

                    else
                        emptyNode
            in
            ( viewSelectInput language
                (Translate.PrenatalLaboratoryTaskResult task)
                form.testResult
                Translate.PrenatalTestResult
                prenatalTestResultToString
                [ PrenatalTestPositive, PrenatalTestNegative, PrenatalTestIndeterminate ]
                SetHepatitisBTestResult
            , taskCompleted form.testResult
            , 1
            )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate task
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalBloodGpRsResultFormAndTasks : Language -> NominalDate -> PrenatalBloodGpRsResultForm -> ( Html Msg, Int, Int )
prenatalBloodGpRsResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( viewSelectInput language
                Translate.PrenatalLaboratoryBloodGroupTestResult
                form.bloodGroup
                Translate.PrenatalLaboratoryBloodGroup
                bloodGroupToString
                [ BloodGroupA, BloodGroupB, BloodGroupAB, BloodGroupO ]
                SetBloodGroup
                ++ viewSelectInput language
                    Translate.PrenatalLaboratoryRhesusTestResult
                    form.rhesus
                    Translate.PrenatalLaboratoryRhesus
                    rhesusToString
                    [ RhesusPositive, RhesusNegative ]
                    SetRhesus
            , taskCompleted form.bloodGroup + taskCompleted form.rhesus
            , 2
            )
    in
    ( div [ class "ui form laboratory blood-group-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskBloodGpRsTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalUrineDipstickResultFormAndTasks : Language -> NominalDate -> PrenatalUrineDipstickResultForm -> ( Html Msg, Int, Int )
prenatalUrineDipstickResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            Maybe.map
                (\testVariant ->
                    let
                        ( commonSection, commonTasksCompleted, commonTasksTotal ) =
                            ( viewSelectInput language
                                Translate.PrenatalLaboratoryProteinTestResult
                                form.protein
                                Translate.PrenatalLaboratoryProteinValue
                                proteinValueToString
                                [ Protein0
                                , ProteinPlus1
                                , ProteinPlus2
                                , ProteinPlus3
                                , ProteinPlus4
                                ]
                                SetProtein
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryPHTestResult
                                    form.ph
                                    Translate.PrenatalLaboratoryPHValue
                                    phValueToString
                                    [ Ph40
                                    , Ph45
                                    , Ph50
                                    , Ph60
                                    , Ph65
                                    , Ph70
                                    , Ph75
                                    , Ph80
                                    , Ph85
                                    ]
                                    SetPH
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryGlucoseTestResult
                                    form.glucose
                                    Translate.PrenatalLaboratoryGlucoseValue
                                    glucoseValueToString
                                    [ Glucose0
                                    , GlucosePlus1
                                    , GlucosePlus2
                                    , GlucosePlus3
                                    , GlucosePlus4
                                    ]
                                    SetGlucose
                            , taskCompleted form.protein + taskCompleted form.ph + taskCompleted form.glucose
                            , 3
                            )
                    in
                    case testVariant of
                        VariantShortTest ->
                            ( commonSection, commonTasksCompleted, commonTasksTotal )

                        VariantLongTest ->
                            ( commonSection
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryLeukocytesTestResult
                                    form.leukocytes
                                    Translate.PrenatalLaboratoryLeukocytesValue
                                    leukocytesValueToString
                                    [ LeukocytesNegative
                                    , LeukocytesSmall
                                    , LeukocytesMedium
                                    , LeukocytesLarge
                                    ]
                                    SetLeukocytes
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryNitriteTestResult
                                    form.nitrite
                                    Translate.PrenatalLaboratoryNitriteValue
                                    nitriteValueToString
                                    [ NitriteNegative
                                    , NitritePlus
                                    , NitritePlusPlus
                                    ]
                                    SetNitrite
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryUrobilinogenTestResult
                                    form.urobilinogen
                                    Translate.PrenatalLaboratoryUrobilinogenValue
                                    urobilinogenValueToString
                                    [ Urobilinogen002
                                    , Urobilinogen10
                                    , Urobilinogen20
                                    , Urobilinogen40
                                    , Urobilinogen80
                                    ]
                                    SetUrobilinogen
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryHaemoglobinTestResult
                                    form.haemoglobin
                                    Translate.PrenatalLaboratoryHaemoglobinValue
                                    haemoglobinValueToString
                                    [ HaemoglobinNegative
                                    , HaemoglobinNonHemolyzedTrace
                                    , HaemoglobinNonHemolyzedModerate
                                    , HaemoglobinHemolyzedTrace
                                    , HaemoglobinSmall
                                    , HaemoglobinModerate
                                    , HaemoglobinLarge
                                    ]
                                    SetHaemoglobin
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryKetoneTestResult
                                    form.ketone
                                    Translate.PrenatalLaboratoryKetoneValue
                                    ketoneValueToString
                                    [ KetoneNegative
                                    , Ketone5
                                    , Ketone10
                                    , Ketone15
                                    , Ketone40
                                    , Ketone80
                                    , Ketone100
                                    ]
                                    SetKetone
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryBilirubinTestResult
                                    form.bilirubin
                                    Translate.PrenatalLaboratoryBilirubinValue
                                    bilirubinValueToString
                                    [ BilirubinNegative
                                    , BilirubinSmall
                                    , BilirubinMedium
                                    , BilirubinLarge
                                    ]
                                    SetBilirubin
                            , commonTasksCompleted
                                + taskCompleted form.leukocytes
                                + taskCompleted form.nitrite
                                + taskCompleted form.urobilinogen
                                + taskCompleted form.haemoglobin
                                + taskCompleted form.ketone
                                + taskCompleted form.bilirubin
                            , commonTasksTotal + 6
                            )
                )
                form.testVariant
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskUrineDipstickTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalHemoglobinResultFormAndTasks : Language -> NominalDate -> PrenatalHemoglobinResultForm -> ( Html Msg, Int, Int )
prenatalHemoglobinResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( [ viewLabel language Translate.PrenatalLaboratoryHemoglobinTestResult
              , viewMeasurementInput language
                    form.hemoglobinCount
                    SetHemoglobin
                    "hemoglobin-count"
                    Translate.UnitGramsPerDeciliter
              ]
            , taskCompleted form.hemoglobinCount
            , 1
            )
    in
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHemoglobinTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalRandomBloodSugarResultFormAndTasks : Language -> NominalDate -> PrenatalRandomBloodSugarResultForm -> ( Html Msg, Int, Int )
prenatalRandomBloodSugarResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( [ viewLabel language Translate.PrenatalLaboratoryRandomBloodSugarTestResult
              , viewMeasurementInput language
                    form.sugarCount
                    SetRandomBloodSugar
                    "sugar-count"
                    Translate.UnitMilliGramsPerDeciliter
              ]
            , taskCompleted form.sugarCount
            , 1
            )
    in
    ( div [ class "ui form laboratory random-blood-sugar-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskRandomBloodSugarTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalHIVPCRResultFormAndTasks : Language -> NominalDate -> PrenatalHIVPCRResultForm -> ( Html Msg, Int, Int )
prenatalHIVPCRResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                ( derrivedSection, derrivedTasksCompleted, derrivedTasksTotal ) =
                    if form.hivViralLoadStatus == Just ViralLoadDetectable then
                        ( [ viewLabel language Translate.PrenatalLaboratoryHIVPCRTestResult
                          , viewMeasurementInput language
                                form.hivViralLoad
                                SetHIVViralLoad
                                "hiv-viral-load"
                                Translate.UnitCopiesPerMM3
                          ]
                        , taskCompleted form.hivViralLoad
                        , 1
                        )

                    else
                        ( [], 0, 0 )
            in
            ( [ viewQuestionLabel language Translate.PrenatalLaboratoryHIVPCRViralLoadStatusQuestion
              , viewBoolInput language
                    (Maybe.map ((==) ViralLoadUndetectable) form.hivViralLoadStatus)
                    SetHIVViralLoadUndetectable
                    "hiv-level-undetectable"
                    Nothing
              ]
                ++ derrivedSection
            , taskCompleted form.hivViralLoadStatus + derrivedTasksCompleted
            , 1 + derrivedTasksTotal
            )
    in
    ( div [ class "ui form laboratory hiv-prc-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHIVPCRTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


resultFormHeaderSection : Language -> NominalDate -> Maybe NominalDate -> LaboratoryTask -> List (Html Msg)
resultFormHeaderSection language currentDate executionDate task =
    let
        executionDateSection =
            Maybe.map
                (\date ->
                    [ viewLabel language <| Translate.PrenatalLaboratoryTaskDate task
                    , p [ class "test-date" ] [ text <| formatDDMMYYYY date ]
                    ]
                )
                executionDate
                |> Maybe.withDefault []
    in
    viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        :: executionDateSection


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
                    nextStepsMeasurementTaken assembled task

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
                    -- @todo
                    -- let
                    --     referralReasons =
                    --         diagnosesCausingHospitalReferralByImmediateDiagnoses assembled
                    -- in
                    -- getMeasurementValueFunc measurements.sendToHC
                    --     |> referralFormWithDefault data.referralForm
                    --     |> viewSendToHospitalForm referralReasons
                    --         language
                    --         currentDate
                    --         SetReferToHealthCenter
                    --         SetReasonForNonReferral
                    --         SetHandReferralForm
                    --         Nothing
                    emptyNode

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



-- HELPER FUNCTIONS


viewSelectInput :
    Language
    -> TranslationId
    -> Maybe a
    -> (a -> TranslationId)
    -> (a -> String)
    -> List a
    -> (String -> Msg)
    -> List (Html Msg)
viewSelectInput language labelTransId formValue valueTransId valueToStringFunc valuesList setMsg =
    [ viewLabel language labelTransId
    , emptyOptionForSelect formValue
        :: List.map
            (\item ->
                option
                    [ value (valueToStringFunc item)
                    , selected (formValue == Just item)
                    ]
                    [ text <| translate language <| valueTransId item ]
            )
            valuesList
        |> select
            [ onInput setMsg
            , class "form-input select"
            ]
    ]


emptyOptionForSelect : Maybe a -> Html any
emptyOptionForSelect value =
    if isNothing value then
        emptySelectOption True

    else
        emptyNode
