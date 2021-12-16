module Pages.PrenatalLabResults.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
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
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (LaboratoryTask(..))
import Pages.PrenatalActivity.Utils exposing (laboratoryTaskIconClass)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.PrenatalLabResults.Model exposing (..)
import Pages.PrenatalLabResults.Utils exposing (..)
import Pages.Utils
    exposing
        ( emptySelectOption
        , isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id db model assembled =
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language id assembled
        , viewContent language currentDate db model assembled
        ]


viewHeader : Language -> PrenatalEncounterId -> AssembledData -> Html Msg
viewHeader language id assembled =
    let
        ( label, icon ) =
            ( Translate.PrenatalActivitiesTitle Laboratory, getActivityIcon Laboratory )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled Nothing
            ++ viewLabResults language currentDate assembled model


viewLabResults : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewLabResults language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectLaboratoryResultTask currentDate assembled) laboratoryResultTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

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
                                [ onClick <| SetActiveTask task ]
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
                                |> prenatalTestResultFormWithDefault data.syphilisTestForm
                                |> prenatalTestResultFormAndTasks language currentDate TaskSyphilisTest

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> prenatalTestResultFormWithDefault data.hepatitisBTestForm
                                |> prenatalTestResultFormAndTasks language currentDate TaskHepatitisBTest

                        TaskMalariaTest ->
                            ( emptyNode, 0, 0 )

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> prenatalBloodGpRsResultFormWithDefault data.bloodGpRsTestForm
                                |> prenatalBloodGpRsResultFormAndTasks language currentDate

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> prenatalUrineDipstickResultFormWithDefault data.urineDipstickTestForm
                                |> prenatalUrineDipstickResultFormAndTasks language currentDate

                        TaskHemoglobinTest ->
                            measurements.hemoglobinTest
                                |> getMeasurementValueFunc
                                |> prenatalHemoglobinResultFormWithDefault data.hemoglobinTestForm
                                |> prenatalHemoglobinResultFormAndTasks language currentDate

                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> prenatalRandomBloodSugarResultFormWithDefault data.randomBloodSugarTestForm
                                |> prenatalRandomBloodSugarResultFormAndTasks language currentDate
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


prenatalTestResultFormAndTasks : Language -> NominalDate -> LaboratoryTask -> PrenatalTestResultForm -> ( Html Msg, Int, Int )
prenatalTestResultFormAndTasks language currentDate task form =
    let
        setTestResultMsg =
            case task of
                TaskHepatitisBTest ->
                    Just SetHepatitisBTestResult

                TaskSyphilisTest ->
                    Just SetSyphilisTestResult

                _ ->
                    Nothing

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            Maybe.map
                (\setResultMsg ->
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
                        setResultMsg
                    , taskCompleted form.testResult
                    , 1
                    )
                )
                setTestResultMsg
                |> Maybe.withDefault ( [], 0, 0 )
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
                                [ ProteinNegative
                                , Protein30
                                , Protein100
                                , Protein300
                                , Protein2000
                                ]
                                SetProtein
                                ++ viewSelectInput language
                                    Translate.PrenatalLaboratoryPHTestResult
                                    form.ph
                                    Translate.PrenatalLaboratoryPHValue
                                    phValueToString
                                    [ Ph50
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
                                    [ Urobilinogen02
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
                                    Translate.PrenatalLaboratorySpecificGravityTestResult
                                    form.specificGravity
                                    Translate.PrenatalLaboratorySpecificGravityValue
                                    specificGravityValueToString
                                    [ SpecificGravity1000
                                    , SpecificGravity1005
                                    , SpecificGravity1010
                                    , SpecificGravity1015
                                    , SpecificGravity1020
                                    , SpecificGravity1025
                                    , SpecificGravity1030
                                    ]
                                    SetSpecificGravity
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
                                + taskCompleted form.specificGravity
                                + taskCompleted form.ketone
                                + taskCompleted form.bilirubin
                            , commonTasksTotal + 7
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
