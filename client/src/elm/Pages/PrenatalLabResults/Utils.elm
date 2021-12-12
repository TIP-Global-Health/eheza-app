module Pages.PrenatalLabResults.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (Html)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Pages.PrenatalActivity.Model exposing (LaboratoryTask(..))
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalLabResults.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewQuestionLabel
        )


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
    ]


laboratoryResultTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted currentDate assembled task =
    let
        taskExpected =
            expectLaboratoryResultTask currentDate assembled

        testResultsCompleted getMeasurementFunc getResultFieldFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.andThen getResultFieldFunc
                |> isJust
    in
    case task of
        TaskHIVTest ->
            not <| taskExpected TaskHIVTest

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || testResultsCompleted .syphilisTest .testResult

        TaskHepatitisBTest ->
            (not <| taskExpected TaskHepatitisBTest) || testResultsCompleted .hepatitisBTest .testResult

        TaskMalariaTest ->
            not <| taskExpected TaskMalariaTest

        TaskBloodGpRsTest ->
            (not <| taskExpected TaskBloodGpRsTest) || testResultsCompleted .bloodGpRsTest .bloodGroup

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || testResultsCompleted .urineDipstickTest .protein

        TaskHemoglobinTest ->
            (not <| taskExpected TaskHemoglobinTest) || testResultsCompleted .hemoglobinTest .hemoglobinCount

        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || testResultsCompleted .randomBloodSugarTest .sugarCount


expectLaboratoryResultTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask currentDate assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                    )
                |> Maybe.withDefault False
    in
    case task of
        TaskHIVTest ->
            False

        TaskSyphilisTest ->
            wasTestPerformed .syphilisTest

        TaskHepatitisBTest ->
            wasTestPerformed .hepatitisBTest

        TaskMalariaTest ->
            False

        TaskBloodGpRsTest ->
            wasTestPerformed .bloodGpRsTest

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskHemoglobinTest ->
            wasTestPerformed .hemoglobinTest

        TaskRandomBloodSugarTest ->
            wasTestPerformed .randomBloodSugarTest


prenatalTestResultFormWithDefault : PrenatalTestResultForm -> Maybe PrenatalRapidTestValue -> PrenatalTestResultForm
prenatalTestResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                }
            )


toPrenatalTestResultsValueWithDefault : Maybe PrenatalRapidTestValue -> PrenatalTestResultForm -> Maybe PrenatalRapidTestValue
toPrenatalTestResultsValueWithDefault saved form =
    prenatalTestResultFormWithDefault form saved
        |> toPrenatalTestResultsValue


toPrenatalTestResultsValue : PrenatalTestResultForm -> Maybe PrenatalRapidTestValue
toPrenatalTestResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            }
        )
        form.executionNote


prenatalBloodGpRsResultFormWithDefault : PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue -> PrenatalBloodGpRsResultForm
prenatalBloodGpRsResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , bloodGroup = or form.bloodGroup value.bloodGroup
                , rhesus = or form.rhesus value.rhesus
                }
            )


toPrenatalBloodGpRsResultsValueWithDefault : Maybe PrenatalBloodGpRsTestValue -> PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue
toPrenatalBloodGpRsResultsValueWithDefault saved form =
    prenatalBloodGpRsResultFormWithDefault form saved
        |> toPrenatalBloodGpRsResultsValue


toPrenatalBloodGpRsResultsValue : PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue
toPrenatalBloodGpRsResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , bloodGroup = form.bloodGroup
            , rhesus = form.rhesus
            }
        )
        form.executionNote


prenatalHemoglobinResultFormWithDefault : PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue -> PrenatalHemoglobinResultForm
prenatalHemoglobinResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , hemoglobinCount = or form.hemoglobinCount value.hemoglobinCount
                }
            )


toPrenatalHemoglobinResultsValueWithDefault : Maybe PrenatalHemoglobinTestValue -> PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue
toPrenatalHemoglobinResultsValueWithDefault saved form =
    prenatalHemoglobinResultFormWithDefault form saved
        |> toPrenatalHemoglobinResultsValue


toPrenatalHemoglobinResultsValue : PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue
toPrenatalHemoglobinResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hemoglobinCount = form.hemoglobinCount
            }
        )
        form.executionNote


prenatalRandomBloodSugarResultFormWithDefault : PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarResultForm
prenatalRandomBloodSugarResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , sugarCount = or form.sugarCount value.sugarCount
                }
            )


toPrenatalRandomBloodSugarResultsValueWithDefault : Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarResultsValueWithDefault saved form =
    prenatalRandomBloodSugarResultFormWithDefault form saved
        |> toPrenatalRandomBloodSugarResultsValue


toPrenatalRandomBloodSugarResultsValue : PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , sugarCount = form.sugarCount
            }
        )
        form.executionNote


prenatalUrineDipstickResultFormWithDefault : PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickResultForm
prenatalUrineDipstickResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { testVariant = or form.testVariant value.testVariant
                , executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , protein = or form.protein value.protein
                , ph = or form.ph value.ph
                , glucose = or form.glucose value.glucose
                , leukocytes = or form.leukocytes value.leukocytes
                , nitrite = or form.nitrite value.nitrite
                , urobilinogen = or form.urobilinogen value.urobilinogen
                , haemoglobin = or form.haemoglobin value.haemoglobin
                , specificGravity = or form.specificGravity value.specificGravity
                , ketone = or form.ketone value.ketone
                , bilirubin = or form.bilirubin value.bilirubin
                }
            )


toPrenatalUrineDipstickResultsValueWithDefault : Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickResultsValueWithDefault saved form =
    prenatalUrineDipstickResultFormWithDefault form saved
        |> toPrenatalUrineDipstickResultsValue


toPrenatalUrineDipstickResultsValue : PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickResultsValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , protein = form.protein
            , ph = form.ph
            , glucose = form.glucose
            , leukocytes = form.leukocytes
            , nitrite = form.nitrite
            , urobilinogen = form.urobilinogen
            , haemoglobin = form.haemoglobin
            , specificGravity = form.specificGravity
            , ketone = form.ketone
            , bilirubin = form.bilirubin
            }
        )
        form.executionNote
