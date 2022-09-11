module Pages.NCD.RecurrentActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.NCDActivity.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (vitalsFormWithDefault)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.RecurrentActivity.Model exposing (..)
import Pages.NCD.RecurrentActivity.Types exposing (..)
import Pages.NCD.Utils
    exposing
        ( medicationDistributionFormWithDefault
        , recommendedTreatmentMeasurementTaken
        , recommendedTreatmentSignsForHypertension
        , referralFormWithDefault
        , resolveMedicationDistributionInputsAndTasks
        , resolveReferralInputsAndTasks
        )
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCustomLabel
        , viewQuestionLabel
        )
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> NCDRecurrentActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTask currentDate assembled
                |> List.isEmpty
                |> not

        RecurrentNextSteps ->
            mandatoryActivitiesForNextStepsCompleted currentDate assembled
                && (resolveNextStepsTasks currentDate assembled
                        |> List.filter (expectNextStepsTask currentDate assembled)
                        |> List.isEmpty
                        |> not
                   )


activityCompleted : NominalDate -> AssembledData -> NCDRecurrentActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity currentDate assembled LabResults)
                || (resolveLaboratoryResultTask currentDate assembled
                        |> List.all (laboratoryResultTaskCompleted currentDate assembled)
                   )

        RecurrentNextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.all (nextStepsTaskCompleted assembled)


resolveNextStepsTasks : NominalDate -> AssembledData -> List Pages.NCD.RecurrentActivity.Types.NextStepsTask
resolveNextStepsTasks currentDate assembled =
    List.filter (expectNextStepsTask currentDate assembled)
        [ TaskMedicationDistribution, TaskReferral ]


expectNextStepsTask : NominalDate -> AssembledData -> Pages.NCD.RecurrentActivity.Types.NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        TaskMedicationDistribution ->
            --@todo
            True

        TaskReferral ->
            --@todo
            True


nextStepsTaskCompleted : AssembledData -> Pages.NCD.RecurrentActivity.Types.NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        TaskMedicationDistribution ->
            --@todo
            -- let
            --     allowedSigns =
            --         recommendedTreatmentSignsForHypertension ++ recommendedTreatmentSignsForDiabetes
            -- in
            -- recommendedTreatmentMeasurementTaken allowedSigns assembled.measurements
            False

        TaskReferral ->
            --@todo
            False


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    --@todo
    True


nextStepsTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> AssembledData
    -> NextStepsData
    -> Pages.NCD.RecurrentActivity.Types.NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate assembled data task =
    case task of
        TaskMedicationDistribution ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks language
                        currentDate
                        assembled
                        SetRecommendedTreatmentSign
                        SetMedicationDistributionBoolInput
                        form
            in
            ( completed, total )

        TaskReferral ->
            let
                form =
                    assembled.measurements.referral
                        |> getMeasurementValueFunc
                        |> referralFormWithDefault data.referralForm

                ( _, tasks ) =
                    resolveReferralInputsAndTasks language
                        currentDate
                        assembled
                        SetReferralBoolInput
                        SetFacilityNonReferralReason
                        form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )


resolveLaboratoryResultTask : NominalDate -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultTask currentDate assembled =
    List.filter (expectLaboratoryResultTask currentDate assembled) laboratoryResultTasks


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

        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || testResultsCompleted .randomBloodSugarTest .sugarCount

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || testResultsCompleted .urineDipstickTest .protein

        TaskPregnancyTest ->
            not <| taskExpected TaskPregnancyTest

        TaskCreatinineTest ->
            (not <| taskExpected TaskCreatinineTest) || testResultsCompleted .creatinineTest .creatinineResult

        TaskLiverFunctionTest ->
            (not <| taskExpected TaskLiverFunctionTest) || testResultsCompleted .liverFunctionTest .altResult

        -- Others are not in use for NCD.
        _ ->
            False


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

        TaskRandomBloodSugarTest ->
            wasTestPerformed .randomBloodSugarTest

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskPregnancyTest ->
            False

        TaskCreatinineTest ->
            wasTestPerformed .creatinineTest

        TaskLiverFunctionTest ->
            wasTestPerformed .liverFunctionTest

        -- Others are not in use for NCD.
        _ ->
            False


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskRandomBloodSugarTest
    , TaskLiverFunctionTest
    , TaskUrineDipstickTest
    , TaskCreatinineTest
    ]
