module Pages.NCD.RecurrentActivity.Utils exposing (activityCompleted, expectActivity, laboratoryResultTaskCompleted, labsResultsValueWithResult, nextStepsTaskCompleted, nextStepsTasksCompletedFromTotal, resolveLaboratoryResultTasks, resolveNextStepsTasks, resolvedLabsResultsValue, withCompletedTestsFromMeasurements)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, testPerformedByExecutionNote)
import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (expectUniversalTestResultTask, testPerformedByValue)
import Pages.GlobalCaseManagement.Utils exposing (allLabsCompleted)
import Pages.NCD.Model exposing (AssembledData, NCDEncounterPhase(..))
import Pages.NCD.RecurrentActivity.Model exposing (Msg(..), NextStepsData)
import Pages.NCD.RecurrentActivity.Types exposing (NextStepsTask(..))
import Pages.NCD.Utils exposing (generateRecommendedTreatmentSignsForHypertension, medicateForDiabetes, medicateForHypertension, medicationDistributionFormWithDefault, recommendedTreatmentMeasurementTaken, recommendedTreatmentSignsForDiabetes, referForDiabetes, referForHypertension, referForRenalComplications, referralFormWithDefault, referralToFacilityCompleted, resolveMedicationDistributionInputsAndTasks, resolveReferralInputsAndTasks)
import Pages.Utils exposing (resolveTasksCompletedFromTotal)
import Translate.Model exposing (Language)


expectActivity : AssembledData -> NCDRecurrentActivity -> Bool
expectActivity assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTasks assembled
                |> List.isEmpty
                |> not

        RecurrentNextSteps ->
            resolveNextStepsTasks assembled
                |> List.filter (expectNextStepsTask assembled)
                |> List.isEmpty
                |> not


activityCompleted : AssembledData -> NCDRecurrentActivity -> Bool
activityCompleted assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity assembled LabResults)
                || (resolveLaboratoryResultTasks assembled
                        |> List.all (laboratoryResultTaskCompleted assembled)
                   )

        RecurrentNextSteps ->
            resolveNextStepsTasks assembled
                |> List.all (nextStepsTaskCompleted assembled)


resolveNextStepsTasks : AssembledData -> List Pages.NCD.RecurrentActivity.Types.NextStepsTask
resolveNextStepsTasks assembled =
    List.filter (expectNextStepsTask assembled)
        [ TaskMedicationDistribution, TaskReferral ]


expectNextStepsTask : AssembledData -> Pages.NCD.RecurrentActivity.Types.NextStepsTask -> Bool
expectNextStepsTask assembled task =
    case task of
        TaskMedicationDistribution ->
            medicateForDiabetes NCDEncounterPhaseRecurrent assembled
                || medicateForHypertension NCDEncounterPhaseRecurrent assembled

        TaskReferral ->
            referForDiabetes NCDEncounterPhaseRecurrent assembled
                || referForHypertension NCDEncounterPhaseRecurrent assembled
                || referForRenalComplications NCDEncounterPhaseRecurrent assembled


nextStepsTaskCompleted : AssembledData -> Pages.NCD.RecurrentActivity.Types.NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        TaskMedicationDistribution ->
            let
                hypertensionTreatmentCompleted =
                    if medicateForHypertension NCDEncounterPhaseRecurrent assembled then
                        let
                            recommendedTreatmentSignsForHypertension =
                                generateRecommendedTreatmentSignsForHypertension assembled
                        in
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True

                diabetesTreatmentCompleted =
                    if medicateForDiabetes NCDEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForDiabetes assembled.measurements

                    else
                        True
            in
            hypertensionTreatmentCompleted && diabetesTreatmentCompleted

        TaskReferral ->
            -- On recurrent phase of the encounter, only referral
            -- facility possible is hospital, since referral to ARV
            -- Services always happens on initial phase.
            referralToFacilityCompleted assembled FacilityHospital


nextStepsTasksCompletedFromTotal :
    Language
    -> AssembledData
    -> NextStepsData
    -> Pages.NCD.RecurrentActivity.Types.NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal language assembled data task =
    case task of
        TaskMedicationDistribution ->
            let
                ( _, completed, total ) =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> resolveMedicationDistributionInputsAndTasks language
                            NCDEncounterPhaseRecurrent
                            assembled
                            SetRecommendedTreatmentSignSingle
                            SetRecommendedTreatmentSignMultiple
                            SetMedicationDistributionBoolInput
            in
            ( completed, total )

        TaskReferral ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.referral
                        |> referralFormWithDefault data.referralForm
                        |> resolveReferralInputsAndTasks language
                            NCDEncounterPhaseRecurrent
                            assembled
                            SetReferralBoolInput
                            SetFacilityNonReferralReason
            in
            resolveTasksCompletedFromTotal tasks


resolveLaboratoryResultTasks : AssembledData -> List LaboratoryTask
resolveLaboratoryResultTasks assembled =
    List.filter (expectLaboratoryResultTask assembled) laboratoryResultTasks


laboratoryResultTaskCompleted : AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted assembled task =
    let
        taskExpected =
            expectLaboratoryResultTask assembled

        resultCompleted =
            (not <| taskExpected task) || measurementResultEntered assembled.measurements task
    in
    case task of
        TaskHIVTest ->
            not <| taskExpected TaskHIVTest

        TaskRandomBloodSugarTest ->
            resultCompleted

        TaskUrineDipstickTest ->
            resultCompleted

        TaskPregnancyTest ->
            not <| taskExpected TaskPregnancyTest

        TaskCreatinineTest ->
            resultCompleted

        TaskLiverFunctionTest ->
            resultCompleted

        TaskLipidPanelTest ->
            resultCompleted

        -- Others are not in use for NCD.
        _ ->
            False


{-| Whether the task's test measurement shows it was run, with its result entered.
-}
measurementResultEntered : NCDMeasurements -> LaboratoryTask -> Bool
measurementResultEntered measurements task =
    let
        resultEntered measurement getResultFieldFunc =
            getMeasurementValueFunc measurement
                |> Maybe.map
                    (\value ->
                        testPerformedByExecutionNote value.executionNote
                            && (isJust <| getResultFieldFunc value)
                    )
                |> Maybe.withDefault False
    in
    case task of
        TaskCreatinineTest ->
            resultEntered measurements.creatinineTest .creatinineResult

        TaskLipidPanelTest ->
            resultEntered measurements.lipidPanelTest .totalCholesterolResult

        TaskLiverFunctionTest ->
            resultEntered measurements.liverFunctionTest .altResult

        TaskRandomBloodSugarTest ->
            resultEntered measurements.randomBloodSugarTest .sugarCount

        TaskUrineDipstickTest ->
            resultEntered measurements.urineDipstickTest .protein

        _ ->
            False


{-| The labs entry with tests read from their own measurements too: a test run
with its result entered is performed and completed.
-}
withCompletedTestsFromMeasurements : NCDMeasurements -> LabsResultsValue -> LabsResultsValue
withCompletedTestsFromMeasurements measurements value =
    let
        testsWithResults =
            [ ( TaskCreatinineTest, TestCreatinine )
            , ( TaskLipidPanelTest, TestLipidPanel )
            , ( TaskLiverFunctionTest, TestLiverFunction )
            , ( TaskRandomBloodSugarTest, TestRandomBloodSugar )
            , ( TaskUrineDipstickTest, TestUrineDipstick )
            ]
                |> List.filter (Tuple.first >> measurementResultEntered measurements)
                |> List.map Tuple.second
                |> EverySet.fromList
    in
    { value
        | performedTests = EverySet.union testsWithResults value.performedTests
        , completedTests = EverySet.union testsWithResults value.completedTests
    }


{-| The labs entry resolved today, when every performed test is completed.
-}
resolvedLabsResultsValue : NominalDate -> LabsResultsValue -> Maybe LabsResultsValue
resolvedLabsResultsValue currentDate value =
    if allLabsCompleted value.performedTests value.completedTests then
        Just { value | resolutionDate = currentDate }

    else
        Nothing


{-| The labs entry once the test's result is entered, or nothing when the entry
already holds every completed test.
-}
labsResultsValueWithResult : NominalDate -> AssembledData -> LaboratoryTest -> LabsResultsValue -> Maybe LabsResultsValue
labsResultsValueWithResult currentDate assembled test storedValue =
    let
        value =
            withCompletedTestsFromMeasurements assembled.measurements storedValue

        -- A result is entered at point of care or later from the lab, so the
        -- test is added to both performed and completed tests.
        updatedValue =
            { value
                | performedTests = EverySet.insert test value.performedTests
                , completedTests = EverySet.insert test value.completedTests
            }

        unchanged =
            EverySet.isEmpty (EverySet.diff updatedValue.performedTests storedValue.performedTests)
                && EverySet.isEmpty (EverySet.diff updatedValue.completedTests storedValue.completedTests)
    in
    if unchanged then
        Nothing

    else if activityCompleted assembled RecurrentNextSteps then
        -- With Next Steps completed or not required, the entry is resolved
        -- once every performed test is completed.
        resolvedLabsResultsValue currentDate updatedValue
            |> Maybe.withDefault updatedValue
            |> Just

    else
        Just updatedValue


expectLaboratoryResultTask : AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> testPerformedByValue
    in
    case task of
        TaskHIVTest ->
            False

        TaskRandomBloodSugarTest ->
            getMeasurementValueFunc assembled.measurements.randomBloodSugarTest
                |> Maybe.map
                    (\value ->
                        testPerformedByExecutionNote value.executionNote
                            && expectUniversalTestResultTask value
                    )
                |> Maybe.withDefault False

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskPregnancyTest ->
            False

        TaskCreatinineTest ->
            wasTestPerformed .creatinineTest

        TaskLiverFunctionTest ->
            wasTestPerformed .liverFunctionTest

        TaskLipidPanelTest ->
            wasTestPerformed .lipidPanelTest

        TaskHbA1cTest ->
            False

        -- Others are not in use for NCD.
        _ ->
            False


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskRandomBloodSugarTest
    , TaskLiverFunctionTest
    , TaskUrineDipstickTest
    , TaskCreatinineTest
    , TaskLipidPanelTest
    ]
