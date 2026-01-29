module Pages.NCD.RecurrentActivity.Utils exposing (activityCompleted, expectActivity, expectLaboratoryResultTask, expectNextStepsTask, laboratoryResultTaskCompleted, laboratoryResultTasks, nextStepsTaskCompleted, nextStepsTasksCompletedFromTotal, resolveLaboratoryResultTasks, resolveNextStepsTasks)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (expectUniversalTestResultTask, testPerformedByExecutionNote, testPerformedByValue)
import Pages.NCD.Model exposing (AssembledData, NCDEncounterPhase(..))
import Pages.NCD.RecurrentActivity.Model exposing (Msg(..), NextStepsData)
import Pages.NCD.RecurrentActivity.Types exposing (NextStepsTask(..))
import Pages.NCD.Utils exposing (generateRecommendedTreatmentSignsForHypertension, medicateForDiabetes, medicateForHypertension, medicationDistributionFormWithDefault, recommendedTreatmentMeasurementTaken, recommendedTreatmentSignsForDiabetes, referForDiabetes, referForHypertension, referForRenalComplications, referralFormWithDefault, referralToFacilityCompleted, resolveMedicationDistributionInputsAndTasks, resolveReferralInputsAndTasks)
import Pages.Utils exposing (resolveTasksCompletedFromTotal)
import Translate.Model exposing (Language)


expectActivity : NominalDate -> AssembledData -> NCDRecurrentActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTasks currentDate assembled
                |> List.isEmpty
                |> not

        RecurrentNextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.filter (expectNextStepsTask currentDate assembled)
                |> List.isEmpty
                |> not


activityCompleted : NominalDate -> AssembledData -> NCDRecurrentActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity currentDate assembled LabResults)
                || (resolveLaboratoryResultTasks currentDate assembled
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
    -> NominalDate
    -> AssembledData
    -> NextStepsData
    -> Pages.NCD.RecurrentActivity.Types.NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate assembled data task =
    case task of
        TaskMedicationDistribution ->
            let
                ( _, completed, total ) =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> resolveMedicationDistributionInputsAndTasks language
                            currentDate
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
                            currentDate
                            NCDEncounterPhaseRecurrent
                            assembled
                            SetReferralBoolInput
                            SetFacilityNonReferralReason
            in
            resolveTasksCompletedFromTotal tasks


resolveLaboratoryResultTasks : NominalDate -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultTasks currentDate assembled =
    List.filter (expectLaboratoryResultTask currentDate assembled) laboratoryResultTasks


laboratoryResultTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted currentDate assembled task =
    let
        taskExpected =
            expectLaboratoryResultTask currentDate assembled

        testResultsCompleted getMeasurementFunc getResultFieldFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        testPerformedByExecutionNote value.executionNote
                            && (isJust <| getResultFieldFunc value)
                    )
                |> Maybe.withDefault False
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

        TaskLipidPanelTest ->
            (not <| taskExpected TaskLipidPanelTest) || testResultsCompleted .lipidPanelTest .totalCholesterolResult

        -- Others are not in use for NCD.
        _ ->
            False


expectLaboratoryResultTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask currentDate assembled task =
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
                |> Maybe.map expectUniversalTestResultTask
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
