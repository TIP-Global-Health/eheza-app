module Pages.NCD.RecurrentActivity.Utils exposing (activityCompleted, expectActivity, laboratoryResultTaskCompleted, nextStepsTaskCompleted, nextStepsTasksCompletedFromTotal, resolveLaboratoryResultTasks, resolveNextStepsTasks)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (expectUniversalTestResultTask, testPerformedByExecutionNote, testPerformedByValue)
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
