module Pages.Prenatal.RecurrentActivity.Utils exposing (activityCompleted, allLabsCompleted, concludedLabsResultsValue, diagnosisRequiresEmergencyReferal, examinationMeasurementTaken, examinationTasksCompletedFromTotal, expectActivity, generateVitalsFormConfig, healthEducationFormInputsAndTasks, healthEducationFormWithDefault, laboratoryResultFollowUpsTaskCompleted, laboratoryResultNotPerformed, laboratoryResultTaskCompleted, labsResultsTests, nextStepsTaskCompleted, nextStepsTasksCompletedFromTotal, resolveLaboratoryResultFollowUpsTasks, resolveLaboratoryResultTasks, resolveNextStepsTasks, resolveReferralInputsAndTasks, toHealthEducationValueWithDefault, withCompletedTestsFromMeasurements)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, testPerformedByExecutionNote)
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Maybe.Extra exposing (isJust, or, unwrap)
import Measurement.Model
    exposing
        ( InvokationModule(..)
        , LaboratoryTask(..)
        , VitalsForm
        , VitalsFormConfig
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( bloodSmearResultSet
        , expectUniversalTestResultTask
        , testNotPerformedByWhyNotAtExecutionNote
        , vitalsFormWithDefault
        )
import Measurement.View exposing (vitalsFormInputsAndTasks)
import Pages.GlobalCaseManagement.Utils exposing (labsResultsTestData)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (ExaminationData, Msg(..), NextStepsData)
import Pages.Prenatal.RecurrentActivity.Types exposing (ExaminationTask(..), NextStepsTask(..))
import Pages.Prenatal.Utils exposing (diabetesDiagnosesRecurrentPhase, diagnosed, diagnosedAnyOf, diagnosedHypertension, diagnosedHypertensionPrevoiusly, diagnosedMalariaByPhase, diagnosedSyphilisByPhase, diagnosesCausingHospitalReferralByPhase, emergencyReferralDiagnosesRecurrent, expectMalariaPreventionActivity, healthEducationFormInputsAndTasksForNurse, hierarchalBloodPressureDiagnosesInitialPhase, hivProgramAtHC, marginalBloodPressureCondition, medicationDistributionFormWithDefaultRecurrentPhase, provideHIVEducation, recommendedTreatmentMeasurementTaken, recommendedTreatmentSignsForHypertension, recommendedTreatmentSignsForMalaria, recommendedTreatmentSignsForSyphilis, referToHospitalDueToAdverseEventForMalariaTreatment, referralFormWithDefault, referralToFacilityCompleted, reinforceTreatmentSignsCompleted, requiredMedicationsAddressed, resolveMedicationDistributionInputsAndTasks, resolveReferralToFacilityInputsAndTasks, resolveRequiredMedicationsSet)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , resolveTasksCompletedFromTotal
        , taskCompleted
        )
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> Bool -> AssembledData -> PrenatalRecurrentActivity -> Bool
expectActivity currentDate isLabTech assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTasks isLabTech assembled
                |> List.isEmpty
                |> not

        RecurrentNextSteps ->
            resolveNextStepsTasks currentDate isLabTech assembled
                |> List.isEmpty
                |> not

        RecurrentExamination ->
            resolveExaminationTasks assembled
                |> List.isEmpty
                |> not

        RecurrentMalariaPrevention ->
            expectMalariaPreventionActivity PhaseRecurrent assembled

        LabsResultsFollowUps ->
            resolveLaboratoryResultFollowUpsTasks assembled
                |> List.isEmpty
                |> not


activityCompleted : NominalDate -> Bool -> AssembledData -> PrenatalRecurrentActivity -> Bool
activityCompleted currentDate isLabTech assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity currentDate isLabTech assembled LabResults)
                || (resolveLaboratoryResultTasks isLabTech assembled
                        |> List.all (laboratoryResultTaskCompleted isLabTech assembled)
                   )

        RecurrentNextSteps ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentNextSteps)
                || (resolveNextStepsTasks currentDate isLabTech assembled
                        |> List.all (nextStepsTaskCompleted currentDate assembled)
                   )

        RecurrentExamination ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentExamination)
                || (resolveExaminationTasks assembled
                        |> List.all (examinationMeasurementTaken assembled)
                   )

        RecurrentMalariaPrevention ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentMalariaPrevention)
                || isJust assembled.measurements.malariaPrevention

        LabsResultsFollowUps ->
            (not <| expectActivity currentDate isLabTech assembled LabsResultsFollowUps)
                || (resolveLaboratoryResultFollowUpsTasks assembled
                        |> List.all (laboratoryResultFollowUpsTaskCompleted assembled)
                   )


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskPartnerHIVTest
    , TaskHIVTest
    , TaskHIVPCRTest
    , TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskMalariaTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
    ]


resolveLaboratoryResultTasks : Bool -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultTasks isLabTech assembled =
    List.filter (expectLaboratoryResultTask isLabTech assembled) laboratoryResultTasks


laboratoryResultTaskCompleted : Bool -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted isLabTech assembled task =
    let
        taskNotExpected =
            not <| expectLaboratoryResultTask isLabTech assembled task
    in
    case task of
        TaskCompletePreviousTests ->
            taskNotExpected

        _ ->
            List.member task laboratoryResultTasks
                && (taskNotExpected || laboratoryResultEntered assembled task)


{-| Whether the test's own measurement holds a result, or the reason it was
not run.
-}
laboratoryResultEntered : AssembledData -> LaboratoryTask -> Bool
laboratoryResultEntered assembled task =
    laboratoryResultValue assembled task
        |> Maybe.map (\value -> value.resultOfRun || isJust value.notPerformedReason)
        |> Maybe.withDefault False


{-| The labs entry with each performed test counted as completed when its own
measurement says so. A device's copy of the entry can be behind its tests.
-}
withCompletedTestsFromMeasurements : AssembledData -> LabsResultsValue -> LabsResultsValue
withCompletedTestsFromMeasurements assembled value =
    let
        completedByMeasurement test =
            case test of
                TestBloodGpRs ->
                    laboratoryResultEntered assembled TaskBloodGpRsTest

                TestCreatinine ->
                    False

                TestHemoglobin ->
                    laboratoryResultEntered assembled TaskHemoglobinTest

                TestHepatitisB ->
                    laboratoryResultEntered assembled TaskHepatitisBTest

                TestHIV ->
                    laboratoryResultEntered assembled TaskHIVTest

                TestHIVPCR ->
                    laboratoryResultEntered assembled TaskHIVPCRTest

                TestLipidPanel ->
                    False

                TestLiverFunction ->
                    False

                TestMalaria ->
                    laboratoryResultEntered assembled TaskMalariaTest

                TestPartnerHIV ->
                    laboratoryResultEntered assembled TaskPartnerHIVTest

                TestRandomBloodSugar ->
                    laboratoryResultEntered assembled TaskRandomBloodSugarTest

                TestSyphilis ->
                    laboratoryResultEntered assembled TaskSyphilisTest

                TestUrineDipstick ->
                    laboratoryResultEntered assembled TaskUrineDipstickTest

                TestVitalsRecheck ->
                    examinationMeasurementTaken assembled ExaminationVitals
    in
    { value
        | completedTests =
            EverySet.filter completedByMeasurement value.performedTests
                |> EverySet.union value.completedTests
    }


{-| The labs entry's performed and completed tests, with completions read from
the test measurements too. A vitals recheck scheduled on another day is left out.
-}
labsResultsTests : NominalDate -> AssembledData -> PrenatalLabsResults -> ( EverySet LaboratoryTest, EverySet LaboratoryTest )
labsResultsTests currentDate assembled results =
    labsResultsTestData currentDate { results | value = withCompletedTestsFromMeasurements assembled results.value }


{-| Whether every performed test is completed.
-}
allLabsCompleted : EverySet LaboratoryTest -> EverySet LaboratoryTest -> Bool
allLabsCompleted performedTests completedTests =
    EverySet.isEmpty <| EverySet.diff performedTests completedTests


{-| The labs entry as a nurse closes it at the end of the recurrent phase:
resolved today, and reviewed once every performed test is completed.
-}
concludedLabsResultsValue : NominalDate -> AssembledData -> PrenatalLabsResults -> LabsResultsValue
concludedLabsResultsValue currentDate assembled results =
    let
        ( performedTests, completedTests ) =
            labsResultsTests currentDate assembled results

        value =
            results.value
    in
    { value
        | performedTests = performedTests
        , completedTests = completedTests
        , reviewState =
            if allLabsCompleted performedTests completedTests then
                Just LabsResultsReviewCompleted

            else
                value.reviewState
        , resolutionDate = currentDate
    }


expectLaboratoryResultTask : Bool -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask isLabTech assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map expectUniversalTestResultTask
                |> Maybe.withDefault False

        -- For nurses, a test with follow up questions from the lab technician
        -- is shown at Lab Results Follow Ups instead. One declined without a
        -- result has none.
        shownToNurse labTask test =
            laboratoryResultDeclinedWithoutResult assembled labTask
                || (getMeasurementValueFunc assembled.measurements.labsResults
                        |> Maybe.andThen .testsWithFollowUp
                        |> Maybe.map (EverySet.member test >> not)
                        |> Maybe.withDefault True
                   )
    in
    case task of
        TaskHIVTest ->
            wasTestPerformed .hivTest
                && (isLabTech || shownToNurse TaskHIVTest TestHIV)

        TaskPartnerHIVTest ->
            wasTestPerformed .partnerHIVTest
                && (isLabTech || shownToNurse TaskPartnerHIVTest TestPartnerHIV)

        TaskSyphilisTest ->
            wasTestPerformed .syphilisTest
                && (isLabTech || shownToNurse TaskSyphilisTest TestSyphilis)

        TaskHepatitisBTest ->
            wasTestPerformed .hepatitisBTest

        TaskMalariaTest ->
            wasTestPerformed .malariaTest

        TaskBloodGpRsTest ->
            wasTestPerformed .bloodGpRsTest

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskHemoglobinTest ->
            wasTestPerformed .hemoglobinTest

        TaskRandomBloodSugarTest ->
            wasTestPerformed .randomBloodSugarTest

        TaskHIVPCRTest ->
            wasTestPerformed .hivPCRTest

        TaskCompletePreviousTests ->
            False

        -- Others are not in use for Prenatal.
        _ ->
            False


{-| Why and when the lab technician did not perform a test listed for results,
and whether a result was entered for it anyway.
-}
laboratoryResultNotPerformed :
    AssembledData
    -> LaboratoryTask
    -> Maybe { executionDate : Maybe NominalDate, holdsResult : Bool, reason : TestExecutionNote }
laboratoryResultNotPerformed assembled task =
    laboratoryResultValue assembled task
        |> Maybe.andThen
            (\value ->
                Maybe.map
                    (\reason ->
                        { executionDate = value.executionDate
                        , holdsResult = value.holdsResult
                        , reason = reason
                        }
                    )
                    value.notPerformedReason
            )


{-| The lab technician declined the test and no result is stored, so the nurse
has nothing to fill in or follow up.
-}
laboratoryResultDeclinedWithoutResult : AssembledData -> LaboratoryTask -> Bool
laboratoryResultDeclinedWithoutResult assembled task =
    laboratoryResultNotPerformed assembled task
        |> Maybe.map (.holdsResult >> not)
        |> Maybe.withDefault False


{-| What a test's record holds: its date, whether any result is stored, whether
that result is from a run, and why the test was not performed.
-}
laboratoryResultValue :
    AssembledData
    -> LaboratoryTask
    ->
        Maybe
            { executionDate : Maybe NominalDate
            , holdsResult : Bool
            , notPerformedReason : Maybe TestExecutionNote
            , resultOfRun : Bool
            }
laboratoryResultValue assembled task =
    let
        notPerformedReasonByNote executionNote =
            if testNotPerformedByWhyNotAtExecutionNote executionNote then
                Just executionNote

            else
                Nothing

        byMeasurement getMeasurementFunc getResultFieldFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        let
                            holdsResult =
                                isJust <| getResultFieldFunc value
                        in
                        { executionDate = value.executionDate
                        , holdsResult = holdsResult
                        , notPerformedReason = notPerformedReasonByNote value.executionNote
                        , resultOfRun = testPerformedByExecutionNote value.executionNote && holdsResult
                        }
                    )
    in
    case task of
        TaskBloodGpRsTest ->
            byMeasurement .bloodGpRsTest .bloodGroup

        TaskHemoglobinTest ->
            byMeasurement .hemoglobinTest .hemoglobinCount

        TaskHepatitisBTest ->
            byMeasurement .hepatitisBTest .testResult

        TaskHIVPCRTest ->
            byMeasurement .hivPCRTest .hivViralLoadStatus

        TaskHIVTest ->
            byMeasurement .hivTest .testResult

        TaskMalariaTest ->
            getMeasurementValueFunc assembled.measurements.malariaTest
                |> Maybe.map
                    (\value ->
                        let
                            smearRead =
                                bloodSmearResultSet value.bloodSmearResult

                            -- The reason is the nurse's, for not running the
                            -- rapid test, when a smear was ordered at the lab,
                            -- or read.
                            smearTaken =
                                smearRead || value.bloodSmearResult == BloodSmearPendingInput
                        in
                        { executionDate = value.executionDate
                        , holdsResult = isJust value.testResult || smearRead
                        , notPerformedReason =
                            if smearTaken then
                                Nothing

                            else
                                notPerformedReasonByNote value.executionNote
                        , resultOfRun =
                            smearRead
                                || (not smearTaken
                                        && testPerformedByExecutionNote value.executionNote
                                        && isJust value.testResult
                                   )
                        }
                    )

        TaskPartnerHIVTest ->
            byMeasurement .partnerHIVTest .testResult

        TaskRandomBloodSugarTest ->
            byMeasurement .randomBloodSugarTest .sugarCount

        TaskSyphilisTest ->
            byMeasurement .syphilisTest .testResult

        TaskUrineDipstickTest ->
            byMeasurement .urineDipstickTest .protein

        -- Others have no result to wait for at Prenatal.
        _ ->
            Nothing


laboratoryResultFollowUpsTasks : List LaboratoryTask
laboratoryResultFollowUpsTasks =
    [ TaskHIVTest
    , TaskSyphilisTest
    , TaskPartnerHIVTest
    ]


resolveLaboratoryResultFollowUpsTasks : AssembledData -> List LaboratoryTask
resolveLaboratoryResultFollowUpsTasks assembled =
    List.filter (expectLaboratoryResultFollowUpsTask assembled) laboratoryResultFollowUpsTasks


laboratoryResultFollowUpsTaskCompleted : AssembledData -> LaboratoryTask -> Bool
laboratoryResultFollowUpsTaskCompleted assembled task =
    let
        taskExpected =
            expectLaboratoryResultFollowUpsTask assembled

        testFollowUpCompleted getMeasurementFunc getResultFieldFunc pendingValue =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.andThen getResultFieldFunc
                |> Maybe.map (EverySet.member pendingValue >> not)
                |> Maybe.withDefault False
    in
    case task of
        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || testFollowUpCompleted .hivTest .hivSigns PrenatalHIVSignPendingInput

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || testFollowUpCompleted .syphilisTest .symptoms IllnessSymptomPendingInput

        TaskPartnerHIVTest ->
            (not <| taskExpected TaskPartnerHIVTest) || testFollowUpCompleted .partnerHIVTest .hivSigns PrenatalHIVSignPendingInput

        -- Others are not in use for Prenatal.
        _ ->
            False


expectLaboratoryResultFollowUpsTask : AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultFollowUpsTask assembled task =
    let
        wasFollowUpScheduled test =
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.andThen .testsWithFollowUp
                |> Maybe.map (EverySet.member test)
                |> Maybe.withDefault False
    in
    case task of
        TaskHIVTest ->
            let
                partnerHIVTestFollowUpExpected =
                    expectLaboratoryResultFollowUpsTask assembled TaskPartnerHIVTest

                hivTestResultPositive =
                    getMeasurementValueFunc assembled.measurements.hivTest
                        |> Maybe.map (.testResult >> (==) (Just TestPositive))
                        |> Maybe.withDefault False
            in
            -- At TaskPartnerHIVTest task we ask same follow up questions,
            -- as we do for TestHIV when test result is negative.
            -- So we either do not expect TaskPartnerHIVTest follow up, or,
            -- only when test HIV result is positive.
            wasFollowUpScheduled TestHIV
                -- The nurse finds a declined test's reason at Lab Results.
                && not (laboratoryResultDeclinedWithoutResult assembled TaskHIVTest)
                && (not partnerHIVTestFollowUpExpected || hivTestResultPositive)

        TaskSyphilisTest ->
            wasFollowUpScheduled TestSyphilis
                && -- Lab tech entered result showing positive Syphilis,
                   -- which requires Syphilis symptoms question.
                   (getMeasurementValueFunc assembled.measurements.syphilisTest
                        |> Maybe.map (.testResult >> (==) (Just TestPositive))
                        |> Maybe.withDefault False
                   )

        TaskPartnerHIVTest ->
            wasFollowUpScheduled TestPartnerHIV
                && -- Lab tech entered result showing positive Syphilis,
                   -- which requires Syphilis symptoms question.
                   (getMeasurementValueFunc assembled.measurements.partnerHIVTest
                        |> Maybe.map (.testResult >> (==) (Just TestPositive))
                        |> Maybe.withDefault False
                   )

        -- Others are not in use for Prenatal.
        _ ->
            False


resolveNextStepsTasks : NominalDate -> Bool -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate isLabTech assembled =
    -- The order is important. Do not change.
    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC ]
        |> List.filter (expectNextStepsTask currentDate isLabTech assembled)


expectNextStepsTask : NominalDate -> Bool -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate isLabTech assembled task =
    case task of
        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.isEmpty
                |> not

        NextStepsMedicationDistribution ->
            -- Emergency referral is not required.
            (not <| emergencyReferalRequired assembled)
                && ((resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseRecurrent assembled
                        |> List.isEmpty
                        |> not
                    )
                        || (diagnosedMalariaByPhase PrenatalEncounterPhaseRecurrent assembled
                                && (not <| referToHospitalDueToAdverseEventForMalariaTreatment assembled)
                           )
                        || diagnosedSyphilisByPhase PrenatalEncounterPhaseRecurrent assembled
                        || diagnosedHypertension PrenatalEncounterPhaseRecurrent assembled
                        || diagnosedAnyOf [ DiagnosisModerateAnemiaRecurrentPhase ] assembled
                   )

        NextStepsHealthEducation ->
            -- Emergency referral is not required.
            (not <| emergencyReferalRequired assembled)
                && diagnosisSourcesCompleted currentDate isLabTech assembled
                && (provideHIVEducation PrenatalEncounterPhaseRecurrent assembled.measurements
                        || diagnosedAnyOf (DiagnosisHIVDetectableViralLoadRecurrentPhase :: diabetesDiagnosesRecurrentPhase) assembled
                   )


{-| A health education question that was not asked is stored exactly like one
answered with No, so the task waits for the activities that make diagnoses.
-}
diagnosisSourcesCompleted : NominalDate -> Bool -> AssembledData -> Bool
diagnosisSourcesCompleted currentDate isLabTech assembled =
    List.all (activityCompleted currentDate isLabTech assembled)
        [ LabResults, LabsResultsFollowUps, RecurrentExamination ]


nextStepsTaskCompleted : NominalDate -> AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.all (referralToFacilityCompleted assembled)

        NextStepsMedicationDistribution ->
            let
                medicationDistributionCompleted =
                    requiredMedicationsAddressed currentDate PrenatalEncounterPhaseRecurrent assembled

                malariaTreatmentCompleted =
                    if diagnosedMalariaByPhase PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForMalaria assembled.measurements

                    else
                        True

                syphilisTreatmentCompleted =
                    if diagnosedSyphilisByPhase PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForSyphilis assembled.measurements

                    else
                        True

                hypertensionTreatmentCompleted =
                    if diagnosedHypertension PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True

                anemiaTreatmentCompleted =
                    if diagnosed DiagnosisModerateAnemiaRecurrentPhase assembled then
                        reinforceTreatmentSignsCompleted assembled.measurements

                    else
                        True
            in
            medicationDistributionCompleted
                && malariaTreatmentCompleted
                && syphilisTreatmentCompleted
                && hypertensionTreatmentCompleted
                && anemiaTreatmentCompleted

        NextStepsHealthEducation ->
            getMeasurementValueFunc assembled.measurements.healthEducation
                |> Maybe.map (.signsPhase2 >> isJust)
                |> Maybe.withDefault False


nextStepsTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate assembled data task =
    case task of
        NextStepsSendToHC ->
            let
                form =
                    assembled.measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> referralFormWithDefault data.referralForm

                ( _, tasks ) =
                    resolveReferralInputsAndTasks language
                        assembled
                        SetReferralBoolInput
                        SetFacilityNonReferralReason
                        form
            in
            resolveTasksCompletedFromTotal tasks

        NextStepsMedicationDistribution ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultRecurrentPhase data.medicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks language
                        currentDate
                        PrenatalEncounterPhaseRecurrent
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        SetRecommendedTreatmentSign
                        (always NoOp)
                        form
            in
            ( completed, total )

        NextStepsHealthEducation ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled form
            in
            ( List.map taskCompleted tasks
                |> List.sum
            , List.length tasks
            )


emergencyReferalRequired : AssembledData -> Bool
emergencyReferalRequired assembled =
    EverySet.toList assembled.encounter.diagnoses
        |> List.filter diagnosisRequiresEmergencyReferal
        |> List.isEmpty
        |> not


diagnosisRequiresEmergencyReferal : PrenatalDiagnosis -> Bool
diagnosisRequiresEmergencyReferal diagnosis =
    List.member diagnosis emergencyReferralDiagnosesRecurrent


resolveExaminationTasks : AssembledData -> List ExaminationTask
resolveExaminationTasks assembled =
    List.filter (expectExaminationTask assembled)
        [ ExaminationVitals ]


expectExaminationTask : AssembledData -> ExaminationTask -> Bool
expectExaminationTask assembled task =
    case task of
        ExaminationVitals ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy. If diagnised, we do not need to recheck the BP.
            -- Measurement taken at initial phase of encounter is sufficient.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedAnyOf hierarchalBloodPressureDiagnosesInitialPhase assembled)
                && (getMeasurementValueFunc assembled.measurements.vitals
                        |> Maybe.andThen
                            (\value ->
                                Maybe.map2
                                    marginalBloodPressureCondition
                                    value.dia
                                    value.sys
                            )
                        |> Maybe.withDefault False
                   )


examinationMeasurementTaken : AssembledData -> ExaminationTask -> Bool
examinationMeasurementTaken assembled task =
    case task of
        ExaminationVitals ->
            getMeasurementValueFunc assembled.measurements.vitals
                |> Maybe.map
                    -- We meassure sysRepeated and diaRepeated, but we know for sure
                    -- that if one is set, other one is set as well.
                    -- So, it's enough to check only one.
                    (.sysRepeated >> isJust)
                |> Maybe.withDefault False


examinationTasksCompletedFromTotal : NominalDate -> AssembledData -> ExaminationData -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal currentDate assembled data task =
    case task of
        ExaminationVitals ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm

                formConfig =
                    generateVitalsFormConfig assembled form

                ( _, tasks ) =
                    vitalsFormInputsAndTasks English currentDate formConfig form
            in
            resolveTasksCompletedFromTotal tasks


generateVitalsFormConfig : AssembledData -> VitalsForm -> VitalsFormConfig Msg
generateVitalsFormConfig assembled form =
    { setIntInputMsg = \_ _ -> NoOp
    , setFloatInputMsg = SetVitalsFloatInput
    , setRespiratoryRateNotTakenMsg = always NoOp
    , setBodyTemperatureNotTakenMsg = always NoOp
    , sysBloodPressurePreviousValue = form.sysBloodPressure
    , diaBloodPressurePreviousValue = form.diaBloodPressure
    , heartRatePreviousValue = Nothing
    , respiratoryRatePreviousValue = Nothing
    , bodyTemperaturePreviousValue = Nothing
    , birthDate = assembled.person.birthDate
    , formClass = "examination vitals"
    , mode = VitalsFormRepeated
    , invokationModule = InvokationModulePrenatal
    , allowSkipping = False
    }


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled form =
    healthEducationFormInputsAndTasksForNurse language
        PrenatalEncounterPhaseRecurrent
        SetHealthEducationBoolInput
        assembled
        form


toHealthEducationValueWithDefault : Maybe PrenatalHealthEducationValue -> HealthEducationForm -> Maybe PrenatalHealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue saved


healthEducationFormWithDefault :
    HealthEducationForm
    -> Maybe PrenatalHealthEducationValue
    -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { hivDetectableViralLoad = or form.hivDetectableViralLoad (Maybe.map (EverySet.member EducationHIVDetectableViralLoad) value.signsPhase2)
                , diabetes = or form.diabetes (Maybe.map (EverySet.member EducationDiabetes) value.signsPhase2)
                , positiveHIV = or form.positiveHIV (Maybe.map (EverySet.member EducationPositiveHIV) value.signsPhase2)
                , saferSexHIV = or form.saferSexHIV (Maybe.map (EverySet.member EducationSaferSexHIV) value.signsPhase2)
                , partnerTesting = or form.partnerTesting (Maybe.map (EverySet.member EducationPartnerTesting) value.signsPhase2)
                , familyPlanning = or form.familyPlanning (Maybe.map (EverySet.member EducationFamilyPlanning) value.signsPhase2)

                -- Signs that do not participate at recurrent phase. Resolved directly from value.
                , expectations = EverySet.member EducationExpectations value.signs |> Just
                , visitsReview = EverySet.member EducationVisitsReview value.signs |> Just
                , warningSigns = EverySet.member EducationWarningSigns value.signs |> Just
                , hemorrhaging = EverySet.member EducationHemorrhaging value.signs |> Just
                , breastfeeding = EverySet.member EducationBreastfeeding value.signs |> Just
                , immunization = EverySet.member EducationImmunization value.signs |> Just
                , hygiene = EverySet.member EducationHygiene value.signs |> Just
                , nauseaVomiting = EverySet.member EducationNauseaVomiting value.signs |> Just
                , legCramps = EverySet.member EducationLegCramps value.signs |> Just
                , lowBackPain = EverySet.member EducationLowBackPain value.signs |> Just
                , constipation = EverySet.member EducationConstipation value.signs |> Just
                , heartburn = EverySet.member EducationHeartburn value.signs |> Just
                , varicoseVeins = EverySet.member EducationVaricoseVeins value.signs |> Just
                , legPainRedness = EverySet.member EducationLegPainRedness value.signs |> Just
                , pelvicPain = EverySet.member EducationPelvicPain value.signs |> Just
                , saferSex = EverySet.member EducationSaferSex value.signs |> Just
                , mentalHealth = EverySet.member EducationMentalHealth value.signs |> Just
                , earlyMastitisOrEngorgment = EverySet.member EducationEarlyMastitisOrEngorgment value.signs |> Just
                , mastitis = EverySet.member EducationMastitis value.signs |> Just
                , grief = EverySet.member EducationGrief value.signs |> Just
                , hivPartnerPresence = EverySet.member EducationHIVPartnerPresence value.signs |> Just
                }
            )


toHealthEducationValue : Maybe PrenatalHealthEducationValue -> HealthEducationForm -> Maybe PrenatalHealthEducationValue
toHealthEducationValue saved form =
    [ ifNullableTrue EducationPositiveHIV form.positiveHIV
    , ifNullableTrue EducationSaferSexHIV form.saferSexHIV
    , ifNullableTrue EducationPartnerTesting form.partnerTesting
    , ifNullableTrue EducationFamilyPlanning form.familyPlanning
    , ifNullableTrue EducationHIVDetectableViralLoad form.hivDetectableViralLoad
    , ifNullableTrue EducationDiabetes form.diabetes
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHealthEducationSigns)
        |> Maybe.map
            (\signsPhase2 ->
                { signs =
                    Maybe.map .signs saved
                        |> Maybe.withDefault (EverySet.singleton NoPrenatalHealthEducationSigns)
                , signsPhase2 = Just signsPhase2
                }
            )


resolveReferralInputsAndTasks :
    Language
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralInputsAndTasks language assembled setReferralBoolInputMsg setNonReferralReasonMsg form =
    let
        foldResults =
            List.foldr
                (\( inputs, tasks ) ( accumInputs, accumTasks ) ->
                    ( inputs ++ accumInputs, tasks ++ accumTasks )
                )
                ( [], [] )
    in
    resolveRequiredReferralFacilities assembled
        |> List.map (resolveReferralToFacilityInputsAndTasks language PrenatalEncounterPhaseRecurrent assembled setReferralBoolInputMsg setNonReferralReasonMsg form)
        |> foldResults


resolveRequiredReferralFacilities : AssembledData -> List ReferralFacility
resolveRequiredReferralFacilities assembled =
    List.filter (matchRequiredReferralFacility assembled) referralFacilities


matchRequiredReferralFacility : AssembledData -> ReferralFacility -> Bool
matchRequiredReferralFacility assembled facility =
    case facility of
        FacilityHospital ->
            diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseRecurrent assembled
                |> EverySet.isEmpty
                |> not

        FacilityMentalHealthSpecialist ->
            False

        FacilityARVProgram ->
            referToARVProgram assembled

        FacilityNCDProgram ->
            -- NCD proram referral is based on diagnoses made at
            -- previous encounters, and therefore, can appear
            -- only on initial phase.
            False

        -- Explicit NCD facility.
        FacilityANCServices ->
            False

        FacilityUltrasound ->
            False

        FacilityHealthCenter ->
            -- We should never get here. HC inputs are resolved
            -- with resolveReferralInputsAndTasksForCHW.
            False


referToARVProgram : AssembledData -> Bool
referToARVProgram assembled =
    -- No need to check Speciality care, since there is no
    -- recurrent phase for postpartum encounter.
    diagnosed DiagnosisHIVRecurrentPhase assembled && (hivProgramAtHC assembled.measurements == Just True)


referralFacilities : List ReferralFacility
referralFacilities =
    [ FacilityHospital
    , FacilityARVProgram
    ]
