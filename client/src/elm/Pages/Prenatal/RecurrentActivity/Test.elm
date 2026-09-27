module Pages.Prenatal.RecurrentActivity.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , BloodSmearResult(..)
        , HIVTestValue
        , LaboratoryTest(..)
        , LabsResultsValue
        , MalariaTestValue
        , MedicationDistributionSign(..)
        , MedicationNonAdministrationSign(..)
        , PartnerHIVTestValue
        , PrenatalHIVSign(..)
        , PrenatalMeasurements
        , PrenatalMedicationDistributionValue
        , RecommendedTreatmentSign(..)
        , TestExecutionNote(..)
        , TestResult(..)
        , VitalsValue
        , emptyPrenatalMeasurements
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.Prenatal.Model exposing (AssembledData, PrenatalEncounterPhase(..), emptyMedicationDistributionForm)
import Pages.Prenatal.RecurrentActivity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.RecurrentActivity.Utils
    exposing
        ( laboratoryResultTaskCompleted
        , nextStepsTaskCompleted
        , resolveLaboratoryResultFollowUpsTasks
        , resolveNextStepsTasks
        )
import Pages.Prenatal.Utils exposing (medicationDistributionFormWithDefaultRecurrentPhase, resolveMedicationDistributionInputsAndTasks, resolveRequiredMedicationsSet)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time
import Translate.Model exposing (Language(..))



-- Labs results follow ups. A lab technician who enters an HIV, Syphilis or
-- Partner HIV result leaves the follow up questions for the nurse, and the
-- test is listed at labsResults.testsWithFollowUp. The nurse then gets one
-- task per listed test.
--
-- The HIV task is the one with a rule of its own: when HIV result is
-- negative, its follow up questions are the partner questions, which the
-- Partner HIV task asks as well. So the HIV task is dropped when the Partner
-- HIV task is shown - unless HIV result is positive, where the HIV task asks
-- a different question ("is she enrolled in the HIV programme at the health
-- centre?") that no other task asks.


{-| The reference "current date" for these tests.
-}
currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| A nurse antenatal encounter. Only the measurements matter here, so
everything else is defaulted.
-}
testEncounter : PrenatalEncounter
testEncounter =
    { participant = toEntityUuid "dummy-participant"
    , startDate = currentDate
    , endDate = Nothing
    , encounterType = NurseEncounter
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , indicators = EverySet.empty
    , nextVisitDate = Nothing
    , deleted = False
    , shard = Nothing
    }


testAssembled : PrenatalMeasurements -> AssembledData
testAssembled measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = testEncounter
    , participant = TestFixtures.testParticipant currentDate AntenatalEncounter
    , person = TestFixtures.testPerson
    , measurements = measurements
    , nursePreviousEncountersData = []
    , chwPreviousMeasurementsWithDates = []
    , globalLmpDate = Just (Date.add Date.Weeks -20 currentDate)
    , globalObstetricHistory = Nothing
    , vaccinationHistory = Dict.empty
    , vaccinationProgress = Dict.empty
    }


{-| Measurements of an encounter where a lab technician entered results for
`testsWithFollowUp`, with the given HIV and Partner HIV results.
-}
measurementsWith : List LaboratoryTest -> Maybe TestResult -> Maybe TestResult -> PrenatalMeasurements
measurementsWith testsWithFollowUp hivResult partnerHIVResult =
    let
        labsResultsValue : LabsResultsValue
        labsResultsValue =
            { performedTests = EverySet.fromList [ TestHIV, TestPartnerHIV ]
            , completedTests = EverySet.fromList [ TestHIV, TestPartnerHIV ]
            , resolutionDate = currentDate
            , patientNotified = False
            , reviewState = Nothing
            , testsWithFollowUp = Just (EverySet.fromList testsWithFollowUp)
            }

        partnerHIVTestValue : PartnerHIVTestValue
        partnerHIVTestValue =
            { executionNote = TestNoteRunToday
            , executionDate = Just currentDate
            , testPrerequisites = Nothing
            , testResult = partnerHIVResult
            , hivSigns = Nothing
            }
    in
    { emptyPrenatalMeasurements
        | labsResults = TestFixtures.wrapMeasurement currentDate labsResultsValue
        , hivTest = TestFixtures.wrapMeasurement currentDate (hivTestValueWith TestNoteRunToday hivResult Nothing)
        , partnerHIVTest = TestFixtures.wrapMeasurement currentDate partnerHIVTestValue
    }


hivTestValueWith : TestExecutionNote -> Maybe TestResult -> Maybe (EverySet PrenatalHIVSign) -> HIVTestValue
hivTestValueWith executionNote testResult hivSigns =
    { executionNote = executionNote
    , executionDate = Just currentDate
    , testPrerequisites = Nothing
    , testResult = testResult
    , hivSigns = hivSigns
    }


withDiagnoses : List PrenatalDiagnosis -> AssembledData -> AssembledData
withDiagnoses diagnoses assembled =
    let
        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | diagnoses = EverySet.fromList diagnoses } }


{-| An encounter carrying the given diagnoses, and a Medication Distribution
measurement listing what was handed over and what was marked as not given.
-}
assembledWith : List PrenatalDiagnosis -> List MedicationDistributionSign -> List MedicationNonAdministrationSign -> AssembledData
assembledWith diagnoses distributionSigns nonAdministrationSigns =
    testAssembled
        { emptyPrenatalMeasurements
            | medicationDistribution =
                TestFixtures.wrapMeasurement currentDate
                    { distributionSigns = EverySet.fromList distributionSigns
                    , nonAdministrationSigns = EverySet.fromList nonAdministrationSigns
                    , recommendedTreatmentSigns = Nothing
                    , avoidingGuidanceReason = Nothing
                    , reinforceTreatmentSigns = Nothing
                    }
        }
        |> withDiagnoses diagnoses


{-| The same encounter, but the nurse never opened Medication Distribution.
-}
assembledWithoutMedicationDistribution : List PrenatalDiagnosis -> AssembledData
assembledWithoutMedicationDistribution diagnoses =
    testAssembled emptyPrenatalMeasurements
        |> withDiagnoses diagnoses


completedMedicationDistribution : AssembledData -> Bool
completedMedicationDistribution assembled =
    nextStepsTaskCompleted currentDate assembled NextStepsMedicationDistribution



-- Medication Distribution is complete when every medication the encounter
-- requires was addressed. High risk of preeclampsia requires Aspirin, and an
-- HIV diagnosis requires both TDF3TC and Dolutegravir.


nextStepsMedicationDistributionCompletedTest : Test
nextStepsMedicationDistributionCompletedTest =
    describe "nextStepsTaskCompleted NextStepsMedicationDistribution"
        [ test "complete when no medication is required, even though the task was never opened" <|
            \_ ->
                assembledWithoutMedicationDistribution []
                    |> completedMedicationDistribution
                    |> Expect.equal True
        , test "incomplete when a medication became required after the task was saved with nothing to give" <|
            \_ ->
                assembledWith [ DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ] [ NoMedicationDistributionSignsRecurrentPhase ] []
                    |> completedMedicationDistribution
                    |> Expect.equal False
        , test "complete when the required medication was handed over" <|
            \_ ->
                assembledWith [ DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ] [ Aspirin ] []
                    |> completedMedicationDistribution
                    |> Expect.equal True
        , test "complete when the required medication was marked as not given, with a reason" <|
            \_ ->
                assembledWith [ DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ]
                    [ NoMedicationDistributionSignsRecurrentPhase ]
                    [ MedicationAspirin NonAdministrationLackOfStock ]
                    |> completedMedicationDistribution
                    |> Expect.equal True
        , test "incomplete while one of two required medications is unanswered" <|
            \_ ->
                assembledWith [ DiagnosisHIVRecurrentPhase ] [ TDF3TC ] []
                    |> completedMedicationDistribution
                    |> Expect.equal False
        , test "complete when both required medications were addressed" <|
            \_ ->
                assembledWith [ DiagnosisHIVRecurrentPhase ]
                    [ TDF3TC ]
                    [ MedicationDolutegravir NonAdministrationPatientDeclined ]
                    |> completedMedicationDistribution
                    |> Expect.equal True
        , test "incomplete when a medication is required and the task was never opened" <|
            \_ ->
                assembledWithoutMedicationDistribution [ DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ]
                    |> completedMedicationDistribution
                    |> Expect.equal False
        ]


{-| The nurse's answers to the Partner HIV follow up questions, which the lab
technician leaves pending when they enter the result.
-}
withPartnerHIVSigns : EverySet PrenatalHIVSign -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVSigns hivSigns measurements =
    getMeasurementValueFunc measurements.partnerHIVTest
        |> Maybe.map
            (\value ->
                { measurements
                    | partnerHIVTest =
                        TestFixtures.wrapMeasurement currentDate { value | hivSigns = Just hivSigns }
                }
            )
        |> Maybe.withDefault measurements


{-| An encounter where Diabetes was diagnosed at the recurrent phase, which is
what the health education task is asked about here.
-}
assembledWithDiabetes : PrenatalMeasurements -> AssembledData
assembledWithDiabetes measurements =
    testAssembled measurements
        |> withDiagnoses [ DiagnosisDiabetesRecurrentPhase ]


{-| An encounter where the lab technician entered a positive Partner HIV
result, which leaves its follow up questions for the nurse.
-}
measurementsWithPartnerHIVFollowUp : PrenatalMeasurements
measurementsWithPartnerHIVFollowUp =
    measurementsWith [ TestPartnerHIV ] (Just TestNegative) (Just TestPositive)


offeredNextStepsTasks : AssembledData -> List NextStepsTask
offeredNextStepsTasks assembled =
    resolveNextStepsTasks currentDate False assembled



-- The questions the health education task asks are decided by the diagnoses,
-- and what was asked is not stored. So the task is offered only once the
-- activities that make diagnoses are completed, and a diagnosis can no longer
-- arrive after it was saved.
--
-- Diabetes refers the patient as well, so the Send to HC task is offered
-- throughout.


nextStepsHealthEducationExpectedTest : Test
nextStepsHealthEducationExpectedTest =
    describe "resolveNextStepsTasks NextStepsHealthEducation"
        [ test "offered when no activity that makes diagnoses is pending" <|
            \_ ->
                assembledWithDiabetes emptyPrenatalMeasurements
                    |> offeredNextStepsTasks
                    |> Expect.equal [ NextStepsHealthEducation, NextStepsSendToHC ]
        , test "not offered while a lab result follow up is unanswered" <|
            \_ ->
                assembledWithDiabetes measurementsWithPartnerHIVFollowUp
                    |> offeredNextStepsTasks
                    |> Expect.equal [ NextStepsSendToHC ]
        , test "offered once the follow up is answered" <|
            \_ ->
                withPartnerHIVSigns (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ]) measurementsWithPartnerHIVFollowUp
                    |> assembledWithDiabetes
                    |> offeredNextStepsTasks
                    |> Expect.equal [ NextStepsHealthEducation, NextStepsSendToHC ]
        , test "the medication task is offered while a follow up is unanswered" <|
            \_ ->
                testAssembled measurementsWithPartnerHIVFollowUp
                    |> withDiagnoses [ DiagnosisDiabetesRecurrentPhase, DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ]
                    |> offeredNextStepsTasks
                    |> Expect.equal [ NextStepsMedicationDistribution, NextStepsSendToHC ]
        ]


resolveLaboratoryResultFollowUpsTasksTest : Test
resolveLaboratoryResultFollowUpsTasksTest =
    describe "resolveLaboratoryResultFollowUpsTasks"
        [ test "no HIV task when no HIV follow up was scheduled" <|
            \_ ->
                measurementsWith [ TestPartnerHIV ] (Just TestNegative) (Just TestPositive)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskPartnerHIVTest ]
        , test "HIV task alone when no Partner HIV task is shown and HIV result is negative" <|
            \_ ->
                measurementsWith [ TestHIV ] (Just TestNegative) (Just TestPositive)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskHIVTest ]
        , test "HIV task alone when no Partner HIV task is shown and HIV result is positive" <|
            \_ ->
                measurementsWith [ TestHIV ] (Just TestPositive) (Just TestPositive)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskHIVTest ]
        , test "no HIV task when the Partner HIV task asks the same questions" <|
            \_ ->
                measurementsWith [ TestHIV, TestPartnerHIV ] (Just TestNegative) (Just TestPositive)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskPartnerHIVTest ]
        , test "HIV task next to the Partner HIV task when HIV result is positive" <|
            \_ ->
                measurementsWith [ TestHIV, TestPartnerHIV ] (Just TestPositive) (Just TestPositive)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskHIVTest, TaskPartnerHIVTest ]
        , test "no Partner HIV task when Partner HIV result is negative, so the HIV task is shown" <|
            \_ ->
                measurementsWith [ TestHIV, TestPartnerHIV ] (Just TestNegative) (Just TestNegative)
                    |> testAssembled
                    |> resolveLaboratoryResultFollowUpsTasks
                    |> Expect.equal [ TaskHIVTest ]
        ]


{-| An encounter where HIV was diagnosed at the recurrent phase from a
positive result, carrying the given follow up answers on the HIV test.
-}
assembledWithHIVDiagnosedBy : EverySet PrenatalHIVSign -> AssembledData
assembledWithHIVDiagnosedBy hivSigns =
    testAssembled
        { emptyPrenatalMeasurements
            | hivTest =
                TestFixtures.wrapMeasurement currentDate
                    (hivTestValueWith TestNoteRunConfirmedByLabTech (Just TestPositive) (Just hivSigns))
        }
        |> withDiagnoses [ DiagnosisHIVRecurrentPhase ]


requiredMedications : AssembledData -> List (List MedicationDistributionSign)
requiredMedications assembled =
    resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseRecurrent assembled
        |> List.map (\( _, medications, _ ) -> medications)



-- The HIV medication set. A positive result entered by the lab technician
-- leaves the follow up questions pending for the nurse; ARVs are required
-- only once she has answered that there is no HIV program at the health
-- center, and a program at the health center is a referral, not medication.


resolveRequiredMedicationsSetHIVTest : Test
resolveRequiredMedicationsSetHIVTest =
    describe "resolveRequiredMedicationsSet, the HIV set"
        [ test "no medication while the follow up answers are pending" <|
            \_ ->
                assembledWithHIVDiagnosedBy (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> requiredMedications
                    |> Expect.equal []
        , test "TDF3TC and Dolutegravir once the nurse answered that there is no HIV program at the health center" <|
            \_ ->
                assembledWithHIVDiagnosedBy (EverySet.singleton NoPrenatalHIVSign)
                    |> requiredMedications
                    |> Expect.equal [ [ TDF3TC, Dolutegravir ] ]
        , test "no medication once the nurse answered that there is an HIV program at the health center" <|
            \_ ->
                assembledWithHIVDiagnosedBy (EverySet.singleton HIVProgramHC)
                    |> requiredMedications
                    |> Expect.equal []
        ]


{-| Measurements of an encounter whose malaria test was sent to the lab, so a
result is expected at the recurrent phase. The nurse did not run the rapid
test and gave a reason; what became of the blood smear is the argument.
-}
measurementsWithMalariaTest : TestExecutionNote -> BloodSmearResult -> PrenatalMeasurements
measurementsWithMalariaTest executionNote bloodSmearResult =
    let
        malariaTestValue : MalariaTestValue
        malariaTestValue =
            { executionNote = executionNote
            , executionDate = Just currentDate
            , testPrerequisites = Just EverySet.empty
            , testResult = Nothing
            , bloodSmearResult = bloodSmearResult
            , bloodSmearOrdered = True
            }
    in
    { emptyPrenatalMeasurements
        | malariaTest = TestFixtures.wrapMeasurement currentDate malariaTestValue
    }


laboratoryResultTaskCompletedMalariaTest : Test
laboratoryResultTaskCompletedMalariaTest =
    let
        resolve executionNote bloodSmearResult =
            measurementsWithMalariaTest executionNote bloodSmearResult
                |> testAssembled
                |> (\assembled -> laboratoryResultTaskCompleted True assembled TaskMalariaTest)
    in
    describe "laboratoryResultTaskCompleted, on the malaria test"
        [ test "a blood smear ordered at the lab is not complete, it is awaited" <|
            \_ ->
                resolve TestNoteLackOfReagents BloodSmearPendingInput
                    |> Expect.equal False
        , test "a blood smear the lab technician declined is complete" <|
            \_ ->
                resolve TestNoteLackOfReagents BloodSmearNotTaken
                    |> Expect.equal True
        , test "a blood smear the lab technician read is complete" <|
            \_ ->
                resolve TestNoteRunConfirmedByLabTech BloodSmearNegative
                    |> Expect.equal True
        ]


{-| Hypertension diagnosed at an earlier visit, treated with Methyldopa 2x a
day. Today's BP is normal, so the recommendation is to keep that dose; the
nurse chose 3x a day and gave no reason.
-}
assembledWithContinuedHypertensionCare : AssembledData
assembledWithContinuedHypertensionCare =
    let
        medicationDistributionWith : List RecommendedTreatmentSign -> PrenatalMedicationDistributionValue
        medicationDistributionWith treatment =
            { distributionSigns = EverySet.empty
            , nonAdministrationSigns = EverySet.empty
            , recommendedTreatmentSigns = Just (EverySet.fromList treatment)
            , avoidingGuidanceReason = Nothing
            , reinforceTreatmentSigns = Nothing
            }

        vitals : VitalsValue
        vitals =
            { sys = Just 120
            , dia = Just 80
            , heartRate = Nothing
            , respiratoryRate = Nothing
            , bodyTemperature = Nothing
            , sysRepeated = Nothing
            , diaRepeated = Nothing
            }

        previousEncounter =
            { startDate = Date.add Date.Weeks -4 currentDate
            , diagnoses = EverySet.singleton DiagnosisChronicHypertensionImmediate
            , pastDiagnoses = EverySet.empty
            , measurements =
                { emptyPrenatalMeasurements
                    | medicationDistribution = TestFixtures.wrapMeasurement currentDate (medicationDistributionWith [ TreatmentMethyldopa2 ])
                }
            }

        assembled =
            testAssembled
                { emptyPrenatalMeasurements
                    | medicationDistribution = TestFixtures.wrapMeasurement currentDate (medicationDistributionWith [ TreatmentMethyldopa3 ])
                    , vitals = TestFixtures.wrapMeasurement currentDate vitals
                }
    in
    { assembled | nursePreviousEncountersData = [ previousEncounter ] }


resolveMedicationDistributionContinuedHypertensionTest : Test
resolveMedicationDistributionContinuedHypertensionTest =
    let
        tasksAtPhase phase =
            let
                form =
                    getMeasurementValueFunc assembledWithContinuedHypertensionCare.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultRecurrentPhase emptyMedicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks English
                        currentDate
                        phase
                        assembledWithContinuedHypertensionCare
                        (\_ _ -> ())
                        (\_ _ _ -> ())
                        (\_ _ -> ())
                        (\_ -> ())
                        form
            in
            ( completed, total )
    in
    describe "resolveMedicationDistributionInputsAndTasks, hypertension diagnosed at an earlier visit"
        [ test "initial phase asks for the treatment, and why it is not the recommended one" <|
            \_ ->
                tasksAtPhase PrenatalEncounterPhaseInitial
                    |> Expect.equal ( 1, 2 )
        , test "recurrent phase does not ask again" <|
            \_ ->
                tasksAtPhase PrenatalEncounterPhaseRecurrent
                    |> Expect.equal ( 0, 0 )
        ]


all : Test
all =
    describe "Pages.Prenatal.RecurrentActivity.Utils"
        [ laboratoryResultTaskCompletedMalariaTest
        , nextStepsHealthEducationExpectedTest
        , nextStepsMedicationDistributionCompletedTest
        , resolveLaboratoryResultFollowUpsTasksTest
        , resolveMedicationDistributionContinuedHypertensionTest
        , resolveRequiredMedicationsSetHIVTest
        ]
