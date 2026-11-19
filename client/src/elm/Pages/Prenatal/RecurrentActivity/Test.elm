module Pages.Prenatal.RecurrentActivity.Test exposing (all)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (PersonId, PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , BloodSmearResult(..)
        , HIVTestValue
        , HemoglobinTestValue
        , HepatitisBTestValue
        , LaboratoryTest(..)
        , LabsResultsReviewState(..)
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
        , TestPrerequisite(..)
        , TestResult(..)
        , VitalsValue
        , emptyPrenatalMeasurements
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb, emptyModelIndexedDb)
import Backend.PrenatalActivity.Model as PrenatalActivity
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (HIVResultForm, HIVTestUniversalForm, LaboratoryTask(..), PartnerHIVResultForm, PartnerHIVTestForm, emptyHIVResultForm, emptyPartnerHIVResultForm, emptyPartnerHIVTestForm)
import Measurement.Utils
    exposing
        ( emptyContentAndTasksForPerformedLaboratoryUniversalTestConfig
        , emptyContentAndTasksLaboratoryResultConfig
        , emptyContentAndTasksLaboratoryUniversalTestInitialConfig
        , hivResultFollowUpsFormAndTasks
        , hivResultFormWithDefault
        , knownAsPositiveUpdateHIVTest
        , knownAsPositiveUpdatePartnerHIVTest
        , partnerHIVResultFollowUpsFormAndTasks
        , partnerHIVResultFormAndTasks
        , partnerHIVResultFormWithDefault
        , partnerHIVTestFormWithDefault
        , viewPartnerHIVTestForm
        )
import Pages.Prenatal.Activity.Model as Initial
import Pages.Prenatal.Activity.Update as ActivityUpdate
import Pages.Prenatal.Activity.Utils exposing (generatePrenatalDiagnosesForNurse)
import Pages.Prenatal.Model exposing (AssembledData, PrenatalEncounterPhase(..), emptyMedicationDistributionForm)
import Pages.Prenatal.RecurrentActivity.Model as Recurrent
import Pages.Prenatal.RecurrentActivity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.RecurrentActivity.Update as RecurrentUpdate
import Pages.Prenatal.RecurrentActivity.Utils
    exposing
        ( activityCompleted
        , concludedLabsResultsValue
        , laboratoryResultFollowUpsTaskCompleted
        , laboratoryResultNotPerformed
        , laboratoryResultTaskCompleted
        , labsResultsValueOnResultSaved
        , nextStepsTaskCompleted
        , resolveLaboratoryResultFollowUpsTasks
        , resolveLaboratoryResultTasks
        , resolveNextStepsTasks
        , withCompletedTestsFromMeasurements
        )
import Pages.Prenatal.Utils exposing (medicationDistributionFormWithDefaultRecurrentPhase, resolveMedicationDistributionInputsAndTasks, resolvePartnerHIVTestResult, resolveRequiredMedicationsSet)
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
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


{-| HIV, partner HIV and Hepatitis B tests first sent to the lab, then corrected
to known as positive. Each record keeps the lab prerequisites of the run it
replaced.
-}
laboratoryResultTaskCompletedKnownAsPositiveTest : Test
laboratoryResultTaskCompletedKnownAsPositiveTest =
    let
        sentToLab =
            Just (EverySet.singleton NoTestPrerequisites)

        hivTestValue : HIVTestValue
        hivTestValue =
            { executionNote = TestNoteKnownAsPositive
            , executionDate = Nothing
            , testPrerequisites = sentToLab
            , testResult = Nothing
            , hivSigns = Nothing
            }

        partnerHIVTestValue : PartnerHIVTestValue
        partnerHIVTestValue =
            hivTestValue

        hepatitisBTestValue : HepatitisBTestValue PrenatalEncounterId
        hepatitisBTestValue =
            { executionNote = TestNoteKnownAsPositive
            , executionDate = Nothing
            , testPrerequisites = sentToLab
            , testResult = Nothing
            , originatingEncounter = Nothing
            }

        measurements =
            { emptyPrenatalMeasurements
                | hivTest = TestFixtures.wrapMeasurement currentDate hivTestValue
                , partnerHIVTest = TestFixtures.wrapMeasurement currentDate partnerHIVTestValue
                , hepatitisBTest = TestFixtures.wrapMeasurement currentDate hepatitisBTestValue
            }

        completed isLabTech task =
            laboratoryResultTaskCompleted isLabTech (testAssembled measurements) task

        caseFor ( label, task ) =
            [ test (label ++ " corrected from sent to the lab owes the lab technician no result") <|
                \_ -> completed True task |> Expect.equal True
            , test (label ++ " corrected from sent to the lab owes the nurse no result") <|
                \_ -> completed False task |> Expect.equal True
            ]
    in
    describe "laboratoryResultTaskCompleted, on a test known as positive"
        (List.concatMap caseFor
            [ ( "an HIV test", TaskHIVTest )
            , ( "a partner HIV test", TaskPartnerHIVTest )
            , ( "a Hepatitis B test", TaskHepatitisBTest )
            ]
        )


{-| Tests sent to the lab, as the nurse's Lab Results reads them after the lab
technician answered.
-}
laboratoryResultNotPerformedTest : Test
laboratoryResultNotPerformedTest =
    let
        sentToLab =
            Just (EverySet.singleton NoTestPrerequisites)

        hemoglobinWith executionNote hemoglobinCount =
            let
                value : HemoglobinTestValue
                value =
                    { executionNote = executionNote
                    , executionDate = Just currentDate
                    , testPrerequisites = sentToLab
                    , hemoglobinCount = hemoglobinCount
                    }
            in
            { emptyPrenatalMeasurements | hemoglobinTest = TestFixtures.wrapMeasurement currentDate value }

        partnerHIVNotPresent =
            let
                value : PartnerHIVTestValue
                value =
                    { executionNote = TestNoteNotPresent
                    , executionDate = Just currentDate
                    , testPrerequisites = sentToLab
                    , testResult = Nothing
                    , hivSigns = Nothing
                    }
            in
            { emptyPrenatalMeasurements | partnerHIVTest = TestFixtures.wrapMeasurement currentDate value }

        -- Saving any HIV answer puts the test on the follow up list, a
        -- decline included.
        hivDeclinedOnFollowUpList =
            let
                declined =
                    hivTestValueWith TestNoteLackOfReagents Nothing Nothing

                measurements =
                    measurementsWith [ TestHIV ] Nothing Nothing
            in
            { measurements | hivTest = TestFixtures.wrapMeasurement currentDate { declined | testPrerequisites = sentToLab } }

        hivDeclinedHoldingResult =
            let
                declined =
                    hivTestValueWith TestNoteLackOfReagents (Just TestPositive) Nothing

                measurements =
                    measurementsWith [ TestHIV ] Nothing Nothing
            in
            { measurements | hivTest = TestFixtures.wrapMeasurement currentDate { declined | testPrerequisites = sentToLab } }

        smearOrderedWithRapidTestResult =
            let
                measurements =
                    measurementsWithMalariaTest TestNoteRunToday BloodSmearPendingInput

                withRapidTestResult value =
                    { value | testResult = Just TestNegative }
            in
            { measurements
                | malariaTest =
                    getMeasurementValueFunc measurements.malariaTest
                        |> Maybe.andThen (withRapidTestResult >> TestFixtures.wrapMeasurement currentDate)
            }

        resolve task measurements =
            laboratoryResultNotPerformed (testAssembled measurements) task

        declinedFor reason holdsResult =
            Just { executionDate = Just currentDate, holdsResult = holdsResult, reason = reason }
    in
    describe "a test the lab technician declined"
        [ test "gives their reason" <|
            \_ ->
                resolve TaskHemoglobinTest (hemoglobinWith TestNoteLackOfReagents Nothing)
                    |> Expect.equal (declinedFor TestNoteLackOfReagents False)
        , test "gives their reason, and says when a result is stored anyway" <|
            \_ ->
                resolve TaskHemoglobinTest (hemoglobinWith TestNoteLackOfReagents (Just 11))
                    |> Expect.equal (declinedFor TestNoteLackOfReagents True)
        , test "a test the lab technician ran gives no reason" <|
            \_ ->
                resolve TaskHemoglobinTest (hemoglobinWith TestNoteRunConfirmedByLabTech Nothing)
                    |> Expect.equal Nothing
        , test "a test still waiting for the lab gives no reason" <|
            \_ ->
                resolve TaskHemoglobinTest (hemoglobinWith TestNoteRunToday Nothing)
                    |> Expect.equal Nothing
        , test "partner not present is a reason" <|
            \_ ->
                resolve TaskPartnerHIVTest partnerHIVNotPresent
                    |> Expect.equal (declinedFor TestNoteNotPresent False)
        , test "partner not present completes the nurse's task" <|
            \_ ->
                laboratoryResultTaskCompleted False (testAssembled partnerHIVNotPresent) TaskPartnerHIVTest
                    |> Expect.equal True
        , test "an HIV test on the follow up list is shown to the nurse" <|
            \_ ->
                resolveLaboratoryResultTasks False (testAssembled hivDeclinedOnFollowUpList)
                    |> List.member TaskHIVTest
                    |> Expect.equal True
        , test "an HIV test has no follow up questions" <|
            \_ ->
                resolveLaboratoryResultFollowUpsTasks (testAssembled hivDeclinedOnFollowUpList)
                    |> Expect.equal []
        , test "an HIV test holding a result is not shown at Lab Results" <|
            \_ ->
                resolveLaboratoryResultTasks False (testAssembled hivDeclinedHoldingResult)
                    |> List.member TaskHIVTest
                    |> Expect.equal False
        , test "an HIV test holding a result keeps its follow up questions" <|
            \_ ->
                resolveLaboratoryResultFollowUpsTasks (testAssembled hivDeclinedHoldingResult)
                    |> Expect.equal [ TaskHIVTest ]
        , test "a blood smear gives their reason" <|
            \_ ->
                resolve TaskMalariaTest (measurementsWithMalariaTest TestNoteLackOfReagents BloodSmearNotTaken)
                    |> Expect.equal (declinedFor TestNoteLackOfReagents False)
        , test "a blood smear still waiting for the lab gives no reason, though the rapid test has one" <|
            \_ ->
                resolve TaskMalariaTest (measurementsWithMalariaTest TestNoteLackOfReagents BloodSmearPendingInput)
                    |> Expect.equal Nothing
        , test "a blood smear still waiting for the lab is not complete, though a rapid test result is stored" <|
            \_ ->
                laboratoryResultTaskCompleted False (testAssembled smearOrderedWithRapidTestResult) TaskMalariaTest
                    |> Expect.equal False
        , test "a blood smear the lab technician read gives no reason" <|
            \_ ->
                resolve TaskMalariaTest (measurementsWithMalariaTest TestNoteRunConfirmedByLabTech BloodSmearNegative)
                    |> Expect.equal Nothing
        , test "a blood smear the nurse read gives no reason, though the rapid test has one" <|
            \_ ->
                resolve TaskMalariaTest (measurementsWithMalariaTest TestNoteLackOfReagents BloodSmearNegative)
                    |> Expect.equal Nothing
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



-- The labs entry is written from the device's copy. That copy can be behind
-- the device's test measurements, so completed tests are read from the
-- measurements too, and never dropped.


{-| Measurements holding an HIV test and a Hemoglobin test with the given
execution notes and results.
-}
measurementsWithTests : ( TestExecutionNote, Maybe TestResult ) -> ( TestExecutionNote, Maybe Float ) -> PrenatalMeasurements
measurementsWithTests ( hivNote, hivResult ) ( hemoglobinNote, hemoglobinCount ) =
    let
        hemoglobinValue : HemoglobinTestValue
        hemoglobinValue =
            { executionNote = hemoglobinNote
            , executionDate = Just currentDate
            , testPrerequisites = Nothing
            , hemoglobinCount = hemoglobinCount
            }
    in
    { emptyPrenatalMeasurements
        | hivTest = TestFixtures.wrapMeasurement currentDate (hivTestValueWith hivNote hivResult Nothing)
        , hemoglobinTest = TestFixtures.wrapMeasurement currentDate hemoglobinValue
    }


withCompletedTestsFromMeasurementsTest : Test
withCompletedTestsFromMeasurementsTest =
    let
        withMeasurements measurements entry =
            withCompletedTestsFromMeasurements (testAssembled measurements) entry

        completedTests measurements entry =
            withMeasurements measurements entry
                |> .completedTests
    in
    describe "withCompletedTestsFromMeasurements"
        [ test "a test whose measurement holds a result is completed" <|
            \_ ->
                completedTests
                    (measurementsWithTests ( TestNoteRunToday, Just TestPositive ) ( TestNoteRunToday, Nothing ))
                    (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin ] [])
                    |> Expect.equal (EverySet.fromList [ TestHIV ])
        , test "a test the lab did not run is completed" <|
            \_ ->
                completedTests
                    (measurementsWithTests ( TestNoteLackOfReagents, Nothing ) ( TestNoteRunToday, Just 11 ))
                    (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin ] [])
                    |> Expect.equal (EverySet.fromList [ TestHIV, TestHemoglobin ])
        , test "a test completed on the entry stays completed without a result on its measurement" <|
            \_ ->
                completedTests
                    (measurementsWithTests ( TestNoteRunToday, Nothing ) ( TestNoteRunToday, Nothing ))
                    (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin ] [ TestHIV ])
                    |> Expect.equal (EverySet.fromList [ TestHIV ])
        , test "a test run with a result is performed and completed, though the entry does not list it" <|
            \_ ->
                withMeasurements
                    (measurementsWithTests ( TestNoteRunToday, Just TestNegative ) ( TestNoteRunToday, Just 11 ))
                    (TestFixtures.labsEntry currentDate [ TestHemoglobin ] [])
                    |> (\value -> ( value.performedTests, value.completedTests ))
                    |> Expect.equal
                        ( EverySet.fromList [ TestHIV, TestHemoglobin ]
                        , EverySet.fromList [ TestHIV, TestHemoglobin ]
                        )
        , test "a test not run is not added when the entry does not list it" <|
            \_ ->
                withMeasurements
                    (measurementsWithTests ( TestNoteLackOfReagents, Nothing ) ( TestNoteRunToday, Just 11 ))
                    (TestFixtures.labsEntry currentDate [ TestHemoglobin ] [])
                    |> (\value -> ( value.performedTests, value.completedTests ))
                    |> Expect.equal
                        ( EverySet.fromList [ TestHemoglobin ]
                        , EverySet.fromList [ TestHemoglobin ]
                        )
        , test "a vitals recheck is completed once the repeated reading is taken" <|
            \_ ->
                let
                    vitals : VitalsValue
                    vitals =
                        { sys = Just 150
                        , dia = Just 95
                        , heartRate = Nothing
                        , respiratoryRate = Nothing
                        , bodyTemperature = Nothing
                        , sysRepeated = Just 145
                        , diaRepeated = Just 92
                        }
                in
                completedTests
                    { emptyPrenatalMeasurements | vitals = TestFixtures.wrapMeasurement currentDate vitals }
                    (TestFixtures.labsEntry currentDate [ TestVitalsRecheck ] [])
                    |> Expect.equal (EverySet.fromList [ TestVitalsRecheck ])
        ]


concludedLabsResultsValueTest : Test
concludedLabsResultsValueTest =
    let
        allResults =
            measurementsWithTests ( TestNoteRunToday, Just TestNegative ) ( TestNoteRunToday, Just 11 )

        concluded measurements entry =
            concludedOn currentDate measurements entry

        concludedOn dateMeasured measurements entry =
            TestFixtures.wrapMeasurement dateMeasured entry
                |> Maybe.map (Tuple.second >> concludedLabsResultsValue currentDate (testAssembled measurements))
    in
    describe "concludedLabsResultsValue"
        [ test "an entry behind its measurements is closed with every test completed and reviewed" <|
            \_ ->
                concluded allResults (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin ] [])
                    |> Expect.equal
                        (Just
                            { performedTests = EverySet.fromList [ TestHIV, TestHemoglobin ]
                            , completedTests = EverySet.fromList [ TestHIV, TestHemoglobin ]
                            , resolutionDate = currentDate
                            , patientNotified = False
                            , reviewState = Just LabsResultsReviewCompleted
                            , testsWithFollowUp = Nothing
                            }
                        )
        , test "an entry with a test still waiting keeps its review state" <|
            \_ ->
                concluded
                    (measurementsWithTests ( TestNoteRunToday, Just TestNegative ) ( TestNoteRunToday, Nothing ))
                    (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin ] [])
                    |> Maybe.map (\value -> ( value.completedTests, value.reviewState, value.resolutionDate ))
                    |> Expect.equal (Just ( EverySet.fromList [ TestHIV ], Nothing, currentDate ))
        , test "a vitals recheck from another day stays on the entry, and does not hold back the review" <|
            \_ ->
                concludedOn (Date.add Date.Days -2 currentDate) allResults (TestFixtures.labsEntry currentDate [ TestHIV, TestHemoglobin, TestVitalsRecheck ] [])
                    |> Maybe.map (\value -> ( value.performedTests, value.completedTests, value.reviewState ))
                    |> Expect.equal
                        (Just
                            ( EverySet.fromList [ TestHIV, TestHemoglobin, TestVitalsRecheck ]
                            , EverySet.fromList [ TestHIV, TestHemoglobin ]
                            , Just LabsResultsReviewCompleted
                            )
                        )
        ]



-- HIV TESTS SENT TO THE LAB, AND THE QUESTIONS THAT FOLLOW THEIR RESULT
--
-- At the visit the nurse may send the partner's HIV test to the lab: the
-- partner is not known to be positive, the test is run, and the result is not
-- immediate. The result comes later - entered by the nurse in Lab Results, or
-- through a later visit's lab history, or by a lab technician. A Positive
-- partner result asks "Is the partner taking ARVs?" (and, on Yes, whether the
-- viral load is suppressed). Those answers decide the Discordant partnership
-- diagnosis when the woman is HIV negative, so an answer nobody gave must
-- never be stored, and a result must not be saveable while they are open.
--
-- The scenarios below are driven through the app's own update functions:
-- the visit's Laboratory activity (Pages.Prenatal.Activity.Update), Lab
-- Results and lab history (Pages.Prenatal.RecurrentActivity.Update). The
-- form the nurse sees is what the page draws: the form in the model laid over
-- the stored value. Its "saveable" is the page's own rule - the Save button
-- is enabled when every task of the form is completed.


personId : PersonId
personId =
    toEntityUuid "dummy-person"


{-| The visit where the tests were taken. It carries the id `testAssembled`
gives its encounter, so the stored measurements are the ones diagnosed.
-}
visitEncounterId : PrenatalEncounterId
visitEncounterId =
    toEntityUuid "dummy-encounter"


{-| A later visit, whose lab history the nurse uses to enter the result of the
test taken at `visitEncounterId`.
-}
laterEncounterId : PrenatalEncounterId
laterEncounterId =
    toEntityUuid "later-encounter"


dbWith : PrenatalMeasurements -> ModelIndexedDb
dbWith measurements =
    { emptyModelIndexedDb
        | prenatalMeasurements = Dict.singleton visitEncounterId (RemoteData.Success measurements)
    }


withPartnerHIVTestValue : PartnerHIVTestValue -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVTestValue value measurements =
    { measurements | partnerHIVTest = TestFixtures.wrapMeasurement currentDate value }


withHIVTestValue : HIVTestValue -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestValue value measurements =
    { measurements | hivTest = TestFixtures.wrapMeasurement currentDate value }


{-| The woman's own HIV test: run at the visit with an immediate result,
Negative, and the partner question answered "not known to be positive". So
any Discordant partnership diagnosis comes from the partner's own test.
-}
womanHIVNegative : HIVTestValue
womanHIVNegative =
    { executionNote = TestNoteRunToday
    , executionDate = Just currentDate
    , testPrerequisites = Just (EverySet.singleton PrerequisiteImmediateResult)
    , testResult = Just TestNegative
    , hivSigns = Just (EverySet.singleton NoPrenatalHIVSign)
    }


{-| A partner HIV test as it was stored when sent to the lab before this
change: the follow up question was never asked, yet the record says No.
-}
legacyPartnerTestSentToLab : PartnerHIVTestValue
legacyPartnerTestSentToLab =
    { executionNote = TestNoteRunToday
    , executionDate = Just currentDate
    , testPrerequisites = Just (EverySet.singleton NoTestPrerequisites)
    , testResult = Nothing
    , hivSigns = Just (EverySet.singleton NoPrenatalHIVSign)
    }



-- The taps the screens make, as the forms' own update functions.


{-| "Was the test performed today?" answered on the visit's Partner HIV form.
-}
tapPartnerTestPerformed : Bool -> PartnerHIVTestForm -> PartnerHIVTestForm
tapPartnerTestPerformed value form =
    { form
        | testPerformed = Just value
        , testPerformedDirty = True
        , immediateResult = Nothing
        , executionNote =
            if value then
                Just TestNoteRunToday

            else
                Nothing
        , executionNoteDirty = True
        , executionDate =
            if value then
                Just currentDate

            else
                Nothing
        , executionDateDirty = True
        , testResult = Nothing
        , testResultDirty = True
    }


{-| "Will the result be available immediately?" answered on the visit's
Partner HIV form. No sends the test to the lab.
-}
tapPartnerImmediateResult : Bool -> PartnerHIVTestForm -> PartnerHIVTestForm
tapPartnerImmediateResult value form =
    { form | immediateResult = Just value, testResult = Nothing, testResultDirty = True }


{-| "Was the test performed today?" answered on the visit's HIV form.
-}
tapHIVTestPerformed : Bool -> HIVTestUniversalForm -> HIVTestUniversalForm
tapHIVTestPerformed value form =
    { form
        | testPerformed = Just value
        , testPerformedDirty = True
        , immediateResult = Nothing
        , executionNote =
            if value then
                Just TestNoteRunToday

            else
                Nothing
        , executionNoteDirty = True
        , executionDate =
            if value then
                Just currentDate

            else
                Nothing
        , executionDateDirty = True
        , testResult = Nothing
        , testResultDirty = True
        , hivProgramHC = Nothing
        , hivProgramHCDirty = True
        , partnerHIVPositive = Nothing
        , partnerHIVPositiveDirty = True
        , partnerTakingARV = Nothing
        , partnerTakingARVDirty = True
        , partnerSurpressedViralLoad = Nothing
        , partnerSurpressedViralLoadDirty = True
    }


tapHIVImmediateResult : Bool -> HIVTestUniversalForm -> HIVTestUniversalForm
tapHIVImmediateResult value form =
    { form | immediateResult = Just value, testResult = Nothing, testResultDirty = True }


{-| "Is the partner taking ARVs?", on any of the forms that ask it.
-}
tapPartnerTakingARV :
    Bool
    -> { f | partnerTakingARV : Maybe Bool, partnerTakingARVDirty : Bool, partnerSurpressedViralLoad : Maybe Bool, partnerSurpressedViralLoadDirty : Bool }
    -> { f | partnerTakingARV : Maybe Bool, partnerTakingARVDirty : Bool, partnerSurpressedViralLoad : Maybe Bool, partnerSurpressedViralLoadDirty : Bool }
tapPartnerTakingARV value form =
    { form
        | partnerTakingARV = Just value
        , partnerTakingARVDirty = True
        , partnerSurpressedViralLoad = Nothing
        , partnerSurpressedViralLoadDirty = True
    }


{-| "Has the partner reached a suppressed viral load?", asked after Yes to ARVs.
-}
tapPartnerSurpressedViralLoad :
    Bool
    -> { f | partnerSurpressedViralLoad : Maybe Bool, partnerSurpressedViralLoadDirty : Bool }
    -> { f | partnerSurpressedViralLoad : Maybe Bool, partnerSurpressedViralLoadDirty : Bool }
tapPartnerSurpressedViralLoad value form =
    { form | partnerSurpressedViralLoad = Just value, partnerSurpressedViralLoadDirty = True }


{-| The lab technician confirms they ran the partner's test.
-}
tapPartnerRunConfirmedByLabTech : Bool -> PartnerHIVResultForm -> PartnerHIVResultForm
tapPartnerRunConfirmedByLabTech value form =
    { form
        | runConfirmedByLabTech = Just value
        , executionNote =
            if value then
                Just TestNoteRunConfirmedByLabTech

            else
                Nothing
        , executionNoteDirty = True
        , testResult = Nothing
        , testResultDirty = True
    }


{-| The lab technician confirms they ran the woman's test.
-}
tapHIVRunConfirmedByLabTech : Bool -> HIVResultForm -> HIVResultForm
tapHIVRunConfirmedByLabTech value form =
    { form
        | runConfirmedByLabTech = Just value
        , executionNote =
            if value then
                Just TestNoteRunConfirmedByLabTech

            else
                Nothing
        , executionNoteDirty = True
        , testResult = Nothing
        , testResultDirty = True
        , hivProgramHC = Nothing
        , hivProgramHCDirty = True
        , partnerHIVPositive = Nothing
        , partnerHIVPositiveDirty = True
        , partnerTakingARV = Nothing
        , partnerTakingARVDirty = True
        , partnerSurpressedViralLoad = Nothing
        , partnerSurpressedViralLoadDirty = True
    }



-- Running the pages.


{-| The value a Save on any of the pages sends to the database.
-}
savedPartnerHIVTest : List App.Model.Msg -> Maybe PartnerHIVTestValue
savedPartnerHIVTest =
    List.filterMap
        (\msg ->
            case msg of
                App.Model.MsgIndexedDb (Backend.Model.MsgPrenatalEncounter _ (Backend.PrenatalEncounter.Model.SavePartnerHIVTest _ _ value)) ->
                    Just value

                _ ->
                    Nothing
        )
        >> List.head


savedHIVTest : List App.Model.Msg -> Maybe HIVTestValue
savedHIVTest =
    List.filterMap
        (\msg ->
            case msg of
                App.Model.MsgIndexedDb (Backend.Model.MsgPrenatalEncounter _ (Backend.PrenatalEncounter.Model.SaveHIVTest _ _ value)) ->
                    Just value

                _ ->
                    Nothing
        )
        >> List.head


{-| The visit's Laboratory activity, on a page holding the given measurements,
after the nurse's taps.
-}
visitModelAfter : PrenatalMeasurements -> List Initial.Msg -> Initial.Model
visitModelAfter measurements taps =
    List.foldl
        (\msg model ->
            ActivityUpdate.update currentDate SiteRwanda visitEncounterId (dbWith measurements) msg model
                |> (\( updated, _, _ ) -> updated)
        )
        Initial.emptyModel
        taps


{-| The taps, then the given Save. Returns what is sent to the database.
-}
visitSaveMsgs : PrenatalMeasurements -> List Initial.Msg -> Initial.Msg -> List App.Model.Msg
visitSaveMsgs measurements taps saveMsg =
    ActivityUpdate.update currentDate
        SiteRwanda
        visitEncounterId
        (dbWith measurements)
        saveMsg
        (visitModelAfter measurements taps)
        |> (\( _, _, appMsgs ) -> appMsgs)


{-| The nurse makes the given taps, then saves the visit's Partner HIV task.
-}
partnerTestSavedAtVisit : PrenatalMeasurements -> List Initial.Msg -> Maybe PartnerHIVTestValue
partnerTestSavedAtVisit measurements taps =
    Initial.SavePartnerHIVTest personId measurements.partnerHIVTest Nothing
        |> visitSaveMsgs measurements taps
        |> savedPartnerHIVTest


{-| The same, saving the woman's own HIV task.
-}
hivTestSavedAtVisit : PrenatalMeasurements -> List Initial.Msg -> Maybe HIVTestValue
hivTestSavedAtVisit measurements taps =
    Initial.SaveHIVTest personId measurements.hivTest Nothing
        |> visitSaveMsgs measurements taps
        |> savedHIVTest


{-| The taps of a nurse sending the partner's test to the lab at the visit:
partner not known to be positive, test performed, result not immediate.
-}
sendPartnerTestToLab : List Initial.Msg
sendPartnerTestToLab =
    [ Initial.SetPartnerHIVTestFormBoolInput knownAsPositiveUpdatePartnerHIVTest False
    , Initial.SetPartnerHIVTestFormBoolInput tapPartnerTestPerformed True
    , Initial.SetPartnerHIVTestFormBoolInput tapPartnerImmediateResult False
    ]


{-| What the visit stores for a partner test sent to the lab.
-}
partnerTestSentToLab : Maybe PartnerHIVTestValue
partnerTestSentToLab =
    partnerTestSavedAtVisit emptyPrenatalMeasurements sendPartnerTestToLab


{-| What the visit stores for the woman's own test sent to the lab.
-}
hivTestSentToLab : Maybe HIVTestValue
hivTestSentToLab =
    hivTestSavedAtVisit emptyPrenatalMeasurements
        [ Initial.SetHIVTestFormBoolInput knownAsPositiveUpdateHIVTest False
        , Initial.SetHIVTestFormBoolInput tapHIVTestPerformed True
        , Initial.SetHIVTestFormBoolInput tapHIVImmediateResult False
        ]


{-| Where the result is entered: Lab Results of the visit itself, or the lab
history of a later visit. Both are filled by a nurse here.
-}
type ResultPage
    = LabHistory
    | LabResults


{-| The model after the given taps on the result page, which holds the stored
partner test, as the form the page draws.
-}
partnerResultFormAfter : ResultPage -> Bool -> PartnerHIVTestValue -> List Recurrent.Msg -> PartnerHIVResultForm
partnerResultFormAfter page isLabTech stored taps =
    let
        data =
            labResultsDataAfter page isLabTech (withPartnerHIVTestValue stored emptyPrenatalMeasurements) taps
    in
    partnerHIVResultFormWithDefault data.partnerHIVTestForm (Just stored)


labResultsDataAfter : ResultPage -> Bool -> PrenatalMeasurements -> List Recurrent.Msg -> Recurrent.LabResultsData
labResultsDataAfter page isLabTech measurements taps =
    let
        db =
            dbWith measurements
    in
    case page of
        LabHistory ->
            List.foldl
                (\msg data ->
                    RecurrentUpdate.updateLabsHistory laterEncounterId visitEncounterId isLabTech db msg data
                        |> (\( updated, _, _ ) -> updated)
                )
                Recurrent.emptyLabResultsData
                taps

        LabResults ->
            List.foldl
                (\msg model ->
                    RecurrentUpdate.update visitEncounterId isLabTech db msg model
                        |> (\( updated, _, _ ) -> updated)
                )
                Recurrent.emptyModel
                taps
                |> .labResultsData


{-| The taps on the result page, then Save. Returns what is sent to the
database.
-}
partnerResultSaved : ResultPage -> Bool -> PartnerHIVTestValue -> List Recurrent.Msg -> Maybe PartnerHIVTestValue
partnerResultSaved page isLabTech stored taps =
    let
        measurements =
            withPartnerHIVTestValue stored emptyPrenatalMeasurements

        db =
            dbWith measurements

        saveMsg =
            Recurrent.SavePartnerHIVResult personId measurements.partnerHIVTest Nothing

        data =
            labResultsDataAfter page isLabTech measurements taps
    in
    case page of
        LabHistory ->
            RecurrentUpdate.updateLabsHistory laterEncounterId visitEncounterId isLabTech db saveMsg data
                |> (\( _, _, appMsgs ) -> savedPartnerHIVTest appMsgs)

        LabResults ->
            let
                model =
                    Recurrent.emptyModel
            in
            RecurrentUpdate.update visitEncounterId isLabTech db saveMsg { model | labResultsData = data }
                |> (\( _, _, appMsgs ) -> savedPartnerHIVTest appMsgs)


hivResultSaved : Bool -> HIVTestValue -> List Recurrent.Msg -> Maybe HIVTestValue
hivResultSaved isLabTech stored taps =
    let
        measurements =
            withHIVTestValue stored emptyPrenatalMeasurements

        model =
            Recurrent.emptyModel

        data =
            labResultsDataAfter LabResults isLabTech measurements taps
    in
    RecurrentUpdate.update visitEncounterId
        isLabTech
        (dbWith measurements)
        (Recurrent.SaveHIVResult personId measurements.hivTest Nothing)
        { model | labResultsData = data }
        |> (\( _, _, appMsgs ) -> savedHIVTest appMsgs)


{-| The ARVs answer the nurse sees on the partner's result form, and whether
Save is enabled.
-}
partnerResultShown : PartnerHIVResultForm -> ( Maybe Bool, Bool )
partnerResultShown form =
    let
        ( _, completed, total ) =
            partnerHIVResultFormAndTasks English
                False
                (emptyContentAndTasksLaboratoryResultConfig Recurrent.NoOp)
                Recurrent.SetPartnerHIVTestResult
                Recurrent.SetPartnerHIVTestFormBoolInput
                form
    in
    ( form.partnerTakingARV, completed == total )


{-| The same, on the follow ups form a nurse gets after a lab technician
entered the result.
-}
partnerFollowUpsShown : PartnerHIVTestValue -> ( Maybe Bool, Bool )
partnerFollowUpsShown stored =
    let
        form =
            partnerHIVResultFormWithDefault emptyPartnerHIVResultForm (Just stored)

        ( _, completed, total ) =
            partnerHIVResultFollowUpsFormAndTasks English Recurrent.SetPartnerHIVTestFormBoolInput form
    in
    ( form.partnerTakingARV, completed == total )


{-| The woman's follow ups form, and whether Save is enabled. No partner test
is on record, so the partner's status is asked by hand.
-}
hivFollowUpsShown : HIVTestValue -> ( HIVResultForm, Bool )
hivFollowUpsShown stored =
    let
        form =
            hivResultFormWithDefault emptyHIVResultForm (Just stored)

        ( _, completed, total ) =
            hivResultFollowUpsFormAndTasks English
                Recurrent.SetHIVTestFormBoolInput
                (resolvePartnerHIVTestResult (withHIVTestValue stored emptyPrenatalMeasurements))
                form
    in
    ( form, completed == total )


{-| Whether a Discordant partnership diagnosis, of either phase, is made for a
woman HIV negative with the given partner test on record.
-}
discordantPartnershipWith : PartnerHIVTestValue -> Bool
discordantPartnershipWith partnerTest =
    let
        diagnoses =
            emptyPrenatalMeasurements
                |> withHIVTestValue womanHIVNegative
                |> withPartnerHIVTestValue partnerTest
                |> testAssembled
                |> generatePrenatalDiagnosesForNurse
    in
    EverySet.member DiagnosisDiscordantPartnershipInitialPhase diagnoses
        || EverySet.member DiagnosisDiscordantPartnershipRecurrentPhase diagnoses


enterPositive : List Recurrent.Msg
enterPositive =
    [ Recurrent.SetPartnerHIVTestResult "positive" ]


{-| A lab technician's entry of a Positive partner result.
-}
partnerPositiveByLabTech : PartnerHIVTestValue -> Maybe PartnerHIVTestValue
partnerPositiveByLabTech stored =
    partnerResultSaved LabResults
        True
        stored
        [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerRunConfirmedByLabTech True
        , Recurrent.SetPartnerHIVTestResult "positive"
        ]


{-| The nurse opens the visit's Partner HIV task again and saves it unchanged.
-}
partnerTaskResaved : PartnerHIVTestValue -> Maybe PartnerHIVTestValue
partnerTaskResaved stored =
    partnerTestSavedAtVisit (withPartnerHIVTestValue stored emptyPrenatalMeasurements) []


{-| A Lab Results page where a lab technician entered the partner's result,
leaving its follow up for the nurse.
-}
assembledWithPartnerFollowUp : PartnerHIVTestValue -> AssembledData
assembledWithPartnerFollowUp partnerTest =
    measurementsWith [ TestPartnerHIV ] (Just TestNegative) Nothing
        |> withHIVTestValue womanHIVNegative
        |> withPartnerHIVTestValue partnerTest
        |> testAssembled


partnerResultEnteredByNurseTest : Test
partnerResultEnteredByNurseTest =
    let
        shownAfter page stored taps =
            partnerResultFormAfter page False stored taps
                |> partnerResultShown
    in
    describe "a partner HIV test sent to the lab, its result entered by the nurse"
        [ test "a Positive result entered in Lab Results shows \"Is the partner taking ARVs?\" unanswered, and cannot be saved until it is answered" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map (\stored -> shownAfter LabResults stored enterPositive)
                    |> Expect.equal (Just ( Nothing, False ))
        , test "a Positive result entered through a later visit's lab history shows \"Is the partner taking ARVs?\" unanswered, and cannot be saved until it is answered" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map (\stored -> shownAfter LabHistory stored enterPositive)
                    |> Expect.equal (Just ( Nothing, False ))
        , test "a partner test sent to the lab before this change: a Positive result entered in Lab Results shows the question unanswered, and cannot be saved" <|
            \_ ->
                shownAfter LabResults legacyPartnerTestSentToLab enterPositive
                    |> Expect.equal ( Nothing, False )
        , test "a partner test sent to the lab before this change: a Positive result entered through lab history shows the question unanswered, and cannot be saved" <|
            \_ ->
                shownAfter LabHistory legacyPartnerTestSentToLab enterPositive
                    |> Expect.equal ( Nothing, False )
        , test "a partner test stored with no follow up answers at all: a Positive result shows the question unanswered, and cannot be saved" <|
            \_ ->
                shownAfter LabResults { legacyPartnerTestSentToLab | hivSigns = Nothing } enterPositive
                    |> Expect.equal ( Nothing, False )
        , test "once the nurse answers, the Positive result can be saved" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map
                        (\stored ->
                            shownAfter LabResults stored (enterPositive ++ [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ])
                        )
                    |> Expect.equal (Just ( Just False, True ))
        , test "a Negative result asks nothing more and can be saved" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map (\stored -> shownAfter LabResults stored [ Recurrent.SetPartnerHIVTestResult "negative" ])
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just True)
        , test "an Indeterminate result asks nothing more and can be saved" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map (\stored -> shownAfter LabHistory stored [ Recurrent.SetPartnerHIVTestResult "indeterminate" ])
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just True)
        ]


partnerResultAnswerSavedTest : Test
partnerResultAnswerSavedTest =
    let
        takingARVsSuppressed =
            enterPositive
                ++ [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV True
                   , Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerSurpressedViralLoad True
                   ]

        notTakingARVs =
            enterPositive ++ [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ]

        discordantAfter page stored taps =
            partnerResultSaved page False stored taps
                |> Maybe.map discordantPartnershipWith
    in
    describe "a partner HIV test sent to the lab: whatever the nurse answers is what is saved"
        [ test "\"Yes, taking ARVs\" with a suppressed viral load, entered in Lab Results, gives no Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> discordantAfter LabResults stored takingARVsSuppressed)
                    |> Expect.equal (Just False)
        , test "\"No\", entered in Lab Results, gives a Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> discordantAfter LabResults stored notTakingARVs)
                    |> Expect.equal (Just True)
        , test "\"Yes, taking ARVs\" with a suppressed viral load, entered through lab history, gives no Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> discordantAfter LabHistory stored takingARVsSuppressed)
                    |> Expect.equal (Just False)
        , test "\"No\", entered through lab history, gives a Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> discordantAfter LabHistory stored notTakingARVs)
                    |> Expect.equal (Just True)
        , test "\"Yes, taking ARVs\" without a suppressed viral load gives a Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen
                        (\stored ->
                            discordantAfter LabResults
                                stored
                                (enterPositive
                                    ++ [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV True
                                       , Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerSurpressedViralLoad False
                                       ]
                                )
                        )
                    |> Expect.equal (Just True)
        , test "on a partner test sent to the lab before this change, \"Yes, taking ARVs\" with a suppressed viral load gives no Discordant partnership diagnosis" <|
            \_ ->
                discordantAfter LabResults legacyPartnerTestSentToLab takingARVsSuppressed
                    |> Expect.equal (Just False)
        , test "an answer of No reads back as No when Lab Results is opened again, and the result stays saveable" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> partnerResultSaved LabResults False stored notTakingARVs)
                    |> Maybe.map (\saved -> partnerResultFormAfter LabResults False saved [] |> partnerResultShown)
                    |> Expect.equal (Just ( Just False, True ))
        , test "an answer of Yes reads back as Yes when Lab Results is opened again" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> partnerResultSaved LabResults False stored takingARVsSuppressed)
                    |> Maybe.map (\saved -> partnerResultFormAfter LabResults False saved [] |> partnerResultShown)
                    |> Expect.equal (Just ( Just True, True ))
        ]


partnerTestAwaitingResultTest : Test
partnerTestAwaitingResultTest =
    describe "a partner HIV test sent to the lab with no result yet"
        [ test "gives no Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just False)
        , test "sent to the lab before this change, gives no Discordant partnership diagnosis" <|
            \_ ->
                discordantPartnershipWith legacyPartnerTestSentToLab
                    |> Expect.equal False
        , test "saving the visit's Partner HIV task again, still without a result, gives no Discordant partnership diagnosis" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerTaskResaved
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just False)
        ]


partnerTaskResavedAfterLabTechTest : Test
partnerTaskResavedAfterLabTechTest =
    let
        resaved =
            partnerTestSentToLab
                |> Maybe.andThen partnerPositiveByLabTech
                |> Maybe.andThen partnerTaskResaved
    in
    describe "a lab technician entered a Positive partner result, then the visit's Partner HIV task is saved again unchanged"
        [ test "\"Is the partner taking ARVs?\" is still asked in Lab Results, unanswered" <|
            \_ ->
                resaved
                    |> Maybe.map partnerFollowUpsShown
                    |> Expect.equal (Just ( Nothing, False ))
        , test "the Partner HIV follow up is still pending in Lab Results" <|
            \_ ->
                resaved
                    |> Maybe.map (assembledWithPartnerFollowUp >> (\assembled -> laboratoryResultFollowUpsTaskCompleted assembled TaskPartnerHIVTest))
                    |> Expect.equal (Just False)
        , test "no Discordant partnership diagnosis is made until it is answered" <|
            \_ ->
                resaved
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just False)
        , test "the lab technician's Positive result is kept" <|
            \_ ->
                resaved
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestPositive))
        , test "once the nurse answers No in the follow up, a Discordant partnership diagnosis is made" <|
            \_ ->
                resaved
                    |> Maybe.andThen (\stored -> partnerResultSaved LabResults False stored [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ])
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just True)
        , test "before the re-save, the lab technician's entry leaves the question to the nurse" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.map partnerFollowUpsShown
                    |> Expect.equal (Just ( Nothing, False ))
        , test "a partner test sent to the lab before this change: after the lab technician and a re-save, the question is still asked" <|
            \_ ->
                partnerPositiveByLabTech legacyPartnerTestSentToLab
                    |> Maybe.andThen partnerTaskResaved
                    |> Maybe.map partnerFollowUpsShown
                    |> Expect.equal (Just ( Nothing, False ))
        ]


{-| A lab technician's entry of the woman's own HIV result, "positive" or
"negative", on a test sent to the lab at the visit.
-}
hivResultByLabTech : String -> Maybe HIVTestValue
hivResultByLabTech result =
    hivTestSentToLab
        |> Maybe.andThen
            (\stored ->
                hivResultSaved True
                    stored
                    [ Recurrent.SetHIVTestFormBoolInput tapHIVRunConfirmedByLabTech True
                    , Recurrent.SetHIVTestResult result
                    ]
            )


{-| The same, then the nurse opens the visit's HIV task again and saves it
unchanged.
-}
hivTaskResavedAfterLabTech : String -> Maybe HIVTestValue
hivTaskResavedAfterLabTech result =
    hivResultByLabTech result
        |> Maybe.andThen
            (\stored ->
                hivTestSavedAtVisit (withHIVTestValue stored emptyPrenatalMeasurements) []
            )


hivTaskResavedAfterLabTechTest : Test
hivTaskResavedAfterLabTechTest =
    let
        resavedAfter =
            hivTaskResavedAfterLabTech

        assembledWithHIVFollowUp hivTest =
            measurementsWith [ TestHIV ] Nothing Nothing
                |> withHIVTestValue hivTest
                |> (\measurements -> { measurements | partnerHIVTest = Nothing })
                |> testAssembled
    in
    describe "a lab technician entered the woman's own HIV result, then the visit's HIV task is saved again unchanged"
        [ test "Positive: \"enrolled in the HIV programme\" is left to be asked, not recorded as No" <|
            \_ ->
                resavedAfter "positive"
                    |> Maybe.map (hivFollowUpsShown >> Tuple.mapFirst .hivProgramHC)
                    |> Expect.equal (Just ( Nothing, False ))
        , test "Positive: the HIV follow up is still pending in Lab Results" <|
            \_ ->
                resavedAfter "positive"
                    |> Maybe.map (assembledWithHIVFollowUp >> (\assembled -> laboratoryResultFollowUpsTaskCompleted assembled TaskHIVTest))
                    |> Expect.equal (Just False)
        , test "Positive: no ARVs are required, since \"no HIV programme at the health centre\" was never answered" <|
            \_ ->
                resavedAfter "positive"
                    |> Maybe.map
                        (\hivTest ->
                            { emptyPrenatalMeasurements | hivTest = TestFixtures.wrapMeasurement currentDate hivTest }
                                |> testAssembled
                                |> withDiagnoses [ DiagnosisHIVRecurrentPhase ]
                                |> requiredMedications
                        )
                    |> Expect.equal (Just [])
        , test "Negative: the partner's status is left to be asked, not recorded as No" <|
            \_ ->
                resavedAfter "negative"
                    |> Maybe.map (hivFollowUpsShown >> Tuple.mapFirst .partnerHIVPositive)
                    |> Expect.equal (Just ( Nothing, False ))
        , test "Negative: the HIV follow up is still pending in Lab Results" <|
            \_ ->
                resavedAfter "negative"
                    |> Maybe.map (assembledWithHIVFollowUp >> (\assembled -> laboratoryResultFollowUpsTaskCompleted assembled TaskHIVTest))
                    |> Expect.equal (Just False)
        , test "the lab technician's result is kept" <|
            \_ ->
                resavedAfter "positive"
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestPositive))
        ]


partnerKnownPositiveAtVisitTest : Test
partnerKnownPositiveAtVisitTest =
    let
        knownPositive =
            Initial.SetPartnerHIVTestFormBoolInput knownAsPositiveUpdatePartnerHIVTest True

        -- The visit's Partner HIV form as drawn after the given taps, and
        -- whether Save is enabled.
        visitFormShown taps =
            let
                form =
                    partnerHIVTestFormWithDefault (visitModelAfter emptyPrenatalMeasurements taps).laboratoryData.partnerHIVTestForm Nothing

                ( _, completed, total ) =
                    viewPartnerHIVTestForm English
                        currentDate
                        (emptyContentAndTasksLaboratoryUniversalTestInitialConfig Initial.NoOp)
                        (emptyContentAndTasksForPerformedLaboratoryUniversalTestConfig Initial.NoOp)
                        form
            in
            ( form.partnerTakingARV, completed == total )

        savedAfter taps =
            partnerTestSavedAtVisit emptyPrenatalMeasurements taps

        answeredNo =
            [ knownPositive, Initial.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ]

        answeredYesSuppressed =
            [ knownPositive
            , Initial.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV True
            , Initial.SetPartnerHIVTestFormBoolInput tapPartnerSurpressedViralLoad True
            ]

        -- Point of care test at the visit, Positive.
        immediatePositive =
            [ Initial.SetPartnerHIVTestFormBoolInput knownAsPositiveUpdatePartnerHIVTest False
            , Initial.SetPartnerHIVTestFormBoolInput tapPartnerTestPerformed True
            , Initial.SetPartnerHIVTestFormBoolInput tapPartnerImmediateResult True
            , Initial.SetPartnerHIVTestResult "positive"
            ]
    in
    describe "a partner known to be HIV positive, during the visit itself"
        [ test "is asked \"Is the partner taking ARVs?\", and the task cannot be saved until it is answered" <|
            \_ ->
                visitFormShown [ knownPositive ]
                    |> Expect.equal ( Nothing, False )
        , test "the task can be saved once the question is answered" <|
            \_ ->
                visitFormShown answeredNo
                    |> Expect.equal ( Just False, True )
        , test "an answer of No is saved as before" <|
            \_ ->
                savedAfter answeredNo
                    |> Maybe.map (\value -> ( value.executionNote, value.hivSigns ))
                    |> Expect.equal (Just ( TestNoteKnownAsPositive, Just (EverySet.singleton NoPrenatalHIVSign) ))
        , test "an answer of Yes with a suppressed viral load is saved as before" <|
            \_ ->
                savedAfter answeredYesSuppressed
                    |> Maybe.map (\value -> ( value.executionNote, value.hivSigns ))
                    |> Expect.equal (Just ( TestNoteKnownAsPositive, Just (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ]) ))
        , test "an answer of No gives a Discordant partnership diagnosis" <|
            \_ ->
                savedAfter answeredNo
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just True)
        , test "an answer of Yes with a suppressed viral load gives no Discordant partnership diagnosis" <|
            \_ ->
                savedAfter answeredYesSuppressed
                    |> Maybe.map discordantPartnershipWith
                    |> Expect.equal (Just False)
        , test "a Positive point of care result at the visit asks the question too, before the task can be saved" <|
            \_ ->
                visitFormShown immediatePositive
                    |> Expect.equal ( Nothing, False )
        , test "a Positive point of care result answered No at the visit reads back as No, and gives a Discordant partnership diagnosis" <|
            \_ ->
                savedAfter (immediatePositive ++ [ Initial.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ])
                    |> Maybe.map
                        (\value ->
                            ( (partnerHIVTestFormWithDefault emptyPartnerHIVTestForm (Just value)).partnerTakingARV
                            , discordantPartnershipWith value
                            )
                        )
                    |> Expect.equal (Just ( Just False, True ))
        ]



-- The woman's ANC Labs entry in Case Management, after the nurse saves the
-- visit's HIV or Partner HIV task again, unchanged.
--
-- A lab technician who enters an HIV or Partner HIV result completes the test
-- on the entry, asks the nurse to review it, and lists the test for follow up.
-- The nurse then sees the entry as "Review & Accept"; through it she reaches
-- Lab Results Follow Ups, the only place the follow up questions are asked.
--
-- Saving the visit's task again fires the same update a nurse's own result
-- does, because the task's record holds a result: the "result saved" update
-- of the labs entry, with the saving user a nurse. That save must leave the
-- entry for the nurse: still listed, still ready for review, the test still
-- on its follow up list, and Lab Results Follow Ups still asking.
--
-- The visit, and the lab technician's entry, are on `currentDate`. The nurse
-- saves the task again two days later, unless a test says otherwise.


{-| The day the nurse saves the visit's task again.
-}
resaveDate : NominalDate
resaveDate =
    Date.add Date.Days 2 currentDate


{-| How the nurse's Case Management shows the labs entry on the given day. It
follows the Prenatal labs pane: an entry whose resolution date has come is
not listed, and one closing within a week shows as closing soon.
-}
type NurseLabsEntry
    = EntryClosingSoon
    | EntryNotListed
    | EntryPending
    | EntryReadyForReview
    | EntryReviewed


nurseLabsEntry : NominalDate -> LabsResultsValue -> NurseLabsEntry
nurseLabsEntry date value =
    if Date.compare date value.resolutionDate /= LT then
        EntryNotListed

    else if Date.diff Date.Days date value.resolutionDate < 8 then
        EntryClosingSoon

    else
        case value.reviewState of
            Just LabsResultsReviewCompleted ->
                EntryReviewed

            Just LabsResultsReviewRequested ->
                EntryReadyForReview

            Nothing ->
                EntryPending


{-| The labs entry as a lab technician leaves it after entering the results
of the given tests at the visit: every test completed, review requested, and
the tests listed for follow up.
-}
entryAfterLabTech : List LaboratoryTest -> LabsResultsValue
entryAfterLabTech tests =
    let
        entry =
            TestFixtures.labsEntry currentDate tests tests
    in
    { entry
        | reviewState = Just LabsResultsReviewRequested
        , testsWithFollowUp = Just (EverySet.fromList tests)
    }


withLabsEntry : LabsResultsValue -> PrenatalMeasurements -> PrenatalMeasurements
withLabsEntry value measurements =
    { measurements | labsResults = TestFixtures.wrapMeasurement currentDate value }


{-| The labs entry after a result of the given test is saved on the given day,
by a lab technician or by the nurse, on a visit whose measurements are
already as the save leaves them. The visit's measurements come back holding
the updated entry.
-}
entrySavedBy : Bool -> NominalDate -> LaboratoryTest -> PrenatalMeasurements -> Maybe PrenatalMeasurements
entrySavedBy isLabTech date test measurements =
    measurements.labsResults
        |> Maybe.map
            (\( _, results ) ->
                withLabsEntry
                    (labsResultsValueOnResultSaved date isLabTech (testAssembled measurements) results test)
                    measurements
            )


nurseResave : NominalDate -> LaboratoryTest -> PrenatalMeasurements -> Maybe PrenatalMeasurements
nurseResave =
    entrySavedBy False


labsEntryOf : PrenatalMeasurements -> Maybe LabsResultsValue
labsEntryOf measurements =
    getMeasurementValueFunc measurements.labsResults


{-| What the nurse's Case Management shows for the visit's labs entry.
-}
entryShown : NominalDate -> PrenatalMeasurements -> Maybe NurseLabsEntry
entryShown date =
    labsEntryOf >> Maybe.map (nurseLabsEntry date)


{-| Whether the test is still on the entry's follow up list.
-}
listedForFollowUp : LaboratoryTest -> PrenatalMeasurements -> Bool
listedForFollowUp test measurements =
    labsEntryOf measurements
        |> Maybe.andThen .testsWithFollowUp
        |> Maybe.map (EverySet.member test)
        |> Maybe.withDefault False


{-| The follow up tasks Lab Results Follow Ups offers the nurse, and whether
the activity is done.
-}
followUpsOffered : PrenatalMeasurements -> ( List LaboratoryTask, Bool )
followUpsOffered measurements =
    let
        assembled =
            testAssembled measurements
    in
    ( resolveLaboratoryResultFollowUpsTasks assembled
    , activityCompleted resaveDate False assembled PrenatalActivity.LabsResultsFollowUps
    )


{-| The visit's measurements once the woman's negative HIV test (point of
care) and the given partner test are recorded, with the labs entry a lab
technician left for the partner test.
-}
partnerVisitWith : PartnerHIVTestValue -> PrenatalMeasurements
partnerVisitWith partnerTest =
    emptyPrenatalMeasurements
        |> withHIVTestValue womanHIVNegative
        |> withPartnerHIVTestValue partnerTest
        |> withLabsEntry (entryAfterLabTech [ TestPartnerHIV ])


{-| The visit's measurements once the woman's own HIV test is recorded, no
partner test on record, with the labs entry a lab technician left for it.
-}
hivVisitWith : HIVTestValue -> PrenatalMeasurements
hivVisitWith hivTest =
    emptyPrenatalMeasurements
        |> withHIVTestValue hivTest
        |> withLabsEntry (entryAfterLabTech [ TestHIV ])


partnerTaskResavedLabsEntryTest : Test
partnerTaskResavedLabsEntryTest =
    let
        -- The partner test sent to the lab at the visit, a Positive result
        -- entered by a lab technician, then the visit's task saved again.
        resavedVisit =
            partnerTestSentToLab
                |> Maybe.andThen partnerPositiveByLabTech
                |> Maybe.andThen partnerTaskResaved
                |> Maybe.map partnerVisitWith

        afterResave =
            Maybe.andThen (nurseResave resaveDate TestPartnerHIV) resavedVisit
    in
    describe "a lab technician entered a Positive partner result, then the nurse saves the visit's Partner HIV task again unchanged: the ANC Labs entry"
        [ test "stays listed for the nurse, as Review & Accept" <|
            \_ ->
                afterResave
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "keeps the Partner HIV test on its follow up list" <|
            \_ ->
                afterResave
                    |> Maybe.map (listedForFollowUp TestPartnerHIV)
                    |> Expect.equal (Just True)
        , test "still leads to Lab Results Follow Ups asking \"Is the partner taking ARVs?\", unanswered" <|
            \_ ->
                afterResave
                    |> Maybe.map followUpsOffered
                    |> Expect.equal (Just ( [ TaskPartnerHIVTest ], False ))
        , test "with the visit's record exactly as the lab technician left it, stays listed for the nurse, as Review & Accept" <|
            -- The re-save stores nothing new here, so only the labs entry's
            -- own update is exercised.
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.map partnerVisitWith
                    |> Maybe.andThen (nurseResave resaveDate TestPartnerHIV)
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "re-saved on the day of the visit, stays listed for the nurse, as Review & Accept" <|
            \_ ->
                resavedVisit
                    |> Maybe.andThen (nurseResave currentDate TestPartnerHIV)
                    |> Maybe.andThen (entryShown currentDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "on a partner test sent to the lab before this change, stays listed for the nurse, as Review & Accept" <|
            \_ ->
                partnerPositiveByLabTech legacyPartnerTestSentToLab
                    |> Maybe.andThen partnerTaskResaved
                    |> Maybe.map partnerVisitWith
                    |> Maybe.andThen (nurseResave resaveDate TestPartnerHIV)
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        ]


hivTaskResavedLabsEntryTest : Test
hivTaskResavedLabsEntryTest =
    let
        afterResave result =
            hivTaskResavedAfterLabTech result
                |> Maybe.map hivVisitWith
                |> Maybe.andThen (nurseResave resaveDate TestHIV)
    in
    describe "a lab technician entered the woman's own HIV result, then the nurse saves the visit's HIV task again unchanged: the ANC Labs entry"
        [ test "Positive: stays listed for the nurse, as Review & Accept" <|
            \_ ->
                afterResave "positive"
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "Positive: keeps the HIV test on its follow up list" <|
            \_ ->
                afterResave "positive"
                    |> Maybe.map (listedForFollowUp TestHIV)
                    |> Expect.equal (Just True)
        , test "Positive: still leads to Lab Results Follow Ups asking \"enrolled in the HIV programme\", unanswered" <|
            \_ ->
                afterResave "positive"
                    |> Maybe.map followUpsOffered
                    |> Expect.equal (Just ( [ TaskHIVTest ], False ))
        , test "Negative: stays listed for the nurse, as Review & Accept" <|
            \_ ->
                afterResave "negative"
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "Negative: still leads to Lab Results Follow Ups asking the partner's status, unanswered" <|
            \_ ->
                afterResave "negative"
                    |> Maybe.map followUpsOffered
                    |> Expect.equal (Just ( [ TaskHIVTest ], False ))
        , test "Positive, with the visit's record exactly as the lab technician left it: stays listed for the nurse, as Review & Accept" <|
            \_ ->
                hivResultByLabTech "positive"
                    |> Maybe.map hivVisitWith
                    |> Maybe.andThen (nurseResave resaveDate TestHIV)
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        , test "Negative, with a Positive partner result from the lab technician on the same entry: stays listed for the nurse, as Review & Accept" <|
            -- Both tests are on the entry and on its follow up list. The
            -- partner's follow up asks the woman's questions too, so it is
            -- the one task offered.
            \_ ->
                Maybe.map2
                    (\hivTest partnerTest ->
                        emptyPrenatalMeasurements
                            |> withHIVTestValue hivTest
                            |> withPartnerHIVTestValue partnerTest
                            |> withLabsEntry (entryAfterLabTech [ TestHIV, TestPartnerHIV ])
                    )
                    (hivTaskResavedAfterLabTech "negative")
                    (partnerTestSentToLab |> Maybe.andThen partnerPositiveByLabTech)
                    |> Maybe.andThen (nurseResave resaveDate TestHIV)
                    |> Maybe.map (\measurements -> ( entryShown resaveDate measurements, followUpsOffered measurements ))
                    |> Expect.equal (Just ( Just EntryReadyForReview, ( [ TaskPartnerHIVTest ], False ) ))
        ]


labsEntryResultSavedGuardTest : Test
labsEntryResultSavedGuardTest =
    let
        -- The labs entry as the visit created it, the partner test sent to
        -- the lab and waiting.
        entryAtVisit =
            TestFixtures.labsEntry currentDate [ TestPartnerHIV ] []

        visitWithWaitingEntry partnerTest =
            emptyPrenatalMeasurements
                |> withHIVTestValue womanHIVNegative
                |> withPartnerHIVTestValue partnerTest
                |> withLabsEntry entryAtVisit
    in
    describe "the ANC Labs entry when a result is saved, outside the re-save"
        [ test "the lab technician's Positive partner result leaves the entry listed for the nurse, as Review & Accept, with the test on its follow up list" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.map visitWithWaitingEntry
                    |> Maybe.andThen (entrySavedBy True currentDate TestPartnerHIV)
                    |> Maybe.map (\measurements -> ( entryShown resaveDate measurements, listedForFollowUp TestPartnerHIV measurements ))
                    |> Expect.equal (Just ( Just EntryReadyForReview, True ))
        , test "once the nurse answers \"Is the partner taking ARVs?\" in the follow up, her save closes the entry as reviewed" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.andThen (\stored -> partnerResultSaved LabResults False stored [ Recurrent.SetPartnerHIVTestFormBoolInput tapPartnerTakingARV False ])
                    |> Maybe.map partnerVisitWith
                    |> Maybe.andThen (nurseResave resaveDate TestPartnerHIV)
                    |> Maybe.andThen labsEntryOf
                    |> Maybe.map .reviewState
                    |> Expect.equal (Just (Just LabsResultsReviewCompleted))
        , test "a Negative partner result the nurse enters herself closes the entry as reviewed" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen (\stored -> partnerResultSaved LabResults False stored [ Recurrent.SetPartnerHIVTestResult "negative" ])
                    |> Maybe.map visitWithWaitingEntry
                    |> Maybe.andThen (nurseResave resaveDate TestPartnerHIV)
                    |> Maybe.andThen labsEntryOf
                    |> Maybe.map .reviewState
                    |> Expect.equal (Just (Just LabsResultsReviewCompleted))
        , test "a re-save while another test on the entry is still waiting for the lab leaves the entry listed for the nurse" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.andThen partnerTaskResaved
                    |> Maybe.map
                        (visitWithWaitingEntry
                            >> withLabsEntry
                                (TestFixtures.labsEntry currentDate [ TestHemoglobin, TestPartnerHIV ] [ TestPartnerHIV ]
                                    |> (\entry -> { entry | testsWithFollowUp = Just (EverySet.singleton TestPartnerHIV) })
                                )
                        )
                    |> Maybe.andThen (nurseResave resaveDate TestPartnerHIV)
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryPending)
        ]


labsEntryLastResultByNurseTest : Test
labsEntryLastResultByNurseTest =
    describe "the nurse enters the entry's last result while a lab technician's follow up question is unanswered"
        [ test "the entry asks the nurse for review, so Review & Accept leads to the question" <|
            \_ ->
                partnerTestSentToLab
                    |> Maybe.andThen partnerPositiveByLabTech
                    |> Maybe.map
                        (\partnerTest ->
                            emptyPrenatalMeasurements
                                |> withHIVTestValue womanHIVNegative
                                |> withPartnerHIVTestValue partnerTest
                                |> withLabsEntry
                                    (TestFixtures.labsEntry currentDate [ TestHemoglobin, TestPartnerHIV ] [ TestPartnerHIV ]
                                        |> (\entry -> { entry | reviewState = Nothing, testsWithFollowUp = Just (EverySet.singleton TestPartnerHIV) })
                                    )
                        )
                    |> Maybe.andThen (nurseResave resaveDate TestHemoglobin)
                    |> Maybe.andThen (entryShown resaveDate)
                    |> Expect.equal (Just EntryReadyForReview)
        ]


all : Test
all =
    describe "Pages.Prenatal.RecurrentActivity.Utils"
        [ concludedLabsResultsValueTest
        , laboratoryResultTaskCompletedKnownAsPositiveTest
        , laboratoryResultNotPerformedTest
        , laboratoryResultTaskCompletedMalariaTest
        , nextStepsHealthEducationExpectedTest
        , nextStepsMedicationDistributionCompletedTest
        , resolveLaboratoryResultFollowUpsTasksTest
        , resolveMedicationDistributionContinuedHypertensionTest
        , resolveRequiredMedicationsSetHIVTest
        , withCompletedTestsFromMeasurementsTest
        , partnerResultEnteredByNurseTest
        , partnerResultAnswerSavedTest
        , partnerTestAwaitingResultTest
        , partnerTaskResavedAfterLabTechTest
        , hivTaskResavedAfterLabTechTest
        , partnerKnownPositiveAtVisitTest
        , partnerTaskResavedLabsEntryTest
        , hivTaskResavedLabsEntryTest
        , labsEntryResultSavedGuardTest
        , labsEntryLastResultByNurseTest
        ]
