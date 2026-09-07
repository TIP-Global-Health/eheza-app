module Pages.Prenatal.RecurrentActivity.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , HIVTestValue
        , LaboratoryTest(..)
        , LabsResultsValue
        , MedicationDistributionSign(..)
        , MedicationNonAdministrationSign(..)
        , PartnerHIVTestValue
        , PrenatalMeasurements
        , TestExecutionNote(..)
        , TestResult(..)
        , emptyPrenatalMeasurements
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.RecurrentActivity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.RecurrentActivity.Utils exposing (nextStepsTaskCompleted, resolveLaboratoryResultFollowUpsTasks)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time



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

        hivTestValue : HIVTestValue
        hivTestValue =
            { executionNote = TestNoteRunToday
            , executionDate = Just currentDate
            , testPrerequisites = Nothing
            , testResult = hivResult
            , hivSigns = Nothing
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
        , hivTest = TestFixtures.wrapMeasurement currentDate hivTestValue
        , partnerHIVTest = TestFixtures.wrapMeasurement currentDate partnerHIVTestValue
    }


{-| An encounter carrying the given diagnoses, and a Medication Distribution
measurement listing what was handed over and what was marked as not given.
-}
assembledWith : List PrenatalDiagnosis -> List MedicationDistributionSign -> List MedicationNonAdministrationSign -> AssembledData
assembledWith diagnoses distributionSigns nonAdministrationSigns =
    let
        assembled =
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

        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | diagnoses = EverySet.fromList diagnoses } }


{-| The same encounter, but the nurse never opened Medication Distribution.
-}
assembledWithoutMedicationDistribution : List PrenatalDiagnosis -> AssembledData
assembledWithoutMedicationDistribution diagnoses =
    let
        assembled =
            testAssembled emptyPrenatalMeasurements

        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | diagnoses = EverySet.fromList diagnoses } }


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


all : Test
all =
    describe "Pages.Prenatal.RecurrentActivity.Utils"
        [ nextStepsMedicationDistributionCompletedTest
        , resolveLaboratoryResultFollowUpsTasksTest
        ]
