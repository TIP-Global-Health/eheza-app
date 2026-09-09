module Pages.Prenatal.RecurrentActivity.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( BloodSmearResult(..)
        , HIVTestValue
        , LaboratoryTest(..)
        , LabsResultsValue
        , MalariaTestValue
        , PartnerHIVTestValue
        , PrenatalMeasurements
        , TestExecutionNote(..)
        , TestResult(..)
        , emptyPrenatalMeasurements
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.RecurrentActivity.Utils
    exposing
        ( laboratoryResultTaskCompleted
        , resolveLaboratoryResultFollowUpsTasks
        )
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


all : Test
all =
    describe "Pages.Prenatal.RecurrentActivity.Utils"
        [ resolveLaboratoryResultFollowUpsTasksTest
        , laboratoryResultTaskCompletedMalariaTest
        ]
