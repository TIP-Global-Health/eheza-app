module Pages.Prenatal.Activity.Test exposing (all)

import AssocList as Dict
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BloodGpRsTestValue
        , BloodSmearResult(..)
        , CorePhysicalExamValue
        , DangerSign(..)
        , DangerSignsValue
        , FollowUpOption(..)
        , HIVPCRTestValue
        , HIVTestValue
        , HemoglobinTestValue
        , HepatitisBTestValue
        , LungsCPESign
        , MalariaTestValue
        , Measurement
        , ObstetricHistoryStep2Sign(..)
        , PartnerHIVTestValue
        , PrenatalAssesment(..)
        , PrenatalHIVSign(..)
        , PrenatalMeasurements
        , PrenatalMentalHealthQuestion(..)
        , PrenatalMentalHealthQuestionOption(..)
        , PrenatalSymptom(..)
        , PrenatalSymptomQuestion(..)
        , PreviousDeliveryPeriod(..)
        , PreviousDeliverySign(..)
        , ProteinValue(..)
        , RandomBloodSugarTestValue
        , RecommendedTreatmentSign(..)
        , Rhesus(..)
        , SyphilisTestValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , UrineDipstickTestValue
        , ViralLoadStatus(..)
        , VitalsValue
        , emptyPrenatalMeasurements
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (HighRiskFactor(..))
import Backend.PrenatalActivity.Utils exposing (generateHighRiskAlertData)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (RangedMeasurement(..), emptyHIVPCRResultForm)
import Measurement.Utils exposing (knownAsPositiveUpdate, knownAsPositiveUpdateHIVTest, knownAsPositiveUpdatePartnerHIVTest, toHIVPCRResultValueWithDefault, toHIVPCRTestValueWithDefault, toHIVTestValueUniversalWithDefault, toHepatitisBTestValueWithDefault, toPartnerHIVTestValueWithDefault)
import Pages.Prenatal.Activity.Model exposing (Msg(..), emptyModel)
import Pages.Prenatal.Activity.Types exposing (GWGClassification(..), NextStepsTask(..), PrePregnancyClassification(..), WarningPopupType(..))
import Pages.Prenatal.Activity.Update exposing (update)
import Pages.Prenatal.Activity.Utils exposing (activityCompleted, bmiToPrePregnancyClassification, generatePrenatalAssesmentForChw, generatePrenatalDiagnosesForNurse, resolveGWGClassificationForHealthyStart, resolveNextStepsTasks, suicideRiskDiagnosedBySigns, zscoreToPrePregnancyClassification)
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData, generateGravida, generatePara, skipObstetricHistorySecondStep)
import Pages.Prenatal.Model exposing (AssembledData, PrenatalEncounterPhase(..), PreviousEncounterData, emptyHealthEducationForm)
import Pages.Prenatal.Utils exposing (diabetesDiagnoses, diagnosedPreviouslyAnyOf, diagnosisKnownAnyOf, healthEducationFormInputsAndTasksForNurse, resolveDiscordantCoupleStatus, resolvePartnerHIVTestResult, resolveRequiredMedicationsSet)
import RemoteData
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures
import Time
import Translate
import Translate.Model exposing (Language(..))



-- Expected classifications come from the WHO standards (the independent
-- oracle), not from the implementation:
--   * adult BMI categories: underweight <18.5, normal 18.5-25, overweight
--     25-30, obese >=30;
--   * BMI-for-age z-score (5-19y): thinness <-2, normal -2..+1, overweight
--     +1..+2, obese >+2.
-- Tests focus on the category boundaries, where a shifted cutoff would hide.


bmiToPrePregnancyClassificationTest : Test
bmiToPrePregnancyClassificationTest =
    describe "bmiToPrePregnancyClassification (WHO adult BMI categories)"
        [ test "17.0 -> underweight" <|
            \_ -> bmiToPrePregnancyClassification 17.0 |> Expect.equal PrePregnancyUnderWeight
        , test "18.4 (just below 18.5) -> underweight" <|
            \_ -> bmiToPrePregnancyClassification 18.4 |> Expect.equal PrePregnancyUnderWeight
        , test "18.5 (boundary) -> normal" <|
            \_ -> bmiToPrePregnancyClassification 18.5 |> Expect.equal PrePregnancyNormal
        , test "24.9 -> normal" <|
            \_ -> bmiToPrePregnancyClassification 24.9 |> Expect.equal PrePregnancyNormal
        , test "25.0 (boundary) -> overweight" <|
            \_ -> bmiToPrePregnancyClassification 25.0 |> Expect.equal PrePregnancyOverweight
        , test "29.9 -> overweight" <|
            \_ -> bmiToPrePregnancyClassification 29.9 |> Expect.equal PrePregnancyOverweight
        , test "30.0 (boundary) -> obese" <|
            \_ -> bmiToPrePregnancyClassification 30.0 |> Expect.equal PrePregnancyObesity
        , test "35.0 -> obese" <|
            \_ -> bmiToPrePregnancyClassification 35.0 |> Expect.equal PrePregnancyObesity
        ]


zscoreToPrePregnancyClassificationTest : Test
zscoreToPrePregnancyClassificationTest =
    describe "zscoreToPrePregnancyClassification (WHO BMI-for-age z-score bands)"
        [ test "-2.5 (thinness) -> underweight" <|
            \_ -> zscoreToPrePregnancyClassification -2.5 |> Expect.equal PrePregnancyUnderWeight
        , test "-2.0 (boundary) -> normal" <|
            \_ -> zscoreToPrePregnancyClassification -2.0 |> Expect.equal PrePregnancyNormal
        , test "+1.0 (boundary) -> normal" <|
            \_ -> zscoreToPrePregnancyClassification 1.0 |> Expect.equal PrePregnancyNormal
        , test "+1.5 -> overweight" <|
            \_ -> zscoreToPrePregnancyClassification 1.5 |> Expect.equal PrePregnancyOverweight
        , test "+2.0 (boundary) -> overweight" <|
            \_ -> zscoreToPrePregnancyClassification 2.0 |> Expect.equal PrePregnancyOverweight
        , test "+2.5 -> obese" <|
            \_ -> zscoreToPrePregnancyClassification 2.5 |> Expect.equal PrePregnancyObesity
        ]



-- HEALTHY START GESTATIONAL WEIGHT GAIN
--
-- Expected gain comes from the Healthy Start protocol (the independent
-- oracle): a woman not severely undernourished at booking is expected to gain
-- 60 g per day, one who was severely undernourished 73 g per day, and 23.5 g
-- per day covers the part of the period before 13 weeks. Gain is adequate when
-- it meets or exceeds the expected gain for the period.
--
-- Both weighings here fall after 13 weeks, so only the later rate applies:
-- 25 days at 60 g per day is an expected gain of 1.5 kg, 30 days one of 1.8 kg.


{-| Classify a gain from 60 kg to `currentWeight` over `days` ending on
`currentDate`. With an LMP 28 weeks back, both weighings are well past 13 weeks
of gestation.
-}
classifyHealthyStartGWG : PrePregnancyClassification -> Int -> Float -> Maybe GWGClassification
classifyHealthyStartGWG prePregnancyClassification days currentWeight =
    resolveGWGClassificationForHealthyStart currentDate
        prePregnancyClassification
        60.0
        (Date.add Date.Days -days currentDate)
        currentWeight
        (testAssembled28Weeks emptyPrenatalMeasurements)


resolveGWGClassificationForHealthyStartTest : Test
resolveGWGClassificationForHealthyStartTest =
    describe "resolveGWGClassificationForHealthyStart (Healthy Start expected daily gain)"
        [ test "over 25 days, a gain of 1.0 kg is below the expected 1.5 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "over 25 days, a gain of exactly the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 61.5
                    |> Expect.equal (Just GWGAdequate)
        , test "over 30 days, a gain of exactly the expected 1.8 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 30 61.8
                    |> Expect.equal (Just GWGAdequate)
        , test "over 25 days, a gain of 2.5 kg is above the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 62.5
                    |> Expect.equal (Just GWGAdequate)
        , test "1.0 kg lost over the period -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 59.0
                    |> Expect.equal (Just GWGInadequate)
        , test "severely undernourished at booking: over 25 days, 1.5 kg is short of the expected 1.825 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyUnderWeight 25 61.5
                    |> Expect.equal (Just GWGInadequate)
        , test "a gain of exactly the expected amount is adequate at every five-day interval up to 20 weeks" <|
            \_ ->
                -- At 60 g per day a five-day interval expects 0.3 kg, so these
                -- are the intervals where a woman exactly on target shows a
                -- whole tenth of a kilogram on the scale. The list is the
                -- intervals that came back anything other than adequate.
                List.range 1 28
                    |> List.filter
                        (\fiveDayBlocks ->
                            classifyHealthyStartGWG PrePregnancyNormal
                                (fiveDayBlocks * 5)
                                (60.0 + 0.3 * toFloat fiveDayBlocks)
                                /= Just GWGAdequate
                        )
                    |> Expect.equal []
        ]



-- LAB-DRIVEN DIAGNOSIS FIXTURES
--
-- End-to-end tests for `generatePrenatalDiagnosesForNurse` on an initial-phase
-- nurse encounter (`NurseEncounter`). The oracle is the ANC Nurse tab of the
-- clinical sheet: a positive disease lab produces that disease's diagnosis.
-- The disease-level mapping is the oracle; the exact `*InitialPhase` variant
-- is structural (this is an initial-phase encounter, and all lab results are
-- entered with the "immediate result" prerequisite).


{-| The reference "current date" for the lab-driven diagnosis tests.
-}
currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| LMP ~20 weeks before `currentDate`, giving EGA ~20 weeks: past the week-12
gates that open the disease diagnoses, and mid-pregnancy (not postpartum).
-}
lmpDate : NominalDate
lmpDate =
    Date.add Date.Weeks -20 currentDate


{-| A dummy date used as `dateMeasured`/`executionDate` for measurements.
-}
dummyDate : NominalDate
dummyDate =
    currentDate


{-| Wrap a measurement `value` into the shape the `PrenatalMeasurements`
fields require, with `dummyDate` as `dateMeasured`.
-}
wrapMeasurement : value -> Maybe ( EntityUuid id, Measurement encounter value )
wrapMeasurement value =
    TestFixtures.wrapMeasurement dummyDate value


{-| The shared adult female fixture, but born 1990: age 30 at `currentDate`.
-}
testPerson : Person
testPerson =
    let
        base =
            TestFixtures.testPerson
    in
    { base | birthDate = Just (Date.fromCalendarDate 1990 Time.Jan 1) }


{-| An initial-phase nurse encounter (`NurseEncounter`), with no prior
diagnoses.
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


testParticipant : IndividualEncounterParticipant
testParticipant =
    TestFixtures.testParticipant currentDate AntenatalEncounter


{-| One reusable `AssembledData` for an initial-phase nurse encounter at
EGA ~20 weeks, parameterized only by the measurements under test.
-}
testAssembled : PrenatalMeasurements -> AssembledData
testAssembled measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = testEncounter
    , participant = testParticipant
    , person = testPerson
    , measurements = measurements
    , nursePreviousEncountersData = []
    , chwPreviousMeasurementsWithDates = []
    , globalLmpDate = Just lmpDate
    , globalObstetricHistory = Nothing
    , vaccinationHistory = Dict.empty
    , vaccinationProgress = Dict.empty
    }


{-| LMP ~28 weeks before `currentDate`, putting EGA safely mid-window for the
moderate-preeclampsia [20,37) gate (away from either boundary).
-}
lmpDate28Weeks : NominalDate
lmpDate28Weeks =
    Date.add Date.Weeks -28 currentDate


{-| The `testAssembled` fixture with a CHW antenatal encounter type
(`ChwFirstEncounter`, a regular -- not postpartum -- CHW encounter). The CHW
assessment reads danger signs from the `.signs` set, which the non-postpartum
branch of `noDangerSigns` consults.
-}
chwAssembled : PrenatalMeasurements -> AssembledData
chwAssembled measurements =
    let
        base =
            testAssembled measurements

        encounter =
            base.encounter
    in
    { base | encounter = { encounter | encounterType = ChwFirstEncounter } }


{-| The `testAssembled` fixture with EGA ~28 weeks, used by the
moderate-preeclampsia group so the result is insensitive to the exact gate
boundary.
-}
testAssembled28Weeks : PrenatalMeasurements -> AssembledData
testAssembled28Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate28Weeks }


{-| LMP ~16 weeks before `currentDate`, giving EGA ~16 weeks (< 20), used by the
chronic-hypertension group: chronic hypertension is the EGA < 20 branch of the
hypertension matchers, gestational is the EGA >= 20 branch.
-}
lmpDate16Weeks : NominalDate
lmpDate16Weeks =
    Date.add Date.Weeks -16 currentDate


{-| The `testAssembled` fixture with EGA ~16 weeks (< 20 weeks).
-}
testAssembled16Weeks : PrenatalMeasurements -> AssembledData
testAssembled16Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate16Weeks }


{-| LMP ~38 weeks before `currentDate`, giving EGA ~38 weeks (>= 37), used by the
EGA37+ pre-eclampsia group. At EGA >= 37 the recurrent moderate/severe
pre-eclampsia diagnoses land on the `*EGA37Plus` emergency-referral variants.
-}
lmpDate38Weeks : NominalDate
lmpDate38Weeks =
    Date.add Date.Weeks -38 currentDate


{-| The `testAssembled` fixture with EGA ~38 weeks (>= 37 weeks).
-}
testAssembled38Weeks : PrenatalMeasurements -> AssembledData
testAssembled38Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate38Weeks }



-- LAB VALUE BUILDERS / SETTERS
--
-- Each lab diagnosis at the initial phase requires the test to have been
-- "performed" (executionNote in {TestNoteRunToday, TestNoteRunPreviously,
-- TestNoteRunConfirmedByLabTech}),
-- a positive result, AND the `PrerequisiteImmediateResult` prerequisite
-- (so the result is available immediately, not deferred to case management).


immediateResultPrerequisites : Maybe (EverySet TestPrerequisite)
immediateResultPrerequisites =
    Just (EverySet.singleton PrerequisiteImmediateResult)


{-| What a test sent to the lab stores: the immediate-result question was
asked and answered No. Distinct from `Nothing`, which is what a test stores
when the question was never asked at all - a partner known to be positive, a
result taken from history, or a record from before the question existed.
-}
deferredResultPrerequisites : Maybe (EverySet TestPrerequisite)
deferredResultPrerequisites =
    Just (EverySet.singleton NoTestPrerequisites)


{-| HIV test with the given execution note, prerequisites and result. No
partner/HIV signs (so this never trips discordant-partnership).
-}
hivTestValueCustom : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> TestResult -> HIVTestValue
hivTestValueCustom executionNote prerequisites result =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = prerequisites
    , testResult = Just result
    , hivSigns = Nothing
    }


{-| HIV test, run today with the given positive/negative result, immediate
result.
-}
hivTestValueWith : TestResult -> HIVTestValue
hivTestValueWith =
    hivTestValueCustom TestNoteRunToday immediateResultPrerequisites


syphilisTestValueWith : TestResult -> SyphilisTestValue encounterId
syphilisTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result

    -- No symptoms -> plain Syphilis (not WithComplications / Neurosyphilis).
    , symptoms = Nothing
    , originatingEncounter = Nothing
    }


hepatitisBTestValueWith : TestResult -> HepatitisBTestValue encounterId
hepatitisBTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result
    , originatingEncounter = Nothing
    }


{-| Malaria RDT, run today with the given positive/negative result, immediate
result. Blood smear not taken (RDT path is used directly).
-}
malariaTestValueWith : TestResult -> MalariaTestValue
malariaTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result
    , bloodSmearResult = BloodSmearNotTaken
    , bloodSmearOrdered = False
    }


withHIVTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTest result measurements =
    { measurements | hivTest = wrapMeasurement (hivTestValueWith result) }


withSyphilisTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withSyphilisTest result measurements =
    { measurements | syphilisTest = wrapMeasurement (syphilisTestValueWith result) }


withHepatitisBTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withHepatitisBTest result measurements =
    { measurements | hepatitisBTest = wrapMeasurement (hepatitisBTestValueWith result) }


withMalariaTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withMalariaTest result measurements =
    { measurements | malariaTest = wrapMeasurement (malariaTestValueWith result) }


withCoartem : PrenatalMeasurements -> PrenatalMeasurements
withCoartem measurements =
    { measurements
        | medicationDistribution =
            wrapMeasurement
                { distributionSigns = EverySet.empty
                , nonAdministrationSigns = EverySet.empty
                , recommendedTreatmentSigns = Just (EverySet.singleton TreatmentCoartem)
                , avoidingGuidanceReason = Nothing
                , reinforceTreatmentSigns = Nothing
                }
    }



-- NON-IMMEDIATE-RESULT LAB VALUE BUILDERS / SETTERS
--
-- Identical to the positive immediate-result builders above (performed
-- executionNote + positive result), EXCEPT `testPrerequisites = Nothing`.
--
-- This is the documented representation for a "non-immediate" result:
-- `labTestWithImmediateResult` does `Maybe.andThen .testPrerequisites`, so a
-- `Nothing` prerequisite set makes `immediateResult` False, while
-- `testedPositiveAt` (which only reads executionNote + testResult) stays True.
-- A positive lab entered this way therefore fails the `*InitialPhase` matcher
-- (which requires `immediateResult`) and instead routes to the
-- `*RecurrentPhase` matcher (positive test WITHOUT the immediate-result
-- requirement) on the SAME initial-phase nurse encounter.


hivTestValueNonImmediate : HIVTestValue
hivTestValueNonImmediate =
    hivTestValueCustom TestNoteRunToday Nothing TestPositive


syphilisTestValueNonImmediate : SyphilisTestValue encounterId
syphilisTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , symptoms = Nothing
    , originatingEncounter = Nothing
    }


hepatitisBTestValueNonImmediate : HepatitisBTestValue encounterId
hepatitisBTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , originatingEncounter = Nothing
    }


malariaTestValueNonImmediate : MalariaTestValue
malariaTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , bloodSmearResult = BloodSmearNotTaken
    , bloodSmearOrdered = False
    }


withHIVTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNonImmediate measurements =
    { measurements | hivTest = wrapMeasurement hivTestValueNonImmediate }


withSyphilisTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withSyphilisTestNonImmediate measurements =
    { measurements | syphilisTest = wrapMeasurement syphilisTestValueNonImmediate }


withHepatitisBTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withHepatitisBTestNonImmediate measurements =
    { measurements | hepatitisBTest = wrapMeasurement hepatitisBTestValueNonImmediate }


withMalariaTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withMalariaTestNonImmediate measurements =
    { measurements | malariaTest = wrapMeasurement malariaTestValueNonImmediate }



-- DISCORDANT-PARTNERSHIP BUILDERS
--
-- The diagnosis has two sources: the partner's own HIV test, and the partner
-- signs recorded on the patient's own HIV test. The second question is drawn
-- on the HIV test form while the partner's own result is unavailable, so both
-- sources are live. The Initial/Recurrent split follows the source that
-- matched, so these builders vary the execution note and prerequisites of each
-- test on its own.


{-| Partner HIV test with the given execution note and prerequisites, positive,
partner not taking ARVs - the discordant-partnership condition. A partner known
to be positive is not tested, so no result is recorded for that note, and the
immediate-result question is never asked - the prerequisites stay `Nothing`.
-}
partnerHIVTestValuePositive : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PartnerHIVTestValue
partnerHIVTestValuePositive executionNote prerequisites =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = prerequisites
    , testResult =
        if executionNote == TestNoteKnownAsPositive then
            Nothing

        else
            Just TestPositive
    , hivSigns = Just (EverySet.singleton NoPrenatalHIVSign)
    }


withPartnerHIVTestPositive : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVTestPositive executionNote prerequisites measurements =
    { measurements | partnerHIVTest = wrapMeasurement (partnerHIVTestValuePositive executionNote prerequisites) }


{-| Partner HIV test entered by a lab technician, with the partner signs in the
state the given step leaves them: the lab technician's own save marks them
pending, since only the nurse answers the follow up questions, and the nurse's
follow ups replace that marker with the answers.
-}
withPartnerHIVTestByLabTech : TestResult -> EverySet PrenatalHIVSign -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVTestByLabTech result hivSigns =
    withPartnerHIVTest TestNoteRunConfirmedByLabTech deferredResultPrerequisites (Just result) (Just hivSigns)


withPartnerHIVTest :
    TestExecutionNote
    -> Maybe (EverySet TestPrerequisite)
    -> Maybe TestResult
    -> Maybe (EverySet PrenatalHIVSign)
    -> PrenatalMeasurements
    -> PrenatalMeasurements
withPartnerHIVTest executionNote prerequisites result hivSigns measurements =
    { measurements
        | partnerHIVTest =
            wrapMeasurement
                { executionNote = executionNote
                , executionDate = Just dummyDate
                , testPrerequisites = prerequisites
                , testResult = result
                , hivSigns = hivSigns
                }
    }


{-| Patient's HIV test, negative, carrying the given partner signs.
-}
withHIVTestNegativeWithSigns : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> EverySet PrenatalHIVSign -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNegativeWithSigns executionNote prerequisites hivSigns measurements =
    let
        value =
            hivTestValueCustom executionNote prerequisites TestNegative
    in
    { measurements | hivTest = wrapMeasurement { value | hivSigns = Just hivSigns } }


withHIVTestNegative : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNegative executionNote prerequisites measurements =
    { measurements | hivTest = wrapMeasurement (hivTestValueCustom executionNote prerequisites TestNegative) }


{-| Patient's HIV test, negative, carrying the partner signs: partner positive,
not taking ARVs.
-}
withHIVTestPartnerPositiveSigns : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestPartnerPositiveSigns executionNote prerequisites =
    withHIVTestNegativeWithSigns executionNote prerequisites (EverySet.singleton PartnerHIVPositive)


{-| Patient's HIV test answered as known as positive, still carrying the
negative result and the partner signs of the test that answer replaced.
-}
withHIVTestKnownAsPositiveCarryingSigns : PrenatalMeasurements -> PrenatalMeasurements
withHIVTestKnownAsPositiveCarryingSigns =
    withHIVTestNegativeWithSigns TestNoteKnownAsPositive immediateResultPrerequisites (EverySet.singleton PartnerHIVPositive)


{-| Hemoglobin test, run today with the given count, immediate result. The
anemia/malaria-with-anemia diagnoses gate on `immediateResult .hemoglobinTest`,
so the `PrerequisiteImmediateResult` prerequisite is required just like the
disease labs.
-}
hemoglobinTestValueWith : Float -> HemoglobinTestValue
hemoglobinTestValueWith count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , hemoglobinCount = Just count
    }


withHemoglobin : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHemoglobin count measurements =
    { measurements | hemoglobinTest = wrapMeasurement (hemoglobinTestValueWith count) }


{-| Hemoglobin test with the given count entered NON-immediately
(`testPrerequisites = Nothing`). `resolveHemoglobinCount` reads `hemoglobinCount`
directly, so the count is still available, but `immediateResult .hemoglobinTest`
is False -- so the `*InitialPhase` anemia matcher (which requires the immediate
result) fails and the diagnosis routes to the `*RecurrentPhase` variant.
-}
hemoglobinTestValueNonImmediate : Float -> HemoglobinTestValue
hemoglobinTestValueNonImmediate count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , hemoglobinCount = Just count
    }


withHemoglobinNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHemoglobinNonImmediate count measurements =
    { measurements | hemoglobinTest = wrapMeasurement (hemoglobinTestValueNonImmediate count) }


{-| Vitals with the given systolic/diastolic blood pressure.
-}
withVitals : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withVitals sys dia measurements =
    { measurements | vitals = wrapMeasurement (TestFixtures.vitalsValueWith sys dia) }


{-| Vitals with a normal initial reading (120/80, so the _initial_ BP is not
itself high) and the given REPEATED systolic/diastolic blood pressure. The
recurrent-phase preeclampsia/hypertension matchers read `sysRepeated`/
`diaRepeated`; `repeatedTestForMarginalBloodPressure` needs BOTH present and
fires when `diaRepeated >= 90 || sysRepeated >= 140`.
-}
vitalsValueRepeatedWith : Float -> Float -> VitalsValue
vitalsValueRepeatedWith sysRepeated diaRepeated =
    { sys = Just 120
    , dia = Just 80
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    , sysRepeated = Just sysRepeated
    , diaRepeated = Just diaRepeated
    }


withVitalsRepeated : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withVitalsRepeated sysRepeated diaRepeated measurements =
    { measurements | vitals = wrapMeasurement (vitalsValueRepeatedWith sysRepeated diaRepeated) }


{-| Urine dipstick with the given protein level, immediate result.
`highUrineProteinInitialPhase` also gates on `PrerequisiteImmediateResult`.
-}
urineDipstickValueWith : ProteinValue -> UrineDipstickTestValue
urineDipstickValueWith protein =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , protein = Just protein
    , ph = Nothing
    , glucose = Nothing
    , leukocytes = Nothing
    , nitrite = Nothing
    , urobilinogen = Nothing
    , haemoglobin = Nothing
    , ketone = Nothing
    , bilirubin = Nothing
    }


withUrineProtein : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
withUrineProtein protein measurements =
    { measurements | urineDipstickTest = wrapMeasurement (urineDipstickValueWith protein) }


{-| Random blood sugar test with the given sugar count, entered NON-immediately.

`diabetesBySugarCount` reads `PrerequisiteFastFor12h`: when present it diagnoses
diabetes at sugarCount > 126; here the prerequisite set holds ONLY
`PrerequisiteFastFor12h` (NOT `PrerequisiteImmediateResult`), so a count above
126 satisfies `diabetesBySugarCount` while `immediateResult .randomBloodSugarTest`
stays False. The `*InitialPhase` diabetes matcher (which requires the immediate
result via `diabetesDiagnosedInitialPhase`) therefore fails and the diagnosis
routes to the `*RecurrentPhase` variant, which gates on `diabetesDiagnosedAnyPhase`
(no immediate-result requirement).

-}
randomBloodSugarValueNonImmediate : Float -> RandomBloodSugarTestValue encounterId
randomBloodSugarValueNonImmediate count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Just (EverySet.singleton PrerequisiteFastFor12h)
    , sugarCount = Just count
    , originatingEncounter = Nothing
    }


withRandomBloodSugarNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withRandomBloodSugarNonImmediate count measurements =
    { measurements | randomBloodSugarTest = wrapMeasurement (randomBloodSugarValueNonImmediate count) }


{-| Blood group / Rhesus test with the given rhesus result, entered
NON-immediately. `rhesusNegativeDiagnosed` reads `.rhesus` directly (no immediate
requirement), but `immediateResult .bloodGpRsTest` is False, so the
`DiagnosisRhesusNegativeInitialPhase` matcher fails and the diagnosis routes to
`DiagnosisRhesusNegativeRecurrentPhase`.
-}
bloodGpRsValueNonImmediate : Rhesus -> BloodGpRsTestValue encounterId
bloodGpRsValueNonImmediate rhesus =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , bloodGroup = Nothing
    , rhesus = Just rhesus
    , originatingEncounter = Nothing
    }


withBloodGpRsNonImmediate : Rhesus -> PrenatalMeasurements -> PrenatalMeasurements
withBloodGpRsNonImmediate rhesus measurements =
    { measurements | bloodGpRsTest = wrapMeasurement (bloodGpRsValueNonImmediate rhesus) }


{-| HIV PCR test carrying the given viral load, entered NON-immediately.
`hivDetectableViralLoadDiagnosed` reads `.hivViralLoad` and fires when it is
20 or above (a detectable viral load); with `testPrerequisites = Nothing`,
`immediateResult .hivPCRTest` is False, so the
`DiagnosisHIVDetectableViralLoadInitialPhase` matcher fails and the diagnosis
routes to `DiagnosisHIVDetectableViralLoadRecurrentPhase`.
-}
hivPCRValueNonImmediate : Float -> HIVPCRTestValue
hivPCRValueNonImmediate viralLoad =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , hivViralLoadStatus = Just ViralLoadDetectable
    , hivViralLoad = Just viralLoad
    }


withHIVPCRNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHIVPCRNonImmediate viralLoad measurements =
    { measurements | hivPCRTest = wrapMeasurement (hivPCRValueNonImmediate viralLoad) }


{-| Core physical exam carrying the given lungs/abdomen severe-preeclampsia
signs (everything else empty). `severePreeclampsiaSigns` reads
`Wheezes`/`Crackles` in `lungs` or `TPRightUpper` in `abdomen`.
-}
corePhysicalExamValueWith : EverySet LungsCPESign -> EverySet AbdomenCPESign -> CorePhysicalExamValue
corePhysicalExamValueWith lungs abdomen =
    { hairHead = EverySet.empty
    , eyes = EverySet.empty
    , heart = EverySet.empty
    , heartMurmur = False
    , neck = EverySet.empty
    , lungs = lungs
    , abdomen = abdomen
    , hands = EverySet.empty
    , legs = EverySet.empty
    }


{-| Core physical exam carrying a severe-preeclampsia sign (`TPRightUpper` in
the abdomen), so `severePreeclampsiaSigns` fires.
-}
withSeverePreeclampsiaSigns : PrenatalMeasurements -> PrenatalMeasurements
withSeverePreeclampsiaSigns measurements =
    { measurements
        | corePhysicalExam =
            wrapMeasurement
                (corePhysicalExamValueWith EverySet.empty (EverySet.singleton TPRightUpper))
    }


{-| Danger-signs measurement carrying the given antenatal danger signs.
-}
dangerSignsValueWith : EverySet DangerSign -> DangerSignsValue
dangerSignsValueWith signs =
    { signs = signs
    , postpartumMother = EverySet.empty
    , postpartumChild = EverySet.empty
    }


withDangerSigns : EverySet DangerSign -> PrenatalMeasurements -> PrenatalMeasurements
withDangerSigns signs measurements =
    { measurements | dangerSigns = wrapMeasurement (dangerSignsValueWith signs) }


{-| Run the function under test on an initial-phase nurse encounter with the
given measurements. Hemoglobin is left unset throughout, so malaria stays at
the plain (non-anemia) variant.
-}
diagnoseNurse : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse measurements =
    generatePrenatalDiagnosesForNurse (testAssembled measurements)


{-| Like `diagnoseNurse`, after one earlier nurse encounter, four weeks back,
holding the given diagnoses and measurements.
-}
diagnoseNurseAfter : List PrenatalDiagnosis -> PrenatalMeasurements -> PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurseAfter previousDiagnoses previousMeasurements measurements =
    let
        assembled =
            testAssembled measurements

        previousEncounter =
            { startDate = Date.add Date.Weeks -4 currentDate
            , diagnoses = EverySet.fromList previousDiagnoses
            , pastDiagnoses = EverySet.empty
            , measurements = previousMeasurements
            }
    in
    generatePrenatalDiagnosesForNurse
        { assembled | nursePreviousEncountersData = [ previousEncounter ] }


{-| The four lab-driven disease diagnoses under test, in their plain
initial-phase form.
-}
diseaseDiagnoses : List PrenatalDiagnosis
diseaseDiagnoses =
    [ DiagnosisHIVInitialPhase
    , DiagnosisSyphilisInitialPhase
    , DiagnosisMalariaInitialPhase
    , DiagnosisHepatitisBInitialPhase
    ]


generatePrenatalDiagnosesForNurseLabsTest : Test
generatePrenatalDiagnosesForNurseLabsTest =
    describe "generatePrenatalDiagnosesForNurse - lab-driven disease diagnoses (initial-phase nurse encounter, ANC Nurse sheet)"
        [ test "HIV Lab = Positive -> DiagnosisHIVInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisHIVInitialPhase
                    |> Expect.equal True
        , test "Syphilis Lab = Positive -> DiagnosisSyphilisInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withSyphilisTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisSyphilisInitialPhase
                    |> Expect.equal True
        , test "Malaria RDT = Positive -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "Hep B Lab = Positive -> DiagnosisHepatitisBInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHepatitisBTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisHepatitisBInitialPhase
                    |> Expect.equal True
        , test "all four disease labs negative -> none of the four disease diagnoses present (ANC Nurse sheet negative controls)" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVTest TestNegative
                            |> withSyphilisTest TestNegative
                            |> withMalariaTest TestNegative
                            |> withHepatitisBTest TestNegative
                            |> diagnoseNurse
                in
                List.filter (\diagnosis -> EverySet.member diagnosis diagnoses) diseaseDiagnoses
                    |> Expect.equal []
        ]



-- GROUP 1 -- CHW ASSESSMENT
--
-- `generatePrenatalAssesmentForChw` collapses to: any danger sign present ->
-- HighRisk, otherwise Normal. Oracle: ANC CHW tab ("any danger sign present ->
-- refer"). Uses a regular CHW antenatal encounter (`ChwFirstEncounter`), whose
-- danger signs come from the `.signs` set of the danger-signs measurement.


generatePrenatalAssesmentForChwTest : Test
generatePrenatalAssesmentForChwTest =
    describe "generatePrenatalAssesmentForChw - CHW danger-sign assessment (ANC CHW tab)"
        [ test "no danger-signs measurement -> AssesmentNormalPregnancy" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> chwAssembled
                    |> generatePrenatalAssesmentForChw
                    |> Expect.equal AssesmentNormalPregnancy
        , test "danger sign present (VaginalBleeding) -> AssesmentHighRiskPregnancy" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withDangerSigns (EverySet.singleton VaginalBleeding)
                    |> chwAssembled
                    |> generatePrenatalAssesmentForChw
                    |> Expect.equal AssesmentHighRiskPregnancy
        ]



-- GROUP 2 -- ANEMIA (nurse)
--
-- Hb comes from `measurements.hemoglobinTest`. Oracle: WHO pregnancy / Labs
-- tab -- severe <7, moderate 7..<11, none >=11. Anemia only fires when malaria
-- is NOT positive (here malaria is left unset, so never positive).


generatePrenatalDiagnosesForNurseAnemiaTest : Test
generatePrenatalDiagnosesForNurseAnemiaTest =
    describe "generatePrenatalDiagnosesForNurse - anemia by hemoglobin count (no malaria; WHO Labs tab)"
        [ test "Hb 9 (7..<11), no malaria -> DiagnosisModerateAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHemoglobin 9
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisModerateAnemiaInitialPhase
                    |> Expect.equal True
        , test "Hb 6 (<7), no malaria, no anemia-complication danger signs -> DiagnosisSevereAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHemoglobin 6
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisSevereAnemiaInitialPhase
                    |> Expect.equal True
        , test "Hb 12 (>=11) -> neither moderate nor severe anemia present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobin 12
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                , EverySet.member DiagnosisSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( False, False )
        ]



-- GROUP 3 -- MALARIA WITH ANEMIA (nurse)
--
-- Positive malaria RDT combined with an anemic Hb. Hb 7..<11 -> moderate
-- (MalariaWithAnemia); Hb <7 -> MalariaWithSevereAnemia. Plain anemia does
-- not fire because anemia requires malaria to be negative (expected).


generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest : Test
generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest =
    let
        -- RDT positive and Hb 9: malaria with mild to moderate anemia.
        currentMalariaWithAnemia =
            emptyPrenatalMeasurements
                |> withMalariaTest TestPositive
                |> withHemoglobin 9

        -- Every malaria diagnosis, so a second one firing alongside shows up.
        malariaVariants =
            EverySet.toList
                >> List.filter
                    (\diagnosis ->
                        List.member diagnosis
                            [ DiagnosisMalariaInitialPhase
                            , DiagnosisMalariaRecurrentPhase
                            , DiagnosisMalariaMedicatedContinuedInitialPhase
                            , DiagnosisMalariaMedicatedContinuedRecurrentPhase
                            , DiagnosisMalariaWithAnemiaInitialPhase
                            , DiagnosisMalariaWithAnemiaRecurrentPhase
                            , DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase
                            , DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase
                            , DiagnosisMalariaWithSevereAnemiaInitialPhase
                            , DiagnosisMalariaWithSevereAnemiaRecurrentPhase
                            ]
                    )
    in
    describe "generatePrenatalDiagnosesForNurse - malaria combined with anemia"
        [ test "malaria RDT positive + Hb 9 (7..<11) -> DiagnosisMalariaWithAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 9
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaWithAnemiaInitialPhase
                    |> Expect.equal True
        , test "malaria RDT positive + Hb 6 (<7) -> DiagnosisMalariaWithSevereAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 6
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaWithSevereAnemiaInitialPhase
                    |> Expect.equal True
        , test "after a treated malaria with severe anemia, Hb 9 -> the medicated-continued diagnosis only" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ]
        , test "after a treated malaria with severe anemia, Hb 9 at a recurrent phase -> the medicated-continued diagnosis only" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTestNonImmediate
                    |> withHemoglobinNonImmediate 9
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaRecurrentPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase ]
        , test "after a treated malaria with anemia, Hb 9 -> the medicated-continued diagnosis only" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithAnemiaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ]
        , test "after an untreated malaria with severe anemia, Hb 9 -> a new episode" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaInitialPhase ] emptyPrenatalMeasurements
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaInitialPhase ]
        , test "after a treated malaria, Hb 12 -> the medicated-continued diagnosis only" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 12
                    |> diagnoseNurseAfter [ DiagnosisMalariaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaMedicatedContinuedInitialPhase ]
        ]



-- GROUP 4 -- MODERATE PRE-ECLAMPSIA (nurse)
--
-- `DiagnosisModeratePreeclampsiaInitialPhase` requires EGA in [20,37) AND high
-- blood pressure AND (high urine protein OR edema). FINDING-adjacent nuance:
-- the code's `highBloodPressureCondition dia sys = dia >= 110 || sys >= 160`
-- uses the SEVERE BP threshold (>=160/110), NOT the clinical >=140/90 starting
-- point at which pre-eclampsia screening normally begins. EGA is set to ~28
-- weeks so the result is insensitive to the [20,37) boundaries.


diagnoseNurse28Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse28Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled28Weeks measurements)


{-| Run the function under test at EGA ~16 weeks (< 20).
-}
diagnoseNurse16Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse16Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled16Weeks measurements)


{-| Run the function under test at EGA ~38 weeks (>= 37).
-}
diagnoseNurse38Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse38Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled38Weeks measurements)


generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest : Test
generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest =
    describe "generatePrenatalDiagnosesForNurse - moderate pre-eclampsia (EGA ~28 weeks)"
        [ test "BP 165/115 (>=160/110) + urine protein +1 -> DiagnosisModeratePreeclampsiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal True

        -- FINDING: marginal BP 145/95 is hypertensive by the clinical
        -- >=140/90 definition, but the code's "moderate pre-eclampsia" gate
        -- demands the SEVERE >=160/110 threshold, so no diagnosis fires here.
        , test "BP 145/95 (marginal, <160/110) + urine protein +1 -> DiagnosisModeratePreeclampsiaInitialPhase NOT present (severe BP threshold)" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 145 95
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal False
        ]



-- GROUP A -- RECURRENT-PHASE DISEASE DIAGNOSES (nurse)
--
-- KEY MECHANISM: every non-postpartum nurse encounter offers BOTH the
-- `*InitialPhase` and `*RecurrentPhase` variant of each disease in its
-- candidate list (`resolveLabResultsAndExaminationDiagnoses`). The Initial
-- variant requires the lab's result to be immediate
-- (`testedPositiveAt ... && immediateResult ...`); the Recurrent variant
-- matches a positive test WITHOUT that requirement, additionally gated on
-- `not (diagnosed the Initial variant)`. So a positive lab entered WITHOUT the
-- `PrerequisiteImmediateResult` prerequisite (here: `testPrerequisites =
-- Nothing`) fails Initial and lands on Recurrent on the SAME initial-phase
-- `NurseEncounter`.
--
-- Oracle = ANC Nurse tab: a positive disease lab produces that disease's
-- diagnosis. The disease-level mapping (HIV+ -> an HIV diagnosis) is the
-- oracle; that a NON-immediate result lands on the Recurrent (not Initial)
-- variant is the structural assertion.


generatePrenatalDiagnosesForNurseRecurrentLabsTest : Test
generatePrenatalDiagnosesForNurseRecurrentLabsTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase disease diagnoses (positive lab WITHOUT immediate result)"
        [ test "HIV positive, non-immediate -> DiagnosisHIVRecurrentPhase present AND DiagnosisHIVInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHIVRecurrentPhase diagnoses
                , EverySet.member DiagnosisHIVInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Syphilis positive, non-immediate -> DiagnosisSyphilisRecurrentPhase present AND DiagnosisSyphilisInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withSyphilisTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisSyphilisRecurrentPhase diagnoses
                , EverySet.member DiagnosisSyphilisInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Malaria RDT positive, non-immediate -> DiagnosisMalariaRecurrentPhase present AND DiagnosisMalariaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Hep B positive, non-immediate -> DiagnosisHepatitisBRecurrentPhase present AND DiagnosisHepatitisBInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHepatitisBTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHepatitisBRecurrentPhase diagnoses
                , EverySet.member DiagnosisHepatitisBInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP B -- RECURRENT MODERATE PRE-ECLAMPSIA (nurse)
--
-- `moderatePreeclampsiaByMeasurementsRecurrentPhase` is
--   (highBloodPressure && highUrineProtein)
--     || (repeatedTestForMarginalBloodPressure && (edema || highUrineProtein)).
-- Here the *initial* reading is normal (120/80, so `highBloodPressure` is
-- False and the Initial-phase variant does not fire), but the REPEATED reading
-- is marginal (`sysRepeated` 145 >= 140), so `repeatedTestForMarginalBloodPressure`
-- is True; combined with urine protein +1 this satisfies the recurrent
-- measurement condition. EGA ~28 weeks sits inside the [20,37) gate.
--
-- This fires on the plain initial-phase `NurseEncounter`: that encounter's
-- candidate list includes `DiagnosisModeratePreeclampsiaRecurrentPhase`, the
-- Initial variant does not fire (normal initial BP), there is no prior
-- diagnosis, and in `applyDiagnosesHierarchy` moderate-preeclampsia-recurrent
-- (rank 11) outranks the competing gestational-hypertension-after-recheck
-- (rank 1) that the marginal repeated BP also produces.


generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent moderate pre-eclampsia (EGA ~28 weeks)"
        [ test "repeated BP marginal (sysRepeated 145) + urine protein +1, normal initial BP -> DiagnosisModeratePreeclampsiaRecurrentPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhase
                    |> Expect.equal True
        ]



-- GROUP C -- RECURRENT-PHASE ANEMIA / MALARIA-WITH-ANEMIA (nurse)
--
-- Same mechanism as GROUP A: the hemoglobin (and malaria) result is entered
-- NON-immediately (`testPrerequisites = Nothing`). `resolveHemoglobinCount`
-- reads the count regardless, so the anemia condition still matches, but
-- `immediateResult .hemoglobinTest` is False, so the `*InitialPhase` matcher
-- fails and the diagnosis routes to the `*RecurrentPhase` variant.
--
-- Oracle = WHO Labs tab: severe Hb < 7, moderate 7..<11; combined with a
-- positive malaria RDT -> the malaria-with-(severe-)anemia diagnoses.


generatePrenatalDiagnosesForNurseAnemiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseAnemiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase anemia / malaria-with-anemia (non-immediate labs)"
        [ test "Hb 9 (7..<11) non-immediate, no malaria -> DiagnosisModerateAnemiaRecurrentPhase present AND DiagnosisModerateAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobinNonImmediate 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisModerateAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Hb 6 (<7) non-immediate, no malaria, no anemia-complication signs -> DiagnosisSevereAnemiaRecurrentPhase present AND DiagnosisSevereAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobinNonImmediate 6
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisSevereAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "malaria RDT positive + Hb 9 (7..<11), both non-immediate -> DiagnosisMalariaWithAnemiaRecurrentPhase present AND DiagnosisMalariaWithAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> withHemoglobinNonImmediate 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaWithAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "malaria RDT positive + Hb 6 (<7), both non-immediate -> DiagnosisMalariaWithSevereAnemiaRecurrentPhase present AND DiagnosisMalariaWithSevereAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> withHemoglobinNonImmediate 6
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithSevereAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaWithSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP D -- RECURRENT-PHASE DIABETES / RHESUS (nurse, non-immediate labs)
--
-- Diabetes: `diabetesBySugarCount` reads `PrerequisiteFastFor12h` (here the
-- ONLY prerequisite, so the result is NON-immediate); a count > 126 satisfies
-- it. The `*InitialPhase` matcher gates on `immediateResult`, so it fails, and
-- the diagnosis routes to the recurrent variant. The EGA gate splits the
-- recurrent diabetes diagnosis: EGA <= 20 -> DiagnosisDiabetesRecurrentPhase;
-- EGA > 20 -> DiagnosisGestationalDiabetesRecurrentPhase. `diagnoseNurse` is at
-- EGA exactly 20 (<= 20), so the code produces DiagnosisDiabetesRecurrentPhase.
--
-- Rhesus: `rhesusNegativeDiagnosed` reads `.rhesus == RhesusNegative` directly;
-- non-immediate entry routes to DiagnosisRhesusNegativeRecurrentPhase.


generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest : Test
generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase diabetes / rhesus (non-immediate labs)"
        [ test "random blood sugar 200 (fasting, > 126) non-immediate, EGA <= 20 -> DiagnosisDiabetesRecurrentPhase present AND DiagnosisDiabetesInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withRandomBloodSugarNonImmediate 200
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisDiabetesRecurrentPhase diagnoses
                , EverySet.member DiagnosisDiabetesInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "blood group / Rh test rhesus = RhesusNegative, non-immediate -> DiagnosisRhesusNegativeRecurrentPhase present AND DiagnosisRhesusNegativeInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withBloodGpRsNonImmediate RhesusNegative
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisRhesusNegativeRecurrentPhase diagnoses
                , EverySet.member DiagnosisRhesusNegativeInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP E -- HYPERTENSION AFTER RECHECK (nurse)
--
-- A repeated marginal BP reading (`sysRepeated` 145 >= 140, normal initial
-- reading) with no proteinuria fires `repeatedTestForMarginalBloodPressure`,
-- which (without proteinuria/edema) yields hypertension-after-recheck rather
-- than recurrent pre-eclampsia. The EGA gate splits the two variants:
--   * EGA < 20  -> DiagnosisChronicHypertensionAfterRecheck
--   * EGA >= 20 -> DiagnosisGestationalHypertensionAfterRecheck
-- That EGA cutoff (chronicHypertensionByMeasurementsAfterRecheck uses
-- egaWeeks < 20; gestationalHypertensionByMeasurementsAfterRecheck uses
-- egaWeeks >= 20) is the ONLY thing distinguishing chronic from gestational
-- here -- no prior-encounter / medical-history data is needed.
--
-- These are the surviving (top) blood-pressure diagnosis after
-- `applyHypertensionlikeDiagnosesHierarchy`: with normal initial BP and no
-- proteinuria, no higher-ranked BP diagnosis competes.


generatePrenatalDiagnosesForNurseHypertensionRecheckTest : Test
generatePrenatalDiagnosesForNurseHypertensionRecheckTest =
    describe "generatePrenatalDiagnosesForNurse - hypertension after recheck (repeated marginal BP, no proteinuria)"
        [ test "EGA >= 20, repeated BP marginal (sysRepeated 145), normal initial BP, no proteinuria -> DiagnosisGestationalHypertensionAfterRecheck present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisGestationalHypertensionAfterRecheck
                    |> Expect.equal True
        , test "EGA < 20 (~16 weeks), repeated BP marginal (sysRepeated 145), normal initial BP, no proteinuria -> DiagnosisChronicHypertensionAfterRecheck present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> diagnoseNurse16Weeks
                    |> EverySet.member DiagnosisChronicHypertensionAfterRecheck
                    |> Expect.equal True
        ]



-- GROUP F -- SEVERE PRE-ECLAMPSIA RECURRENT (nurse, EGA < 37)
--
-- `severePreeclampsiaRecurrentPhase` =
--   (initial BP >=160/110 OR repeated BP >=160/110)
--     && highUrineProtein && severePreeclampsiaSigns.
-- The initial reading is normal (120/80) and the REPEATED reading is severe
-- (165/115 >= 160/110), so the recurrent (not initial) severe-BP branch fires;
-- combined with urine protein and a severe-preeclampsia core-physical-exam sign
-- (TPRightUpper in the abdomen) the diagnosis fires. EGA ~28 weeks (< 37) keeps
-- it off the EGA37+ emergency-referral variant.


generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent severe pre-eclampsia (EGA ~28 weeks, < 37)"
        [ test "repeated BP severe (165/115 >=160/110) + urine protein +1 + severe-preeclampsia sign, normal initial BP -> DiagnosisSeverePreeclampsiaRecurrentPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 165 115
                    |> withUrineProtein ProteinPlus1
                    |> withSeverePreeclampsiaSigns
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisSeverePreeclampsiaRecurrentPhase
                    |> Expect.equal True
        ]



-- GROUP G -- RECURRENT HIV DETECTABLE VIRAL LOAD (nurse)
--
-- `hivDetectableViralLoadDiagnosed` reads `.hivViralLoad` and fires when it is
-- >= 20 (a detectable viral load). Entered non-immediately
-- (`testPrerequisites = Nothing`), `immediateResult .hivPCRTest` is False, so
-- the `*InitialPhase` matcher fails and the diagnosis routes to the
-- `*RecurrentPhase` variant.


generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest : Test
generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent HIV detectable viral load (non-immediate PCR)"
        [ test "HIV PCR viral load 1000 (>= 20, detectable), non-immediate -> DiagnosisHIVDetectableViralLoadRecurrentPhase present AND DiagnosisHIVDetectableViralLoadInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVPCRNonImmediate 1000
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHIVDetectableViralLoadRecurrentPhase diagnoses
                , EverySet.member DiagnosisHIVDetectableViralLoadInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]


{-| A viral load answered "undetectable" is under 20 copies, so it is saved
without a number. A number typed before stays on the form, so a mistaken Yes can
be undone, and a record that still holds one is not diagnosed from it.
-}
hivViralLoadUndetectableTest : Test
hivViralLoadUndetectableTest =
    let
        savedDetectable =
            hivPCRValueNonImmediate 5000

        valueAfter msgs =
            msgs
                |> List.foldl
                    (\msg model ->
                        update dummyDate SiteRwanda (toEntityUuid "encounter") emptyModelIndexedDb msg model
                            |> (\( updated, _, _ ) -> updated)
                    )
                    emptyModel
                |> .laboratoryData
                |> .hivPCRTestForm
                |> toHIVPCRTestValueWithDefault (Just savedDetectable)

        statusAndLoad value =
            ( value.hivViralLoadStatus, value.hivViralLoad )

        detectableDiagnosed value =
            { emptyPrenatalMeasurements | hivPCRTest = wrapMeasurement value }
                |> diagnoseNurse
                |> EverySet.member DiagnosisHIVDetectableViralLoadRecurrentPhase
    in
    describe "HIV PCR viral load answered undetectable"
        [ test "first visit: Yes on a saved detectable result saves no number" <|
            \_ ->
                valueAfter [ SetHIVViralLoadUndetectable True ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadUndetectable, Nothing ))
        , test "first visit: Yes then No on a saved detectable result keeps its number" <|
            \_ ->
                valueAfter [ SetHIVViralLoadUndetectable True, SetHIVViralLoadUndetectable False ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadDetectable, Just 5000 ))
        , test "first visit: a typed number survives a mistaken Yes" <|
            \_ ->
                valueAfter [ SetHIVViralLoad "300", SetHIVViralLoadUndetectable True, SetHIVViralLoadUndetectable False ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadDetectable, Just 300 ))
        , test "lab results: a saved detectable result answered undetectable saves no number" <|
            \_ ->
                { emptyHIVPCRResultForm | hivViralLoadStatus = Just ViralLoadUndetectable }
                    |> toHIVPCRResultValueWithDefault (Just savedDetectable)
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadUndetectable, Nothing ))
        , test "a record answered undetectable is not diagnosed from the number it holds" <|
            \_ ->
                ( detectableDiagnosed { savedDetectable | hivViralLoadStatus = Just ViralLoadUndetectable }
                , detectableDiagnosed savedDetectable
                )
                    |> Expect.equal ( False, True )
        ]


{-| The discordant-partnership diagnosis carries an Initial and a Recurrent
variant, and only the Recurrent one prescribes PrEP at the recurrent phase. The
variant has to follow the test that produced the match: the partner's own test
when the match came from there, and the patient's test when it came from the
partner signs recorded on it. A match from the partner's test also needs the
patient's own result, so that result must be known at the initial phase too -
run with an immediate result, or taken from the patient's history.
-}
generatePrenatalDiagnosesForNurseDiscordantPartnershipTest : Test
generatePrenatalDiagnosesForNurseDiscordantPartnershipTest =
    let
        discordantPartnershipPhases measurements =
            let
                diagnoses =
                    diagnoseNurse measurements
            in
            ( EverySet.member DiagnosisDiscordantPartnershipInitialPhase diagnoses
            , EverySet.member DiagnosisDiscordantPartnershipRecurrentPhase diagnoses
            )
    in
    describe "generatePrenatalDiagnosesForNurse - discordant partnership phase"
        [ test "partner test positive and immediate, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner test positive and NOT immediate, patient HIV negative and immediate -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive and immediate, patient HIV negative and NOT immediate -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday deferredResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive and immediate, patient HIV negative from history -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunPreviously Nothing
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive after a point of care test, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive after the test was sent to the lab, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner test positive from history, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunPreviously Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's immediate HIV test, no partner test -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's HIV test taken from history -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunPreviously Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's HIV test sent to the lab -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive entered by a lab technician, the nurse has not answered the follow ups yet -> no diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner test positive entered by a lab technician, nurse answers partner not on ARVs -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "both tests sent to the lab, partner positive and patient negative entered there -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunConfirmedByLabTech deferredResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive entered by a lab technician, nurse answers partner on ARVs with a surpressed viral load -> no diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "patient known as positive, partner signs of the replaced negative test still on it -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestKnownAsPositiveCarryingSigns
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "patient known as positive, partner test positive -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestKnownAsPositiveCarryingSigns
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner then tested negative -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner test then sent to the lab -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday deferredResultPrerequisites Nothing Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs not on ARVs on the patient's HIV test, partner tested positive, on ARVs with a surpressed viral load -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday
                        immediateResultPrerequisites
                        (Just TestPositive)
                        (Just <| EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner test not run for lack of reagents -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteLackOfReagents Nothing Nothing Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        ]


{-| The partner's HIV result decides whether the nurse is asked for the
partner's status by hand: that question is drawn on the patient's own HIV test
form only while the result resolves to `TestIndeterminate`. A result a lab
technician entered carries its own execution note, and has to resolve like any
other, so the question the lab already answered is not asked again.
-}
resolvePartnerHIVTestResultTest : Test
resolvePartnerHIVTestResultTest =
    describe "resolvePartnerHIVTestResult"
        [ test "positive result entered by a lab technician -> TestPositive" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestPositive
        , test "negative result entered by a lab technician -> TestNegative" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestByLabTech TestNegative (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestNegative
        , test "positive result the nurse ran today -> TestPositive" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestPositive
        , test "no partner test -> TestIndeterminate" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestIndeterminate
        ]


{-| The line the progress report shows for a discordant couple states what the
partner's ARV and viral load status is. A lab technician can enter the partner's
result but not those answers, so until the nurse answers them there is no status
to state.
-}
resolveDiscordantCoupleStatusTest : Test
resolveDiscordantCoupleStatusTest =
    describe "resolveDiscordantCoupleStatus"
        [ test "partner positive by a lab technician, follow ups not answered yet -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        , test "partner positive by a lab technician, nurse answers partner not on ARVs -> not taking ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus False False)
        , test "partner positive by a lab technician, nurse answers partner on ARVs and surpressed -> taking ARVs, surpressed" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus True True)
        , test "partner signs on the patient's own HIV test -> status from those signs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus False False)
        , test "partner signs on the patient's own HIV test, partner then tested negative -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        , test "no partner test and no partner signs -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        ]


{-| The partner signs on the patient's own HIV test are asked only while the
partner has no test result. Once the partner is tested, the partner's test is
what the medication helper text and the HIV health education follow.
-}
partnerSignsAfterPartnerTestedTest : Test
partnerSignsAfterPartnerTestedTest =
    let
        medicationHelpers measurements =
            let
                assembled =
                    testAssembled measurements
            in
            resolveRequiredMedicationsSet English
                currentDate
                PrenatalEncounterPhaseInitial
                { assembled
                    | encounter =
                        { testEncounter | diagnoses = EverySet.singleton DiagnosisDiscordantPartnershipInitialPhase }
                }
                |> List.map (\( helper, _, _ ) -> helper)

        hivEducationTasks measurements =
            healthEducationFormInputsAndTasksForNurse English
                PrenatalEncounterPhaseInitial
                (\_ _ -> ())
                (testAssembled measurements)
                emptyHealthEducationForm
                |> Tuple.second
                |> List.length

        partnerOnARVsSurpressed =
            EverySet.fromList [ PartnerHIVPositive, PartnerTakingARV, PartnerSurpressedViralLoad ]
    in
    describe "partner signs on the patient's HIV test once the partner is tested"
        [ test "partner signs on ARVs, partner tested positive and not on ARVs -> helper for a partner not on ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> medicationHelpers
                    |> Expect.equal [ Translate.MedicationDistributionHelperDiscordantPartnershipNoARVs ]
        , test "partner signs on ARVs, no partner test -> helper for a partner on ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> medicationHelpers
                    |> Expect.equal [ Translate.MedicationDistributionHelperDiscordantPartnership ]
        , test "partner signs on ARVs and surpressed, partner tested negative -> partner testing education asked" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> hivEducationTasks
                    |> Expect.equal
                        (emptyPrenatalMeasurements
                            |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                            |> hivEducationTasks
                        )
        , test "partner signs on ARVs and surpressed, no partner test -> partner testing education not asked" <|
            \_ ->
                (emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> hivEducationTasks
                )
                    |> Expect.equal
                        ((emptyPrenatalMeasurements
                            |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                            |> hivEducationTasks
                         )
                            - 1
                        )
        ]



-- GROUP H -- EGA37+ RECURRENT PRE-ECLAMPSIA (nurse, EGA >= 37)
--
-- The same recurrent moderate / severe pre-eclampsia recipes as GROUP B / F,
-- but at EGA ~38 weeks (>= 37). At that EGA the recurrent pre-eclampsia
-- diagnoses are produced by `matchEmergencyReferalPrenatalDiagnosis` as the
-- `*EGA37Plus` emergency-referral variants instead.
--
--   * Moderate: repeated marginal BP (sysRepeated 145) + urine protein, normal
--     initial BP -> DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus.
--   * Severe: repeated severe BP (165/115) + urine protein + severe sign,
--     normal initial BP -> DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus.
--
-- The EGA37+ variants are the surviving (top) blood-pressure diagnosis after
-- `applyHypertensionlikeDiagnosesHierarchy` (ranks 31 / 41).


generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent pre-eclampsia EGA37+ (EGA ~38 weeks, >= 37)"
        [ test "repeated BP marginal (sysRepeated 145) + urine protein +1, normal initial BP, EGA >= 37 -> DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse38Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
                    |> Expect.equal True
        , test "repeated BP severe (165/115 >=160/110) + urine protein +1 + severe-preeclampsia sign, normal initial BP, EGA >= 37 -> DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 165 115
                    |> withUrineProtein ProteinPlus1
                    |> withSeverePreeclampsiaSigns
                    |> diagnoseNurse38Weeks
                    |> EverySet.member DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
                    |> Expect.equal True
        ]


suicideRiskDiagnosedBySignsTest : Test
suicideRiskDiagnosedBySignsTest =
    -- Oracle: the suicide-risk screening item (question 10). Any positive
    -- response (Option1/2/3) flags risk; Option0 (none) does not; an unanswered
    -- question 10 yields Nothing.
    let
        withQuestion10 option =
            Dict.fromList [ ( MentalHealthQuestion10, option ) ]
    in
    describe "suicideRiskDiagnosedBySigns"
        [ test "Q10 = Option0 (none) -> Just False" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption0) |> Expect.equal (Just False)
        , test "Q10 = Option1 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption1) |> Expect.equal (Just True)
        , test "Q10 = Option2 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption2) |> Expect.equal (Just True)
        , test "Q10 = Option3 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption3) |> Expect.equal (Just True)
        , test "no question 10 answered -> Nothing" <|
            \_ -> suicideRiskDiagnosedBySigns Dict.empty |> Expect.equal Nothing
        , test "only other questions answered (Q10 absent) -> Nothing" <|
            \_ -> suicideRiskDiagnosedBySigns (Dict.fromList [ ( MentalHealthQuestion1, MentalHealthQuestionOption3 ) ]) |> Expect.equal Nothing
        ]


{-| Abnormal vaginal discharge splits into three diagnoses by the two follow up
questions. Answering no to both is trichomonas or bacterial vaginosis; each
diagnosis then has a "continued" variant, chosen by whether the patient was
given that same diagnosis at an earlier nurse encounter. The plain variant
prescribes medication, the continued one refers to hospital.
-}
vaginalDischargeContinuedTest : Test
vaginalDischargeContinuedTest =
    let
        withVaginalDischarge questions measurements =
            { measurements
                | symptomReview =
                    wrapMeasurement
                        { symptoms = EverySet.singleton AbnormalVaginalDischarge
                        , symptomQuestions = questions
                        , flankPainSign = Nothing
                        }
            }

        -- No vaginal itching and no partner urethral discharge.
        bacterialVaginosis =
            withVaginalDischarge EverySet.empty

        gonorrhea =
            withVaginalDischarge (EverySet.singleton SymptomQuestionPartnerUrethralDischarge)

        diagnoseNurseAfterDiagnoses previousDiagnoses =
            diagnoseNurseAfter previousDiagnoses emptyPrenatalMeasurements
    in
    describe "generatePrenatalDiagnosesForNurse - abnormal vaginal discharge, continued variants"
        [ test "first episode -> the plain diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal True
        , test "diagnosed at an earlier encounter -> the continued diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosisContinued
                    |> Expect.equal True
        , test "diagnosed at an earlier encounter -> the plain diagnosis is not given as well" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal False
        , test "an earlier gonorrhea does not make a first episode continued" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosisContinued
                    |> Expect.equal False
        , test "an earlier gonorrhea leaves a first episode at the plain diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal True
        , test "an earlier bacterial vaginosis does not make a first gonorrhea continued" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> gonorrhea
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisGonorrheaContinued
                    |> Expect.equal False
        , test "gonorrhea diagnosed at an earlier encounter -> the continued diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> gonorrhea
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisGonorrheaContinued
                    |> Expect.equal True
        ]



-- HISTORY OF AN OLDER ENCOUNTER
--
-- A lab result can be entered for an encounter after a later encounter of the
-- pregnancy has started, and the older encounter is then diagnosed again. Its
-- history is the encounters started on an earlier day. There is one encounter a
-- day, so another encounter of the same day is not part of it either.


{-| A prenatal encounter as stored in the database: its id, start date, type,
diagnoses, the diagnoses reported to it from late lab results, and the
measurements taken at it.
-}
type alias StoredEncounter =
    { id : String
    , startDate : NominalDate
    , encounterType : PrenatalEncounterType
    , diagnoses : List PrenatalDiagnosis
    , pastDiagnoses : List PrenatalDiagnosis
    , measurements : PrenatalMeasurements
    }


storedNurseEncounter : String -> NominalDate -> List PrenatalDiagnosis -> PrenatalMeasurements -> StoredEncounter
storedNurseEncounter id startDate diagnoses measurements =
    { id = id
    , startDate = startDate
    , encounterType = NurseEncounter
    , diagnoses = diagnoses
    , pastDiagnoses = []
    , measurements = measurements
    }


{-| Assemble the encounter with the given id from a database holding the given
encounters of one pregnancy -- the way the pages and the assessment run after
a save build their data. EGA is fixed at ~20 weeks, as for `testAssembled`.
-}
assembledFromDb : String -> List StoredEncounter -> Maybe AssembledData
assembledFromDb encounterId storedEncounters =
    let
        encounters =
            List.map
                (\stored ->
                    ( toEntityUuid stored.id
                    , { testEncounter
                        | startDate = stored.startDate
                        , encounterType = stored.encounterType
                        , diagnoses = EverySet.fromList stored.diagnoses
                        , pastDiagnoses = EverySet.fromList stored.pastDiagnoses
                      }
                    )
                )
                storedEncounters

        db =
            { emptyModelIndexedDb
                | prenatalEncounters = Dict.fromList <| List.map (Tuple.mapSecond RemoteData.Success) encounters
                , prenatalEncountersByParticipant =
                    Dict.singleton testEncounter.participant (RemoteData.Success <| Dict.fromList encounters)
                , prenatalMeasurements =
                    Dict.fromList <|
                        List.map (\stored -> ( toEntityUuid stored.id, RemoteData.Success stored.measurements )) storedEncounters
                , individualParticipants = Dict.singleton testEncounter.participant (RemoteData.Success testParticipant)
                , people = Dict.singleton testParticipant.person (RemoteData.Success testPerson)
            }
    in
    generateAssembledData (toEntityUuid encounterId) db
        |> RemoteData.toMaybe
        |> Maybe.map (\assembled -> { assembled | globalLmpDate = Just lmpDate })


historyBeforeEncounterTest : Test
historyBeforeEncounterTest =
    let
        withSymptom symptom measurements =
            { measurements
                | symptomReview =
                    wrapMeasurement
                        { symptoms = EverySet.singleton symptom
                        , symptomQuestions = EverySet.empty
                        , flankPainSign = Nothing
                        }
            }

        weekAgo =
            Date.add Date.Weeks -1 currentDate

        weekLater =
            Date.add Date.Weeks 1 currentDate

        diagnosesOf encounterId storedEncounters =
            assembledFromDb encounterId storedEncounters
                |> Maybe.map (generatePrenatalDiagnosesForNurse >> EverySet.toList)

        hasDiagnosis diagnosis encounterId storedEncounters =
            diagnosesOf encounterId storedEncounters
                |> Maybe.map (List.member diagnosis)

        diagnosesAmong candidates encounterId storedEncounters =
            diagnosesOf encounterId storedEncounters
                |> Maybe.map (List.filter (\diagnosis -> List.member diagnosis candidates))

        pelvicPain =
            withSymptom PelvicPain emptyPrenatalMeasurements

        nauseaAndVomiting =
            withSymptom NauseaAndVomiting emptyPrenatalMeasurements
    in
    describe "generatePrenatalDiagnosesForNurse - history of an older encounter"
        [ test "pelvic pain at a LATER encounter does not make the older encounter's pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "older"
                    [ storedNurseEncounter "older" currentDate [] pelvicPain
                    , storedNurseEncounter "later" weekLater [] pelvicPain
                    ]
                    |> Expect.equal (Just False)
        , test "nausea and vomiting at a LATER encounter does not send the older encounter to hospital" <|
            \_ ->
                diagnosesAmong [ DiagnosisHyperemesisGravidumBySymptoms, DiagnosisSevereVomitingBySymptoms ]
                    "older"
                    [ storedNurseEncounter "older" currentDate [] nauseaAndVomiting
                    , storedNurseEncounter "later" weekLater [] nauseaAndVomiting
                    ]
                    |> Expect.equal (Just [])
        , test "malaria treated at a LATER encounter does not make the older encounter's malaria medicated-continued" <|
            \_ ->
                diagnosesAmong
                    [ DiagnosisMalariaInitialPhase
                    , DiagnosisMalariaMedicatedContinuedInitialPhase
                    , DiagnosisMalariaMedicatedContinuedRecurrentPhase
                    ]
                    "older"
                    [ storedNurseEncounter "older" currentDate [] (withMalariaTest TestPositive emptyPrenatalMeasurements)
                    , storedNurseEncounter "later" weekLater [ DiagnosisMalariaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    ]
                    |> Expect.equal (Just [ DiagnosisMalariaInitialPhase ])
        , test "pelvic pain at an EARLIER encounter makes the pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "current"
                    [ storedNurseEncounter "earlier" weekAgo [] pelvicPain
                    , storedNurseEncounter "current" currentDate [] pelvicPain
                    ]
                    |> Expect.equal (Just True)
        , test "pelvic pain at another encounter of the SAME day does not make the pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "current"
                    [ storedNurseEncounter "same-day" currentDate [] pelvicPain
                    , storedNurseEncounter "current" currentDate [] pelvicPain
                    ]
                    |> Expect.equal (Just False)
        , test "only CHW encounters started on an earlier day are in the history" <|
            \_ ->
                let
                    storedChwEncounter id startDate encounterType =
                        { id = id
                        , startDate = startDate
                        , encounterType = encounterType
                        , diagnoses = []
                        , pastDiagnoses = []
                        , measurements = emptyPrenatalMeasurements
                        }
                in
                assembledFromDb "current"
                    [ storedChwEncounter "chw-earlier" weekAgo ChwFirstEncounter
                    , storedChwEncounter "chw-same-day" currentDate ChwSecondEncounter
                    , storedNurseEncounter "current" currentDate [] emptyPrenatalMeasurements
                    , storedChwEncounter "chw-later" weekLater ChwThirdPlusEncounter
                    ]
                    |> Maybe.map (.chwPreviousMeasurementsWithDates >> List.map (\( date, encounterType, _ ) -> ( date, encounterType )))
                    |> Expect.equal (Just [ ( weekAgo, ChwFirstEncounter ) ])
        ]



-- BLOOD SMEAR AS THE MALARIA DIAGNOSIS
--
-- When the rapid test cannot be run, a blood smear is taken instead and read
-- at the lab; issue #631 introduced it as "a different way to diagnose
-- malaria". Whoever reads the smear, the smear result is the diagnosis.
--
-- Two people can record that result, and they leave DIFFERENT execution notes.
-- A nurse entering it keeps the nurse's reason for not running the RDT; a lab
-- technician confirming the run replaces the note with
-- `TestNoteRunConfirmedByLabTech` (`toMalariaResultValue`). Neither of them
-- writes a `testResult`, because the form shows the smear input instead.


{-| A malaria test the nurse did not run as an RDT, with a blood smear read in
its place — under the execution note the given reader leaves behind.
-}
malariaTestValueWithBloodSmear : TestExecutionNote -> BloodSmearResult -> MalariaTestValue
malariaTestValueWithBloodSmear executionNote bloodSmearResult =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Nothing
    , bloodSmearResult = bloodSmearResult
    , bloodSmearOrdered = True
    }


{-| A blood smear that also carries a rapid test result, the way a client that
does not know about the smear order writes one.
-}
withBloodSmearAndRapidTestResult : TestExecutionNote -> BloodSmearResult -> TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withBloodSmearAndRapidTestResult executionNote bloodSmearResult testResult measurements =
    let
        value =
            malariaTestValueWithBloodSmear executionNote bloodSmearResult
    in
    { measurements | malariaTest = wrapMeasurement { value | testResult = Just testResult } }


{-| The same record as written before `bloodSmearOrdered` existed: the smear
result is all that says a smear was taken.
-}
withLegacyBloodSmearAndRapidTestResult : TestExecutionNote -> BloodSmearResult -> TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withLegacyBloodSmearAndRapidTestResult executionNote bloodSmearResult testResult measurements =
    let
        value =
            malariaTestValueWithBloodSmear executionNote bloodSmearResult
    in
    { measurements
        | malariaTest =
            wrapMeasurement { value | testResult = Just testResult, bloodSmearOrdered = False }
    }


withBloodSmear : TestExecutionNote -> BloodSmearResult -> PrenatalMeasurements -> PrenatalMeasurements
withBloodSmear executionNote bloodSmearResult measurements =
    { measurements
        | malariaTest = wrapMeasurement (malariaTestValueWithBloodSmear executionNote bloodSmearResult)
    }


generatePrenatalDiagnosesForNurseBloodSmearTest : Test
generatePrenatalDiagnosesForNurseBloodSmearTest =
    describe "generatePrenatalDiagnosesForNurse - malaria read from a blood smear"
        [ test "smear + under the nurse's reason -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteLackOfReagents BloodSmearPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear + under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear +++ under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlusPlusPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear negative under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase absent" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearNegative
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , test "a smear still awaited by the lab diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteLackOfReagents BloodSmearPendingInput
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , -- A record that ordered a smear did not run the rapid test, so a
          -- rapid test result on it is one nobody entered for it. An older
          -- client that still writes one must not diagnose through it.
          test "a positive rapid test result on a record that ordered a smear diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmearAndRapidTestResult TestNoteRunConfirmedByLabTech BloodSmearNegative TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , -- Same record, written before the order was recorded: the smear
          -- result is the only thing that says a smear was taken.
          test "a positive rapid test result on a legacy smear record diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withLegacyBloodSmearAndRapidTestResult TestNoteRunConfirmedByLabTech BloodSmearNegative TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , test "smear + read by the lab technician, with Hb 9, is malaria WITH anemia - not anemia alone" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlus
                            |> withHemoglobin 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithAnemiaInitialPhase diagnoses
                , EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]


all : Test
all =
    describe "Prenatal Activity tests"
        [ measurementOutOfRangeTest
        , bmiToPrePregnancyClassificationTest
        , zscoreToPrePregnancyClassificationTest
        , resolveGWGClassificationForHealthyStartTest
        , generatePrenatalDiagnosesForNurseLabsTest
        , generatePrenatalDiagnosesForNurseRecurrentLabsTest
        , generatePrenatalAssesmentForChwTest
        , generatePrenatalDiagnosesForNurseAnemiaTest
        , generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest
        , generatePrenatalDiagnosesForNurseBloodSmearTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseAnemiaRecurrentTest
        , generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest
        , generatePrenatalDiagnosesForNurseHypertensionRecheckTest
        , generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest
        , hivViralLoadUndetectableTest
        , generatePrenatalDiagnosesForNurseDiscordantPartnershipTest
        , resolvePartnerHIVTestResultTest
        , resolveDiscordantCoupleStatusTest
        , partnerSignsAfterPartnerTestedTest
        , generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest
        , suicideRiskDiagnosedBySignsTest
        , vaginalDischargeContinuedTest
        , historyBeforeEncounterTest
        , repeatedTapOnSavedTest
        , diagnosisKnownAnyOfTest
        , obstetricHistoryFirstPregnancyTest
        , nextVisitTaskTest
        , lateLabResultSameDayTest
        , lateLabResultHypertensionTest
        , lateLabResultDiabetesTest
        , lateLabResultModeratePreeclampsiaTest
        , lateLabResultHyperemesisTest
        , lateLabResultAddsNothingTest
        , lateLabResultPreeclampsiaRiskTest
        , lateLabResultLabTechAndHistoryTest
        ]


{-| Next ANC Visit shows a date computed from the pregnancy's EGA, and only
saves that date. A pregnancy with no LMP has none, so the task must not be
asked: it could never be completed, and the visit could never be ended.

CHW visits 2 and 3 have no Pregnancy Dating activity, so an undated
pregnancy stays undated there.

-}
nextVisitTaskTest : Test
nextVisitTaskTest =
    let
        withEncounterType encounterType assembled =
            let
                encounter =
                    assembled.encounter
            in
            { assembled | encounter = { encounter | encounterType = encounterType } }

        undated assembled =
            { assembled | globalLmpDate = Nothing }

        asksNextVisit assembled =
            resolveNextStepsTasks currentDate assembled
                |> List.member NextStepsNextVisit

        -- The CHW's other Next Steps tasks on a visit without danger signs.
        withAppointmentAndFollowUp measurements =
            { measurements
                | appointmentConfirmation = wrapMeasurement { date = Date.add Date.Weeks 4 currentDate }
                , followUp =
                    wrapMeasurement
                        { options = EverySet.singleton OneDay
                        , resolutionDate = Nothing
                        , assesment = AssesmentNormalPregnancy
                        }
            }

        nextStepsCompleted assembled =
            activityCompleted currentDate SiteRwanda EverySet.empty assembled Backend.PrenatalActivity.Model.NextSteps
    in
    describe "Next Steps › Next ANC Visit"
        [ test "CHW visit 2 on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwSecondEncounter
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "CHW visit 3 on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwThirdPlusEncounter
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "a nurse visit on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "CHW visit 2 on an undated pregnancy completes Next Steps once its other tasks are saved" <|
            \_ ->
                testAssembled (withAppointmentAndFollowUp emptyPrenatalMeasurements)
                    |> withEncounterType ChwSecondEncounter
                    |> undated
                    |> nextStepsCompleted
                    |> Expect.equal True
        , test "CHW visit 2 on a dated pregnancy still needs Next ANC Visit to complete Next Steps" <|
            \_ ->
                testAssembled (withAppointmentAndFollowUp emptyPrenatalMeasurements)
                    |> withEncounterType ChwSecondEncounter
                    |> nextStepsCompleted
                    |> Expect.equal False
        , test "CHW visit 2 on a dated pregnancy asks for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwSecondEncounter
                    |> asksNextVisit
                    |> Expect.equal True
        , test "a nurse visit on a dated pregnancy asks for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> asksNextVisit
                    |> Expect.equal True
        ]


{-| Nothing was checked on this encounter, so a mistyped height, weight, MUAC or
fundal height saved without a word. Each is named on a warning now, and nothing
is saved until it is entered again.

A height measured at an earlier encounter is carried over and its input is not
drawn, so it is not asked about: there would be nothing on screen to correct.

-}
measurementOutOfRangeTest : Test
measurementOutOfRangeTest =
    let
        person =
            toEntityUuid "person"

        preSaveNutrition site carriedOverHeight height weight muac =
            let
                data =
                    emptyModel.examinationData

                form =
                    data.nutritionAssessmentForm

                model =
                    { emptyModel
                        | examinationData =
                            { data
                                | nutritionAssessmentForm =
                                    { form | height = height, weight = weight, muac = muac }
                            }
                    }

                ( updatedModel, _, appMsgs ) =
                    update (Date.fromCalendarDate 2026 Time.Jul 28)
                        site
                        (toEntityUuid "encounter")
                        emptyModelIndexedDb
                        (PreSaveNutritionAssessment person Nothing carriedOverHeight Nothing Nothing)
                        model
            in
            -- What the warning names, and whether anything was saved.
            ( updatedModel.warningPopupState, not <| List.isEmpty appMsgs )

        preSaveObstetrical palpable fundalHeight =
            let
                data =
                    emptyModel.examinationData

                form =
                    data.obstetricalExamForm

                model =
                    { emptyModel
                        | examinationData =
                            { data
                                | obstetricalExamForm =
                                    { form | fundalPalpable = palpable, fundalHeight = fundalHeight }
                            }
                    }

                ( updatedModel, _, appMsgs ) =
                    update (Date.fromCalendarDate 2026 Time.Jul 28)
                        SiteRwanda
                        (toEntityUuid "encounter")
                        emptyModelIndexedDb
                        (PreSaveObstetricalExam person Nothing Nothing)
                        model
            in
            ( updatedModel.warningPopupState, not <| List.isEmpty appMsgs )

        named measurements =
            Just (WarningPopupMeasurementOutOfRange measurements)
    in
    describe "the ANC save gate"
        [ test "a height of 1050 cm is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 1050) Nothing Nothing
                    |> Expect.equal ( named [ MeasurementHeight ], False )
        , test "a weight of 850 kg is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing Nothing (Just 850) Nothing
                    |> Expect.equal ( named [ MeasurementWeight ], False )
        , test "a MUAC of 125, being millimetres, is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing Nothing Nothing (Just 125)
                    |> Expect.equal ( named [ MeasurementMuac ], False )
        , test "Burundi holds MUAC in centimetres too, so 12.5 is within range there" <|
            \_ ->
                preSaveNutrition SiteBurundi Nothing Nothing Nothing (Just 12.5)
                    |> Expect.equal ( Nothing, True )
        , test "every one that is wrong is named, not just the first" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 1050) (Just 850) (Just 125)
                    |> Expect.equal
                        ( named [ MeasurementHeight, MeasurementWeight, MeasurementMuac ], False )
        , test "a height carried over from an earlier encounter is not asked about" <|
            \_ ->
                preSaveNutrition SiteRwanda (Just 165) (Just 1050) Nothing Nothing
                    |> Expect.equal ( Nothing, True )
        , test "measurements within range save with no warning" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 165) (Just 65) (Just 25)
                    |> Expect.equal ( Nothing, True )
        , test "a fundal height of 120 cm is named and saves nothing" <|
            \_ ->
                preSaveObstetrical (Just True) (Just 120)
                    |> Expect.equal ( named [ MeasurementFundalHeight ], False )
        , test "a fundal height within range saves with no warning" <|
            \_ ->
                preSaveObstetrical (Just True) (Just 30)
                    |> Expect.equal ( Nothing, True )
        , test "nothing is asked when the uterus cannot be felt" <|
            \_ ->
                preSaveObstetrical (Just False) (Just 120)
                    |> Expect.equal ( Nothing, True )
        ]


{-| "Known as positive" fires on every tap, including one that repeats the
answer shown. On a test loaded from the record the form in the model is still
empty, so the update acts on the form as displayed; otherwise a repeated No
would clear a saved test.
-}
repeatedTapOnSavedTest : Test
repeatedTapOnSavedTest =
    let
        encounterId =
            toEntityUuid "encounter"

        savedHIVTest =
            hivTestValueWith TestNegative

        savedPartnerHIVTest =
            partnerHIVTestValuePositive TestNoteRunToday immediateResultPrerequisites

        savedHepatitisBTest =
            hepatitisBTestValueWith TestNegative

        db =
            { emptyModelIndexedDb
                | prenatalMeasurements =
                    Dict.singleton encounterId
                        (RemoteData.Success
                            { emptyPrenatalMeasurements
                                | hivTest = wrapMeasurement savedHIVTest
                                , partnerHIVTest = wrapMeasurement savedPartnerHIVTest
                                , hepatitisBTest = wrapMeasurement savedHepatitisBTest
                            }
                        )
            }

        dataAfter msg =
            update dummyDate SiteRwanda encounterId db msg emptyModel
                |> (\( model, _, _ ) -> model.laboratoryData)

        noteAndResult value =
            ( value.executionNote, value.testResult )
    in
    describe "a repeated tap on known as positive, on a test loaded from the record"
        [ test "HIV test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetHIVTestFormBoolInput knownAsPositiveUpdateHIVTest False)).hivTestForm
                    |> toHIVTestValueUniversalWithDefault (Just savedHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestNegative ))
        , test "HIV test: Yes still replaces the run" <|
            \_ ->
                (dataAfter (SetHIVTestFormBoolInput knownAsPositiveUpdateHIVTest True)).hivTestForm
                    |> toHIVTestValueUniversalWithDefault (Just savedHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteKnownAsPositive, Nothing ))
        , test "partner HIV test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetPartnerHIVTestFormBoolInput knownAsPositiveUpdatePartnerHIVTest False)).partnerHIVTestForm
                    |> toPartnerHIVTestValueWithDefault (Just savedPartnerHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestPositive ))
        , test "hepatitis B test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetHepatitisBTestFormBoolInput knownAsPositiveUpdate False)).hepatitisBTestForm
                    |> toHepatitisBTestValueWithDefault (Just savedHepatitisBTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestNegative ))
        ]



-- WHAT THE ENCOUNTER A LATE LAB RESULT REPORTS TO ALREADY KNOWS
--
-- A lab result entered for an older encounter from a later one reports the
-- diagnosis it adds back to the later encounter, unless that encounter already
-- knows it. The older encounter's history cannot hold a diagnosis made at a
-- visit after it, so the question is asked of the later encounter.


diagnosisKnownAnyOfTest : Test
diagnosisKnownAnyOfTest =
    let
        weekAgo =
            Date.add Date.Weeks -1 currentDate

        weekLater =
            Date.add Date.Weeks 1 currentDate

        rhesusNegative =
            [ DiagnosisRhesusNegativeInitialPhase, DiagnosisRhesusNegativeRecurrentPhase ]

        older =
            storedNurseEncounter "older" weekAgo [] emptyPrenatalMeasurements

        rhesusAtMiddle =
            storedNurseEncounter "middle" currentDate [ DiagnosisRhesusNegativeRecurrentPhase ] emptyPrenatalMeasurements

        current =
            storedNurseEncounter "current" weekLater [] emptyPrenatalMeasurements

        known diagnoses encounterId storedEncounters =
            assembledFromDb encounterId storedEncounters
                |> Maybe.map (diagnosisKnownAnyOf diagnoses)
    in
    describe "diagnosisKnownAnyOf - what the encounter a late lab result reports to already knows"
        [ test "a diagnosis made at a visit between the older encounter and the current one is known to the current one" <|
            \_ ->
                known rhesusNegative "current" [ older, rhesusAtMiddle, current ]
                    |> Expect.equal (Just True)
        , test "the older encounter's own history cannot see that visit, which is why the current one is asked" <|
            \_ ->
                assembledFromDb "older" [ older, rhesusAtMiddle, current ]
                    |> Maybe.map (diagnosedPreviouslyAnyOf rhesusNegative)
                    |> Expect.equal (Just False)
        , test "a diagnosis reported to a visit between the older encounter and the current one is known to the current one" <|
            \_ ->
                known rhesusNegative "current" [ older, { rhesusAtMiddle | diagnoses = [], pastDiagnoses = [ DiagnosisRhesusNegativeRecurrentPhase ] }, current ]
                    |> Expect.equal (Just True)
        , test "a diagnosis made at the current encounter itself is known" <|
            \_ ->
                known rhesusNegative "current" [ older, { current | diagnoses = [ DiagnosisRhesusNegativeRecurrentPhase ] } ]
                    |> Expect.equal (Just True)
        , test "a diagnosis reported to the current encounter by an earlier late result is known" <|
            \_ ->
                known diabetesDiagnoses "current" [ older, { current | pastDiagnoses = [ DiagnosisDiabetesRecurrentPhase ] } ]
                    |> Expect.equal (Just True)
        , test "a diagnosis made nowhere in the pregnancy is not known" <|
            \_ ->
                known rhesusNegative "current" [ older, current ]
                    |> Expect.equal (Just False)
        ]


{-| Step 1 corrected to a first pregnancy leaves the step 2 answers on the
encounter. They describe deliveries that never happened, so no reader counts them.
-}
obstetricHistoryFirstPregnancyTest : Test
obstetricHistoryFirstPregnancyTest =
    let
        firstPregnancy =
            { currentlyPregnant = True
            , termPregnancy = 0
            , preTermPregnancy = 0
            , stillbirthsAtTerm = 0
            , stillbirthsPreTerm = 0
            , abortions = 0
            , liveChildren = 0
            }

        secondPregnancy =
            { firstPregnancy | termPregnancy = 1, liveChildren = 1 }

        step2With signs period =
            { cSections = 0
            , cSectionReason = Nothing
            , previousDelivery = EverySet.singleton NoPreviousDeliverySign
            , previousDeliveryPeriod = EverySet.singleton period
            , signs = EverySet.fromList signs
            }

        withHistory step1 step2 =
            { emptyPrenatalMeasurements
                | obstetricHistory = wrapMeasurement step1
                , obstetricHistoryStep2 = wrapMeasurement step2
            }

        previousPreeclampsia =
            step2With [ ObstetricHistoryPreeclampsiaPreviousPregnancy ] Neither

        moreThan10Years =
            step2With [ NoObstetricHistoryStep2Sign ] MoreThan10Years

        convulsions =
            step2With [ ObstetricHistoryConvulsionsPreviousDelivery ] Neither

        highRisk =
            EverySet.member DiagnosisHighRiskOfPreeclampsiaInitialPhase

        moderateRisk =
            EverySet.member DiagnosisModerateRiskOfPreeclampsia

        convulsionsAlert measurements =
            generateHighRiskAlertData English measurements HighRiskConvulsionsPreviousDelivery
    in
    describe "obstetric history step 2 on a first pregnancy"
        [ test "previous preeclampsia gives high risk when step 1 records an earlier pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory secondPregnancy previousPreeclampsia)
                    |> highRisk
                    |> Expect.equal True
        , test "previous preeclampsia gives no high risk when step 1 says first pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory firstPregnancy previousPreeclampsia)
                    |> highRisk
                    |> Expect.equal False
        , test "the latest step 1 decides: a first pregnancy at the current visit drops step 2 saved at an earlier one" <|
            \_ ->
                diagnoseNurseAfter []
                    (withHistory secondPregnancy previousPreeclampsia)
                    { emptyPrenatalMeasurements | obstetricHistory = wrapMeasurement firstPregnancy }
                    |> highRisk
                    |> Expect.equal False
        , test "a first pregnancy recorded at an earlier visit is read the same way at a later one" <|
            \_ ->
                diagnoseNurseAfter [] (withHistory firstPregnancy previousPreeclampsia) emptyPrenatalMeasurements
                    |> highRisk
                    |> Expect.equal False
        , test "more than 10 years since delivery gives moderate risk when step 1 records an earlier pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory secondPregnancy moreThan10Years)
                    |> moderateRisk
                    |> Expect.equal True
        , test "more than 10 years since delivery gives no moderate risk when step 1 says first pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory firstPregnancy moreThan10Years)
                    |> moderateRisk
                    |> Expect.equal False
        , test "convulsions at a previous delivery raise the high-risk alert when step 1 records an earlier pregnancy" <|
            \_ ->
                convulsionsAlert (withHistory secondPregnancy convulsions)
                    |> Expect.notEqual Nothing
        , test "convulsions at a previous delivery raise no high-risk alert when step 1 says first pregnancy" <|
            \_ ->
                convulsionsAlert (withHistory firstPregnancy convulsions)
                    |> Expect.equal Nothing
        , test "the skip rule is gravida 01 and para 0000 for every combination of counts" <|
            \_ ->
                let
                    counts =
                        [ 0, 1, 2, 10 ]

                    values =
                        List.concatMap
                            (\currentlyPregnant ->
                                List.concatMap
                                    (\term ->
                                        List.concatMap
                                            (\preTerm ->
                                                List.concatMap
                                                    (\atTerm ->
                                                        List.concatMap
                                                            (\stillPreTerm ->
                                                                List.concatMap
                                                                    (\abortions ->
                                                                        List.map
                                                                            (\liveChildren ->
                                                                                { currentlyPregnant = currentlyPregnant
                                                                                , termPregnancy = term
                                                                                , preTermPregnancy = preTerm
                                                                                , stillbirthsAtTerm = atTerm
                                                                                , stillbirthsPreTerm = stillPreTerm
                                                                                , abortions = abortions
                                                                                , liveChildren = liveChildren
                                                                                }
                                                                            )
                                                                            counts
                                                                    )
                                                                    counts
                                                            )
                                                            counts
                                                    )
                                                    counts
                                            )
                                            counts
                                    )
                                    counts
                            )
                            [ True, False ]
                in
                values
                    |> List.filter
                        (\value ->
                            skipObstetricHistorySecondStep (Just value)
                                /= (generateGravida value == "01" && generatePara value == "0000")
                        )
                    |> Expect.equal []
        ]



-- LATE LAB RESULTS: DIAGNOSES FOLLOW THE VISIT'S PREGNANCY WEEK
--
-- A nurse visit sends a test to the lab; its result is entered on a later
-- day, from the encounter's Lab results page (Case Management), by a lab
-- technician, or from a later visit's lab history. Every time a result is
-- saved, the app reassesses the whole encounter with
-- `generatePrenatalDiagnosesForNurse <entry day> <the visit's assembled data>`
-- and stores the outcome; the diagnoses it adds over the stored set decide
-- the emergency-referral warning and the recurrent Next Steps.
--
-- Oracle (issue "Expected behaviour"): every week-dependent line is graded
-- at the pregnancy week of the VISIT (`assembled.encounter.startDate`), never
-- at the week of the day the result is entered. The week lines are:
--   * chronic (< 20) vs gestational (>= 20) hypertension;
--   * diabetes (<= 20) vs gestational diabetes (> 20);
--   * moderate preeclampsia [20, 37) vs its emergency EGA37+ variant;
--   * hyperemesis gravidum (< 20) vs severe vomiting (>= 20);
--   * the emergency lines at 22 and 37 weeks;
--   * the preeclampsia risk diagnoses, offered from week 12.
--
-- Each scenario is built the way the app builds it: the visit's measurements
-- are first assessed on the visit day (the set the nurse's device stores when
-- the encounter is completed), then the result is entered into the pending
-- test, the visit-day set is stored on the encounter, the encounter is
-- closed on the visit day, and the encounter is assessed again on the entry
-- day. Ages are written as (weeks, days) of EGA on the visit day.


{-| The visit day of every late-result scenario.
-}
visitDay : NominalDate
visitDay =
    currentDate


{-| The LMP that puts `visitDay` at the given EGA (weeks, days).
-}
lmpForVisitAt : ( Int, Int ) -> NominalDate
lmpForVisitAt ( weeks, days ) =
    Date.add Date.Days -(weeks * 7 + days) visitDay


{-| A nurse encounter started on `visitDay` at the given EGA, after the given
earlier nurse visits, with the given diagnoses stored on it.
-}
visitAssembled : ( Int, Int ) -> List PreviousEncounterData -> EverySet PrenatalDiagnosis -> PrenatalMeasurements -> AssembledData
visitAssembled ega previousEncounters storedDiagnoses measurements =
    let
        base =
            testAssembled measurements

        encounter =
            base.encounter
    in
    { base
        | encounter = { encounter | startDate = visitDay, diagnoses = storedDiagnoses }
        , globalLmpDate = Just (lmpForVisitAt ega)
        , nursePreviousEncountersData = previousEncounters
    }


type alias LateResult =
    { visitDayDiagnoses : EverySet PrenatalDiagnosis
    , afterEntry : EverySet PrenatalDiagnosis
    }


{-| A visit at `ega`, after `previousEncounters`, with `visitMeasurements`
recorded (the lab test still pending). `daysLater` days after the visit,
`enterResult` fills in the pending result, and the encounter is reassessed
with the visit-day diagnoses stored on it.
-}
enterResultLaterAfter : List PreviousEncounterData -> ( Int, Int ) -> Int -> (PrenatalMeasurements -> PrenatalMeasurements) -> PrenatalMeasurements -> LateResult
enterResultLaterAfter previousEncounters ega _ enterResult visitMeasurements =
    let
        stored =
            generatePrenatalDiagnosesForNurse
                (visitAssembled ega previousEncounters EverySet.empty visitMeasurements)

        assembled =
            visitAssembled ega previousEncounters stored (enterResult visitMeasurements)

        encounter =
            assembled.encounter
    in
    { visitDayDiagnoses = stored
    , afterEntry =
        generatePrenatalDiagnosesForNurse
            { assembled | encounter = { encounter | endDate = Just visitDay } }
    }


{-| `enterResultLaterAfter` for a first visit.
-}
enterResultLater : ( Int, Int ) -> Int -> (PrenatalMeasurements -> PrenatalMeasurements) -> PrenatalMeasurements -> LateResult
enterResultLater =
    enterResultLaterAfter []


{-| The diagnoses the late result added over the stored visit-day set: the
ones that bring up the emergency-referral warning and recurrent Next Steps.
-}
addedByLateResult : LateResult -> List PrenatalDiagnosis
addedByLateResult result =
    EverySet.diff result.afterEntry result.visitDayDiagnoses
        |> EverySet.toList


{-| Membership of each given diagnosis in a diagnosis set.
-}
membership : List PrenatalDiagnosis -> EverySet PrenatalDiagnosis -> List ( PrenatalDiagnosis, Bool )
membership diagnoses set =
    List.map (\diagnosis -> ( diagnosis, EverySet.member diagnosis set )) diagnoses



-- Pending tests and the results entered into them later. A test sent to the
-- lab is "run today" on the visit day, with no result and without the
-- immediate-result prerequisite.


pendingHemoglobinValue : HemoglobinTestValue
pendingHemoglobinValue =
    { executionNote = TestNoteRunToday
    , executionDate = Just visitDay
    , testPrerequisites = Nothing
    , hemoglobinCount = Nothing
    }


withPendingHemoglobin : PrenatalMeasurements -> PrenatalMeasurements
withPendingHemoglobin measurements =
    { measurements | hemoglobinTest = TestFixtures.wrapMeasurement visitDay pendingHemoglobinValue }


{-| A nurse enters the hemoglobin count on the encounter's Lab results page.
-}
enterHemoglobin : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterHemoglobin count measurements =
    { measurements
        | hemoglobinTest =
            TestFixtures.wrapMeasurement visitDay { pendingHemoglobinValue | hemoglobinCount = Just count }
    }


{-| A lab technician enters the hemoglobin count, confirming the test was run.
-}
enterHemoglobinByLabTech : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterHemoglobinByLabTech count measurements =
    { measurements
        | hemoglobinTest =
            TestFixtures.wrapMeasurement visitDay
                { pendingHemoglobinValue
                    | executionNote = TestNoteRunConfirmedByLabTech
                    , hemoglobinCount = Just count
                }
    }


{-| Fasting random blood sugar, sent to the lab: above 126 means diabetes.
-}
pendingRandomBloodSugarValue : RandomBloodSugarTestValue PrenatalEncounterId
pendingRandomBloodSugarValue =
    { executionNote = TestNoteRunToday
    , executionDate = Just visitDay
    , testPrerequisites = Just (EverySet.singleton PrerequisiteFastFor12h)
    , sugarCount = Nothing
    , originatingEncounter = Nothing
    }


withPendingRandomBloodSugar : PrenatalMeasurements -> PrenatalMeasurements
withPendingRandomBloodSugar measurements =
    { measurements | randomBloodSugarTest = TestFixtures.wrapMeasurement visitDay pendingRandomBloodSugarValue }


enterRandomBloodSugar : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterRandomBloodSugar count measurements =
    { measurements
        | randomBloodSugarTest =
            TestFixtures.wrapMeasurement visitDay { pendingRandomBloodSugarValue | sugarCount = Just count }
    }


{-| The result is entered from a later visit's lab history: it is saved on
the visit's own test, naming the later visit as the one it came from.
-}
enterRandomBloodSugarFromLaterVisit : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterRandomBloodSugarFromLaterVisit count measurements =
    { measurements
        | randomBloodSugarTest =
            TestFixtures.wrapMeasurement visitDay
                { pendingRandomBloodSugarValue
                    | sugarCount = Just count
                    , originatingEncounter = Just (toEntityUuid "later-visit")
                }
    }


pendingUrineDipstickValue : UrineDipstickTestValue
pendingUrineDipstickValue =
    let
        immediate =
            urineDipstickValueWith ProteinPlus1
    in
    { immediate | testPrerequisites = Nothing, protein = Nothing }


withPendingUrineDipstick : PrenatalMeasurements -> PrenatalMeasurements
withPendingUrineDipstick measurements =
    { measurements | urineDipstickTest = TestFixtures.wrapMeasurement visitDay pendingUrineDipstickValue }


enterUrineProtein : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
enterUrineProtein protein measurements =
    { measurements
        | urineDipstickTest =
            TestFixtures.wrapMeasurement visitDay { pendingUrineDipstickValue | protein = Just protein }
    }


{-| Visit measurements: high blood pressure (165/115) and a hemoglobin test
sent to the lab.
-}
highBloodPressureVisit : PrenatalMeasurements
highBloodPressureVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withPendingHemoglobin


{-| Visit measurements: a danger sign and a hemoglobin test sent to the lab.
-}
dangerSignVisit : DangerSign -> PrenatalMeasurements
dangerSignVisit sign =
    emptyPrenatalMeasurements
        |> withDangerSigns (EverySet.singleton sign)
        |> withPendingHemoglobin


{-| Visit measurements: moderate preeclampsia found at the visit (high blood
pressure with urine protein +1, read at the visit) and a hemoglobin test
sent to the lab.
-}
moderatePreeclampsiaVisit : PrenatalMeasurements
moderatePreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withUrineProtein ProteinPlus1
        |> withPendingHemoglobin


{-| Obstetric history of a second pregnancy whose step 2 carries the given
sign and delivery period: preeclampsia at a previous pregnancy gives HIGH
risk of preeclampsia, more than 10 years since delivery gives MODERATE risk.
-}
withPreeclampsiaRiskHistory : ObstetricHistoryStep2Sign -> PreviousDeliveryPeriod -> PrenatalMeasurements -> PrenatalMeasurements
withPreeclampsiaRiskHistory sign period measurements =
    { measurements
        | obstetricHistory =
            wrapMeasurement
                { currentlyPregnant = True
                , termPregnancy = 1
                , preTermPregnancy = 0
                , stillbirthsAtTerm = 0
                , stillbirthsPreTerm = 0
                , abortions = 0
                , liveChildren = 1
                }
        , obstetricHistoryStep2 =
            wrapMeasurement
                { cSections = 0
                , cSectionReason = Nothing
                , previousDelivery = EverySet.singleton NoPreviousDeliverySign
                , previousDeliveryPeriod = EverySet.singleton period
                , signs = EverySet.singleton sign
                }
    }


highRiskOfPreeclampsiaVisit : PrenatalMeasurements
highRiskOfPreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withPreeclampsiaRiskHistory ObstetricHistoryPreeclampsiaPreviousPregnancy Neither
        |> withPendingHemoglobin


moderateRiskOfPreeclampsiaVisit : PrenatalMeasurements
moderateRiskOfPreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withPreeclampsiaRiskHistory NoObstetricHistoryStep2Sign MoreThan10Years
        |> withPendingHemoglobin


preeclampsiaRiskDiagnoses : List PrenatalDiagnosis
preeclampsiaRiskDiagnoses =
    [ DiagnosisHighRiskOfPreeclampsiaInitialPhase
    , DiagnosisHighRiskOfPreeclampsiaRecurrentPhase
    , DiagnosisModerateRiskOfPreeclampsia
    ]


lateLabResultSameDayTest : Test
lateLabResultSameDayTest =
    describe "late lab result: A lab result entered on the visit day gives the same diagnoses as it does today"
        [ test "high blood pressure at 19 weeks 5 days, result entered the same day: chronic hypertension, not gestational" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]
        , test "high blood pressure at 20 weeks 0 days, result entered the same day: gestational hypertension, not chronic" <|
            \_ ->
                enterResultLater ( 20, 0 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, False ), ( DiagnosisGestationalHypertensionImmediate, True ) ]
        , test "high blood sugar from a visit at 20 weeks 6 days, entered the same day: diabetes, not gestational diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 0 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        , test "high blood sugar from a visit at 21 weeks 0 days, entered the same day: gestational diabetes, not diabetes" <|
            \_ ->
                enterResultLater ( 21, 0 ) 0 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, True ) ]
        , test "moderate preeclampsia at 36 weeks 6 days, result entered the same day: moderate preeclampsia, not the EGA 37+ emergency" <|
            \_ ->
                enterResultLater ( 36, 6 ) 0 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]
        , test "moderate preeclampsia at 37 weeks 0 days, result entered the same day: the EGA 37+ emergency" <|
            \_ ->
                enterResultLater ( 37, 0 ) 0 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, False ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, True ) ]
        , test "severe vomiting at 19 weeks 6 days, result entered the same day: hyperemesis gravidum" <|
            \_ ->
                enterResultLater ( 19, 6 ) 0 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "severe vomiting at 20 weeks 0 days, result entered the same day: severe vomiting" <|
            \_ ->
                enterResultLater ( 20, 0 ) 0 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, False ), ( DiagnosisSevereVomiting, True ) ]
        , test "a normal result entered the same day adds nothing to the stored diagnoses" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal []
        , test "a low hemoglobin entered the same day adds only moderate anemia" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 9) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal [ DiagnosisModerateAnemiaRecurrentPhase ]
        , test "a pregnancy with no LMP: a high blood pressure gets no week-graded diagnosis on any day" <|
            \_ ->
                let
                    assembled =
                        visitAssembled ( 19, 5 ) [] EverySet.empty (enterHemoglobin 12 highBloodPressureVisit)

                    undated =
                        { assembled | globalLmpDate = Nothing }
                in
                [ visitDay, Date.add Date.Days 3 visitDay ]
                    |> List.map
                        (\_ ->
                            generatePrenatalDiagnosesForNurse undated
                                |> EverySet.toList
                                |> List.filter (\diagnosis -> List.member diagnosis [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ])
                        )
                    |> Expect.equal [ [], [] ]
        ]


{-| A random blood sugar test sent to the lab, and nothing else.
-}
withPendingRandomBloodSugar_ : PrenatalMeasurements
withPendingRandomBloodSugar_ =
    withPendingRandomBloodSugar emptyPrenatalMeasurements


lateLabResultHypertensionTest : Test
lateLabResultHypertensionTest =
    let
        chronicNotGestational =
            [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]

        hypertension =
            membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
    in
    describe "late lab result: chronic vs gestational hypertension"
        [ test "High blood pressure measured before week 20 stays chronic hypertension when a lab result for that visit is entered after week 20" <|
            -- The issue's own scenario: 19 weeks 5 days, result three days
            -- later at 20 weeks 1 day.
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "visit at 19 weeks 6 days, result entered the next day at 20 weeks 0 days: chronic hypertension stays" <|
            \_ ->
                enterResultLater ( 19, 6 ) 1 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "visit at 16 weeks, result entered ten weeks later: chronic hypertension stays" <|
            \_ ->
                enterResultLater ( 16, 0 ) 70 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "marginal blood pressure confirmed on recheck at 19 weeks 5 days stays chronic hypertension after recheck when a result is entered at 20 weeks 1 day" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 95
                    |> withPendingHemoglobin
                    |> enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12)
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionAfterRecheck, DiagnosisGestationalHypertensionAfterRecheck ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionAfterRecheck, True ), ( DiagnosisGestationalHypertensionAfterRecheck, False ) ]
        , test "a second visit at 19 weeks 5 days (first visit at 15 weeks, no hypertension then): chronic hypertension stays after a result at 20 weeks 1 day" <|
            \_ ->
                let
                    firstVisit =
                        { startDate = Date.add Date.Weeks -4 visitDay
                        , diagnoses = EverySet.empty
                        , pastDiagnoses = EverySet.empty
                        , measurements = emptyPrenatalMeasurements
                        }
                in
                enterResultLaterAfter [ firstVisit ] ( 19, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "guard: high blood pressure at 20 weeks 1 day is gestational hypertension, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 20, 1 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, False ), ( DiagnosisGestationalHypertensionImmediate, True ) ]
        ]


lateLabResultDiabetesTest : Test
lateLabResultDiabetesTest =
    let
        diabetes =
            membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]

        diabetesNotGestational =
            [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
    in
    describe "late lab result: diabetes vs gestational diabetes"
        [ test "A high blood sugar result from a visit at week 20 or earlier is diagnosed as diabetes, not gestational diabetes, even when it is entered after week 20" <|
            -- Visit at 20 weeks 6 days (still week 20), result entered at
            -- 21 weeks 2 days.
            \_ ->
                enterResultLater ( 20, 6 ) 3 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "visit at 20 weeks 6 days, result entered the next day at 21 weeks 0 days: diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 1 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "visit at 14 weeks, result entered eight weeks later at 22 weeks: diabetes" <|
            \_ ->
                enterResultLater ( 14, 0 ) 56 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "guard: a high blood sugar from a visit at 21 weeks 0 days is gestational diabetes when entered later" <|
            \_ ->
                enterResultLater ( 21, 0 ) 3 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, True ) ]
        , test "guard: a normal blood sugar from a visit at week 20 entered after week 20 gives neither diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 3 (enterRandomBloodSugar 90) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        ]


lateLabResultModeratePreeclampsiaTest : Test
lateLabResultModeratePreeclampsiaTest =
    describe "late lab result: moderate preeclampsia before week 37"
        [ test "Moderate preeclampsia found at a visit before week 37 is still diagnosed when a lab result for that visit is entered at week 37 or later" <|
            -- Visit at 36 weeks 5 days, result entered at 37 weeks 1 day.
            \_ ->
                enterResultLater ( 36, 5 ) 3 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal True
        , test "visit at 36 weeks 6 days, result entered the next day at 37 weeks 0 days: moderate preeclampsia, and not its EGA 37+ emergency variant" <|
            \_ ->
                enterResultLater ( 36, 6 ) 1 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]

        -- The visit records high blood pressure and sends urine to the lab;
        -- the protein result entered at 37 weeks 1 day completes moderate
        -- preeclampsia. The visit was at week 36, so it is the [20, 37)
        -- diagnosis, not the EGA 37+ emergency one.
        , test "high blood pressure at 36 weeks 5 days with urine protein +1 entered at 37 weeks 1 day: moderate preeclampsia, not the EGA 37+ emergency" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 36, 5 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaRecurrentPhase, DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaRecurrentPhase, True ), ( DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus, False ) ]
        , test "guard: moderate preeclampsia at 37 weeks 0 days stays the EGA 37+ emergency when a result is entered later" <|
            \_ ->
                enterResultLater ( 37, 0 ) 3 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, False ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, True ) ]
        ]


lateLabResultHyperemesisTest : Test
lateLabResultHyperemesisTest =
    let
        vomiting =
            membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
    in
    describe "late lab result: hyperemesis gravidum vs severe vomiting"
        [ test "A severe-vomiting danger sign at a visit before week 20 stays hyperemesis gravidum, not severe vomiting, when a lab result for that visit is entered after week 20" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "visit at 19 weeks 6 days, result entered the next day at 20 weeks 0 days: hyperemesis gravidum stays" <|
            \_ ->
                enterResultLater ( 19, 6 ) 1 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "guard: severe vomiting at 20 weeks 0 days stays severe vomiting when a result is entered later" <|
            \_ ->
                enterResultLater ( 20, 0 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, False ), ( DiagnosisSevereVomiting, True ) ]
        ]


{-| Every scenario here crosses one week line between the visit and the entry
day, and enters a normal hemoglobin (12), which by itself diagnoses nothing.
So whatever the reassessment adds over the stored visit-day set could only
come from the later week, and must be nothing: an added diagnosis is what
brings up the emergency-referral warning and recurrent Next Steps.
-}
lateLabResultAddsNothingTest : Test
lateLabResultAddsNothingTest =
    let
        addsNothing ega daysLater visitMeasurements =
            enterResultLater ega daysLater (enterHemoglobin 12) visitMeasurements
                |> addedByLateResult
                |> Expect.equal []
    in
    describe "late lab result: diagnoses added over the stored set"
        [ test "Entering a late lab result never brings up an emergency-referral warning, or recurrent Next Steps, for a diagnosis that only the later week would produce" <|
            -- Severe vomiting at 19 weeks 5 days, result at 20 weeks 1 day:
            -- the severe-vomiting emergency must not be added.
            \_ ->
                addsNothing ( 19, 5 ) 3 (dangerSignVisit SevereVomiting)
        , test "high blood pressure at 19 weeks 5 days, result at 20 weeks 1 day: no gestational hypertension added" <|
            \_ ->
                addsNothing ( 19, 5 ) 3 highBloodPressureVisit
        , test "moderate preeclampsia at 36 weeks 5 days, result at 37 weeks 1 day: no EGA 37+ emergency added" <|
            \_ ->
                addsNothing ( 36, 5 ) 3 moderatePreeclampsiaVisit
        , test "headache with blurred vision at 36 weeks 5 days, result at 37 weeks 1 day: no EGA 37+ severe preeclampsia emergency added" <|
            \_ ->
                addsNothing ( 36, 5 ) 3 (dangerSignVisit HeadacheBlurredVision)
        , test "vaginal bleeding at 22 weeks 5 days, result at 23 weeks 1 day: no placenta previa, abruption or uterine rupture added" <|
            \_ ->
                addsNothing ( 22, 5 ) 3 (dangerSignVisit VaginalBleeding)
        , test "abdominal pain at 21 weeks 5 days, result at 22 weeks 1 day: no obstructed labor or its follow-ons added" <|
            \_ ->
                addsNothing ( 21, 5 ) 3 (dangerSignVisit AbdominalPain)
        , test "leaking vaginal fluid at 37 weeks 5 days, result at 38 weeks 1 day: no PROM added" <|
            \_ ->
                addsNothing ( 37, 5 ) 3 (dangerSignVisit GushLeakingVaginalFluid)
        , test "a low hemoglobin entered at 20 weeks 1 day for high blood pressure at 19 weeks 5 days adds only moderate anemia" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 9) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal [ DiagnosisModerateAnemiaRecurrentPhase ]
        , test "the reassessment keeps the stored week-graded emergency: severe vomiting at 19 weeks 5 days keeps hyperemesis gravidum at 20 weeks 1 day" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> (\result -> EverySet.diff result.visitDayDiagnoses result.afterEntry |> EverySet.toList)
                    |> Expect.equal []
        ]


lateLabResultPreeclampsiaRiskTest : Test
lateLabResultPreeclampsiaRiskTest =
    let
        risks =
            membership preeclampsiaRiskDiagnoses
    in
    describe "late lab result: preeclampsia risk from week 12"
        [ test "The preeclampsia risk diagnoses, which start at week 12, follow the visit's week, not the entry day's" <|
            -- Previous-pregnancy preeclampsia (high risk) at a visit at 11
            -- weeks 5 days; result entered at 12 weeks 1 day.
            \_ ->
                enterResultLater ( 11, 5 ) 3 (enterHemoglobin 12) highRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> risks
                    |> List.filter Tuple.second
                    |> Expect.equal []
        , test "moderate risk (more than 10 years since delivery) at 11 weeks 6 days, result at 12 weeks 0 days: no risk diagnosis" <|
            \_ ->
                enterResultLater ( 11, 6 ) 1 (enterHemoglobin 12) moderateRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> risks
                    |> List.filter Tuple.second
                    |> Expect.equal []
        , test "high blood pressure at 11 weeks 5 days, result at 12 weeks 1 day: chronic hypertension stays, and no high risk of preeclampsia is added" <|
            \_ ->
                enterResultLater ( 11, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal []
        , test "guard: previous-pregnancy preeclampsia at 12 weeks 0 days gives high risk, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 12, 0 ) 3 (enterHemoglobin 12) highRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisHighRiskOfPreeclampsiaInitialPhase
                    |> Expect.equal True
        , test "guard: more than 10 years since delivery at 12 weeks 0 days gives moderate risk, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 12, 0 ) 3 (enterHemoglobin 12) moderateRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisModerateRiskOfPreeclampsia
                    |> Expect.equal True
        ]


{-| The lab technician's device and a later visit's lab history run the same
reassessment on the visit's encounter; only the stored result differs in
shape (a lab technician's confirmation note, or the later visit named as the
result's origin).
-}
lateLabResultLabTechAndHistoryTest : Test
lateLabResultLabTechAndHistoryTest =
    describe "late lab result: lab technician and later visit's lab history"
        [ test "The same holds when the result is entered by a lab technician, and when it is entered from a later visit's lab history" <|
            -- Lab technician: hemoglobin for high blood pressure at 19 weeks
            -- 5 days, entered at 20 weeks 1 day.
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobinByLabTech 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]
        , test "lab technician: severe vomiting at 19 weeks 5 days with a result at 20 weeks 1 day adds nothing" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobinByLabTech 12) (dangerSignVisit SevereVomiting)
                    |> addedByLateResult
                    |> Expect.equal []
        , test "lab technician: moderate preeclampsia at 36 weeks 5 days stays after a result at 37 weeks 1 day" <|
            \_ ->
                enterResultLater ( 36, 5 ) 3 (enterHemoglobinByLabTech 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]
        , test "lab history: a high blood sugar from a visit at 18 weeks entered at the next visit at 22 weeks is diabetes, not gestational diabetes" <|
            \_ ->
                enterResultLater ( 18, 0 ) 28 (enterRandomBloodSugarFromLaterVisit 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        , test "lab history: a normal blood sugar for high blood pressure at 19 weeks 5 days, entered at the next visit at 23 weeks 5 days, keeps chronic hypertension and adds nothing" <|
            \_ ->
                let
                    result =
                        emptyPrenatalMeasurements
                            |> withVitals 165 115
                            |> withPendingRandomBloodSugar
                            |> enterResultLater ( 19, 5 ) 28 (enterRandomBloodSugarFromLaterVisit 90)
                in
                ( EverySet.member DiagnosisChronicHypertensionImmediate result.afterEntry
                , addedByLateResult result
                )
                    |> Expect.equal ( True, [] )
        ]
