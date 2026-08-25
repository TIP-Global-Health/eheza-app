module Pages.Prenatal.Activity.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BloodGpRsTestValue
        , BloodSmearResult(..)
        , CorePhysicalExamValue
        , DangerSign(..)
        , DangerSignsValue
        , HIVPCRTestValue
        , HIVTestValue
        , HemoglobinTestValue
        , HepatitisBTestValue
        , LungsCPESign
        , MalariaTestValue
        , Measurement
        , PrenatalAssesment(..)
        , PrenatalMeasurements
        , PrenatalMentalHealthQuestion(..)
        , PrenatalMentalHealthQuestionOption(..)
        , ProteinValue(..)
        , RandomBloodSugarTestValue
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
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (RangedMeasurement(..))
import Pages.Prenatal.Activity.Model exposing (Msg(..), emptyModel)
import Pages.Prenatal.Activity.Types exposing (GWGClassification(..), PrePregnancyClassification(..), WarningPopupType(..))
import Pages.Prenatal.Activity.Update exposing (update)
import Pages.Prenatal.Activity.Utils exposing (bmiToPrePregnancyClassification, generatePrenatalAssesmentForChw, generatePrenatalDiagnosesForNurse, resolveGWGClassificationForHealthyStart, suicideRiskDiagnosedBySigns, zscoreToPrePregnancyClassification)
import Pages.Prenatal.Model exposing (AssembledData)
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures
import Time



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
-- 25 days at 60 g per day is an expected gain of 1.5 kg.


{-| The previous weighing, 25 days before `currentDate`. With an LMP 28 weeks
back, that date is well past 13 weeks of gestation.
-}
previousWeightDate25Days : NominalDate
previousWeightDate25Days =
    Date.add Date.Days -25 currentDate


{-| Classify a gain from 60 kg to `currentWeight` over those 25 days.
-}
classifyHealthyStartGWG : PrePregnancyClassification -> Float -> Maybe GWGClassification
classifyHealthyStartGWG prePregnancyClassification currentWeight =
    resolveGWGClassificationForHealthyStart currentDate
        prePregnancyClassification
        60.0
        previousWeightDate25Days
        currentWeight
        (testAssembled28Weeks emptyPrenatalMeasurements)


resolveGWGClassificationForHealthyStartTest : Test
resolveGWGClassificationForHealthyStartTest =
    describe "resolveGWGClassificationForHealthyStart (Healthy Start expected daily gain)"
        [ test "gain of 1.0 kg, below the expected 1.5 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "gain of exactly the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 61.5
                    |> Expect.equal (Just GWGAdequate)
        , test "gain of 2.5 kg, above the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 62.5
                    |> Expect.equal (Just GWGAdequate)
        , test "1.0 kg lost over the period -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 59.0
                    |> Expect.equal (Just GWGInadequate)
        , test "severely undernourished at booking: 1.5 kg is short of the expected 1.825 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyUnderWeight 61.5
                    |> Expect.equal (Just GWGInadequate)
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


{-| HIV test, run today with the given positive/negative result, immediate
result. No partner/HIV signs (so this never trips discordant-partnership).
-}
hivTestValueWith : TestResult -> HIVTestValue
hivTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result
    , hivSigns = Nothing
    }


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
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , hivSigns = Nothing
    }


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
    generatePrenatalDiagnosesForNurse currentDate (testAssembled measurements)


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
    generatePrenatalDiagnosesForNurse currentDate (testAssembled28Weeks measurements)


{-| Run the function under test at EGA ~16 weeks (< 20).
-}
diagnoseNurse16Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse16Weeks measurements =
    generatePrenatalDiagnosesForNurse currentDate (testAssembled16Weeks measurements)


{-| Run the function under test at EGA ~38 weeks (>= 37).
-}
diagnoseNurse38Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse38Weeks measurements =
    generatePrenatalDiagnosesForNurse currentDate (testAssembled38Weeks measurements)


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
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseAnemiaRecurrentTest
        , generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest
        , generatePrenatalDiagnosesForNurseHypertensionRecheckTest
        , generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest
        , generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest
        , suicideRiskDiagnosedBySignsTest
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
