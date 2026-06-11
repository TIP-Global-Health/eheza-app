module Pages.Prenatal.Activity.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( BloodSmearResult(..)
        , DangerSign(..)
        , DangerSignsValue
        , Gender(..)
        , HIVTestValue
        , HemoglobinTestValue
        , HepatitisBTestValue
        , MalariaTestValue
        , Measurement
        , PrenatalAssesment(..)
        , PrenatalMeasurements
        , ProteinValue(..)
        , SyphilisTestValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , UrineDipstickTestValue
        , VitalsValue
        , emptyPrenatalMeasurements
        )
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Activity.Types exposing (PrePregnancyClassification(..))
import Pages.Prenatal.Activity.Utils exposing (bmiToPrePregnancyClassification, generatePrenatalAssesmentForChw, generatePrenatalDiagnosesForNurse, zscoreToPrePregnancyClassification)
import Pages.Prenatal.Model exposing (AssembledData)
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
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


{-| Wrap a measurement `value` into the full `Measurement` record shape that
the `PrenatalMeasurements` fields require, paired with a dummy entity id.

The signature is polymorphic in the id tag, encounter type, and value, so it
unifies with each concrete `PrenatalMeasurements` field type.

-}
wrapMeasurement : value -> Maybe ( EntityUuid id, Measurement encounter value )
wrapMeasurement value =
    Just
        ( toEntityUuid "dummy-id"
        , { dateMeasured = dummyDate
          , nurse = Nothing
          , healthCenter = Nothing
          , participantId = toEntityUuid "dummy-person"
          , deleted = False
          , encounterId = Nothing
          , value = value
          }
        )


{-| An adult female person. Everything except birthDate/gender is
defaulted/empty (mirrors `testPerson` in the acute-illness test file).
-}
testPerson : Person
testPerson =
    { name = "Test Person"
    , firstName = "Test"
    , secondName = "Person"
    , nationalIdNumber = Nothing
    , hmisNumber = Nothing
    , avatarUrl = Nothing
    , birthDate = Just (Date.fromCalendarDate 1990 Time.Jan 1)
    , isDateOfBirthEstimated = False
    , gender = Female
    , hivStatus = Nothing
    , numberOfChildren = Nothing
    , modeOfDelivery = Nothing
    , ubudehe = Nothing
    , educationLevel = Nothing
    , maritalStatus = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , registrationLatitude = Nothing
    , registrationLongitude = Nothing
    , saveGPSLocation = False
    , telephoneNumber = Nothing
    , spouseName = Nothing
    , spousePhoneNumber = Nothing
    , nextOfKinName = Nothing
    , nextOfKinPhoneNumber = Nothing
    , healthCenterId = Nothing
    , deleted = False
    , shard = Nothing
    }


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
    { person = toEntityUuid "dummy-person"
    , encounterType = AntenatalEncounter
    , startDate = currentDate
    , endDate = Nothing
    , eddDate = Nothing
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , newborn = Nothing
    , deleted = False
    , shard = Nothing
    }


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



-- LAB VALUE BUILDERS / SETTERS
--
-- Each lab diagnosis at the initial phase requires the test to have been
-- "performed" (executionNote in {RunToday, RunPreviously, RunConfirmedByLabTech}),
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


{-| Vitals with the given systolic/diastolic blood pressure. Respiratory rate
is left unset so the anemia-complication path (which keys off an elevated
respiratory rate) stays inert.
-}
vitalsValueWith : Float -> Float -> VitalsValue
vitalsValueWith sys dia =
    { sys = Just sys
    , dia = Just dia
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    , sysRepeated = Nothing
    , diaRepeated = Nothing
    }


withVitals : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withVitals sys dia measurements =
    { measurements | vitals = wrapMeasurement (vitalsValueWith sys dia) }


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


all : Test
all =
    describe "Prenatal Activity tests"
        [ bmiToPrePregnancyClassificationTest
        , zscoreToPrePregnancyClassificationTest
        , generatePrenatalDiagnosesForNurseLabsTest
        , generatePrenatalAssesmentForChwTest
        , generatePrenatalDiagnosesForNurseAnemiaTest
        , generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest
        ]
