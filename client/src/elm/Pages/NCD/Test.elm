module Pages.NCD.Test exposing (all)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( CreatinineTestValue
        , Gender(..)
        , GlucoseValue(..)
        , Measurement
        , MedicalCondition(..)
        , NCDMeasurements
        , ProteinValue(..)
        , RandomBloodSugarTestValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , UrineDipstickTestValue
        , VitalsValue
        )
import Backend.NCDEncounter.Model as NCDEncounterModel
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.NCD.Model exposing (AssembledData, PreviousEncounterData)
import Pages.NCD.Utils
    exposing
        ( generateNCDDiagnoses
        , lowerHypertensionStageCondition
        , stage1BloodPressureCondition
        , stage2BloodPressureCondition
        , stage3BloodPressureCondition
        )
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
import Time



-- Expected hypertension staging comes from the NCDs tab of the clinical sheet
-- (the independent oracle), tested at the band boundaries:
--   Stage 1: systolic 140-159 AND diastolic 90-99
--   Stage 2: systolic 160-179 OR  diastolic 100-109
--   Stage 3: systolic >=180   OR  diastolic >=110
--
-- FINDING: for Stage 1 the code uses OR (either value elevated), NOT the
-- sheet's AND. The systolic bands here use a normal diastolic (and vice versa),
-- so they only pass because of that OR; per the sheet they would be False.
-- These pin current behavior and are marked [FINDING]. (Stages 2 and 3 are OR
-- in both code and sheet, so those tests assert the oracle directly.)


stage1Test : Test
stage1Test =
    describe "stage1BloodPressureCondition (sheet: sys 140-159 AND dia 90-99)"
        [ test "both elevated (150/95) -> True" <|
            \_ -> stage1BloodPressureCondition 150 95 |> Expect.equal True
        , test "both normal (130/85) -> False" <|
            \_ -> stage1BloodPressureCondition 130 85 |> Expect.equal False
        , test "sys 139 (dia normal) -> False (below band)" <|
            \_ -> stage1BloodPressureCondition 139 85 |> Expect.equal False
        , test "sys 140 (dia normal) -> True [FINDING: code OR; sheet AND -> False]" <|
            \_ -> stage1BloodPressureCondition 140 85 |> Expect.equal True
        , test "sys 159 (dia normal) -> True (top of band) [FINDING: code OR]" <|
            \_ -> stage1BloodPressureCondition 159 85 |> Expect.equal True
        , test "sys 160 (dia normal) -> False (into stage-2 band)" <|
            \_ -> stage1BloodPressureCondition 160 85 |> Expect.equal False
        , test "dia 89 (sys normal) -> False (below band)" <|
            \_ -> stage1BloodPressureCondition 130 89 |> Expect.equal False
        , test "dia 90 (sys normal) -> True [FINDING: code OR; sheet AND -> False]" <|
            \_ -> stage1BloodPressureCondition 130 90 |> Expect.equal True
        , test "dia 99 (sys normal) -> True (top of band) [FINDING: code OR]" <|
            \_ -> stage1BloodPressureCondition 130 99 |> Expect.equal True
        , test "dia 100 (sys normal) -> False (into stage-2 band)" <|
            \_ -> stage1BloodPressureCondition 130 100 |> Expect.equal False
        ]


stage2Test : Test
stage2Test =
    describe "stage2BloodPressureCondition (sheet: sys 160-179 OR dia 100-109)"
        [ test "both elevated (170/105) -> True" <|
            \_ -> stage2BloodPressureCondition 170 105 |> Expect.equal True
        , test "both in stage-1 range (150/95) -> False" <|
            \_ -> stage2BloodPressureCondition 150 95 |> Expect.equal False
        , test "sys 159 -> False (below band)" <|
            \_ -> stage2BloodPressureCondition 159 85 |> Expect.equal False
        , test "sys 160 -> True (bottom of band)" <|
            \_ -> stage2BloodPressureCondition 160 85 |> Expect.equal True
        , test "sys 179 -> True (top of band)" <|
            \_ -> stage2BloodPressureCondition 179 85 |> Expect.equal True
        , test "sys 180 -> False (into stage-3 band)" <|
            \_ -> stage2BloodPressureCondition 180 85 |> Expect.equal False
        , test "dia 99 -> False (below band)" <|
            \_ -> stage2BloodPressureCondition 130 99 |> Expect.equal False
        , test "dia 100 -> True (bottom of band)" <|
            \_ -> stage2BloodPressureCondition 130 100 |> Expect.equal True
        , test "dia 109 -> True (top of band)" <|
            \_ -> stage2BloodPressureCondition 130 109 |> Expect.equal True
        , test "dia 110 -> False (into stage-3 band)" <|
            \_ -> stage2BloodPressureCondition 130 110 |> Expect.equal False
        ]


stage3Test : Test
stage3Test =
    describe "stage3BloodPressureCondition (sheet: sys >=180 OR dia >=110)"
        [ test "sys 179 -> False" <|
            \_ -> stage3BloodPressureCondition 179 85 |> Expect.equal False
        , test "sys 180 -> True (boundary)" <|
            \_ -> stage3BloodPressureCondition 180 85 |> Expect.equal True
        , test "dia 109 -> False" <|
            \_ -> stage3BloodPressureCondition 130 109 |> Expect.equal False
        , test "dia 110 -> True (boundary)" <|
            \_ -> stage3BloodPressureCondition 130 110 |> Expect.equal True
        , test "both very high (200/120) -> True" <|
            \_ -> stage3BloodPressureCondition 200 120 |> Expect.equal True
        , test "stage-1 range (150/95) -> False" <|
            \_ -> stage3BloodPressureCondition 150 95 |> Expect.equal False
        ]


lowerHypertensionStageTest : Test
lowerHypertensionStageTest =
    -- Oracle: the Vitals tab adult systolic low alert is 100 mmHg.
    describe "lowerHypertensionStageCondition (sys < 100)"
        [ test "sys 99 -> True" <|
            \_ -> lowerHypertensionStageCondition 99 0 |> Expect.equal True
        , test "sys 100 -> False (boundary)" <|
            \_ -> lowerHypertensionStageCondition 100 0 |> Expect.equal False
        , test "normal/high sys 140 -> False" <|
            \_ -> lowerHypertensionStageCondition 140 90 |> Expect.equal False
        ]



-- END-TO-END generateNCDDiagnoses
--
-- ORACLE: the NCDs tab of the clinical sheet. Its diagnosis combos are SETS
-- (e.g. "Stage 1 with Renal Complications" = {Stage1, RenalComplications}).
--
-- No-op constraint that makes the full pipeline reduce to "the set of directly
-- matched diagnoses": every fixture is a FIRST encounter --
-- encounter.diagnoses = EverySet.empty AND previousEncountersData = []. With
-- both empty, resolveCurrentHypertensionCondition returns Nothing, so the
-- hypertension-hierarchy step (applyHypertensionDiagnosesLogic) and the
-- determined-conditions filter (filterDiagnosesOfDeterminedConditions) are
-- both no-ops, and generateNCDDiagnoses == the matcher set.


dummyDate : NominalDate
dummyDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| Wrap a measurement value into the `Maybe ( id, Measurement encounter value )`
shape stored on `NCDMeasurements`. Polymorphic in both `id` and `encounter`.
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


emptyNCDMeasurements : NCDMeasurements
emptyNCDMeasurements =
    { coMorbidities = Nothing
    , coreExam = Nothing
    , creatinineTest = Nothing
    , dangerSigns = Nothing
    , familyHistory = Nothing
    , familyPlanning = Nothing
    , hba1cTest = Nothing
    , healthEducation = Nothing
    , hivTest = Nothing
    , labsResults = Nothing
    , lipidPanelTest = Nothing
    , liverFunctionTest = Nothing
    , medicationDistribution = Nothing
    , medicationHistory = Nothing
    , outsideCare = Nothing
    , pregnancyTest = Nothing
    , randomBloodSugarTest = Nothing
    , referral = Nothing
    , socialHistory = Nothing
    , symptomReview = Nothing
    , urineDipstickTest = Nothing
    , vitals = Nothing
    }



-- VALUE BUILDERS


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


creatinineValueWith : Float -> CreatinineTestValue
creatinineValueWith creatinineResult =
    { executionNote = TestNoteRunToday
    , executionDate = Nothing
    , creatinineResult = Just creatinineResult
    , bunResult = Nothing
    }


{-| Urine dipstick value with only the protein reading set; the matcher reads
`.protein` (renal) and `.glucose` (diabetes), both defaulted otherwise.
-}
urineProteinValue : ProteinValue -> UrineDipstickTestValue
urineProteinValue protein =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites = Nothing
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


urineGlucoseValue : GlucoseValue -> UrineDipstickTestValue
urineGlucoseValue glucose =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , protein = Nothing
    , ph = Nothing
    , glucose = Just glucose
    , leukocytes = Nothing
    , nitrite = Nothing
    , urobilinogen = Nothing
    , haemoglobin = Nothing
    , ketone = Nothing
    , bilirubin = Nothing
    }


randomBloodSugarValue : Bool -> Float -> RandomBloodSugarTestValue encounterId
randomBloodSugarValue fasting sugar =
    { executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites =
        Just
            (if fasting then
                EverySet.singleton PrerequisiteFastFor12h

             else
                EverySet.empty
            )
    , sugarCount = Just sugar
    , originatingEncounter = Nothing
    }



-- MEASUREMENT SETTERS


withVitals : Float -> Float -> NCDMeasurements -> NCDMeasurements
withVitals sys dia measurements =
    { measurements | vitals = wrapMeasurement (vitalsValueWith sys dia) }


withCoMorbidities : EverySet MedicalCondition -> NCDMeasurements -> NCDMeasurements
withCoMorbidities conditions measurements =
    { measurements | coMorbidities = wrapMeasurement conditions }


withCreatinine : Float -> NCDMeasurements -> NCDMeasurements
withCreatinine result measurements =
    { measurements | creatinineTest = wrapMeasurement (creatinineValueWith result) }


withUrineProtein : ProteinValue -> NCDMeasurements -> NCDMeasurements
withUrineProtein protein measurements =
    { measurements | urineDipstickTest = wrapMeasurement (urineProteinValue protein) }


withUrineGlucose : GlucoseValue -> NCDMeasurements -> NCDMeasurements
withUrineGlucose glucose measurements =
    { measurements | urineDipstickTest = wrapMeasurement (urineGlucoseValue glucose) }


withRandomBloodSugar : Bool -> Float -> NCDMeasurements -> NCDMeasurements
withRandomBloodSugar fasting sugar measurements =
    { measurements | randomBloodSugarTest = wrapMeasurement (randomBloodSugarValue fasting sugar) }



-- ASSEMBLED DATA FIXTURE


{-| An adult person. Everything except birthDate/gender is defaulted/empty.
-}
testPerson : Person
testPerson =
    { name = "Test Person"
    , firstName = "Test"
    , secondName = "Person"
    , nationalIdNumber = Nothing
    , hmisNumber = Nothing
    , avatarUrl = Nothing
    , birthDate = Just (Date.fromCalendarDate 1985 Time.Jan 1)
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


{-| Dummy NCD encounter. CRITICAL: `diagnoses = EverySet.empty` (first encounter)
so the hypertension-hierarchy / determined-conditions pipeline steps are no-ops.
-}
dummyEncounter : NCDEncounterModel.NCDEncounter
dummyEncounter =
    { participant = toEntityUuid "dummy-participant"
    , startDate = dummyDate
    , endDate = Nothing
    , diagnoses = EverySet.empty
    , deleted = False
    , shard = Nothing
    }


dummyParticipant : IndividualEncounterParticipant
dummyParticipant =
    { person = toEntityUuid "dummy-person"
    , encounterType = NCDEncounter
    , startDate = dummyDate
    , endDate = Nothing
    , eddDate = Nothing
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , newborn = Nothing
    , deleted = False
    , shard = Nothing
    }


{-| Build a first-encounter `AssembledData` for the given measurements.
`previousEncountersData = []` (combined with empty `encounter.diagnoses`)
is what keeps the full pipeline a no-op around the matcher.
-}
ncdAssembled : NCDMeasurements -> AssembledData
ncdAssembled measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = dummyEncounter
    , participant = dummyParticipant
    , person = testPerson
    , measurements = measurements
    , previousEncountersData = []
    }


{-| Base measurements: all empty + a normal vitals reading (sys 120 / dia 80).
-}
baseMeasurements : NCDMeasurements
baseMeasurements =
    emptyNCDMeasurements |> withVitals 120 80


expectDiagnoses : List NCDDiagnosis -> NCDMeasurements -> Expect.Expectation
expectDiagnoses expected measurements =
    generateNCDDiagnoses (ncdAssembled measurements)
        |> Expect.equal (EverySet.fromList expected)


{-| A prior NCD encounter carrying the given diagnoses. Measurements are left
empty: the hypertension hierarchy reads the prior encounter's `.diagnoses`, not
its measurements.
-}
previousEncounterWith : List NCDDiagnosis -> PreviousEncounterData
previousEncounterWith diagnoses =
    { id = toEntityUuid "prev-encounter"
    , startDate = Date.add Date.Months -1 dummyDate
    , diagnoses = EverySet.fromList diagnoses
    , measurements = emptyNCDMeasurements
    }


{-| Like `expectDiagnoses`, but with one prior encounter in the history -- to
exercise the hypertension escalation / persistence / lowering logic.
-}
expectDiagnosesWithHistory : List NCDDiagnosis -> List NCDDiagnosis -> NCDMeasurements -> Expect.Expectation
expectDiagnosesWithHistory previousDiagnoses expected measurements =
    generateNCDDiagnoses
        { id = toEntityUuid "dummy-encounter"
        , encounter = dummyEncounter
        , participant = dummyParticipant
        , person = testPerson
        , measurements = measurements
        , previousEncountersData = [ previousEncounterWith previousDiagnoses ]
        }
        |> Expect.equal (EverySet.fromList expected)


hypertensionHierarchyTest : Test
hypertensionHierarchyTest =
    -- Across encounters the hypertension stage is adjusted, not just re-derived
    -- from the current reading. The oracle is the clinical principle that
    -- hypertension is a chronic diagnosis: escalate to a higher stage on a
    -- higher reading, but do NOT downgrade on a single lower reading; only a
    -- low reading (systolic < 100) steps the stage DOWN by one. The exact
    -- step-down-by-one rule is the code's, pinned here.
    describe "generateNCDDiagnoses - hypertension hierarchy across encounters"
        [ test "prior Stage 1 + current Stage-3 reading (185) -> escalates to Stage 3" <|
            \_ ->
                (baseMeasurements |> withVitals 185 85)
                    |> expectDiagnosesWithHistory [ DiagnosisHypertensionStage1 ] [ DiagnosisHypertensionStage3 ]
        , test "prior Stage 3 + current Stage-1 reading (145/95) -> persists at Stage 3 (no downgrade)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95)
                    |> expectDiagnosesWithHistory [ DiagnosisHypertensionStage3 ] [ DiagnosisHypertensionStage3 ]
        , test "prior Stage 2 + low reading (sys 95) -> steps down to Stage 1" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesWithHistory [ DiagnosisHypertensionStage2 ] [ DiagnosisHypertensionStage1 ]
        , test "prior Stage 3 + low reading (sys 95) -> steps down to Stage 2" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesWithHistory [ DiagnosisHypertensionStage3 ] [ DiagnosisHypertensionStage2 ]
        , test "prior Stage 1 + low reading (sys 95) -> stays at Stage 1 (no lower stage)" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesWithHistory [ DiagnosisHypertensionStage1 ] [ DiagnosisHypertensionStage1 ]
        , test "no prior hypertension + low reading (sys 95) -> no diagnosis" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesWithHistory [] []
        ]


generateNCDDiagnosesTest : Test
generateNCDDiagnosesTest =
    describe "generateNCDDiagnoses (first encounter; oracle = NCDs tab of clinical sheet)"
        [ test "1. normal 120/80 -> no diagnosis" <|
            \_ ->
                baseMeasurements
                    |> expectDiagnoses []
        , test "2. sys 145/dia 95 -> Stage 1 (sheet: Stage One, both values in the 140-159 / 90-99 band)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95)
                    |> expectDiagnoses [ DiagnosisHypertensionStage1 ]
        , test "3. sys 165/dia 85 -> Stage 2 (sheet: Stage Two)" <|
            \_ ->
                (baseMeasurements |> withVitals 165 85)
                    |> expectDiagnoses [ DiagnosisHypertensionStage2 ]
        , test "4. sys 185/dia 85 -> Stage 3 (sheet: Stage Three)" <|
            \_ ->
                (baseMeasurements |> withVitals 185 85)
                    |> expectDiagnoses [ DiagnosisHypertensionStage3 ]
        , test "5. coMorbidities {Hypertension}, normal BP -> Stage 1 (sheet: Stage One, medical history)" <|
            \_ ->
                (baseMeasurements |> withCoMorbidities (EverySet.singleton MedicalConditionHypertension))
                    |> expectDiagnoses [ DiagnosisHypertensionStage1 ]
        , test "6. sys 145 + creatinine 1.5 -> Stage 1 + Renal (sheet: Stage One with Renal Complications, creatinine >1.3)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95 |> withCreatinine 1.5)
                    |> expectDiagnoses [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ]
        , test "7. sys 145 + urine protein +1 -> Stage 1 + Renal (sheet: renal by protein >=+1)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95 |> withUrineProtein ProteinPlus1)
                    |> expectDiagnoses [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ]
        , test "8. sys 145 + coMorbidities {Diabetes} -> Stage 1 + Diabetes Initial (sheet: Stage One with Diabetes)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95 |> withCoMorbidities (EverySet.singleton MedicalConditionDiabetes))
                    |> expectDiagnoses [ DiagnosisHypertensionStage1, DiagnosisDiabetesInitial ]
        , test "9. sys 165 + coMorbidities {Diabetes} -> Stage 2 + Diabetes Initial (sheet: Stage Two with Diabetes)" <|
            \_ ->
                (baseMeasurements |> withVitals 165 85 |> withCoMorbidities (EverySet.singleton MedicalConditionDiabetes))
                    |> expectDiagnoses [ DiagnosisHypertensionStage2, DiagnosisDiabetesInitial ]
        , test "10. normal BP + fasting RBS 150 -> Diabetes Recurrent (sheet: Diabetes, fasting >126)" <|
            \_ ->
                (baseMeasurements |> withRandomBloodSugar True 150)
                    |> expectDiagnoses [ DiagnosisDiabetesRecurrent ]
        , test "11. normal BP + urine glucose +2 -> Diabetes Recurrent (sheet: Diabetes, urine glucose +2)" <|
            \_ ->
                (baseMeasurements |> withUrineGlucose GlucosePlus2)
                    |> expectDiagnoses [ DiagnosisDiabetesRecurrent ]
        , test "12. normal BP + creatinine 1.5 (no BP, no diabetes) -> Renal alone [CODE: tab only lists renal WITH a hypertension stage; code matches it independently]" <|
            \_ ->
                (baseMeasurements |> withCreatinine 1.5)
                    |> expectDiagnoses [ DiagnosisRenalComplications ]
        , test "13. sys 145/dia 80 (isolated systolic) -> Stage 1 [FINDING: code OR; sheet AND -> no diagnosis]" <|
            \_ ->
                (baseMeasurements |> withVitals 145 80)
                    |> expectDiagnoses [ DiagnosisHypertensionStage1 ]
        , test "14. normal BP + creatinine 1.3 -> no diagnosis (boundary: code uses >1.3)" <|
            \_ ->
                (baseMeasurements |> withCreatinine 1.3)
                    |> expectDiagnoses []
        ]


all : Test
all =
    describe "NCD diagnosis tests"
        [ stage1Test
        , stage2Test
        , stage3Test
        , lowerHypertensionStageTest
        , generateNCDDiagnosesTest
        , hypertensionHierarchyTest
        ]
