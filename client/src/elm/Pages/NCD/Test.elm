module Pages.NCD.Test exposing (all)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( CreatinineTestValue
        , GlucoseValue(..)
        , Measurement
        , MedicalCondition(..)
        , NCDMeasurements
        , ProteinValue(..)
        , RandomBloodSugarTestValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , UrineDipstickTestValue
        )
import Backend.NCDEncounter.Model as NCDEncounterModel
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.NCD.Activity.Types exposing (NextStepsTask(..))
import Pages.NCD.Activity.Utils exposing (expectLaboratoryTask, resolveNextStepsTasks, resolvePreviousMaybeValue)
import Pages.NCD.Model exposing (AssembledData, PreviousEncounterData)
import Pages.NCD.Utils
    exposing
        ( generateNCDDiagnoses
        , lowerHypertensionStageCondition
        , patientIsPregnant
        , stage1BloodPressureCondition
        , stage2BloodPressureCondition
        , stage3BloodPressureCondition
        )
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)
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


{-| Wrap a measurement value into the shape stored on `NCDMeasurements`, with
`dummyDate` as `dateMeasured`.
-}
wrapMeasurement : value -> Maybe ( EntityUuid id, Measurement encounter value )
wrapMeasurement value =
    TestFixtures.wrapMeasurement dummyDate value


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
    { measurements | vitals = wrapMeasurement (TestFixtures.vitalsValueWith sys dia) }


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
    { measurements | urineDipstickTest = wrapMeasurement (TestFixtures.urineGlucoseValue glucose) }


withRandomBloodSugar : Bool -> Float -> NCDMeasurements -> NCDMeasurements
withRandomBloodSugar fasting sugar measurements =
    { measurements | randomBloodSugarTest = wrapMeasurement (randomBloodSugarValue fasting sugar) }


{-| Pregnancy test as recorded on an encounter, with no execution date -- the
shape it has when the patient is reported as known to be pregnant, and when
the test was not performed. The encounter date is then used as the date of
the answer.
-}
withPregnancyTest : TestExecutionNote -> Maybe TestResult -> NCDMeasurements -> NCDMeasurements
withPregnancyTest executionNote testResult measurements =
    { measurements
        | pregnancyTest =
            wrapMeasurement
                { executionNote = executionNote
                , executionDate = Nothing
                , testResult = testResult
                }
    }


{-| Pregnancy test performed on a given date. This is the shape that used to
stop the test being offered at any later encounter.
-}
withPregnancyTestOn : NominalDate -> TestResult -> NCDMeasurements -> NCDMeasurements
withPregnancyTestOn executionDate testResult measurements =
    { measurements
        | pregnancyTest =
            wrapMeasurement
                { executionNote = TestNoteRunToday
                , executionDate = Just executionDate
                , testResult = Just testResult
                }
    }



-- ASSEMBLED DATA FIXTURE


{-| Dummy NCD encounter, carrying no diagnoses. With the history empty too, the
hypertension-hierarchy / determined-conditions pipeline steps are no-ops.
`assembledWithHistory` overrides `diagnoses` where a re-assessment is under test.
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
    TestFixtures.testParticipant dummyDate NCDEncounter


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


{-| A prior NCD encounter carrying the given measurements, held the given
number of months before the encounter being assessed.
-}
previousEncounterAt : Int -> NCDMeasurements -> PreviousEncounterData
previousEncounterAt monthsAgo measurements =
    { id = toEntityUuid ("prev-encounter-" ++ String.fromInt monthsAgo)
    , startDate = Date.add Date.Months -monthsAgo dummyDate
    , diagnoses = EverySet.empty
    , measurements = measurements
    }


{-| An `AssembledData` with one prior encounter in the history, and with the
encounter being assessed already carrying `encounterDiagnoses`.
-}
assembledWithHistory : List NCDDiagnosis -> List NCDDiagnosis -> NCDMeasurements -> AssembledData
assembledWithHistory encounterDiagnoses previousDiagnoses measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = { dummyEncounter | diagnoses = EverySet.fromList encounterDiagnoses }
    , participant = dummyParticipant
    , person = testPerson
    , measurements = measurements
    , previousEncountersData = [ previousEncounterWith previousDiagnoses ]
    }


{-| Like `expectDiagnoses`, but with one prior encounter in the history -- to
exercise the hypertension escalation / persistence / lowering logic.
-}
expectDiagnosesWithHistory : List NCDDiagnosis -> List NCDDiagnosis -> NCDMeasurements -> Expect.Expectation
expectDiagnosesWithHistory previousDiagnoses expected measurements =
    assembledWithHistory [] previousDiagnoses measurements
        |> generateNCDDiagnoses
        |> Expect.equal (EverySet.fromList expected)


{-| Like `expectDiagnosesWithHistory`, but the encounter being assessed already
carries diagnoses of its own -- the state every assessment after the first
measurement save runs in.
-}
expectDiagnosesOnReassessment :
    List NCDDiagnosis
    -> List NCDDiagnosis
    -> List NCDDiagnosis
    -> NCDMeasurements
    -> Expect.Expectation
expectDiagnosesOnReassessment encounterDiagnoses previousDiagnoses expected measurements =
    assembledWithHistory encounterDiagnoses previousDiagnoses measurements
        |> generateNCDDiagnoses
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


reassessmentTest : Test
reassessmentTest =
    -- Diagnoses are regenerated on every Vitals, CoMorbidities, RandomBloodSugar,
    -- UrineDipstick and Creatinine save, and the result is written back onto the
    -- encounter. Generation must therefore give the same answer on the second and
    -- third save of a visit as it gave on the first.
    describe "generateNCDDiagnoses - re-assessment of an encounter that already carries diagnoses"
        [ test "prior Stage 3 + low reading, Stage 2 already written -> stays Stage 2 (step down once per visit)" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage2 ]
                        [ DiagnosisHypertensionStage3 ]
                        [ DiagnosisHypertensionStage2 ]
        , test "prior Stage 2 + low reading, Stage 1 already written -> stays Stage 1" <|
            \_ ->
                (baseMeasurements |> withVitals 95 70)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage1 ]
                        [ DiagnosisHypertensionStage2 ]
                        [ DiagnosisHypertensionStage1 ]
        , test "no history, Stage 3 already written, reading corrected to 145/95 -> Stage 1" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage3 ]
                        []
                        [ DiagnosisHypertensionStage1 ]
        , test "no history, Stage 3 already written, reading corrected to 120/80 -> no diagnosis" <|
            \_ ->
                baseMeasurements
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage3 ]
                        []
                        []
        , test "prior Stage 1 + Stage-3 reading, Stage 3 already written -> stays Stage 3 (escalation is idempotent)" <|
            \_ ->
                (baseMeasurements |> withVitals 185 85)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage3 ]
                        [ DiagnosisHypertensionStage1 ]
                        [ DiagnosisHypertensionStage3 ]
        , test "prior Stage 3 + Stage-1 reading, Stage 3 already written -> stays Stage 3 (no downgrade)" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisHypertensionStage3 ]
                        [ DiagnosisHypertensionStage3 ]
                        [ DiagnosisHypertensionStage3 ]
        , test "diabetes co-morbidity unticked, Diabetes written, sugar count on file -> diagnosed from the sugar count" <|
            \_ ->
                (baseMeasurements |> withRandomBloodSugar True 150)
                    |> expectDiagnosesOnReassessment
                        [ DiagnosisDiabetesInitial ]
                        []
                        [ DiagnosisDiabetesRecurrent ]
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


{-| The Next Steps tasks a first encounter offers for the given measurements.
Generation runs first and its diagnoses are written onto the encounter, which is
the order the app follows: every measurement save re-assesses, and the tasks are
resolved from the set that assessment wrote.
-}
nextStepsTasksFor : NCDMeasurements -> List NextStepsTask
nextStepsTasksFor measurements =
    let
        assembled =
            ncdAssembled measurements

        encounter =
            assembled.encounter
    in
    resolveNextStepsTasks
        { assembled | encounter = { encounter | diagnoses = generateNCDDiagnoses assembled } }


nextStepsTasksTest : Test
nextStepsTasksTest =
    -- A blood sugar or urine glucose read at the point of care diagnoses
    -- diabetes during the initial phase of the encounter, so the initial-phase
    -- tasks have to treat it the same as a diabetes reported as a co-morbidity.
    -- Oracle: a patient diagnosed diabetic is medicated at the visit where the
    -- diagnosis is made.
    describe "resolveNextStepsTasks - diabetes found from a reading taken at the point of care"
        [ test "normal BP + blood sugar 250 -> medication is offered" <|
            \_ ->
                (baseMeasurements |> withRandomBloodSugar False 250)
                    |> nextStepsTasksFor
                    |> Expect.equal [ TaskMedicationDistribution ]
        , test "normal BP + urine glucose +3 -> medication is offered" <|
            \_ ->
                (baseMeasurements |> withUrineGlucose GlucosePlus3)
                    |> nextStepsTasksFor
                    |> Expect.equal [ TaskMedicationDistribution ]
        , test "Stage 1 BP + blood sugar 250 -> medication and referral, and no health education" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95 |> withRandomBloodSugar False 250)
                    |> nextStepsTasksFor
                    |> Expect.equal [ TaskMedicationDistribution, TaskReferral ]
        , test "Stage 1 BP alone -> health education only" <|
            \_ ->
                (baseMeasurements |> withVitals 145 95)
                    |> nextStepsTasksFor
                    |> Expect.equal [ TaskHealthEducation ]
        , test "diabetes reported as a co-morbidity -> medication is offered, as before" <|
            \_ ->
                (baseMeasurements |> withCoMorbidities (EverySet.singleton MedicalConditionDiabetes))
                    |> nextStepsTasksFor
                    |> Expect.equal [ TaskMedicationDistribution ]
        , test "normal BP, no diabetes -> no tasks at all" <|
            \_ ->
                baseMeasurements
                    |> nextStepsTasksFor
                    |> Expect.equal []
        ]


resolvePreviousMaybeValueTest : Test
resolvePreviousMaybeValueTest =
    let
        -- The history is passed most recent first, the order
        -- generatePreviousEncountersData produces.
        resolveSysWith previous =
            let
                assembled =
                    ncdAssembled emptyNCDMeasurements
            in
            resolvePreviousMaybeValue { assembled | previousEncountersData = previous } .vitals .sys
    in
    describe "resolvePreviousMaybeValue"
        [ test "the previous value is the most recent recorded one" <|
            \_ ->
                resolveSysWith
                    [ previousEncounterAt 1 (emptyNCDMeasurements |> withVitals 150 80)
                    , previousEncounterAt 2 (emptyNCDMeasurements |> withVitals 100 80)
                    ]
                    |> Expect.equal (Just 150)
        , test "an encounter where the value was not recorded is skipped" <|
            \_ ->
                resolveSysWith
                    [ previousEncounterAt 1 emptyNCDMeasurements
                    , previousEncounterAt 2 (emptyNCDMeasurements |> withVitals 100 80)
                    ]
                    |> Expect.equal (Just 100)
        ]


pregnancyAcrossEncountersTest : Test
pregnancyAcrossEncountersTest =
    let
        -- The history is passed most recent first, the order
        -- generatePreviousEncountersData produces.
        isPregnantWith previous measurements =
            let
                assembled =
                    ncdAssembled measurements
            in
            patientIsPregnant { assembled | previousEncountersData = previous }
    in
    describe "patientIsPregnant"
        [ test "positive test at the encounter being assessed" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive))
                    |> isPregnantWith []
                    |> Expect.equal True
        , test "reported as known to be pregnant at the encounter being assessed" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteKnownAsPositive Nothing)
                    |> isPregnantWith []
                    |> Expect.equal True
        , test "negative test at the encounter being assessed" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestNegative))
                    |> isPregnantWith []
                    |> Expect.equal False
        , test "the question was never answered" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith []
                    |> Expect.equal False
        , test "positive a month ago, nothing recorded since" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith
                        [ previousEncounterAt 1 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "reported as known to be pregnant a month ago, nothing recorded since" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith
                        [ previousEncounterAt 1 (emptyNCDMeasurements |> withPregnancyTest TestNoteKnownAsPositive Nothing) ]
                    |> Expect.equal True
        , test "positive 8 months ago is still within the validity period" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith
                        [ previousEncounterAt 8 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "positive 9 months ago has expired" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith
                        [ previousEncounterAt 9 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal False
        , -- Recording the test at all means "is this patient known to be
          -- pregnant" was answered, so a test that was not performed still
          -- carries that answer.
          test "the test was not indicated at the encounter being assessed, over an earlier positive" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteNotIndicated Nothing)
                    |> isPregnantWith
                        [ previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal False
        , -- A test that could not be run, or whose result is not conclusive,
          -- says nothing about the patient.
          test "an indeterminate result does not override an earlier positive" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestIndeterminate))
                    |> isPregnantWith
                        [ previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "a test that could not be run does not override an earlier positive" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteNoEquipment Nothing)
                    |> isPregnantWith
                        [ previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "reported as known to be pregnant, then not known three months later" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteNotIndicated Nothing)
                    |> isPregnantWith
                        [ previousEncounterAt 3 (emptyNCDMeasurements |> withPregnancyTest TestNoteKnownAsPositive Nothing) ]
                    |> Expect.equal False
        , -- The expiry boundary, measured from an execution date rather than
          -- from the date of the encounter that recorded it.
          test "a positive dated 8 months back is still current when recorded today" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTestOn (Date.add Date.Months -8 dummyDate) TestPositive)
                    |> isPregnantWith []
                    |> Expect.equal True
        , test "a positive dated 9 months back has expired when recorded today" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTestOn (Date.add Date.Months -9 dummyDate) TestPositive)
                    |> isPregnantWith []
                    |> Expect.equal False
        , test "an answer dated before an earlier encounter's answer does not override it" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTestOn (Date.add Date.Months -10 dummyDate) TestPositive)
                    |> isPregnantWith
                        [ previousEncounterAt 1 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "a negative at the encounter being assessed overrides an earlier positive" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestNegative))
                    |> isPregnantWith
                        [ previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal False
        , test "the most recent answer wins when several encounters answered" <|
            \_ ->
                emptyNCDMeasurements
                    |> isPregnantWith
                        [ previousEncounterAt 1 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestNegative))
                        , previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive))
                        ]
                    |> Expect.equal False
        ]


pregnancyTestExpectedTest : Test
pregnancyTestExpectedTest =
    let
        pregnancyTestExpectedWith previous =
            let
                assembled =
                    ncdAssembled emptyNCDMeasurements
            in
            expectLaboratoryTask dummyDate { assembled | previousEncountersData = previous } TaskPregnancyTest
    in
    describe "expectLaboratoryTask TaskPregnancyTest"
        [ test "offered at the first encounter" <|
            \_ ->
                pregnancyTestExpectedWith []
                    |> Expect.equal True
        , test "offered again after a negative test at a previous encounter" <|
            \_ ->
                pregnancyTestExpectedWith
                    [ previousEncounterAt 1
                        (emptyNCDMeasurements
                            |> withPregnancyTestOn (Date.add Date.Months -1 dummyDate) TestNegative
                        )
                    ]
                    |> Expect.equal True
        , test "offered again after a positive test at a previous encounter" <|
            \_ ->
                pregnancyTestExpectedWith
                    [ previousEncounterAt 1
                        (emptyNCDMeasurements
                            |> withPregnancyTestOn (Date.add Date.Months -1 dummyDate) TestPositive
                        )
                    ]
                    |> Expect.equal True
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
        , reassessmentTest
        , nextStepsTasksTest
        , resolvePreviousMaybeValueTest
        , pregnancyAcrossEncountersTest
        , pregnancyTestExpectedTest
        ]
