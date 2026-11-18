module Pages.NCD.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( CreatinineTestValue
        , GlucoseValue(..)
        , LaboratoryTest(..)
        , Measurement
        , MedicalCondition(..)
        , NCDMeasurements
        , NonReferralSign(..)
        , ProteinValue(..)
        , RandomBloodSugarTestValue
        , ReasonForNonReferral(..)
        , RecommendedTreatmentSign(..)
        , ReferToFacilitySign(..)
        , ReferralFacility(..)
        , ReferralValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , UrineDipstickTestValue
        )
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, laboratoryTestToString)
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.NCDActivity.Model
import Backend.NCDEncounter.Model as NCDEncounterModel
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (LaboratoryTask(..), emptyRandomBloodSugarTestForm)
import Measurement.Utils exposing (rdtKnownAsPositiveUpdate, rdtTestPerformedUpdate, toHIVTestValueWithDefault, toPregnancyTestValueWithDefault, toRandomBloodSugarTestValueWithDefault)
import Pages.NCD.Activity.Model exposing (Msg(..), emptyModel)
import Pages.NCD.Activity.Types exposing (NextStepsTask(..))
import Pages.NCD.Activity.Update
import Pages.NCD.Activity.Utils exposing (activityCompleted, expectLaboratoryTask, nextStepsTaskCompleted, resolveNextStepsTasks, resolvePreviousMaybeValue)
import Pages.NCD.Model exposing (AssembledData, NCDEncounterPhase(..), PreviousEncounterData, ReferralForm, emptyReferralForm)
import Pages.NCD.RecurrentActivity.Model as RecurrentModel
import Pages.NCD.RecurrentActivity.Types as RecurrentTypes
import Pages.NCD.RecurrentActivity.Update as RecurrentUpdate
import Pages.NCD.RecurrentActivity.Utils as RecurrentUtils exposing (labsResultsValueWithResult, resolveLaboratoryResultTasks, resolvedLabsResultsValue, withCompletedTestsFromMeasurements)
import Pages.NCD.Utils
    exposing
        ( generateAssembledData
        , generateNCDDiagnoses
        , lowerHypertensionStageCondition
        , patientIsPregnant
        , referForRenalComplications
        , referralFormWithDefault
        , resolveReferralInputsAndTasks
        , stage1BloodPressureCondition
        , stage2BloodPressureCondition
        , stage3BloodPressureCondition
        , toReferralValueWithDefault
        )
import RemoteData
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)
import Time
import Translate.Model exposing (Language(..))



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


{-| An NCD encounter as stored in the database: its start date, the diagnoses
written onto it, and the measurements taken at it.
-}
type alias StoredEncounter =
    { id : String
    , startDate : NominalDate
    , diagnoses : List NCDDiagnosis
    , measurements : NCDMeasurements
    }


{-| Assemble the encounter with the given id from a database holding the given
encounters of one participant -- the way every NCD page and the assessment run
after a save build their data.
-}
assembledFromDb : String -> List StoredEncounter -> Maybe AssembledData
assembledFromDb encounterId storedEncounters =
    let
        encounters =
            List.map
                (\stored ->
                    ( toEntityUuid stored.id
                    , { dummyEncounter
                        | startDate = stored.startDate
                        , diagnoses = EverySet.fromList stored.diagnoses
                      }
                    )
                )
                storedEncounters

        db =
            { emptyModelIndexedDb
                | ncdEncounters = Dict.fromList <| List.map (Tuple.mapSecond RemoteData.Success) encounters
                , ncdEncountersByParticipant =
                    Dict.singleton dummyEncounter.participant (RemoteData.Success <| Dict.fromList encounters)
                , ncdMeasurements =
                    Dict.fromList <|
                        List.map (\stored -> ( toEntityUuid stored.id, RemoteData.Success stored.measurements )) storedEncounters
                , individualParticipants = Dict.singleton dummyEncounter.participant (RemoteData.Success dummyParticipant)
                , people = Dict.singleton dummyParticipant.person (RemoteData.Success testPerson)
            }
    in
    generateAssembledData (toEntityUuid encounterId) db
        |> RemoteData.toMaybe


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


olderEncounterTest : Test
olderEncounterTest =
    -- A lab result can be entered for an encounter after a later encounter of
    -- the same participant has started. The history of the older encounter is
    -- still made of the encounters before it. Here the older encounter is held
    -- on dummyDate, and the later one a month after it.
    let
        olderEncounter diagnoses measurements =
            { id = "older-encounter"
            , startDate = dummyDate
            , diagnoses = diagnoses
            , measurements = measurements
            }

        encounterAt monthsAfter diagnoses =
            { id = "encounter-" ++ String.fromInt monthsAfter
            , startDate = Date.add Date.Months monthsAfter dummyDate
            , diagnoses = diagnoses
            , measurements = emptyNCDMeasurements
            }

        diagnosesOfOlderEncounter storedEncounters =
            assembledFromDb "older-encounter" storedEncounters
                |> Maybe.map generateNCDDiagnoses
    in
    describe "generateAssembledData - an encounter assessed after a later one started"
        [ test "Stage 1 encounter + creatinine 2.0, later encounter at Stage 3 -> stays Stage 1" <|
            \_ ->
                diagnosesOfOlderEncounter
                    [ olderEncounter [ DiagnosisHypertensionStage1 ] (baseMeasurements |> withVitals 145 95 |> withCreatinine 2.0)
                    , encounterAt 1 [ DiagnosisHypertensionStage3 ]
                    ]
                    |> Expect.equal (Just <| EverySet.fromList [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ])
        , test "blood sugar 250, later encounter already diabetic -> diabetes is kept" <|
            \_ ->
                diagnosesOfOlderEncounter
                    [ olderEncounter [] (baseMeasurements |> withRandomBloodSugar False 250)
                    , encounterAt 1 [ DiagnosisDiabetesRecurrent ]
                    ]
                    |> Expect.equal (Just <| EverySet.singleton DiagnosisDiabetesRecurrent)
        , test "renal complications found at recurrent phase, later encounter already has them -> referral is offered" <|
            \_ ->
                assembledFromDb "older-encounter"
                    [ olderEncounter [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ] baseMeasurements
                    , encounterAt 1 [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ]
                    ]
                    |> Maybe.map (referForRenalComplications NCDEncounterPhaseRecurrent)
                    |> Expect.equal (Just True)
        , test "Stage-1 reading, earlier encounter at Stage 3 -> stays Stage 3 (earlier encounters are history)" <|
            \_ ->
                diagnosesOfOlderEncounter
                    [ olderEncounter [] (baseMeasurements |> withVitals 145 95)
                    , encounterAt -1 [ DiagnosisHypertensionStage3 ]
                    ]
                    |> Expect.equal (Just <| EverySet.singleton DiagnosisHypertensionStage3)
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
        , -- A record can carry a result left behind when the run was withdrawn:
          -- older builds keep it when "performed" is changed to No.
          test "a negative left behind by a withdrawn test does not override an earlier positive" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteLackOfReagents (Just TestNegative))
                    |> isPregnantWith
                        [ previousEncounterAt 2 (emptyNCDMeasurements |> withPregnancyTest TestNoteRunToday (Just TestPositive)) ]
                    |> Expect.equal True
        , test "a positive left behind by a withdrawn test does not count" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteLackOfReagents (Just TestPositive))
                    |> isPregnantWith []
                    |> Expect.equal False
        , test "a positive left behind on a test that was not indicated still answers No" <|
            \_ ->
                (emptyNCDMeasurements |> withPregnancyTest TestNoteNotIndicated (Just TestPositive))
                    |> isPregnantWith []
                    |> Expect.equal False
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
        , olderEncounterTest
        , nextStepsTasksTest
        , resolvePreviousMaybeValueTest
        , pregnancyAcrossEncountersTest
        , pregnancyTestExpectedTest
        , repeatedTapOnSavedTest
        , randomBloodSugarNotPerformedTest
        , labsEntryClosingTest
        , referralTasksTest
        ]


{-| The bool inputs of the NCD rapid tests fire on every tap, including one
that repeats the answer shown. On a test loaded from the record the form in
the model is still empty, so the update has to act on the form as displayed
for a repeated tap to leave the saved run and its result alone.
-}
repeatedTapOnSavedTest : Test
repeatedTapOnSavedTest =
    let
        encounterId =
            toEntityUuid "dummy-encounter"

        savedHIVTest =
            { executionNote = TestNoteRunToday
            , executionDate = Nothing
            , testPrerequisites = Nothing
            , testResult = Just TestNegative
            , hivSigns = Nothing
            }

        savedPregnancyTest =
            { executionNote = TestNoteRunToday
            , executionDate = Nothing
            , testResult = Just TestNegative
            }

        db =
            { emptyModelIndexedDb
                | ncdMeasurements =
                    Dict.singleton encounterId
                        (RemoteData.Success
                            { emptyNCDMeasurements
                                | hivTest = wrapMeasurement savedHIVTest
                                , pregnancyTest = wrapMeasurement savedPregnancyTest
                            }
                        )
            }

        formAfter msg =
            Pages.NCD.Activity.Update.update dummyDate encounterId db msg emptyModel
                |> (\( model, _, _ ) -> model.laboratoryData)
    in
    describe "a repeated tap on a test loaded from the record"
        [ test "HIV test: Yes on performed keeps the saved result" <|
            \_ ->
                formAfter (SetHIVTestFormBoolInput rdtTestPerformedUpdate True)
                    |> .hivTestForm
                    |> toHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestNegative))
        , test "HIV test: No on known as positive keeps the saved run and result" <|
            \_ ->
                formAfter (SetHIVTestFormBoolInput rdtKnownAsPositiveUpdate False)
                    |> .hivTestForm
                    |> toHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map (\value -> ( value.executionNote, value.testResult ))
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestNegative ))
        , test "pregnancy test: Yes on performed keeps the saved result" <|
            \_ ->
                formAfter (SetPregnancyTestFormBoolInput rdtTestPerformedUpdate True)
                    |> .pregnancyTestForm
                    |> toPregnancyTestValueWithDefault (Just savedPregnancyTest)
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestNegative))
        , test "pregnancy test: No on performed withdraws the saved result" <|
            \_ ->
                formAfter (SetPregnancyTestFormBoolInput rdtTestPerformedUpdate False)
                    |> .pregnancyTestForm
                    |> toPregnancyTestValueWithDefault (Just savedPregnancyTest)
                    |> Maybe.map .testResult
                    |> Expect.equal Nothing
        ]


{-| A random blood sugar test sent to the lab can be corrected to "not
performed" while the encounter is open. The correction owes no lab result.
-}
randomBloodSugarNotPerformedTest : Test
randomBloodSugarNotPerformedTest =
    let
        encounterId =
            toEntityUuid "dummy-encounter"

        rbsValue executionNote =
            { executionNote = executionNote
            , executionDate = Nothing
            , testPrerequisites = Just (EverySet.singleton NoTestPrerequisites)
            , sugarCount = Nothing
            , originatingEncounter = Nothing
            }

        sentToLab =
            rbsValue TestNoteRunToday

        db =
            { emptyModelIndexedDb
                | ncdMeasurements =
                    Dict.singleton encounterId
                        (RemoteData.Success { emptyNCDMeasurements | randomBloodSugarTest = wrapMeasurement sentToLab })
            }

        -- The form as the "No" answer to "performed?" leaves it.
        answeredNo =
            { emptyRandomBloodSugarTestForm
                | testPerformed = Just False
                , testPerformedDirty = True
                , testPerformedTodayDirty = True
                , executionNoteDirty = True
                , executionDateDirty = True
                , sugarCountDirty = True
            }

        laboratoryData =
            emptyModel.laboratoryData

        savedAfterCorrection =
            Pages.NCD.Activity.Update.update dummyDate
                encounterId
                db
                (SetRandomBloodSugarTestExecutionNote TestNoteNotIndicated)
                { emptyModel | laboratoryData = { laboratoryData | randomBloodSugarTestForm = answeredNo } }
                |> (\( model, _, _ ) -> model.laboratoryData.randomBloodSugarTestForm)
                |> toRandomBloodSugarTestValueWithDefault (Just sentToLab)

        resultTasksWith executionNote =
            { emptyNCDMeasurements | randomBloodSugarTest = wrapMeasurement (rbsValue executionNote) }
                |> ncdAssembled
                |> resolveLaboratoryResultTasks
    in
    describe "a random blood sugar test corrected to not performed"
        [ test "the correction saves no prerequisites" <|
            \_ ->
                savedAfterCorrection
                    |> Maybe.map (\value -> ( value.executionNote, value.testPrerequisites ))
                    |> Expect.equal (Just ( TestNoteNotIndicated, Nothing ))
        , test "a test that was not performed owes no lab result" <|
            \_ ->
                resultTasksWith TestNoteNotIndicated
                    |> Expect.equal []
        , test "a test sent to the lab owes a lab result" <|
            \_ ->
                resultTasksWith TestNoteRunToday
                    |> Expect.equal [ TaskRandomBloodSugarTest ]
        ]


{-| The labs entry is written from the device's copy, which can be behind the
test measurements. Completed tests are read from the measurements too, and the
entry is resolved only when every performed test is completed.
-}
labsEntryClosingTest : Test
labsEntryClosingTest =
    let
        labsEntry =
            TestFixtures.labsEntry dummyDate

        -- Test names, sorted, since set order follows insertion.
        names =
            EverySet.toList >> List.map laboratoryTestToString >> List.sort

        tests value =
            ( names value.performedTests, names value.completedTests )

        withMeasurements measurements entry =
            withCompletedTestsFromMeasurements measurements entry
                |> tests

        -- The entry saved when the test's result is entered, with no Next
        -- Steps required.
        savedWithResult test measurements entry =
            labsResultsValueWithResult dummyDate (ncdAssembled measurements) test entry
                |> Maybe.map (\value -> ( tests value, value.resolutionDate ))

        bothResults =
            emptyNCDMeasurements |> withCreatinine 90 |> withRandomBloodSugar False 6

        -- Resolved two days ago, so its resolution date is not after today.
        resolvedEntry =
            let
                entry =
                    labsEntry [ TestRandomBloodSugar ] [ TestRandomBloodSugar ]
            in
            { entry | resolutionDate = Date.add Date.Days -2 dummyDate }

        sentToLab =
            { emptyNCDMeasurements
                | randomBloodSugarTest =
                    wrapMeasurement
                        { executionNote = TestNoteRunToday
                        , executionDate = Nothing
                        , testPrerequisites = Nothing
                        , sugarCount = Nothing
                        , originatingEncounter = Nothing
                        }
            }

        notPerformedWithResult =
            let
                value =
                    creatinineValueWith 90
            in
            { emptyNCDMeasurements | creatinineTest = wrapMeasurement { value | executionNote = TestNoteNotIndicated } }
    in
    describe "closing the NCD labs entry"
        [ test "a test whose measurement holds its result is completed" <|
            \_ ->
                withMeasurements (withCreatinine 90 sentToLab) (labsEntry [ TestCreatinine, TestRandomBloodSugar ] [])
                    |> Expect.equal ( [ "creatinine", "random-blood-sugar" ], [ "creatinine" ] )
        , test "a test run with its result is performed and completed, though the entry does not list it" <|
            \_ ->
                withMeasurements (withCreatinine 90 sentToLab) (labsEntry [ TestRandomBloodSugar ] [])
                    |> Expect.equal ( [ "creatinine", "random-blood-sugar" ], [ "creatinine" ] )
        , test "a test sent to the lab stays pending until its result is entered" <|
            \_ ->
                withMeasurements sentToLab (labsEntry [ TestRandomBloodSugar ] [])
                    |> Expect.equal ( [ "random-blood-sugar" ], [] )
        , test "a test not performed is not added, though a result is stored" <|
            \_ ->
                withMeasurements notPerformedWithResult (labsEntry [ TestRandomBloodSugar ] [])
                    |> Expect.equal ( [ "random-blood-sugar" ], [] )
        , test "a test completed on the entry stays completed" <|
            \_ ->
                withMeasurements emptyNCDMeasurements (labsEntry [ TestCreatinine ] [ TestCreatinine ])
                    |> Expect.equal ( [ "creatinine" ], [ "creatinine" ] )
        , test "a completed test no longer performed does not resolve an entry with a pending one" <|
            \_ ->
                resolvedLabsResultsValue dummyDate (labsEntry [ TestRandomBloodSugar ] [ TestCreatinine ])
                    |> Expect.equal Nothing
        , test "an entry with every performed test completed is resolved today" <|
            \_ ->
                resolvedLabsResultsValue dummyDate (labsEntry [ TestCreatinine ] [ TestCreatinine ])
                    |> Maybe.map .resolutionDate
                    |> Expect.equal (Just dummyDate)
        , test "an entry behind its measurements is resolved with the tests it was missing" <|
            \_ ->
                withCompletedTestsFromMeasurements bothResults
                    (labsEntry [ TestCreatinine, TestRandomBloodSugar ] [ TestRandomBloodSugar ])
                    |> resolvedLabsResultsValue dummyDate
                    |> Maybe.map (\value -> ( tests value, value.resolutionDate ))
                    |> Expect.equal (Just ( ( [ "creatinine", "random-blood-sugar" ], [ "creatinine", "random-blood-sugar" ] ), dummyDate ))
        , test "entering the last result resolves the entry today" <|
            \_ ->
                savedWithResult TestCreatinine (withCreatinine 90 emptyNCDMeasurements) (labsEntry [ TestCreatinine ] [])
                    |> Expect.equal (Just ( ( [ "creatinine" ], [ "creatinine" ] ), dummyDate ))
        , test "a result entered again saves the completed tests the entry was missing" <|
            \_ ->
                savedWithResult TestRandomBloodSugar bothResults (labsEntry [ TestCreatinine, TestRandomBloodSugar ] [ TestRandomBloodSugar ])
                    |> Expect.equal (Just ( ( [ "creatinine", "random-blood-sugar" ], [ "creatinine", "random-blood-sugar" ] ), dummyDate ))
        , test "a result entered again resolves an open entry that already holds every completed test" <|
            \_ ->
                savedWithResult TestRandomBloodSugar (withRandomBloodSugar False 6 emptyNCDMeasurements) (labsEntry [ TestRandomBloodSugar ] [ TestRandomBloodSugar ])
                    |> Expect.equal (Just ( ( [ "random-blood-sugar" ], [ "random-blood-sugar" ] ), dummyDate ))
        , test "a result entered again saves nothing when the entry is already resolved" <|
            \_ ->
                savedWithResult TestRandomBloodSugar (withRandomBloodSugar False 6 emptyNCDMeasurements) resolvedEntry
                    |> Expect.equal Nothing
        , test "an entry resolved on an earlier day keeps its resolution date" <|
            \_ ->
                resolvedLabsResultsValue dummyDate resolvedEntry
                    |> Expect.equal Nothing
        , test "an expired entry is not resolved" <|
            \_ ->
                resolvedLabsResultsValue (Date.add Date.Days 31 dummyDate) (labsEntry [ TestCreatinine ] [ TestCreatinine ])
                    |> Expect.equal Nothing
        ]



-- REFERRAL TASKS
--
-- The initial Next Steps and the lab-results Next Steps of one NCD visit share
-- one referral record. For a pregnant woman with hypertension the initial phase
-- asks for the ANC services referral, while the lab-results phase asks for the
-- hospital referral (renal complications found from lab results). Each phase's
-- Referral task must be decided by the answer to its own question, and the
-- question of one facility must not open already answered because the other
-- facility's answer is on the record.


{-| The record as the lab-results phase saves it when the hospital referral is
answered Yes and the referral form was handed.
-}
hospitalReferred : ReferralValue
hospitalReferred =
    { referralSigns = EverySet.fromList [ ReferToHospital, ReferralFormHospital ]
    , nonReferralReasons = Nothing
    }


{-| The same record as it reads after a round trip through the backend, where
an empty list of reasons decodes as an empty set rather than as nothing.
-}
hospitalReferredAfterSync : ReferralValue
hospitalReferredAfterSync =
    { hospitalReferred | nonReferralReasons = Just EverySet.empty }


{-| The record saved when the hospital referral is answered No, with a reason.
No referral sign is set, so the answer lives only in the reasons.
-}
hospitalNotReferred : ReferralValue
hospitalNotReferred =
    { referralSigns = EverySet.singleton NoReferToFacilitySigns
    , nonReferralReasons = Just (EverySet.singleton (NonReferralReasonHospital NoAmbulance))
    }


{-| The record saved when the ANC services referral is answered Yes, the form
handed and the patient accompanied.
-}
ancReferred : ReferralValue
ancReferred =
    { referralSigns = EverySet.fromList [ ReferToANCServices, ReferralFormANCServices, AccompanyToANCServices ]
    , nonReferralReasons = Nothing
    }


{-| The record saved when the ANC services referral is answered No, with a
reason.
-}
ancNotReferred : ReferralValue
ancNotReferred =
    { referralSigns = EverySet.singleton NoReferToFacilitySigns
    , nonReferralReasons = Just (EverySet.singleton (NonReferralReasonANCServices ClientRefused))
    }


knownPregnant : NCDMeasurements -> NCDMeasurements
knownPregnant =
    withPregnancyTest TestNoteKnownAsPositive Nothing


testedNotPregnant : NCDMeasurements -> NCDMeasurements
testedNotPregnant =
    withPregnancyTest TestNoteRunToday (Just TestNegative)


{-| Measurements of a visit whose medication was already given, so the
Referral task is the only Next Steps task left to decide whether Next Steps is
complete. Methyldopa is accepted for pregnant and non-pregnant patients alike.
-}
referralMeasurements : (NCDMeasurements -> NCDMeasurements) -> Maybe ReferralValue -> NCDMeasurements
referralMeasurements pregnancy referral =
    { emptyNCDMeasurements
        | medicationDistribution =
            wrapMeasurement
                { recommendedTreatmentSigns = EverySet.singleton TreatmentMethyldopa2
                , guidanceSigns = EverySet.empty
                }
        , referral = Maybe.andThen wrapMeasurement referral
    }
        |> pregnancy


{-| A first visit carrying the given diagnoses on the encounter.
-}
referralAssembled : (NCDMeasurements -> NCDMeasurements) -> List NCDDiagnosis -> Maybe ReferralValue -> AssembledData
referralAssembled pregnancy diagnoses referral =
    let
        assembled =
            ncdAssembled (referralMeasurements pregnancy referral)

        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | diagnoses = EverySet.fromList diagnoses } }


{-| A later visit: hypertension was diagnosed, and pregnancy recorded, at a
visit two months earlier. Only the renal complications found from today's lab
results are on today's encounter.
-}
laterVisitAssembled : Maybe ReferralValue -> AssembledData
laterVisitAssembled referral =
    let
        assembled =
            referralAssembled identity [ DiagnosisRenalComplications ] referral

        previous =
            previousEncounterAt 2 (knownPregnant emptyNCDMeasurements)
    in
    { assembled | previousEncountersData = [ { previous | diagnoses = EverySet.singleton DiagnosisHypertensionStage2 } ] }


{-| The shapes of "pregnant woman with hypertension" the criteria cover. Each
one carries the renal complications that make the lab-results phase ask for a
hospital referral. Stage 3 is the stage that sends a woman who is not pregnant
to hospital, so it checks that pregnancy still decides the facility.
-}
pregnantWithHypertension : List ( String, Maybe ReferralValue -> AssembledData )
pregnantWithHypertension =
    [ ( "Stage 1 at this visit", referralAssembled knownPregnant [ DiagnosisHypertensionStage1, DiagnosisRenalComplications ] )
    , ( "Stage 3 at this visit", referralAssembled knownPregnant [ DiagnosisHypertensionStage3, DiagnosisRenalComplications ] )
    , ( "hypertension and pregnancy from an earlier visit", laterVisitAssembled )
    ]


{-| Stage 3 hypertension with renal complications, for a woman who is not
pregnant: a negative test, and no pregnancy answer at all.
-}
notPregnantWithHypertension : List ( String, Maybe ReferralValue -> AssembledData )
notPregnantWithHypertension =
    [ ( "pregnancy test negative", referralAssembled testedNotPregnant [ DiagnosisHypertensionStage3, DiagnosisRenalComplications ] )
    , ( "no pregnancy answer", referralAssembled identity [ DiagnosisHypertensionStage3, DiagnosisRenalComplications ] )
    ]


initialReferralDone : AssembledData -> Bool
initialReferralDone assembled =
    nextStepsTaskCompleted assembled TaskReferral


recurrentReferralDone : AssembledData -> Bool
recurrentReferralDone assembled =
    RecurrentUtils.nextStepsTaskCompleted assembled RecurrentTypes.TaskReferral


nextStepsCompleted : AssembledData -> Bool
nextStepsCompleted assembled =
    activityCompleted dummyDate assembled Backend.NCDActivity.Model.NextSteps


{-| The answers the Referral form of a phase counts as its tasks, for the given
form as displayed. The first one is the "referred to the facility?" question.
-}
referralTasks : NCDEncounterPhase -> AssembledData -> ReferralForm -> List (Maybe Bool)
referralTasks phase assembled form =
    resolveReferralInputsAndTasks English phase assembled SetReferralBoolInput SetFacilityNonReferralReason form
        |> Tuple.second


{-| The form each phase displays: the form in the model, filled from the saved
record.
-}
displayed : ReferralValue -> ReferralForm -> ReferralForm
displayed saved form =
    referralFormWithDefault form (Just saved)


referralDb : ReferralValue -> Backend.Model.ModelIndexedDb
referralDb saved =
    { emptyModelIndexedDb
        | ncdMeasurements =
            Dict.singleton (toEntityUuid "dummy-encounter")
                (RemoteData.Success { emptyNCDMeasurements | referral = wrapMeasurement saved })
    }


{-| The referral form in the initial phase's model after the nurse's inputs,
with the given record already saved.
-}
initialFormAfter : ReferralValue -> List Msg -> ReferralForm
initialFormAfter saved msgs =
    List.foldl
        (\msg model ->
            Pages.NCD.Activity.Update.update dummyDate (toEntityUuid "dummy-encounter") (referralDb saved) msg model
                |> (\( updated, _, _ ) -> updated)
        )
        emptyModel
        msgs
        |> .nextStepsData
        |> .referralForm


{-| The referral form in the lab-results phase's model after the nurse's
inputs, with the given record already saved.
-}
recurrentFormAfter : ReferralValue -> List RecurrentModel.Msg -> ReferralForm
recurrentFormAfter saved msgs =
    List.foldl
        (\msg model ->
            RecurrentUpdate.update dummyDate (toEntityUuid "dummy-encounter") (referralDb saved) msg model
                |> (\( updated, _, _ ) -> updated)
        )
        RecurrentModel.emptyModel
        msgs
        |> .nextStepsData
        |> .referralForm


{-| What the ANC "referred?" answer does to the form, as the Referral form
sets it.
-}
setReferToANCServices : Bool -> ReferralForm -> ReferralForm
setReferToANCServices value form =
    { form | referToANCServices = Just value, referralFormANCServices = Nothing, accompanyToANCServices = Nothing }


setReferralFormANCServices : Bool -> ReferralForm -> ReferralForm
setReferralFormANCServices value form =
    { form | referralFormANCServices = Just value }


setAccompanyToANCServices : Bool -> ReferralForm -> ReferralForm
setAccompanyToANCServices value form =
    { form | accompanyToANCServices = Just value }


{-| What the hospital "referred?" answer does to the form, as the Referral form
sets it.
-}
setReferToHospital : Bool -> ReferralForm -> ReferralForm
setReferToHospital value form =
    { form | referToHospital = Just value, referralFormHospital = Nothing }


setReferralFormHospital : Bool -> ReferralForm -> ReferralForm
setReferralFormHospital value form =
    { form | referralFormHospital = Just value }


{-| The nurse answers the ANC referral Yes, hands the form and accompanies.
-}
ancYesInputs : List Msg
ancYesInputs =
    [ SetReferralBoolInput setReferToANCServices True
    , SetReferralBoolInput setReferralFormANCServices True
    , SetReferralBoolInput setAccompanyToANCServices True
    ]


{-| The nurse answers the ANC referral No and picks a reason.
-}
ancNoInputs : List Msg
ancNoInputs =
    [ SetReferralBoolInput setReferToANCServices False
    , SetFacilityNonReferralReason Nothing FacilityANCServices ClientRefused
    ]


hospitalYesInputs : List RecurrentModel.Msg
hospitalYesInputs =
    [ RecurrentModel.SetReferralBoolInput setReferToHospital True
    , RecurrentModel.SetReferralBoolInput setReferralFormHospital True
    ]


hospitalNoInputs : List RecurrentModel.Msg
hospitalNoInputs =
    [ RecurrentModel.SetReferralBoolInput setReferToHospital False
    , RecurrentModel.SetFacilityNonReferralReason Nothing FacilityHospital NoAmbulance
    ]


{-| The record the initial phase saves after the inputs, over the saved one.
-}
savedAfterInitial : ReferralValue -> List Msg -> Maybe ReferralValue
savedAfterInitial saved msgs =
    toReferralValueWithDefault (Just saved) (initialFormAfter saved msgs)


savedAfterRecurrent : ReferralValue -> List RecurrentModel.Msg -> Maybe ReferralValue
savedAfterRecurrent saved msgs =
    toReferralValueWithDefault (Just saved) (recurrentFormAfter saved msgs)


hasSigns : List ReferToFacilitySign -> Maybe ReferralValue -> Bool
hasSigns signs =
    Maybe.map (\value -> List.all (\sign -> EverySet.member sign value.referralSigns) signs)
        >> Maybe.withDefault False


reasonsFor : Maybe ReferralValue -> ( Maybe ReasonForNonReferral, Maybe ReasonForNonReferral )
reasonsFor =
    Maybe.andThen .nonReferralReasons
        >> (\reasons ->
                ( getCurrentReasonForNonReferral NonReferralReasonHospital reasons
                , getCurrentReasonForNonReferral NonReferralReasonANCServices reasons
                )
           )


{-| One test per pregnant-with-hypertension scenario.
-}
forEachPregnantScenario : String -> ((Maybe ReferralValue -> AssembledData) -> Expect.Expectation) -> List Test
forEachPregnantScenario name expectation =
    List.map
        (\( scenario, assembledWith ) -> test (scenario ++ ": " ++ name) <| \_ -> expectation assembledWith)
        pregnantWithHypertension


forEachNotPregnantScenario : String -> ((Maybe ReferralValue -> AssembledData) -> Expect.Expectation) -> List Test
forEachNotPregnantScenario name expectation =
    List.map
        (\( scenario, assembledWith ) -> test (scenario ++ ": " ++ name) <| \_ -> expectation assembledWith)
        notPregnantWithHypertension


referralTasksTest : Test
referralTasksTest =
    describe "NCD Next Steps referral tasks of a visit that refers to two facilities"
        [ describe "At an NCD visit of a pregnant woman with hypertension, the initial Next Steps Referral task asks for the ANC services referral and stays not done until it is answered." <|
            forEachPregnantScenario "the initial phase offers the Referral task"
                (\assembledWith ->
                    resolveNextStepsTasks (assembledWith Nothing)
                        |> List.member TaskReferral
                        |> Expect.equal True
                )
                ++ forEachPregnantScenario "the lab-results phase offers its Referral task"
                    (\assembledWith ->
                        RecurrentUtils.resolveNextStepsTasks (assembledWith Nothing)
                            |> List.member RecurrentTypes.TaskReferral
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "the initial Referral form asks the ANC services questions"
                    (\assembledWith ->
                        { emptyReferralForm
                            | referToANCServices = Just True
                            , referralFormANCServices = Just True
                            , accompanyToANCServices = Just True
                        }
                            |> referralTasks NCDEncounterPhaseInitial (assembledWith Nothing)
                            |> Expect.equal [ Just True, Just True, Just True ]
                    )
                ++ forEachPregnantScenario "with no referral saved, the initial Referral task is not done"
                    (\assembledWith ->
                        assembledWith Nothing
                            |> initialReferralDone
                            |> Expect.equal False
                    )
        , describe "This holds when a hospital referral was already saved from lab results in the same visit: the ANC question is still asked, and Next Steps cannot complete until it is answered." <|
            forEachPregnantScenario "hospital referred from lab results, the initial Referral task is not done"
                (\assembledWith ->
                    assembledWith (Just hospitalReferred)
                        |> initialReferralDone
                        |> Expect.equal False
                )
                ++ forEachPregnantScenario "hospital not referred from lab results, with a reason, the initial Referral task is not done"
                    (\assembledWith ->
                        assembledWith (Just hospitalNotReferred)
                            |> initialReferralDone
                            |> Expect.equal False
                    )
                ++ forEachPregnantScenario "hospital referred, record read back after sync, the initial Referral task is not done"
                    (\assembledWith ->
                        assembledWith (Just hospitalReferredAfterSync)
                            |> initialReferralDone
                            |> Expect.equal False
                    )
                ++ forEachPregnantScenario "hospital referred from lab results, Next Steps is not completed"
                    (\assembledWith ->
                        assembledWith (Just hospitalReferred)
                            |> nextStepsCompleted
                            |> Expect.equal False
                    )
                ++ forEachPregnantScenario "hospital referred from lab results, the lab-results Referral task is done"
                    (\assembledWith ->
                        assembledWith (Just hospitalReferred)
                            |> recurrentReferralDone
                            |> Expect.equal True
                    )
        , describe "The ANC services question then starts unanswered. It does not show as already answered No." <|
            [ test "hospital referred: the ANC question starts unanswered" <|
                \_ ->
                    displayed hospitalReferred emptyReferralForm
                        |> .referToANCServices
                        |> Expect.equal Nothing
            , test "hospital not referred, with a reason: the ANC question starts unanswered" <|
                \_ ->
                    displayed hospitalNotReferred emptyReferralForm
                        |> .referToANCServices
                        |> Expect.equal Nothing
            , test "hospital referred, record read back after sync: the ANC question starts unanswered" <|
                \_ ->
                    displayed hospitalReferredAfterSync emptyReferralForm
                        |> .referToANCServices
                        |> Expect.equal Nothing
            ]
                ++ forEachPregnantScenario "hospital referred: the ANC question on the initial Referral form is unanswered"
                    (\assembledWith ->
                        displayed hospitalReferred emptyReferralForm
                            |> referralTasks NCDEncounterPhaseInitial (assembledWith (Just hospitalReferred))
                            |> List.head
                            |> Expect.equal (Just Nothing)
                    )
                ++ [ -- The follow-up questions belong to the ANC section too: once
                     -- the nurse answers Yes, "handed the referral form?" and
                     -- "accompanied?" must not open answered No either.
                     test "hospital referred, ANC answered Yes: the handed-form and accompany questions start unanswered" <|
                        \_ ->
                            initialFormAfter hospitalReferred [ SetReferralBoolInput setReferToANCServices True ]
                                |> displayed hospitalReferred
                                |> (\form -> ( form.referToANCServices, form.referralFormANCServices, form.accompanyToANCServices ))
                                |> Expect.equal ( Just True, Nothing, Nothing )
                   , test "hospital referred: the form keeps the hospital answers" <|
                        \_ ->
                            displayed hospitalReferred emptyReferralForm
                                |> (\form -> ( form.referToHospital, form.referralFormHospital ))
                                |> Expect.equal ( Just True, Just True )
                   , test "hospital not referred, with a reason: the hospital question still reads No, with its reason" <|
                        \_ ->
                            displayed hospitalNotReferred emptyReferralForm
                                |> (\form -> ( form.referToHospital, getCurrentReasonForNonReferral NonReferralReasonHospital form.nonReferralReasons ))
                                |> Expect.equal ( Just False, Just NoAmbulance )
                   , test "an ANC answer the nurse gave wins over the saved record" <|
                        \_ ->
                            displayed hospitalReferred { emptyReferralForm | referToANCServices = Just True }
                                |> .referToANCServices
                                |> Expect.equal (Just True)
                   , test "an ANC referral saved as No still reads No" <|
                        \_ ->
                            displayed ancNotReferred emptyReferralForm
                                |> .referToANCServices
                                |> Expect.equal (Just False)
                   , test "an ANC referral saved as Yes reads Yes, with its handed-form and accompany answers" <|
                        \_ ->
                            displayed ancReferred emptyReferralForm
                                |> (\form -> ( form.referToANCServices, form.referralFormANCServices, form.accompanyToANCServices ))
                                |> Expect.equal ( Just True, Just True, Just True )
                   , test "an ANC referral saved as Yes, with no form handed and no one accompanying, reads No for those two" <|
                        \_ ->
                            displayed { ancReferred | referralSigns = EverySet.singleton ReferToANCServices } emptyReferralForm
                                |> (\form -> ( form.referToANCServices, form.referralFormANCServices, form.accompanyToANCServices ))
                                |> Expect.equal ( Just True, Just False, Just False )
                   ]
        , describe "Answering the ANC referral Yes, or No with a reason, marks the task done. The hospital referral saved from lab results stays as it was." <|
            forEachPregnantScenario "hospital referred, then ANC answered Yes: the initial Referral task is done"
                (\assembledWith ->
                    assembledWith (savedAfterInitial hospitalReferred ancYesInputs)
                        |> initialReferralDone
                        |> Expect.equal True
                )
                ++ forEachPregnantScenario "hospital referred, then ANC answered No with a reason: the initial Referral task is done"
                    (\assembledWith ->
                        assembledWith (savedAfterInitial hospitalReferred ancNoInputs)
                            |> initialReferralDone
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "hospital not referred, then ANC answered No with a reason: the initial Referral task is done"
                    (\assembledWith ->
                        assembledWith (savedAfterInitial hospitalNotReferred ancNoInputs)
                            |> initialReferralDone
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "hospital referred, then ANC answered Yes: Next Steps is completed"
                    (\assembledWith ->
                        assembledWith (savedAfterInitial hospitalReferred ancYesInputs)
                            |> nextStepsCompleted
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "hospital referred, then ANC answered Yes: the lab-results Referral task stays done"
                    (\assembledWith ->
                        assembledWith (savedAfterInitial hospitalReferred ancYesInputs)
                            |> recurrentReferralDone
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "hospital not referred, then ANC answered No: the lab-results Referral task stays done"
                    (\assembledWith ->
                        assembledWith (savedAfterInitial hospitalNotReferred ancNoInputs)
                            |> recurrentReferralDone
                            |> Expect.equal True
                    )
                ++ [ test "hospital referred, then ANC answered Yes: the record keeps the hospital referral and holds the ANC one" <|
                        \_ ->
                            savedAfterInitial hospitalReferred ancYesInputs
                                |> (\value ->
                                        ( hasSigns [ ReferToHospital, ReferralFormHospital ] value
                                        , hasSigns [ ReferToANCServices, ReferralFormANCServices, AccompanyToANCServices ] value
                                        )
                                   )
                                |> Expect.equal ( True, True )
                   , test "hospital referred, then ANC answered No: the record keeps the hospital referral and holds the ANC reason" <|
                        \_ ->
                            savedAfterInitial hospitalReferred ancNoInputs
                                |> (\value -> ( hasSigns [ ReferToHospital, ReferralFormHospital ] value, reasonsFor value ))
                                |> Expect.equal ( True, ( Nothing, Just ClientRefused ) )
                   , test "hospital not referred, then ANC answered No: the record keeps both reasons" <|
                        \_ ->
                            savedAfterInitial hospitalNotReferred ancNoInputs
                                |> (\value -> ( hasSigns [ ReferToHospital ] value, reasonsFor value ))
                                |> Expect.equal ( False, ( Just NoAmbulance, Just ClientRefused ) )
                   ]
        , describe "In the opposite order, ANC referral saved first, hospital referral asked later from lab results: the hospital question starts unanswered, and that task is done only when it is answered." <|
            [ test "ANC referred: the hospital question starts unanswered" <|
                \_ ->
                    displayed ancReferred emptyReferralForm
                        |> .referToHospital
                        |> Expect.equal Nothing
            , test "ANC not referred, with a reason: the hospital question starts unanswered" <|
                \_ ->
                    displayed ancNotReferred emptyReferralForm
                        |> .referToHospital
                        |> Expect.equal Nothing
            , -- As on the ANC side: once the nurse answers Yes, "handed the
              -- referral form?" must not open answered No.
              test "ANC referred, hospital answered Yes: the handed-form question starts unanswered" <|
                \_ ->
                    recurrentFormAfter ancReferred [ RecurrentModel.SetReferralBoolInput setReferToHospital True ]
                        |> displayed ancReferred
                        |> (\form -> ( form.referToHospital, form.referralFormHospital ))
                        |> Expect.equal ( Just True, Nothing )
            ]
                ++ forEachPregnantScenario "ANC referred: the hospital question on the lab-results Referral form is unanswered"
                    (\assembledWith ->
                        displayed ancReferred emptyReferralForm
                            |> referralTasks NCDEncounterPhaseRecurrent (assembledWith (Just ancReferred))
                            |> List.head
                            |> Expect.equal (Just Nothing)
                    )
                ++ forEachPregnantScenario "the lab-results Referral form asks the hospital questions"
                    (\assembledWith ->
                        { emptyReferralForm | referToHospital = Just True, referralFormHospital = Just True }
                            |> referralTasks NCDEncounterPhaseRecurrent (assembledWith (Just ancReferred))
                            |> Expect.equal [ Just True, Just True ]
                    )
                ++ forEachPregnantScenario "ANC referred: the lab-results Referral task is not done"
                    (\assembledWith ->
                        assembledWith (Just ancReferred)
                            |> recurrentReferralDone
                            |> Expect.equal False
                    )
                ++ forEachPregnantScenario "ANC not referred, with a reason: the lab-results Referral task is not done"
                    (\assembledWith ->
                        assembledWith (Just ancNotReferred)
                            |> recurrentReferralDone
                            |> Expect.equal False
                    )
                ++ forEachPregnantScenario "ANC referred: the initial Referral task is done"
                    (\assembledWith ->
                        assembledWith (Just ancReferred)
                            |> initialReferralDone
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "ANC not referred, with a reason: the initial Referral task is done"
                    (\assembledWith ->
                        assembledWith (Just ancNotReferred)
                            |> initialReferralDone
                            |> Expect.equal True
                    )
                ++ forEachPregnantScenario "ANC referred, then hospital answered Yes: both Referral tasks are done"
                    (\assembledWith ->
                        assembledWith (savedAfterRecurrent ancReferred hospitalYesInputs)
                            |> (\assembled -> ( initialReferralDone assembled, recurrentReferralDone assembled ))
                            |> Expect.equal ( True, True )
                    )
                ++ forEachPregnantScenario "ANC not referred, then hospital answered No with a reason: both Referral tasks are done"
                    (\assembledWith ->
                        assembledWith (savedAfterRecurrent ancNotReferred hospitalNoInputs)
                            |> (\assembled -> ( initialReferralDone assembled, recurrentReferralDone assembled ))
                            |> Expect.equal ( True, True )
                    )
                ++ [ test "ANC referred, then hospital answered Yes: the record keeps the ANC referral and holds the hospital one" <|
                        \_ ->
                            savedAfterRecurrent ancReferred hospitalYesInputs
                                |> (\value ->
                                        ( hasSigns [ ReferToANCServices, ReferralFormANCServices, AccompanyToANCServices ] value
                                        , hasSigns [ ReferToHospital, ReferralFormHospital ] value
                                        )
                                   )
                                |> Expect.equal ( True, True )
                   , test "ANC not referred, then hospital answered No: the record keeps both reasons" <|
                        \_ ->
                            savedAfterRecurrent ancNotReferred hospitalNoInputs
                                |> reasonsFor
                                |> Expect.equal ( Just NoAmbulance, Just ClientRefused )
                   ]
        , describe "For a woman who is not pregnant, or has no hypertension, both Referral tasks work as before." <|
            forEachNotPregnantScenario "both phases offer the Referral task"
                (\assembledWith ->
                    ( resolveNextStepsTasks (assembledWith Nothing) |> List.member TaskReferral
                    , RecurrentUtils.resolveNextStepsTasks (assembledWith Nothing) |> List.member RecurrentTypes.TaskReferral
                    )
                        |> Expect.equal ( True, True )
                )
                ++ forEachNotPregnantScenario "the initial Referral form asks the hospital questions"
                    (\assembledWith ->
                        { emptyReferralForm | referToHospital = Just True, referralFormHospital = Just True }
                            |> referralTasks NCDEncounterPhaseInitial (assembledWith Nothing)
                            |> Expect.equal [ Just True, Just True ]
                    )
                ++ forEachNotPregnantScenario "with no referral saved, neither Referral task is done"
                    (\assembledWith ->
                        assembledWith Nothing
                            |> (\assembled -> ( initialReferralDone assembled, recurrentReferralDone assembled ))
                            |> Expect.equal ( False, False )
                    )
                ++ forEachNotPregnantScenario "hospital referred: both Referral tasks are done"
                    (\assembledWith ->
                        assembledWith (Just hospitalReferred)
                            |> (\assembled -> ( initialReferralDone assembled, recurrentReferralDone assembled ))
                            |> Expect.equal ( True, True )
                    )
                ++ forEachNotPregnantScenario "hospital not referred, with a reason: both Referral tasks are done"
                    (\assembledWith ->
                        assembledWith (Just hospitalNotReferred)
                            |> (\assembled -> ( initialReferralDone assembled, recurrentReferralDone assembled ))
                            |> Expect.equal ( True, True )
                    )
                ++ forEachNotPregnantScenario "hospital referred: Next Steps is completed"
                    (\assembledWith ->
                        assembledWith (Just hospitalReferred)
                            |> nextStepsCompleted
                            |> Expect.equal True
                    )
                ++ [ test "not pregnant, hospital referred: the hospital question reads Yes, with its handed-form answer" <|
                        \_ ->
                            displayed hospitalReferred emptyReferralForm
                                |> (\form -> ( form.referToHospital, form.referralFormHospital ))
                                |> Expect.equal ( Just True, Just True )
                   , test "not pregnant, hospital referred with no form handed: the handed-form question reads No" <|
                        \_ ->
                            displayed { hospitalReferred | referralSigns = EverySet.singleton ReferToHospital } emptyReferralForm
                                |> (\form -> ( form.referToHospital, form.referralFormHospital ))
                                |> Expect.equal ( Just True, Just False )
                   , -- With no hypertension there is no referral at all: the
                     -- renal complications and diabetes referrals both need it.
                     test "pregnant, no hypertension: neither phase offers a Referral task" <|
                        \_ ->
                            referralAssembled knownPregnant [] Nothing
                                |> (\assembled ->
                                        ( resolveNextStepsTasks assembled |> List.member TaskReferral
                                        , RecurrentUtils.resolveNextStepsTasks assembled |> List.member RecurrentTypes.TaskReferral
                                        )
                                   )
                                |> Expect.equal ( False, False )
                   , test "pregnant, no hypertension, renal complications from lab results: neither phase offers a Referral task" <|
                        \_ ->
                            referralAssembled knownPregnant [ DiagnosisRenalComplications ] Nothing
                                |> (\assembled ->
                                        ( resolveNextStepsTasks assembled |> List.member TaskReferral
                                        , RecurrentUtils.resolveNextStepsTasks assembled |> List.member RecurrentTypes.TaskReferral
                                        )
                                   )
                                |> Expect.equal ( False, False )
                   ]
        ]
