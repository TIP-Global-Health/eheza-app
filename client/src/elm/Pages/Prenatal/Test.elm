module Pages.Prenatal.Test exposing (all)

import AssocList
import Backend.Entities exposing (IndividualEncounterParticipantId, PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualEncounterParticipantOutcome(..)
        , IndividualEncounterType(..)
        , PregnancyOutcome(..)
        )
import Backend.Measurement.Model exposing (LaboratoryTest(..), PrenatalMeasurements, emptyPrenatalMeasurements)
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..), emptyPrenatalEncounter)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Activity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.Activity.Utils exposing (generatePrenatalDiagnosesForNurse, resolveNextStepsTasks)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.Participant.Utils exposing (isPregnancyActive, pregnancyConcludedInTodaysPostpartumVisit, resolveShownPregnancy)
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.Utils exposing (marginalBloodPressureCondition, vitalsRecheckAllowed)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time



-- marginalBloodPressureCondition flags "marginal" (elevated but not yet severe)
-- blood pressure in pregnancy. Oracle: the pregnancy-hypertension bands --
-- elevated at >=140/90, severe at >=160/110 -- so the marginal band is
-- systolic 140-159 OR diastolic 90-109.
--
-- NOTE: the argument order is (diastolic, systolic) -- the reverse of the usual
-- systolic-first convention, a footgun for callers. Tests pass args in that
-- order and label them explicitly.


marginalBloodPressureConditionTest : Test
marginalBloodPressureConditionTest =
    describe "marginalBloodPressureCondition (args: dia, sys)"
        [ test "normal 80/130 (dia/sys) -> False" <|
            \_ -> marginalBloodPressureCondition 80 130 |> Expect.equal False

        -- Diastolic band, systolic held normal (130).
        , test "dia 89 -> False (below band)" <|
            \_ -> marginalBloodPressureCondition 89 130 |> Expect.equal False
        , test "dia 90 -> True (bottom of marginal band)" <|
            \_ -> marginalBloodPressureCondition 90 130 |> Expect.equal True
        , test "dia 109 -> True (top of marginal band)" <|
            \_ -> marginalBloodPressureCondition 109 130 |> Expect.equal True
        , test "dia 110 -> False (severe threshold; no longer marginal)" <|
            \_ -> marginalBloodPressureCondition 110 130 |> Expect.equal False

        -- Systolic band, diastolic held normal (80).
        , test "sys 139 -> False (below band)" <|
            \_ -> marginalBloodPressureCondition 80 139 |> Expect.equal False
        , test "sys 140 -> True (bottom of marginal band)" <|
            \_ -> marginalBloodPressureCondition 80 140 |> Expect.equal True
        , test "sys 159 -> True (top of marginal band)" <|
            \_ -> marginalBloodPressureCondition 80 159 |> Expect.equal True
        , test "sys 160 -> False (severe threshold; no longer marginal)" <|
            \_ -> marginalBloodPressureCondition 80 160 |> Expect.equal False

        -- Both readings in the marginal band.
        , test "both marginal (95/145) -> True" <|
            \_ -> marginalBloodPressureCondition 95 145 |> Expect.equal True

        -- OR quirk: one reading is already SEVERE while the other is marginal,
        -- and the function still returns True. It does not exclude severe BP on
        -- its own -- callers must check the severe (>=160/110) condition first.
        , test "severe sys 170 + marginal dia 95 -> True [CODE: severe checked elsewhere]" <|
            \_ -> marginalBloodPressureCondition 95 170 |> Expect.equal True
        , test "severe dia 120 + marginal sys 145 -> True [CODE: severe checked elsewhere]" <|
            \_ -> marginalBloodPressureCondition 120 145 |> Expect.equal True
        ]


currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2026 Time.Jun 1


{-| An antenatal participant registered `days` before `currentDate`, with no
EDD, no end date and no outcome -- the shape of an undated pregnancy.
-}
pregnancyRegisteredDaysAgo : Int -> IndividualEncounterParticipant
pregnancyRegisteredDaysAgo days =
    TestFixtures.testParticipant (Date.add Date.Days -days currentDate) AntenatalEncounter



-- A pregnancy stays active until 92 days past its EDD. When EDD was never
-- recorded, it is estimated as registration date + 280 days (pregnancy
-- duration), so an undated pregnancy expires 372 days after registration.


isPregnancyActiveTest : Test
isPregnancyActiveTest =
    describe "isPregnancyActive"
        [ test "concluded (end date + outcome) -> False" <|
            \_ ->
                let
                    participant =
                        pregnancyRegisteredDaysAgo 100
                in
                isPregnancyActive currentDate
                    { participant
                        | endDate = Just currentDate
                        , outcome = Just (Pregnancy OutcomeLiveAtTerm)
                    }
                    |> Expect.equal False

        -- EDD recorded: the 92 day grace period, unchanged.
        , test "EDD 91 days overdue -> True (within grace period)" <|
            \_ ->
                let
                    participant =
                        pregnancyRegisteredDaysAgo 300
                in
                isPregnancyActive currentDate
                    { participant | eddDate = Just (Date.add Date.Days -91 currentDate) }
                    |> Expect.equal True
        , test "EDD 92 days overdue -> False (grace period elapsed)" <|
            \_ ->
                let
                    participant =
                        pregnancyRegisteredDaysAgo 300
                in
                isPregnancyActive currentDate
                    { participant | eddDate = Just (Date.add Date.Days -92 currentDate) }
                    |> Expect.equal False

        -- No EDD: expiry is estimated from the registration date.
        , test "no EDD, registered today -> True" <|
            \_ -> isPregnancyActive currentDate (pregnancyRegisteredDaysAgo 0) |> Expect.equal True
        , test "no EDD, registered 371 days ago -> True (last active day)" <|
            \_ -> isPregnancyActive currentDate (pregnancyRegisteredDaysAgo 371) |> Expect.equal True
        , test "no EDD, registered 372 days ago -> False (280 + 92)" <|
            \_ -> isPregnancyActive currentDate (pregnancyRegisteredDaysAgo 372) |> Expect.equal False
        , test "no EDD, registered 5 years ago -> False" <|
            \_ -> isPregnancyActive currentDate (pregnancyRegisteredDaysAgo 1825) |> Expect.equal False
        ]


{-| A pregnancy whose outcome was recorded `days` before `currentDate`.
-}
pregnancyConcludedDaysAgo : Int -> IndividualEncounterParticipant
pregnancyConcludedDaysAgo days =
    let
        participant =
            pregnancyRegisteredDaysAgo 200
    in
    { participant
        | endDate = Just (Date.add Date.Days -days currentDate)
        , outcome = Just (Pregnancy OutcomeLiveAtTerm)
    }


visitStartedDaysAgo : Int -> PrenatalEncounterType -> PrenatalEncounter
visitStartedDaysAgo days encounterType =
    emptyPrenatalEncounter (toEntityUuid "pregnancy") (Date.add Date.Days -days currentDate) encounterType Nothing


{-| A visit started and ended `days` before `currentDate`.
-}
visitEndedDaysAgo : Int -> PrenatalEncounterType -> PrenatalEncounter
visitEndedDaysAgo days encounterType =
    let
        visit =
            visitStartedDaysAgo days encounterType
    in
    { visit | endDate = Just visit.startDate }



-- The participant page keeps a pregnancy concluded today only for the role
-- whose postpartum visit is still open or ended today; otherwise as before.


pregnancyConcludedInTodaysPostpartumVisitTest : Test
pregnancyConcludedInTodaysPostpartumVisitTest =
    let
        isNurse =
            False

        isChw =
            True
    in
    describe "pregnancyConcludedInTodaysPostpartumVisit"
        [ test "nurse, outcome today inside today's nurse postpartum visit -> True" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 0 NursePostpartumEncounter ]
                    |> Expect.equal True
        , test "CHW, outcome today inside today's CHW postpartum visit -> True" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isChw (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 0 ChwPostpartumEncounter ]
                    |> Expect.equal True
        , test "earlier visits do not get in the way -> True" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate
                    isNurse
                    (pregnancyConcludedDaysAgo 0)
                    [ visitStartedDaysAgo 0 NursePostpartumEncounter, visitStartedDaysAgo 30 NurseEncounter ]
                    |> Expect.equal True
        , test "CHW viewing a nurse postpartum visit -> False (other role)" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isChw (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 0 NursePostpartumEncounter ]
                    |> Expect.equal False
        , test "nurse viewing a CHW postpartum visit -> False (other role)" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 0 ChwPostpartumEncounter ]
                    |> Expect.equal False
        , test "outcome today in a visit opened yesterday and still open -> True" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 1 NursePostpartumEncounter ]
                    |> Expect.equal True
        , test "outcome today, postpartum visit ended today -> True" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isChw (pregnancyConcludedDaysAgo 0) [ visitEndedDaysAgo 0 ChwPostpartumEncounter ]
                    |> Expect.equal True
        , test "outcome today, postpartum visit ended yesterday -> False" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 0) [ visitEndedDaysAgo 1 NursePostpartumEncounter ]
                    |> Expect.equal False
        , test "outcome today after an antenatal visit only -> False" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 0 NurseEncounter ]
                    |> Expect.equal False
        , test "outcome today with no visit today (CHW warning popup) -> False" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isChw (pregnancyConcludedDaysAgo 0) [ visitStartedDaysAgo 40 ChwFirstEncounter ]
                    |> Expect.equal False
        , test "outcome recorded yesterday -> False" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyConcludedDaysAgo 1) [ visitStartedDaysAgo 0 NursePostpartumEncounter ]
                    |> Expect.equal False
        , test "no outcome recorded -> False" <|
            \_ ->
                pregnancyConcludedInTodaysPostpartumVisit currentDate isNurse (pregnancyRegisteredDaysAgo 200) [ visitStartedDaysAgo 0 NursePostpartumEncounter ]
                    |> Expect.equal False
        ]


{-| Participants keyed by id, in the order the device loaded them.
-}
participants : List ( String, IndividualEncounterParticipant ) -> AssocList.Dict IndividualEncounterParticipantId IndividualEncounterParticipant
participants =
    List.map (Tuple.mapFirst toEntityUuid) >> AssocList.fromList


{-| The id of the pregnancy the participant page shows.
-}
shownPregnancyId : (IndividualEncounterParticipantId -> List ( PrenatalEncounterId, PrenatalEncounter )) -> List ( String, IndividualEncounterParticipant ) -> Maybe String
shownPregnancyId encountersFor loaded =
    resolveShownPregnancy currentDate False encountersFor (participants loaded)
        |> Maybe.map (.sessionId >> fromEntityUuid)


{-| An old pregnancy that gained an EDD after it expired, loaded before the
pregnancy opened today, and a child's visit.
-}
twoActivePregnancies : List ( String, IndividualEncounterParticipant )
twoActivePregnancies =
    let
        old =
            pregnancyRegisteredDaysAgo 400
    in
    [ ( "earlier", { old | eddDate = Just (Date.add Date.Days 30 currentDate) } )
    , ( "today", pregnancyRegisteredDaysAgo 0 )
    , ( "child-visit", TestFixtures.testParticipant currentDate NutritionEncounter )
    ]


{-| One prenatal visit for the pregnancy with the given id, none for others.
-}
visitsFor : String -> IndividualEncounterParticipantId -> List ( PrenatalEncounterId, PrenatalEncounter )
visitsFor id sessionId =
    if fromEntityUuid sessionId == id then
        [ ( toEntityUuid "visit", visitStartedDaysAgo 30 NurseEncounter ) ]

    else
        []


resolveShownPregnancyTest : Test
resolveShownPregnancyTest =
    describe "resolveShownPregnancy"
        [ test "of two active pregnancies, the newest is shown, even with fewer visits" <|
            \_ ->
                shownPregnancyId (visitsFor "earlier") twoActivePregnancies
                    |> Expect.equal (Just "today")
        , test "an active pregnancy is shown over a newer concluded one" <|
            \_ ->
                let
                    concluded =
                        pregnancyConcludedDaysAgo 5
                in
                shownPregnancyId (always [])
                    [ ( "new-concluded", { concluded | startDate = Date.add Date.Days -30 currentDate } )
                    , ( "old-active", pregnancyRegisteredDaysAgo 300 )
                    ]
                    |> Expect.equal (Just "old-active")
        , test "of two active pregnancies registered the same day, the one with visits is shown" <|
            \_ ->
                shownPregnancyId (visitsFor "b") [ ( "a", pregnancyRegisteredDaysAgo 10 ), ( "b", pregnancyRegisteredDaysAgo 10 ) ]
                    |> Expect.equal (Just "b")
        , test "of two active pregnancies registered the same day, the choice does not depend on load order" <|
            \_ ->
                [ [ ( "b", pregnancyRegisteredDaysAgo 10 ), ( "a", pregnancyRegisteredDaysAgo 10 ) ]
                , [ ( "a", pregnancyRegisteredDaysAgo 10 ), ( "b", pregnancyRegisteredDaysAgo 10 ) ]
                ]
                    |> List.map (shownPregnancyId (always []))
                    |> Expect.equal [ Just "a", Just "a" ]
        , test "without an active pregnancy, one concluded in today's postpartum visit is kept" <|
            \_ ->
                let
                    encountersFor sessionId =
                        if fromEntityUuid sessionId == "concluded-today" then
                            [ ( toEntityUuid "visit", visitStartedDaysAgo 0 NursePostpartumEncounter ) ]

                        else
                            []
                in
                resolveShownPregnancy currentDate
                    False
                    encountersFor
                    (participants [ ( "concluded-earlier", pregnancyConcludedDaysAgo 30 ), ( "concluded-today", pregnancyConcludedDaysAgo 0 ) ])
                    |> Maybe.map (\shown -> ( fromEntityUuid shown.sessionId, shown.keptForPostpartumVisit ))
                    |> Expect.equal (Just ( "concluded-today", True ))
        ]



-- VITALS RECHECK
--
-- At a nurse antenatal visit, a borderline blood pressure (systolic 140-159
-- or diastolic 90-109) is measured again two hours later. While the recheck
-- is pending the Next Steps activity shows the Wait task, and the recheck
-- itself is the Vitals task of the recurrent phase. A repeated reading still
-- at 140 / 90 or above diagnoses hypertension: gestational from week 20,
-- chronic before it. A reading of 160 / 110 or above diagnoses hypertension
-- at once, with no recheck.
--
-- Hypertension and moderate preeclampsia are chronic for the rest of the
-- pregnancy, so once either was diagnosed at an earlier visit, there is
-- nothing for a recheck to decide: no recheck is scheduled, and no recheck
-- vitals task is shown. Postpartum visits never recheck.
--
-- The scheduling decision is `vitalsRecheckAllowed`: the vitals save handler
-- asks it whether to record the recheck as a pending test. The Wait task and
-- the recurrent Vitals task are read from what was saved, so they are tested
-- here from the saved shapes: a pending recheck test in the labs results
-- record, and the vitals measurement.
--
-- Every visit below happens on `currentDate`; earlier visits of the same
-- pregnancy are whole weeks before it.


{-| The LMP that puts a visit on `currentDate` at the given gestational age,
counted in days (140 days is exactly week 20).
-}
lmpForEGADays : Int -> NominalDate
lmpForEGADays days =
    Date.add Date.Days -days currentDate


{-| A nurse visit of the given type on `currentDate`, at the given
gestational age in days, after the given earlier nurse visits of the
pregnancy, holding the given measurements. Nothing is diagnosed at it yet.
-}
nurseVisit : PrenatalEncounterType -> Int -> List PreviousEncounterData -> PrenatalMeasurements -> AssembledData
nurseVisit encounterType egaDays earlierVisits measurements =
    { id = toEntityUuid "visit"
    , encounter = emptyPrenatalEncounter (toEntityUuid "pregnancy") currentDate encounterType Nothing
    , participant = TestFixtures.testParticipant (lmpForEGADays egaDays) AntenatalEncounter
    , person = TestFixtures.testPerson
    , measurements = measurements
    , nursePreviousEncountersData = earlierVisits
    , chwPreviousMeasurementsWithDates = []
    , globalLmpDate = Just (lmpForEGADays egaDays)
    , globalObstetricHistory = Nothing
    , vaccinationHistory = AssocList.empty
    , vaccinationProgress = AssocList.empty
    }


{-| A nurse antenatal visit at 24 weeks: past week 20, so a hypertension
found here is gestational.
-}
antenatalVisit : List PreviousEncounterData -> PrenatalMeasurements -> AssembledData
antenatalVisit =
    nurseVisit NurseEncounter (24 * 7)


{-| A nurse postpartum visit, two weeks after a term birth.
-}
postpartumVisit : List PreviousEncounterData -> PrenatalMeasurements -> AssembledData
postpartumVisit =
    nurseVisit NursePostpartumEncounter (42 * 7)


{-| An earlier nurse antenatal visit of the pregnancy, `weeksAgo` weeks
before `currentDate`, where the given diagnoses were made. Its blood
pressure was normal.
-}
earlierVisit : Int -> List PrenatalDiagnosis -> PreviousEncounterData
earlierVisit weeksAgo diagnoses =
    { startDate = Date.add Date.Weeks -weeksAgo currentDate
    , diagnoses = EverySet.fromList diagnoses
    , pastDiagnoses = EverySet.empty
    , measurements = withBloodPressure 120 80 emptyPrenatalMeasurements
    }


{-| One earlier visit, four weeks back, with nothing diagnosed.
-}
oneUneventfulEarlierVisit : List PreviousEncounterData
oneUneventfulEarlierVisit =
    [ earlierVisit 4 [] ]


{-| The vitals measurement with the given first reading, systolic first, and
no repeated reading yet.
-}
withBloodPressure : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withBloodPressure sys dia measurements =
    { measurements | vitals = TestFixtures.wrapMeasurement currentDate (TestFixtures.vitalsValueWith sys dia) }


{-| The vitals measurement with a first reading and the repeated reading
taken two hours later, systolic first in each pair.
-}
withRecheckedBloodPressure : ( Float, Float ) -> ( Float, Float ) -> PrenatalMeasurements -> PrenatalMeasurements
withRecheckedBloodPressure ( sys, dia ) ( sysRepeated, diaRepeated ) measurements =
    let
        first =
            TestFixtures.vitalsValueWith sys dia
    in
    { measurements
        | vitals =
            TestFixtures.wrapMeasurement currentDate
                { first | sysRepeated = Just sysRepeated, diaRepeated = Just diaRepeated }
    }


{-| The labs results record with the given tests pending: ordered at this
visit, no result yet. A scheduled recheck is the pending `TestVitalsRecheck`.
-}
withPendingTests : List LaboratoryTest -> PrenatalMeasurements -> PrenatalMeasurements
withPendingTests tests measurements =
    { measurements | labsResults = TestFixtures.wrapMeasurement currentDate (TestFixtures.labsEntry currentDate tests []) }


{-| A borderline first reading, 150 / 95, as in the issue's example.
-}
borderlineReading : PrenatalMeasurements
borderlineReading =
    withBloodPressure 150 95 emptyPrenatalMeasurements


{-| The visit as the app holds it after the initial phase: the diagnoses
the visit's measurements give are stored on the encounter.
-}
withItsDiagnoses : AssembledData -> AssembledData
withItsDiagnoses assembled =
    let
        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | diagnoses = generatePrenatalDiagnosesForNurse assembled } }


{-| Whether the Wait task is one of the visit's Next Steps tasks.
-}
waitTaskShown : AssembledData -> Bool
waitTaskShown assembled =
    resolveNextStepsTasks currentDate assembled
        |> List.member NextStepsWait


{-| Whether the recurrent phase shows the recheck vitals task (a nurse, not
a lab technician).
-}
recheckVitalsTaskShown : AssembledData -> Bool
recheckVitalsTaskShown assembled =
    Pages.Prenatal.RecurrentActivity.Utils.expectActivity currentDate False assembled RecurrentExamination


{-| The hypertension diagnoses the visit's measurements give, in a fixed
order: chronic before gestational, immediate before after-recheck.
-}
hypertensionDiagnosed : AssembledData -> List PrenatalDiagnosis
hypertensionDiagnosed assembled =
    let
        diagnoses =
            generatePrenatalDiagnosesForNurse assembled
    in
    List.filter (\diagnosis -> EverySet.member diagnosis diagnoses) allHypertensionDiagnoses


allHypertensionDiagnoses : List PrenatalDiagnosis
allHypertensionDiagnoses =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisGestationalHypertensionAfterRecheck
    ]


{-| Every stored form of moderate preeclampsia: both phases, before and from
week 37.
-}
allModeratePreeclampsiaDiagnoses : List PrenatalDiagnosis
allModeratePreeclampsiaDiagnoses =
    [ DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    ]


vitalsRecheckTest : Test
vitalsRecheckTest =
    describe "Vitals recheck at nurse visits"
        [ laterVisitWithoutEarlierHypertensionTest
        , laterVisitRecheckDiagnosesTest
        , firstVisitTest
        , earlierHypertensionTest
        , postpartumTest
        , severeReadingTest
        , normalReadingTest
        , scheduledRecheckTest
        ]


{-| A recheck saved before the rules changed, or before an earlier diagnosis
synced in, stays completable in the recurrent phase.
-}
scheduledRecheckTest : Test
scheduledRecheckTest =
    describe "A scheduled recheck keeps its vitals task"
        [ test "at a postpartum visit" <|
            \_ ->
                postpartumVisit [] (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> withItsDiagnoses
                    |> recheckVitalsTaskShown
                    |> Expect.equal True
        , test "after hypertension diagnosed at an earlier visit" <|
            \_ ->
                antenatalVisit [ earlierVisit 4 [ DiagnosisGestationalHypertensionImmediate ] ]
                    (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> withItsDiagnoses
                    |> recheckVitalsTaskShown
                    |> Expect.equal True
        , test "not when it was scheduled on an earlier day" <|
            \_ ->
                let
                    yesterday =
                        Date.add Date.Days -1 currentDate
                in
                { borderlineReading | labsResults = TestFixtures.wrapMeasurement yesterday (TestFixtures.labsEntry yesterday [ TestVitalsRecheck ] []) }
                    |> antenatalVisit []
                    |> withItsDiagnoses
                    |> recheckVitalsTaskShown
                    |> Expect.equal False
        , test "not when only another test is pending" <|
            \_ ->
                antenatalVisit [] (withPendingTests [ TestHemoglobin ] borderlineReading)
                    |> withItsDiagnoses
                    |> recheckVitalsTaskShown
                    |> Expect.equal False
        ]


laterVisitWithoutEarlierHypertensionTest : Test
laterVisitWithoutEarlierHypertensionTest =
    describe "At a nurse antenatal visit after the first, with no hypertension or moderate preeclampsia diagnosed earlier in the pregnancy, a borderline reading schedules the two-hour recheck, and the Wait task appears"
        [ test "a borderline reading schedules the two-hour recheck" <|
            \_ ->
                antenatalVisit oneUneventfulEarlierVisit borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal True

        -- The third visit: two earlier visits, neither diagnosing hypertension.
        , test "after two earlier visits, a borderline reading schedules the two-hour recheck" <|
            \_ ->
                antenatalVisit [ earlierVisit 4 [], earlierVisit 8 [] ] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal True

        -- What was diagnosed earlier is unrelated to blood pressure.
        , test "after an earlier visit that diagnosed only malaria and heartburn, a borderline reading schedules the two-hour recheck" <|
            \_ ->
                antenatalVisit [ earlierVisit 4 [ DiagnosisHeartburn, DiagnosisMalariaInitialPhase ] ] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal True

        -- Before week 20 the recheck still decides, as chronic hypertension.
        , test "at a later visit at 16 weeks, a borderline reading schedules the two-hour recheck" <|
            \_ ->
                nurseVisit NurseEncounter (16 * 7) oneUneventfulEarlierVisit borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal True

        -- Once the recheck is recorded as a pending test, Next Steps waits for it.
        , test "the Wait task appears while the scheduled recheck is pending" <|
            \_ ->
                antenatalVisit oneUneventfulEarlierVisit (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> waitTaskShown
                    |> Expect.equal True

        -- The recheck itself, in the recurrent phase, for each borderline shape:
        -- both numbers, systolic only, diastolic only.
        , test "the recurrent phase shows the recheck vitals task for 150 / 95, 150 / 80 and 130 / 95" <|
            \_ ->
                [ ( 150, 95 ), ( 150, 80 ), ( 130, 95 ) ]
                    |> List.map
                        (\( sys, dia ) ->
                            withBloodPressure sys dia emptyPrenatalMeasurements
                                |> withPendingTests [ TestVitalsRecheck ]
                                |> antenatalVisit oneUneventfulEarlierVisit
                                |> withItsDiagnoses
                                |> recheckVitalsTaskShown
                        )
                    |> Expect.equal [ True, True, True ]
        ]


laterVisitRecheckDiagnosesTest : Test
laterVisitRecheckDiagnosesTest =
    let
        recheckedAt egaDays first repeated =
            emptyPrenatalMeasurements
                |> withRecheckedBloodPressure first repeated
                |> nurseVisit NurseEncounter egaDays oneUneventfulEarlierVisit
                |> hypertensionDiagnosed
    in
    describe "At such a visit, a repeated reading that is still 140 / 90 or above diagnoses hypertension, gestational from week 20 and chronic before it, as at the first visit"
        [ test "repeated 150 / 95 at 24 weeks diagnoses gestational hypertension after recheck" <|
            \_ ->
                recheckedAt (24 * 7) ( 150, 95 ) ( 150, 95 )
                    |> Expect.equal [ DiagnosisGestationalHypertensionAfterRecheck ]
        , test "repeated 150 / 95 at 16 weeks diagnoses chronic hypertension after recheck" <|
            \_ ->
                recheckedAt (16 * 7) ( 150, 95 ) ( 150, 95 )
                    |> Expect.equal [ DiagnosisChronicHypertensionAfterRecheck ]
        , test "repeated 150 / 95 at exactly 20 weeks is gestational" <|
            \_ ->
                recheckedAt 140 ( 150, 95 ) ( 150, 95 )
                    |> Expect.equal [ DiagnosisGestationalHypertensionAfterRecheck ]
        , test "repeated 150 / 95 at 19 weeks and 6 days is chronic" <|
            \_ ->
                recheckedAt 139 ( 150, 95 ) ( 150, 95 )
                    |> Expect.equal [ DiagnosisChronicHypertensionAfterRecheck ]
        , test "repeated 140 / 80 (systolic at the threshold) diagnoses hypertension" <|
            \_ ->
                recheckedAt (24 * 7) ( 150, 95 ) ( 140, 80 )
                    |> Expect.equal [ DiagnosisGestationalHypertensionAfterRecheck ]
        , test "repeated 130 / 90 (diastolic at the threshold) diagnoses hypertension" <|
            \_ ->
                recheckedAt (24 * 7) ( 150, 95 ) ( 130, 90 )
                    |> Expect.equal [ DiagnosisGestationalHypertensionAfterRecheck ]
        , test "repeated 139 / 89 diagnoses no hypertension" <|
            \_ ->
                recheckedAt (24 * 7) ( 150, 95 ) ( 139, 89 )
                    |> Expect.equal []
        ]


firstVisitTest : Test
firstVisitTest =
    describe "At the first nurse visit, the recheck works as it does today"
        [ test "a borderline reading schedules the two-hour recheck" <|
            \_ ->
                antenatalVisit [] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal True

        -- CHW visits came first; this is still the first nurse visit.
        , test "after CHW visits only, a borderline reading schedules the two-hour recheck" <|
            \_ ->
                let
                    assembled =
                        antenatalVisit [] borderlineReading
                in
                { assembled
                    | chwPreviousMeasurementsWithDates =
                        [ ( Date.add Date.Weeks -4 currentDate, ChwFirstEncounter, emptyPrenatalMeasurements ) ]
                }
                    |> vitalsRecheckAllowed
                    |> Expect.equal True
        , test "the Wait task appears while the scheduled recheck is pending" <|
            \_ ->
                antenatalVisit [] (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> waitTaskShown
                    |> Expect.equal True
        , test "the recurrent phase shows the recheck vitals task" <|
            \_ ->
                antenatalVisit [] (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> withItsDiagnoses
                    |> recheckVitalsTaskShown
                    |> Expect.equal True
        , test "repeated 150 / 95 at 24 weeks diagnoses gestational hypertension after recheck" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withRecheckedBloodPressure ( 150, 95 ) ( 150, 95 )
                    |> antenatalVisit []
                    |> hypertensionDiagnosed
                    |> Expect.equal [ DiagnosisGestationalHypertensionAfterRecheck ]
        ]


earlierHypertensionTest : Test
earlierHypertensionTest =
    let
        diagnosedEarlier =
            allHypertensionDiagnoses ++ allModeratePreeclampsiaDiagnoses

        -- For each diagnosis made at the earlier visit, the answer at this one.
        forEachEarlierDiagnosis check =
            List.map (\diagnosis -> ( diagnosis, check [ earlierVisit 4 [ diagnosis ] ] )) diagnosedEarlier

        allFalse =
            List.map (\diagnosis -> ( diagnosis, False )) diagnosedEarlier
    in
    describe "When hypertension or moderate preeclampsia was diagnosed at an earlier visit of the pregnancy, a borderline reading schedules no recheck and shows no Wait task. The recheck vitals task does not appear even when another test opens the recurrent phase"
        [ test "a borderline reading schedules no recheck" <|
            \_ ->
                forEachEarlierDiagnosis (\earlier -> vitalsRecheckAllowed (antenatalVisit earlier borderlineReading))
                    |> Expect.equal allFalse

        -- Diagnosed two visits back, with an uneventful visit in between.
        , test "diagnosed at a visit before the latest one, a borderline reading schedules no recheck" <|
            \_ ->
                antenatalVisit [ earlierVisit 4 [], earlierVisit 8 [ DiagnosisModeratePreeclampsiaInitialPhase ] ] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal False

        -- Nothing was scheduled, so no test is pending.
        , test "no Wait task is shown" <|
            \_ ->
                forEachEarlierDiagnosis (\earlier -> waitTaskShown (antenatalVisit earlier borderlineReading))
                    |> Expect.equal allFalse

        -- A pending hemoglobin result opens the recurrent phase on its own.
        , test "the recheck vitals task does not appear when a pending hemoglobin test opens the recurrent phase" <|
            \_ ->
                forEachEarlierDiagnosis
                    (\earlier ->
                        antenatalVisit earlier (withPendingTests [ TestHemoglobin ] borderlineReading)
                            |> withItsDiagnoses
                            |> recheckVitalsTaskShown
                    )
                    |> Expect.equal allFalse
        ]


postpartumTest : Test
postpartumTest =
    describe "At a postpartum visit, a borderline reading never schedules a recheck, even when it is the woman's first nurse visit"
        [ test "at the woman's first nurse visit, a borderline reading schedules no recheck" <|
            \_ ->
                postpartumVisit [] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal False
        , test "after antenatal visits with no hypertension, a borderline reading schedules no recheck" <|
            \_ ->
                postpartumVisit [ earlierVisit 6 [], earlierVisit 20 [] ] borderlineReading
                    |> vitalsRecheckAllowed
                    |> Expect.equal False

        -- Even a pending recheck saved by an older version shows no Wait task.
        , test "no Wait task is shown, even with a pending recheck test" <|
            \_ ->
                postpartumVisit [] (withPendingTests [ TestVitalsRecheck ] borderlineReading)
                    |> waitTaskShown
                    |> Expect.equal False
        ]


severeReadingTest : Test
severeReadingTest =
    let
        -- The reading at a visit at the given gestational age, after the
        -- given earlier visits: its hypertension diagnoses, and whether the
        -- recurrent phase shows the recheck vitals task once the visit's
        -- diagnoses are stored.
        readingAt egaDays earlier ( sys, dia ) =
            let
                assembled =
                    nurseVisit NurseEncounter egaDays earlier (withBloodPressure sys dia emptyPrenatalMeasurements)
            in
            ( hypertensionDiagnosed assembled
            , recheckVitalsTaskShown (withItsDiagnoses assembled)
            )

        immediately diagnosis =
            ( [ diagnosis ], False )
    in
    describe "A reading of 160 / 110 or above at any visit diagnoses without a recheck, as today"
        [ test "170 / 85 at a later visit at 24 weeks diagnoses gestational hypertension at once" <|
            \_ ->
                readingAt (24 * 7) oneUneventfulEarlierVisit ( 170, 85 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)
        , test "130 / 112 at a later visit at 24 weeks diagnoses gestational hypertension at once" <|
            \_ ->
                readingAt (24 * 7) oneUneventfulEarlierVisit ( 130, 112 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)
        , test "160 / 80 (systolic at the threshold) at a later visit diagnoses at once" <|
            \_ ->
                readingAt (24 * 7) oneUneventfulEarlierVisit ( 160, 80 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)
        , test "130 / 110 (diastolic at the threshold) at a later visit diagnoses at once" <|
            \_ ->
                readingAt (24 * 7) oneUneventfulEarlierVisit ( 130, 110 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)

        -- One number severe, the other borderline: still no recheck.
        , test "165 / 95 at a later visit diagnoses at once, with no recheck vitals task" <|
            \_ ->
                readingAt (24 * 7) oneUneventfulEarlierVisit ( 165, 95 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)
        , test "170 / 85 at a later visit at 16 weeks diagnoses chronic hypertension at once" <|
            \_ ->
                readingAt (16 * 7) oneUneventfulEarlierVisit ( 170, 85 )
                    |> Expect.equal (immediately DiagnosisChronicHypertensionImmediate)
        , test "170 / 85 at the first visit at 24 weeks diagnoses gestational hypertension at once" <|
            \_ ->
                readingAt (24 * 7) [] ( 170, 85 )
                    |> Expect.equal (immediately DiagnosisGestationalHypertensionImmediate)
        ]


normalReadingTest : Test
normalReadingTest =
    let
        -- The normal reading at a visit after the given earlier visits: its
        -- hypertension diagnoses, whether the Wait task is shown, and
        -- whether the recurrent phase shows the recheck vitals task.
        readingAfter earlier ( sys, dia ) =
            let
                assembled =
                    antenatalVisit earlier (withBloodPressure sys dia emptyPrenatalMeasurements)
            in
            ( hypertensionDiagnosed assembled
            , waitTaskShown assembled
            , recheckVitalsTaskShown (withItsDiagnoses assembled)
            )

        nothing =
            ( [], False, False )
    in
    describe "A normal reading schedules no recheck at any visit"
        [ test "120 / 80 at the first visit: no diagnosis, no Wait task, no recheck vitals task" <|
            \_ ->
                readingAfter [] ( 120, 80 )
                    |> Expect.equal nothing
        , test "120 / 80 at a later visit: no diagnosis, no Wait task, no recheck vitals task" <|
            \_ ->
                readingAfter oneUneventfulEarlierVisit ( 120, 80 )
                    |> Expect.equal nothing

        -- Just under both borderline thresholds.
        , test "139 / 89 at a later visit: no diagnosis, no Wait task, no recheck vitals task" <|
            \_ ->
                readingAfter oneUneventfulEarlierVisit ( 139, 89 )
                    |> Expect.equal nothing
        ]


all : Test
all =
    describe "Prenatal tests"
        [ isPregnancyActiveTest
        , marginalBloodPressureConditionTest
        , pregnancyConcludedInTodaysPostpartumVisitTest
        , resolveShownPregnancyTest
        , vitalsRecheckTest
        ]
