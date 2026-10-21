module Pages.Prenatal.Test exposing (all)

import AssocList
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualEncounterParticipantOutcome(..)
        , IndividualEncounterType(..)
        , PregnancyOutcome(..)
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..), emptyPrenatalEncounter)
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Participant.Utils exposing (antenatalPregnanciesNewestFirst, isPregnancyActive, pregnancyConcludedInTodaysPostpartumVisit)
import Pages.Prenatal.Utils exposing (marginalBloodPressureCondition)
import Restful.Endpoint exposing (toEntityUuid)
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


{-| A person with an old pregnancy that gained an EDD after it expired, loaded
before the pregnancy opened today.
-}
twoActivePregnancies : AssocList.Dict String IndividualEncounterParticipant
twoActivePregnancies =
    let
        old =
            pregnancyRegisteredDaysAgo 400
    in
    AssocList.fromList
        [ ( "old", { old | eddDate = Just (Date.add Date.Days 30 currentDate) } )
        , ( "new", pregnancyRegisteredDaysAgo 0 )
        , ( "child-visit", TestFixtures.testParticipant currentDate NutritionEncounter )
        ]


antenatalPregnanciesNewestFirstTest : Test
antenatalPregnanciesNewestFirstTest =
    describe "antenatalPregnanciesNewestFirst"
        [ test "lists only antenatal pregnancies, newest first" <|
            \_ ->
                antenatalPregnanciesNewestFirst twoActivePregnancies
                    |> List.map Tuple.first
                    |> Expect.equal [ "new", "old" ]
        , test "of two active pregnancies, the newest is shown" <|
            \_ ->
                antenatalPregnanciesNewestFirst twoActivePregnancies
                    |> List.filter (Tuple.second >> isPregnancyActive currentDate)
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Expect.equal (Just "new")
        ]


all : Test
all =
    describe "Prenatal tests"
        [ antenatalPregnanciesNewestFirstTest
        , marginalBloodPressureConditionTest
        , isPregnancyActiveTest
        , pregnancyConcludedInTodaysPostpartumVisitTest
        ]
