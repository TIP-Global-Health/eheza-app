module Pages.Prenatal.Test exposing (all)

import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualEncounterParticipantOutcome(..)
        , IndividualEncounterType(..)
        , PregnancyOutcome(..)
        )
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Participant.Utils exposing (isPregnancyActive)
import Pages.Prenatal.Utils exposing (marginalBloodPressureCondition)
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


all : Test
all =
    describe "Prenatal tests"
        [ marginalBloodPressureConditionTest
        , isPregnancyActiveTest
        ]
