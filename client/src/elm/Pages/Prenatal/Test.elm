module Pages.Prenatal.Test exposing (all)

import Expect
import Pages.Prenatal.Utils exposing (marginalBloodPressureCondition)
import Test exposing (Test, describe, test)



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


all : Test
all =
    describe "Prenatal blood-pressure condition tests"
        [ marginalBloodPressureConditionTest ]
