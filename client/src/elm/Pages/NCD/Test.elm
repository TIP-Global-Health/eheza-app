module Pages.NCD.Test exposing (all)

import Expect
import Pages.NCD.Utils
    exposing
        ( lowerHypertensionStageCondition
        , stage1BloodPressureCondition
        , stage2BloodPressureCondition
        , stage3BloodPressureCondition
        )
import Test exposing (Test, describe, test)



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


all : Test
all =
    describe "NCD hypertension staging tests"
        [ stage1Test
        , stage2Test
        , stage3Test
        , lowerHypertensionStageTest
        ]
