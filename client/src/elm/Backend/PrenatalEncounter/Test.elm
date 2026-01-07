module Backend.PrenatalEncounter.Test exposing (all)

import Backend.PrenatalEncounter.Utils exposing (..)
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Test exposing (Test, describe, test)


{-| Tests for GWG and AWG calculation functions
-}
isSeverelyUndernourishedTest : Test
isSeverelyUndernourishedTest =
    describe "isSeverelyUndernourished"
        [ test "BMI < 17.5 is severely undernourished" <|
            \_ ->
                isSeverelyUndernourished (Just 17.0) Nothing
                    |> Expect.equal True
        , test "BMI >= 17.5 is not severely undernourished (by BMI)" <|
            \_ ->
                isSeverelyUndernourished (Just 18.0) (Just 22.0)
                    |> Expect.equal False
        , test "MUAC < 21 is severely undernourished" <|
            \_ ->
                isSeverelyUndernourished (Just 18.0) (Just 20.0)
                    |> Expect.equal True
        , test "MUAC >= 21 is not severely undernourished (by MUAC)" <|
            \_ ->
                isSeverelyUndernourished (Just 18.0) (Just 21.0)
                    |> Expect.equal False
        , test "Either condition triggers severely undernourished" <|
            \_ ->
                isSeverelyUndernourished (Just 17.0) (Just 22.0)
                    |> Expect.equal True
        , test "No measurements means not severely undernourished" <|
            \_ ->
                isSeverelyUndernourished Nothing Nothing
                    |> Expect.equal False
        ]


expectedDailyWeightGainTest : Test
expectedDailyWeightGainTest =
    describe "expectedDailyWeightGain"
        [ test "Before 13 weeks, any woman: 23.5g/day" <|
            \_ ->
                expectedDailyWeightGain False True
                    |> Expect.equal 23.5
        , test "Before 13 weeks, severely undernourished: 23.5g/day" <|
            \_ ->
                expectedDailyWeightGain True True
                    |> Expect.equal 23.5
        , test "After 13 weeks, not severely undernourished: 60g/day" <|
            \_ ->
                expectedDailyWeightGain False False
                    |> Expect.equal 60.0
        , test "After 13 weeks, severely undernourished: 73g/day" <|
            \_ ->
                expectedDailyWeightGain True False
                    |> Expect.equal 73.0
        ]


calculateExpectedWeightGainTest : Test
calculateExpectedWeightGainTest =
    let
        -- Helper to create dates
        makeDate : Int -> Int -> Int -> NominalDate
        makeDate year month day =
            Date.fromCalendarDate year (Date.numberToMonth month) day
    in
    describe "calculateExpectedWeightGain"
        [ test "Period entirely before 13 weeks" <|
            \_ ->
                let
                    lmpDate =
                        makeDate 2024 1 1

                    previousVisit =
                        makeDate 2024 1 15

                    currentVisit =
                        makeDate 2024 1 29
                in
                -- 14 days * 23.5 g/day = 329g
                calculateExpectedWeightGain False lmpDate previousVisit currentVisit
                    |> Expect.equal 329.0
        , test "Period entirely after 13 weeks, not undernourished" <|
            \_ ->
                let
                    lmpDate =
                        makeDate 2024 1 1

                    previousVisit =
                        makeDate 2024 4 10

                    currentVisit =
                        makeDate 2024 4 24
                in
                -- 14 days * 60 g/day = 840g
                calculateExpectedWeightGain False lmpDate previousVisit currentVisit
                    |> Expect.equal 840.0
        , test "Period entirely after 13 weeks, severely undernourished" <|
            \_ ->
                let
                    lmpDate =
                        makeDate 2024 1 1

                    previousVisit =
                        makeDate 2024 4 10

                    currentVisit =
                        makeDate 2024 4 24
                in
                -- 14 days * 73 g/day = 1022g
                calculateExpectedWeightGain True lmpDate previousVisit currentVisit
                    |> Expect.equal 1022.0
        , test "Period spanning 13 weeks boundary" <|
            \_ ->
                let
                    lmpDate =
                        makeDate 2024 1 1

                    -- 13 weeks = 91 days from LMP = April 1, 2024
                    previousVisit =
                        makeDate 2024 3 25

                    -- 7 days before 13 weeks
                    currentVisit =
                        makeDate 2024 4 8

                    -- 7 days after 13 weeks
                in
                -- 7 days * 23.5 g/day + 7 days * 60 g/day = 164.5 + 420 = 584.5g
                calculateExpectedWeightGain False lmpDate previousVisit currentVisit
                    |> Expect.equal 584.5
        ]


calculateGestationalWeightGainTest : Test
calculateGestationalWeightGainTest =
    describe "calculateGestationalWeightGain"
        [ test "Positive weight gain" <|
            \_ ->
                calculateGestationalWeightGain 60.0 62.5
                    |> Expect.equal 2.5
        , test "No weight gain" <|
            \_ ->
                calculateGestationalWeightGain 60.0 60.0
                    |> Expect.equal 0.0
        , test "Weight loss (negative gain)" <|
            \_ ->
                calculateGestationalWeightGain 60.0 59.0
                    |> Expect.equal -1.0
        ]


isAdequateWeightGainTest : Test
isAdequateWeightGainTest =
    describe "isAdequateWeightGain"
        [ test "Actual gain equals expected: adequate" <|
            \_ ->
                isAdequateWeightGain 1.0 1.0
                    |> Expect.equal True
        , test "Actual gain exceeds expected: adequate" <|
            \_ ->
                isAdequateWeightGain 1.5 1.0
                    |> Expect.equal True
        , test "Actual gain less than expected: not adequate" <|
            \_ ->
                isAdequateWeightGain 0.5 1.0
                    |> Expect.equal False
        , test "Zero gain with zero expected: adequate" <|
            \_ ->
                isAdequateWeightGain 0.0 0.0
                    |> Expect.equal True
        ]


all : Test
all =
    describe "Prenatal Encounter GWG/AWG tests"
        [ isSeverelyUndernourishedTest
        , expectedDailyWeightGainTest
        , calculateExpectedWeightGainTest
        , calculateGestationalWeightGainTest
        , isAdequateWeightGainTest
        ]
