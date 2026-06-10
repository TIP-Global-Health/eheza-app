module Pages.Prenatal.Activity.Test exposing (all)

import Expect
import Pages.Prenatal.Activity.Types exposing (PrePregnancyClassification(..))
import Pages.Prenatal.Activity.Utils exposing (bmiToPrePregnancyClassification, zscoreToPrePregnancyClassification)
import Test exposing (Test, describe, test)



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


all : Test
all =
    describe "Prenatal pre-pregnancy nutrition classification tests"
        [ bmiToPrePregnancyClassificationTest
        , zscoreToPrePregnancyClassificationTest
        ]
