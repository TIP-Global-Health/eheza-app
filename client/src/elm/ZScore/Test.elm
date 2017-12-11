module ZScore.Test exposing (all)

import Backend.Child.Model exposing (Gender(..))
import Expect
import Test exposing (Test, describe, test)
import Utils.NominalDate exposing (Days(..))
import ZScore.Model exposing (..)
import ZScore.Utils exposing (..)


viewZScoreTest : Test
viewZScoreTest =
    describe "viewZScore"
        [ test "ZScore3Neg" <| \() -> Expect.equal "-3" (viewZScore ZScore3Neg)
        , test "ZScore2Neg" <| \() -> Expect.equal "-2" (viewZScore ZScore2Neg)
        , test "ZScore1Neg" <| \() -> Expect.equal "-1" (viewZScore ZScore1Neg)
        , test "ZScore0" <| \() -> Expect.equal "0" (viewZScore ZScore0)
        , test "ZScore1" <| \() -> Expect.equal "1" (viewZScore ZScore1)
        , test "ZScore2" <| \() -> Expect.equal "2" (viewZScore ZScore2)
        , test "ZScore3" <| \() -> Expect.equal "3" (viewZScore ZScore3)
        ]


comparisons : List ( ZScore, ZScore, Order )
comparisons =
    [ ( ZScore3Neg, ZScore3Neg, EQ )
    , ( ZScore3Neg, ZScore2Neg, LT )
    , ( ZScore3Neg, ZScore1Neg, LT )
    , ( ZScore3Neg, ZScore0, LT )
    , ( ZScore3Neg, ZScore1, LT )
    , ( ZScore3Neg, ZScore2, LT )
    , ( ZScore3Neg, ZScore3, LT )
    , ( ZScore2Neg, ZScore3Neg, GT )
    , ( ZScore2Neg, ZScore2Neg, EQ )
    , ( ZScore2Neg, ZScore1Neg, LT )
    , ( ZScore2Neg, ZScore0, LT )
    , ( ZScore2Neg, ZScore1, LT )
    , ( ZScore2Neg, ZScore2, LT )
    , ( ZScore2Neg, ZScore3, LT )
    , ( ZScore1Neg, ZScore3Neg, GT )
    , ( ZScore1Neg, ZScore2Neg, GT )
    , ( ZScore1Neg, ZScore1Neg, EQ )
    , ( ZScore1Neg, ZScore0, LT )
    , ( ZScore1Neg, ZScore1, LT )
    , ( ZScore1Neg, ZScore2, LT )
    , ( ZScore1Neg, ZScore3, LT )
    , ( ZScore0, ZScore3Neg, GT )
    , ( ZScore0, ZScore2Neg, GT )
    , ( ZScore0, ZScore1Neg, GT )
    , ( ZScore0, ZScore0, EQ )
    , ( ZScore0, ZScore1, LT )
    , ( ZScore0, ZScore2, LT )
    , ( ZScore0, ZScore3, LT )
    , ( ZScore1, ZScore3Neg, GT )
    , ( ZScore1, ZScore2Neg, GT )
    , ( ZScore1, ZScore1Neg, GT )
    , ( ZScore1, ZScore0, GT )
    , ( ZScore1, ZScore1, EQ )
    , ( ZScore1, ZScore2, LT )
    , ( ZScore1, ZScore3, LT )
    , ( ZScore2, ZScore3Neg, GT )
    , ( ZScore2, ZScore2Neg, GT )
    , ( ZScore2, ZScore1Neg, GT )
    , ( ZScore2, ZScore0, GT )
    , ( ZScore2, ZScore1, GT )
    , ( ZScore2, ZScore2, EQ )
    , ( ZScore2, ZScore3, LT )
    , ( ZScore3, ZScore3Neg, GT )
    , ( ZScore3, ZScore2Neg, GT )
    , ( ZScore3, ZScore1Neg, GT )
    , ( ZScore3, ZScore0, GT )
    , ( ZScore3, ZScore1, GT )
    , ( ZScore3, ZScore2, GT )
    , ( ZScore3, ZScore3, EQ )
    ]


compareZScoreTest : Test
compareZScoreTest =
    comparisons
        |> List.map
            (\( z1, z2, expected ) ->
                test (viewZScore z1 ++ " " ++ viewZScore z2) <|
                    \() -> Expect.equal expected (compareZScore z1 z2)
            )
        |> describe "compareZScore"


all : Test
all =
    describe "ZScore"
        [ compareZScoreTest
        , viewZScoreTest
        ]
