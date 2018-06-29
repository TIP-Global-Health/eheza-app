module ZScore.Test exposing (all)

import Backend.Child.Model exposing (Gender(..))
import Expect
import Http
import Json.Decode exposing (Decoder, decodeString)
import RemoteData exposing (WebData)
import Test exposing (Test, describe, test)
import Utils.NominalDate exposing (Days(..))
import ZScore.Decoder exposing (..)
import ZScore.Fixture.Lhfa_boys_z_exp
import ZScore.Fixture.Lhfa_girls_z_exp
import ZScore.Fixture.Wfa_boys_z_exp
import ZScore.Fixture.Wfa_girls_z_exp
import ZScore.Fixture.Wfl_boys_z_exp
import ZScore.Fixture.Wfl_girls_z_exp
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


testModel : Model
testModel =
    { heightForAgeBoys = decodeFixture decodeZScoreEntriesByDay ZScore.Fixture.Lhfa_boys_z_exp.json
    , heightForAgeGirls = decodeFixture decodeZScoreEntriesByDay ZScore.Fixture.Lhfa_girls_z_exp.json
    , weightForAgeBoys = decodeFixture decodeZScoreEntriesByDay ZScore.Fixture.Wfa_boys_z_exp.json
    , weightForAgeGirls = decodeFixture decodeZScoreEntriesByDay ZScore.Fixture.Wfa_girls_z_exp.json
    , weightForHeightBoys = decodeFixture decodeZScoreEntriesByHeight ZScore.Fixture.Wfl_boys_z_exp.json
    , weightForHeightGirls = decodeFixture decodeZScoreEntriesByHeight ZScore.Fixture.Wfl_girls_z_exp.json
    }


decodeFixture : Decoder a -> String -> WebData a
decodeFixture decoder input =
    -- The `mapError` isn't really a `BadUrl`, but that's the simplest way to
    -- make this work
    decodeString decoder input
        |> RemoteData.fromResult
        |> RemoteData.mapError Http.BadUrl


{-| This is copied with formatting changes from the server tests, so we can
test both the client and server with essentially the same cases. You
wouldn't do it this way if this was a purely Elm test.
-}
zScoreCalculationData : List ( String, Float, Gender, Float, Maybe Int )
zScoreCalculationData =
    [ ( "lfa", 0, Male, 40.1, Just -3 )
    , ( "lfa", 0, Male, 44.1, Just -3 )
    , ( "lfa", 0, Male, 46.0, Just -2 )
    , ( "lfa", 0, Male, 47.9, Just -1 )
    , ( "lfa", 0, Male, 49.5, Just 0 )
    , ( "lfa", 0, Male, 51.6, Just 1 )
    , ( "lfa", 0, Male, 53.6, Just 2 )
    , ( "lfa", 0, Male, 54.0, Just 3 )
    , ( "lfa", 0, Male, 56.0, Just 3 )
    , ( "lfa", 440, Male, 44, Just -3 )
    , ( "lfa", 440, Male, 69, Just -3 )
    , ( "lfa", 440, Male, 72, Just -2 )
    , ( "lfa", 440, Male, 74, Just -1 )
    , ( "lfa", 440, Male, 77, Just 0 )
    , ( "lfa", 440, Male, 80, Just 1 )
    , ( "lfa", 440, Male, 82, Just 2 )
    , ( "lfa", 440, Male, 84, Just 3 )
    , ( "lfa", 440, Male, 87, Just 3 )
    , ( "lfa", 30000, Male, 234, Nothing )
    , ( "lfa", 0, Female, 40, Just -3 )
    , ( "lfa", 0, Female, 42, Just -3 )
    , ( "lfa", 0, Female, 44, Just -2 )
    , ( "lfa", 0, Female, 46, Just -1 )
    , ( "lfa", 0, Female, 48, Just 0 )
    , ( "lfa", 0, Female, 50, Just 1 )
    , ( "lfa", 0, Female, 52, Just 2 )
    , ( "lfa", 0, Female, 54, Just 3 )
    , ( "lfa", 0, Female, 56, Just 3 )
    , ( "lfa", 440, Female, 65, Just -3 )
    , ( "lfa", 440, Female, 67, Just -3 )
    , ( "lfa", 440, Female, 69, Just -2 )
    , ( "lfa", 440, Female, 72, Just -1 )
    , ( "lfa", 440, Female, 75, Just 0 )
    , ( "lfa", 440, Female, 78, Just 1 )
    , ( "lfa", 440, Female, 80, Just 2 )
    , ( "lfa", 440, Female, 83, Just 3 )
    , ( "lfa", 440, Female, 86, Just 3 )
    , ( "lfa", 30000, Female, 234, Nothing )
    , ( "wfa", 0, Male, 1.5, Just -3 )
    , ( "wfa", 0, Male, 2.0, Just -3 )
    , ( "wfa", 0, Male, 2.4, Just -2 )
    , ( "wfa", 0, Male, 2.8, Just -1 )
    , ( "wfa", 0, Male, 3.3, Just 0 )
    , ( "wfa", 0, Male, 3.8, Just 1 )
    , ( "wfa", 0, Male, 4.4, Just 2 )
    , ( "wfa", 0, Male, 5.0, Just 3 )
    , ( "wfa", 0, Male, 5.6, Just 3 )
    , ( "wfa", 440, Male, 6, Just -3 )
    , ( "wfa", 440, Male, 7, Just -3 )
    , ( "wfa", 440, Male, 8, Just -2 )
    , ( "wfa", 440, Male, 9, Just -1 )
    , ( "wfa", 440, Male, 10, Just 0 )
    , ( "wfa", 440, Male, 11, Just 1 )
    , ( "wfa", 440, Male, 12, Just 2 )
    , ( "wfa", 440, Male, 14, Just 3 )
    , ( "wfa", 440, Male, 15, Just 3 )
    , ( "wfa", 30000, Male, 234, Nothing )
    , ( "wfa", 0, Female, 1.6, Just -3 )
    , ( "wfa", 0, Female, 2.0, Just -3 )
    , ( "wfa", 0, Female, 2.3, Just -2 )
    , ( "wfa", 0, Female, 2.7, Just -1 )
    , ( "wfa", 0, Female, 3.2, Just 0 )
    , ( "wfa", 0, Female, 3.7, Just 1 )
    , ( "wfa", 0, Female, 4.2, Just 2 )
    , ( "wfa", 0, Female, 4.7, Just 3 )
    , ( "wfa", 0, Female, 5.3, Just 3 )
    , ( "wfa", 440, Female, 5, Just -3 )
    , ( "wfa", 440, Female, 6, Just -3 )
    , ( "wfa", 440, Female, 7, Just -2 )
    , ( "wfa", 440, Female, 8, Just -1 )
    , ( "wfa", 440, Female, 9, Just 0 )
    , ( "wfa", 440, Female, 10, Just 1 )
    , ( "wfa", 440, Female, 12, Just 2 )
    , ( "wfa", 440, Female, 13, Just 3 )
    , ( "wfa", 440, Female, 14, Just 3 )
    , ( "wfa", 30000, Female, 234, Nothing )
    , ( "wfl", 45, Male, 1.7, Just -3 )
    , ( "wfl", 45, Male, 1.8, Just -3 )
    , ( "wfl", 45, Male, 2.0, Just -2 )
    , ( "wfl", 45, Male, 2.2, Just -1 )
    , ( "wfl", 45, Male, 2.4, Just 0 )
    , ( "wfl", 45, Male, 2.6, Just 1 )
    , ( "wfl", 45, Male, 2.9, Just 2 )
    , ( "wfl", 45, Male, 3.2, Just 3 )
    , ( "wfl", 45, Male, 3.5, Just 3 )
    , ( "wfl", 98.6, Male, 10.77, Just -3 )
    , ( "wfl", 98.6, Male, 11.69, Just -3 )
    , ( "wfl", 98.6, Male, 12.62, Just -2 )
    , ( "wfl", 98.6, Male, 13.64, Just -1 )
    , ( "wfl", 98.6, Male, 14.78, Just 0 )
    , ( "wfl", 98.6, Male, 16.06, Just 1 )
    , ( "wfl", 98.6, Male, 17.48, Just 2 )
    , ( "wfl", 98.6, Male, 19.09, Just 3 )
    , ( "wfl", 98.6, Male, 20.6, Just 3 )
    , ( "wfl", 30000, Male, 234, Nothing )
    , ( "wfl", 45, Female, 1.7, Just -3 )
    , ( "wfl", 45, Female, 1.9, Just -3 )
    , ( "wfl", 45, Female, 2.0, Just -2 )
    , ( "wfl", 45, Female, 2.2, Just -1 )
    , ( "wfl", 45, Female, 2.4, Just 0 )
    , ( "wfl", 45, Female, 2.6, Just 1 )
    , ( "wfl", 45, Female, 2.9, Just 2 )
    , ( "wfl", 45, Female, 3.2, Just 3 )
    , ( "wfl", 45, Female, 3.5, Just 3 )
    , ( "wfl", 98.6, Female, 10.33, Just -3 )
    , ( "wfl", 98.6, Female, 11.31, Just -3 )
    , ( "wfl", 98.6, Female, 12.29, Just -2 )
    , ( "wfl", 98.6, Female, 13.4, Just -1 )
    , ( "wfl", 98.6, Female, 14.64, Just 0 )
    , ( "wfl", 98.6, Female, 16.04, Just 1 )
    , ( "wfl", 98.6, Female, 17.66, Just 2 )
    , ( "wfl", 98.6, Female, 19.49, Just 3 )
    , ( "wfl", 98.6, Female, 21.33, Just 3 )
    , ( "wfl", 30000, Female, 234, Nothing )
    ]


{-| This makes a test for each of the data points above. Again, this isn't
how you'd construct an Elm-only test, but it's convenient for sharing the
same basic test cases with the backend tests.
-}
calculateZScoreTest : Test
calculateZScoreTest =
    zScoreCalculationData
        |> List.map
            (\( func, scale, gender, measurement, expected ) ->
                let
                    testName =
                        String.join " " [ func, toString scale, toString gender, toString measurement, toString expected ]
                in
                case func of
                    "lfa" ->
                        test testName <|
                            \_ ->
                                zScoreHeightForAge testModel (Days (round scale)) gender (Centimetres measurement)
                                    |> Maybe.map (viewZScore >> String.toInt)
                                    |> Expect.equal (Maybe.map Ok expected)

                    "wfa" ->
                        test testName <|
                            \_ ->
                                zScoreWeightForAge testModel (Days (round scale)) gender (Kilograms measurement)
                                    |> Maybe.map (viewZScore >> String.toInt)
                                    |> Expect.equal (Maybe.map Ok expected)

                    "wfl" ->
                        test testName <|
                            \_ ->
                                zScoreWeightForHeight testModel (Centimetres scale) gender (Kilograms measurement)
                                    |> Maybe.map (viewZScore >> String.toInt)
                                    |> Expect.equal (Maybe.map Ok expected)

                    _ ->
                        test testName <|
                            \_ ->
                                Expect.fail <| "Unknown test: " ++ func
            )
        |> describe "Calculations"


all : Test
all =
    describe "ZScore"
        [ compareZScoreTest
        , viewZScoreTest
        , calculateZScoreTest
        ]
