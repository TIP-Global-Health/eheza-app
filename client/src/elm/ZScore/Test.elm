module ZScore.Test exposing (all)

import AllDict exposing (AllDict)
import Backend.Child.Model exposing (Gender(..))
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Http
import Json.Decode exposing (Decoder, decodeString)
import RemoteData exposing (WebData)
import Test exposing (Test, describe, test)
import Utils.NominalDate exposing (Days(..), Months(..))
import ZScore.Decoder exposing (..)
import ZScore.Fixture.Bfawho2007
import ZScore.Fixture.Bmianthro
import ZScore.Fixture.Hfawho2007
import ZScore.Fixture.Lenanthro
import ZScore.Fixture.Weianthro
import ZScore.Fixture.Wfawho2007
import ZScore.Fixture.Wfhanthro
import ZScore.Fixture.Wflanthro
import ZScore.Model exposing (..)
import ZScore.Utils exposing (..)


type alias MonthsAndDays a =
    { days : a
    , months : a
    }


testModel : Model
testModel =
    { bmiForAge =
        fetchForAge BMI
            { days = ZScore.Fixture.Bmianthro.json
            , months = ZScore.Fixture.Bfawho2007.json
            }
    , lengthHeightForAge =
        fetchForAge Centimetres
            { days = ZScore.Fixture.Lenanthro.json
            , months = ZScore.Fixture.Hfawho2007.json
            }
    , weightForAge =
        fetchForAge Kilograms
            { days = ZScore.Fixture.Weianthro.json
            , months = ZScore.Fixture.Wfawho2007.json
            }
    , weightForHeight = fetchForHeight ZScore.Fixture.Wfhanthro.json
    , weightForLength = fetchForLength ZScore.Fixture.Wflanthro.json
    }


fetchForAge : (Float -> a) -> MonthsAndDays String -> WebData (MaleAndFemale (ByDaysAndMonths a))
fetchForAge wrapper tables =
    RemoteData.map2
        (\days months ->
            { male =
                { byDay = days.male
                , byMonth = months.male
                }
            , female =
                { byDay = days.female
                , byMonth = months.female
                }
            }
        )
        (decodeFixture (decodeForAge Days (\(Days x) -> x) wrapper) tables.days)
        (decodeFixture (decodeForAge Months (\(Months x) -> x) wrapper) tables.months)


fetchForHeight : String -> WebData (MaleAndFemale (AllDict Height (ZScoreEntry Kilograms) Int))
fetchForHeight table =
    decodeFixture (decodeForCentimetres "height" Height (\(Height x) -> x)) table


fetchForLength : String -> WebData (MaleAndFemale (AllDict Length (ZScoreEntry Kilograms) Int))
fetchForLength table =
    decodeFixture (decodeForCentimetres "length" Length (\(Length x) -> x)) table


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
zScoreCalculationData : List ( String, Float, Gender, Float, Maybe Float )
zScoreCalculationData =
    [ ( "lfa", 0, Male, 40.1, Just -5.17 )
    , ( "lfa", 0, Male, 44.1, Just -3.06 )
    , ( "lfa", 0, Male, 46.0, Just -2.05 )
    , ( "lfa", 0, Male, 47.9, Just -1.05 )
    , ( "lfa", 0, Male, 49.5, Just -0.2 )
    , ( "lfa", 0, Male, 51.6, Just 0.91 )
    , ( "lfa", 0, Male, 53.6, Just 1.96 )
    , ( "lfa", 0, Male, 54.0, Just 2.17 )
    , ( "lfa", 0, Male, 56.0, Just 3.23 )
    , ( "lfa", 440, Male, 44, Just -13.82 )
    , ( "lfa", 440, Male, 69, Just -3.82 )
    , ( "lfa", 440, Male, 72, Just -2.62 )
    , ( "lfa", 440, Male, 74, Just -1.82 )
    , ( "lfa", 440, Male, 77, Just -0.62 )
    , ( "lfa", 440, Male, 80, Just 0.58 )
    , ( "lfa", 440, Male, 82, Just 1.38 )
    , ( "lfa", 440, Male, 84, Just 2.18 )
    , ( "lfa", 440, Male, 87, Just 3.38 )
    , ( "lfa", 30000, Male, 234, Nothing )
    , ( "lfa", 0, Female, 40, Just -4.91 )
    , ( "lfa", 0, Female, 42, Just -3.84 )
    , ( "lfa", 0, Female, 44, Just -2.76 )
    , ( "lfa", 0, Female, 46, Just -1.69 )
    , ( "lfa", 0, Female, 48, Just -0.61 )
    , ( "lfa", 0, Female, 50, Just 0.46 )
    , ( "lfa", 0, Female, 52, Just 1.53 )
    , ( "lfa", 0, Female, 54, Just 2.6 )
    , ( "lfa", 0, Female, 56, Just 3.68 )
    , ( "lfa", 440, Female, 65, Just -4.39 )
    , ( "lfa", 440, Female, 67, Just -3.65 )
    , ( "lfa", 440, Female, 69, Just -2.92 )
    , ( "lfa", 440, Female, 72, Just -1.81 )
    , ( "lfa", 440, Female, 75, Just -0.7 )
    , ( "lfa", 440, Female, 78, Just 0.41 )
    , ( "lfa", 440, Female, 80, Just 1.14 )
    , ( "lfa", 440, Female, 83, Just 2.25 )
    , ( "lfa", 440, Female, 86, Just 3.36 )
    , ( "lfa", 30000, Female, 234, Nothing )
    , ( "wfa", 0, Male, 1.5, Just -4.53 )
    , ( "wfa", 0, Male, 2.0, Just -3.21 )
    , ( "wfa", 0, Male, 2.4, Just -2.15 )
    , ( "wfa", 0, Male, 2.8, Just -1.18 )
    , ( "wfa", 0, Male, 3.3, Just -0.1 )
    , ( "wfa", 0, Male, 3.8, Just 0.89 )
    , ( "wfa", 0, Male, 4.4, Just 1.97 )
    , ( "wfa", 0, Male, 5.0, Just 2.95 )
    , ( "wfa", 0, Male, 5.6, Just 3.93 )
    , ( "wfa", 440, Male, 6, Just -4.52 )
    , ( "wfa", 440, Male, 7, Just -3.36 )
    , ( "wfa", 440, Male, 8, Just -2.19 )
    , ( "wfa", 440, Male, 9, Just -1.13 )
    , ( "wfa", 440, Male, 10, Just -0.17 )
    , ( "wfa", 440, Male, 11, Just 0.69 )
    , ( "wfa", 440, Male, 12, Just 1.49 )
    , ( "wfa", 440, Male, 14, Just 2.91 )
    , ( "wfa", 440, Male, 15, Just 3.59 )
    , ( "wfa", 30000, Male, 234, Nothing )
    , ( "wfa", 0, Female, 1.6, Just -4.2 )
    , ( "wfa", 0, Female, 2.0, Just -3.09 )
    , ( "wfa", 0, Female, 2.3, Just -2.25 )
    , ( "wfa", 0, Female, 2.7, Just -1.23 )
    , ( "wfa", 0, Female, 3.2, Just -0.07 )
    , ( "wfa", 0, Female, 3.7, Just 0.98 )
    , ( "wfa", 0, Female, 4.2, Just 1.94 )
    , ( "wfa", 0, Female, 4.7, Just 2.84 )
    , ( "wfa", 0, Female, 5.3, Just 3.9 )
    , ( "wfa", 440, Female, 5, Just -5.05 )
    , ( "wfa", 440, Female, 6, Just -3.81 )
    , ( "wfa", 440, Female, 7, Just -2.56 )
    , ( "wfa", 440, Female, 8, Just -1.41 )
    , ( "wfa", 440, Female, 9, Just -0.43 )
    , ( "wfa", 440, Female, 10, Just 0.43 )
    , ( "wfa", 440, Female, 12, Just 1.86 )
    , ( "wfa", 440, Female, 13, Just 2.47 )
    , ( "wfa", 440, Female, 14, Just 3.03 )
    , ( "wfa", 30000, Female, 234, Nothing )
    , ( "wfl", 45, Male, 1.7, Just -4.06 )
    , ( "wfl", 45, Male, 1.8, Just -3.46 )
    , ( "wfl", 45, Male, 2.0, Just -2.25 )
    , ( "wfl", 45, Male, 2.2, Just -1.15 )
    , ( "wfl", 45, Male, 2.4, Just -0.18 )
    , ( "wfl", 45, Male, 2.6, Just 0.68 )
    , ( "wfl", 45, Male, 2.9, Just 1.82 )
    , ( "wfl", 45, Male, 3.2, Just 2.81 )
    , ( "wfl", 45, Male, 3.5, Just 3.77 )
    , ( "wfl", 98.6, Male, 10.77, Just -4.01 )
    , ( "wfl", 98.6, Male, 11.69, Just -3.01 )
    , ( "wfl", 98.6, Male, 12.62, Just -2.0 )
    , ( "wfl", 98.6, Male, 13.64, Just -1.01 )
    , ( "wfl", 98.6, Male, 14.78, Just -0.01 )
    , ( "wfl", 98.6, Male, 16.06, Just 1.0 )
    , ( "wfl", 98.6, Male, 17.48, Just 1.99 )
    , ( "wfl", 98.6, Male, 19.09, Just 3.0 )
    , ( "wfl", 98.6, Male, 20.6, Just 3.94 )
    , ( "wfl", 30000, Male, 234, Nothing )
    , ( "wfl", 45, Female, 1.7, Just -4.22 )
    , ( "wfl", 45, Female, 1.9, Just -3.01 )
    , ( "wfl", 45, Female, 2.0, Just -2.39 )
    , ( "wfl", 45, Female, 2.2, Just -1.27 )
    , ( "wfl", 45, Female, 2.4, Just -0.27 )
    , ( "wfl", 45, Female, 2.6, Just 0.6 )
    , ( "wfl", 45, Female, 2.9, Just 1.76 )
    , ( "wfl", 45, Female, 3.2, Just 2.77 )
    , ( "wfl", 45, Female, 3.5, Just 3.73 )
    , ( "wfl", 98.6, Female, 10.33, Just -4.0 )
    , ( "wfl", 98.6, Female, 11.31, Just -3.01 )
    , ( "wfl", 98.6, Female, 12.29, Just -2.01 )
    , ( "wfl", 98.6, Female, 13.4, Just -1.0 )
    , ( "wfl", 98.6, Female, 14.64, Just 0.0 )
    , ( "wfl", 98.6, Female, 16.04, Just 0.99 )
    , ( "wfl", 98.6, Female, 17.66, Just 2.0 )
    , ( "wfl", 98.6, Female, 19.49, Just 3.0 )
    , ( "wfl", 98.6, Female, 21.33, Just 4.0 )
    , ( "wfl", 30000, Female, 234, Nothing )
    ]


expectNear : Maybe ZScore -> Maybe ZScore -> Expectation
expectNear expected actual =
    case ( expected, actual ) of
        ( Just e, Just a ) ->
            Expect.within (Absolute 0.01) e a

        ( Nothing, Nothing ) ->
            Expect.pass

        ( Nothing, a ) ->
            Expect.fail <| "Expected Nothing, got " ++ toString a

        ( e, Nothing ) ->
            Expect.fail <| "Expected " ++ toString e ++ ", got Nothing"


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
                                zScoreLengthHeightForAge testModel (Days (round scale)) gender (Centimetres measurement)
                                    |> expectNear expected

                    "wfa" ->
                        test testName <|
                            \_ ->
                                zScoreWeightForAge testModel (Days (round scale)) gender (Kilograms measurement)
                                    |> expectNear expected

                    "wfl" ->
                        test testName <|
                            \_ ->
                                zScoreWeightForLength testModel (Length scale) gender (Kilograms measurement)
                                    |> expectNear expected

                    _ ->
                        test testName <|
                            \_ ->
                                Expect.fail <| "Unknown test: " ++ func
            )
        |> describe "Calculations"


all : Test
all =
    describe "ZScore"
        [ calculateZScoreTest
        ]
