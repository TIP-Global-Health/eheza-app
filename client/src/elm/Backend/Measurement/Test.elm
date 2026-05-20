module Backend.Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (ColorAlertIndication(..), MuacInCm(..))
import Backend.Measurement.Utils exposing (muacIndicationForAdult, muacIndicationForChild)
import Expect
import Test exposing (Test, describe, test)


muacIndicationTest : Test
muacIndicationTest =
    describe "MuacIndication tests"
        [ describe "muacIndicationForChild"
            [ test "red" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 11.5)
                        |> Expect.equal ColorAlertRed
            , test "yellow1" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 11.6)
                        |> Expect.equal ColorAlertYellow
            , test "yellow2" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 12.5)
                        |> Expect.equal ColorAlertYellow
            , test "green" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 12.6)
                        |> Expect.equal ColorAlertGreen
            ]
        , describe "muacIndicationForAdult"
            [ test "red" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 18.4)
                        |> Expect.equal ColorAlertRed
            , test "yellow boundary" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 18.5)
                        |> Expect.equal ColorAlertYellow
            , test "yellow" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 21.9)
                        |> Expect.equal ColorAlertYellow
            , test "green" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 22)
                        |> Expect.equal ColorAlertGreen
            ]
        ]


all : Test
all =
    describe "Measurement data tests"
        [ muacIndicationTest
        ]
