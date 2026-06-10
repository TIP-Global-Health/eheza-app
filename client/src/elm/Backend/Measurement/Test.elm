module Backend.Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (ColorAlertIndication(..), HeadCircumferenceInCm(..), MuacInCm(..))
import Backend.Measurement.Utils exposing (headCircumferenceIndication, muacIndicationForAdult, muacIndicationForChild)
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


headCircumferenceIndicationTest : Test
headCircumferenceIndicationTest =
    -- WHO: a head-circumference z-score outside +/-3 SD is a red flag.
    describe "headCircumferenceIndication"
        [ test "-3.5 -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm -3.5)
                    |> Expect.equal ColorAlertRed
        , test "-3.0 boundary -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm -3.0)
                    |> Expect.equal ColorAlertRed
        , test "0.0 -> green" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 0.0)
                    |> Expect.equal ColorAlertGreen
        , test "2.9 -> green" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 2.9)
                    |> Expect.equal ColorAlertGreen
        , test "3.0 boundary -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 3.0)
                    |> Expect.equal ColorAlertRed
        , test "3.5 -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 3.5)
                    |> Expect.equal ColorAlertRed
        ]


all : Test
all =
    describe "Measurement data tests"
        [ muacIndicationTest
        , headCircumferenceIndicationTest
        ]
