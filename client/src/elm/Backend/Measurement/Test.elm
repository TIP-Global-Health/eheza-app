module Backend.Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (MuacIndication(..), MuacInCm(..))
import Backend.Measurement.Utils exposing (muacIndication)
import Expect
import Test exposing (Test, describe, test)


muacIndicationTest : Test
muacIndicationTest =
    describe "MuacIndication tests"
        [ describe "muacIndication"
            [ test "red" <|
                \_ ->
                    muacIndication (MuacInCm 11.5)
                        |> Expect.equal MuacRed
            , test "yellow1" <|
                \_ ->
                    muacIndication (MuacInCm 11.6)
                        |> Expect.equal MuacYellow
            , test "yellow2" <|
                \_ ->
                    muacIndication (MuacInCm 12.5)
                        |> Expect.equal MuacYellow
            , test "green" <|
                \_ ->
                    muacIndication (MuacInCm 12.6)
                        |> Expect.equal MuacGreen
            ]
        ]


all : Test
all =
    describe "Measurement data tests"
        [ muacIndicationTest
        ]
