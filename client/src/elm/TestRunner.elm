port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


-- Register Test Stubs Here

import App.Test exposing (all)
import Measurement.Test exposing (all)
import Participant.Test exposing (all)
import Pusher.Test exposing (all)


allTests : Test
allTests =
    describe "All tests"
        [ App.Test.all
        , Measurement.Test.all
        , Participant.Test.all
        , Pusher.Test.all
        ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
