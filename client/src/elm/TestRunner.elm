port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


-- Register Test Stubs Here

import App.Test
import Backend.Measurement.Test
import Measurement.Test
import Pages.Activity.Test
import Pages.Participant.Test
import Participant.Test
import ProgressReport.Test
import ZScore.Test


allTests : Test
allTests =
    describe "All tests"
        [ App.Test.all
        , Backend.Measurement.Test.all
        , Measurement.Test.all
        , Pages.Activity.Test.all
        , Pages.Participant.Test.all
        , Participant.Test.all
        , ProgressReport.Test.all
        , ZScore.Test.all
        ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
