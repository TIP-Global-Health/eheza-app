module TestRunner exposing (allTests)

{-| This module includes all of the tests in the "main" source
tree, so that the `elm-test` command will find them.
-}

import Activity.Test
import App.Test
import Backend.Measurement.Test
import Backend.Session.Test
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Measurement.Test
import Pages.Activity.Test
import Pages.Participant.Test
import Pages.ProgressReport.Test
import Test exposing (..)
import Utils.NominalDateTest
import ZScore.Test


allTests : Test
allTests =
    describe "All tests"
        [ Activity.Test.all
        , App.Test.all
        , Backend.Measurement.Test.all
        , Backend.Session.Test.all
        , Measurement.Test.all
        , Pages.Activity.Test.all
        , Pages.Participant.Test.all
        , Pages.ProgressReport.Test.all
        , Utils.NominalDateTest.all
        , ZScore.Test.all
        ]
