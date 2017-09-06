module ZScore.Test exposing (all)

import Child.Model
import Expect
import Test exposing (Test, describe, test)
import ZScore.Model exposing (..)


viewZScoreTest : Test
viewZScoreTest =
    describe "viewZScore"
        [ test "ZScore3Neg" <| \() -> Expect.equal "-3" (viewZScore ZScore3Neg)
        ]


all : Test
all =
    describe "ZScore"
        [ viewZScoreTest
        ]
