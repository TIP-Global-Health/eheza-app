module Participant.Test exposing (all)

import Date
import Expect
import Fixtures exposing (exampleChild)
import Test exposing (describe, test, Test)
import Participant.Model exposing (AgeDay(..), ParticipantType(ParticipantChild))
import Participant.Utils exposing (getParticipantAge)


getParticipantAgeTest : Test
getParticipantAgeTest =
    let
        today =
            Date.fromTime 1503920848000
    in
        describe "age calculation"
            [ test "for newborn" <|
                \() ->
                    Expect.equal
                        (getParticipantAge
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1503834862000 }) }
                            today
                        )
                        (Participant.Model.AgeDay 1)
            , test "for a week old newborn" <|
                \() ->
                    Expect.equal
                        (getParticipantAge
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1503316462000 }) }
                            today
                        )
                        (Participant.Model.AgeDay 7)
            ]


all : Test
all =
    describe "Participant tests"
        [ getParticipantAgeTest
        ]
