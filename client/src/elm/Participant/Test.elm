module Participant.Test exposing (all)

import Date
import Expect
import Fixtures exposing (exampleChild, exampleMother)
import Test exposing (describe, test, Test)
import Participant.Model exposing (AgeDay(..), ParticipantType(ParticipantChild, ParticipantMother))
import Participant.Utils exposing (getParticipantAge, renderParticipantAge)
import Translate exposing (Language(English))


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
            , test "for a one month old baby" <|
                \() ->
                    Expect.equal
                        (getParticipantAge
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1501156048000 }) }
                            today
                        )
                        (Participant.Model.AgeDay 32)
            , test "for a thirteen months old baby" <|
                \() ->
                    Expect.equal
                        (getParticipantAge
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1469101648000 }) }
                            today
                        )
                        (Participant.Model.AgeDay 403)
            , test "for a 30 years old mother" <|
                \() ->
                    Expect.equal
                        (getParticipantAge
                            { info = (ParticipantMother { exampleMother | birthDate = Date.fromTime 557840848000 }) }
                            today
                        )
                        (Participant.Model.AgeDay 10950)
            ]


renderParticipantAgeTest : Test
renderParticipantAgeTest =
    let
        today =
            Date.fromTime 1503920848000
    in
        describe "age calculation"
            [ test "for newborn" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1503834862000 }) }
                            today
                        )
                        "1 day"
            , test "for a week old newborn" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1503316462000 }) }
                            today
                        )
                        "7 days"
            , test "for a one month old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1501156048000 }) }
                            today
                        )
                        "1 month and 1 days"
            , test "for a thirteen months old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1469101648000 }) }
                            today
                        )
                        "13 months and 2 days"
            , test "for a 30 years old mother" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantMother { exampleMother | birthDate = Date.fromTime 557840848000 }) }
                            today
                        )
                        "360 months and 2 days"
            ]


all : Test
all =
    describe "Participant tests"
        [ getParticipantAgeTest
        , renderParticipantAgeTest
        ]
