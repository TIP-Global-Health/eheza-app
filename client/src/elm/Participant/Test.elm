module Participant.Test exposing (all)

import Date
import Expect
import Fixtures exposing (exampleChild, exampleMother)
import Test exposing (describe, test, Test)
import Participant.Model exposing (AgeDay(..), ParticipantType(ParticipantChild, ParticipantMother))
import Participant.Utils exposing (getParticipantAge, renderParticipantAge, renderParticipantDateOfBirth)
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
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1501242448000 }) }
                            today
                        )
                        "1 month"
            , test "for a one month, one day old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1501156048000 }) }
                            today
                        )
                        "1 month and 1 day"
            , test "for a thirteen months old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1469101648000 }) }
                            today
                        )
                        "13 months and 7 days"
            , test "for a 30 years old mother" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            { info = (ParticipantMother { exampleMother | birthDate = Date.fromTime 557840848000 }) }
                            today
                        )
                        "359 months and 23 days"
            ]


renderParticipantDateOfBirthTest : Test
renderParticipantDateOfBirthTest =
    describe "date of birth renderring"
        [ test "for July" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1501404215000 }) }
                    )
                    "30 July 2017"
        , test "for March" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1490751015000 }) }
                    )
                    "29 March 2017"
        , test "for January" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1484961345000 }) }
                    )
                    "21 January 2017"
        , test "for August 2014" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1408995000000 }) }
                    )
                    "25 August 2014"
        , test "for May 2017" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        { info = (ParticipantChild { exampleChild | birthDate = Date.fromTime 1494081915000 }) }
                    )
                    "06 May 2017"
        ]


all : Test
all =
    describe "Participant tests"
        [ getParticipantAgeTest
        , renderParticipantAgeTest
        , renderParticipantDateOfBirthTest
        ]
