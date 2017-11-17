module Participant.Test exposing (all)

import Date
import Expect
import Fixtures exposing (exampleChildA, exampleChildB, exampleMother)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Test exposing (describe, test, Test)
import Participant.Model exposing (AgeDay(..), Participant(ParticipantChild, ParticipantMother))
import Participant.Utils exposing (getAgeDays, getParticipantBirthDate, renderAgeMonthsDays, renderDateOfBirth)
import Time.Date exposing (date)
import Translate exposing (Language(English))


renderParticipantAge : Language -> Participant -> NominalDate -> String
renderParticipantAge language participant today =
    getParticipantBirthDate participant
        |> (\birth -> renderAgeMonthsDays language birth today)


renderParticipantDateOfBirth : Language -> Participant -> String
renderParticipantDateOfBirth language =
    getParticipantBirthDate >> renderDateOfBirth language


getAgeDaysTest : Test
getAgeDaysTest =
    let
        today =
            fromLocalDateTime (Date.fromTime 1503920848000)
    in
        describe "age calculation"
            [ test "for newborn" <|
                \() ->
                    Expect.equal
                        (getAgeDays
                            (fromLocalDateTime <| Date.fromTime 1503834862000)
                            today
                        )
                        (Participant.Model.AgeDay 1)
            , test "for a week old newborn" <|
                \() ->
                    Expect.equal
                        (getAgeDays
                            (fromLocalDateTime <| Date.fromTime 1503316462000)
                            today
                        )
                        (Participant.Model.AgeDay 7)
            , test "for a one month old baby" <|
                \() ->
                    Expect.equal
                        (getAgeDays
                            (fromLocalDateTime <| Date.fromTime 1501156048000)
                            today
                        )
                        (Participant.Model.AgeDay 32)
            , test "for a thirteen months old baby" <|
                \() ->
                    Expect.equal
                        (getAgeDays
                            (fromLocalDateTime <| Date.fromTime 1469101648000)
                            today
                        )
                        (Participant.Model.AgeDay 403)
            , test "for a 30 years old mother" <|
                \() ->
                    Expect.equal
                        (getAgeDays
                            (fromLocalDateTime <| Date.fromTime 557840848000)
                            today
                        )
                        (Participant.Model.AgeDay 10950)
            ]


renderParticipantAgeTest : Test
renderParticipantAgeTest =
    let
        today =
            fromLocalDateTime <|
                Date.fromTime 1503920848000
    in
        describe "age calculation"
            [ test "for newborn" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantChild { exampleChildA | birthDate = fromLocalDateTime <| Date.fromTime 1503834862000 })
                            today
                        )
                        "1 day"
            , test "for a week old newborn" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantChild { exampleChildA | birthDate = fromLocalDateTime <| Date.fromTime 1503316462000 })
                            today
                        )
                        "7 days"
            , test "for a one month old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantChild { exampleChildA | birthDate = fromLocalDateTime <| Date.fromTime 1501242448000 })
                            today
                        )
                        "1 month"
            , test "for a one month, one day old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantChild { exampleChildB | birthDate = fromLocalDateTime <| Date.fromTime 1501156048000 })
                            today
                        )
                        "1 month and 1 day"
            , test "for a thirteen months old baby" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantChild { exampleChildA | birthDate = fromLocalDateTime <| Date.fromTime 1469101648000 })
                            today
                        )
                        "13 months and 7 days"
            , test "for a 30 years old mother" <|
                \() ->
                    Expect.equal
                        (renderParticipantAge English
                            (ParticipantMother { exampleMother | birthDate = fromLocalDateTime <| Date.fromTime 557840848000 })
                            today
                        )
                        "359 months and 23 days"
            ]


renderParticipantDateOfBirthTest : Test
renderParticipantDateOfBirthTest =
    describe "date of birth rendering"
        [ test "for July" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        (ParticipantChild { exampleChildA | birthDate = date 2017 7 30 })
                    )
                    "30 July 2017"
        , test "for March" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        (ParticipantChild { exampleChildB | birthDate = date 2017 3 29 })
                    )
                    "29 March 2017"
        , test "for January" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        (ParticipantChild { exampleChildA | birthDate = date 2017 1 21 })
                    )
                    "21 January 2017"
        , test "for August 2014" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        (ParticipantChild { exampleChildA | birthDate = date 2014 8 25 })
                    )
                    "25 August 2014"
        , test "for May 2017" <|
            \() ->
                Expect.equal
                    (renderParticipantDateOfBirth English
                        (ParticipantChild { exampleChildB | birthDate = date 2017 5 6 })
                    )
                    "06 May 2017"
        ]


all : Test
all =
    describe "Participant tests"
        [ getAgeDaysTest
        , renderParticipantAgeTest
        , renderParticipantDateOfBirthTest
        ]
