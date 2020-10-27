module Utils.NominalDateTest exposing (all)

import Date
import Expect
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Test exposing (Test, describe, test)
import Time.Date exposing (date)
import Translate.Model exposing (Language(..))
import Utils.NominalDate exposing (Days(..), diffDays, renderAgeMonthsDays, renderDate)


diffDaysTest : Test
diffDaysTest =
    let
        today =
            fromLocalDateTime (Date.fromTime 1503920848000)
    in
    describe "diffDays"
        [ test "for newborn" <|
            \() ->
                Expect.equal
                    (diffDays
                        (fromLocalDateTime <| Date.fromTime 1503834862000)
                        today
                    )
                    (Days 1)
        , test "for a week old newborn" <|
            \() ->
                Expect.equal
                    (diffDays
                        (fromLocalDateTime <| Date.fromTime 1503316462000)
                        today
                    )
                    (Days 7)
        , test "for a one month old baby" <|
            \() ->
                Expect.equal
                    (diffDays
                        (fromLocalDateTime <| Date.fromTime 1501156048000)
                        today
                    )
                    (Days 32)
        , test "for a thirteen months old baby" <|
            \() ->
                Expect.equal
                    (diffDays
                        (fromLocalDateTime <| Date.fromTime 1469101648000)
                        today
                    )
                    (Days 403)
        , test "for a 30 years old mother" <|
            \() ->
                Expect.equal
                    (diffDays
                        (fromLocalDateTime <| Date.fromTime 557840848000)
                        today
                    )
                    (Days 10950)
        ]


renderAgeMonthsDaysTest : Test
renderAgeMonthsDaysTest =
    let
        today =
            fromLocalDateTime <|
                Date.fromTime 1503920848000
    in
    describe "renderAgeMonthDays"
        [ test "for newborn" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 1503834862000)
                        today
                    )
                    "1 day"
        , test "for a week old newborn" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 1503316462000)
                        today
                    )
                    "7 days"
        , test "for a one month old baby" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 1501242448000)
                        today
                    )
                    "1 month"
        , test "for a one month, one day old baby" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 1501156048000)
                        today
                    )
                    "1 month 1 day"
        , test "for a thirteen months old baby" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 1469101648000)
                        today
                    )
                    "13 months 7 days"
        , test "for a 30 years old mother" <|
            \() ->
                Expect.equal
                    (renderAgeMonthsDays English
                        (fromLocalDateTime <| Date.fromTime 557840848000)
                        today
                    )
                    "359 months 23 days"
        ]


renderDateTest : Test
renderDateTest =
    describe "date of birth rendering"
        [ test "for July" <|
            \() ->
                date 2017 7 30
                    |> renderDate English
                    |> Expect.equal "30 July, 2017"
        , test "for March" <|
            \() ->
                date 2017 3 29
                    |> renderDate English
                    |> Expect.equal "29 March, 2017"
        , test "for January" <|
            \() ->
                date 2017 1 21
                    |> renderDate English
                    |> Expect.equal "21 January, 2017"
        , test "for August 2014" <|
            \() ->
                date 2014 8 25
                    |> renderDate English
                    |> Expect.equal "25 August, 2014"
        , test "for May 2017" <|
            \() ->
                date 2017 5 6
                    |> renderDate English
                    |> Expect.equal "06 May, 2017"
        ]


all : Test
all =
    describe "NominalDate tests"
        [ diffDaysTest
        , renderAgeMonthsDaysTest
        , renderDateTest
        ]
