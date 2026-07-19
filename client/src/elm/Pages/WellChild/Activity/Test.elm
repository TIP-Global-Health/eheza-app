module Pages.WellChild.Activity.Test exposing (all)

import Date
import Expect
import Pages.WellChild.Activity.Utils
    exposing
        ( resolveFirstEncounterDateAfterMilestone
        , resolveNextDateForECDVisit
        )
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.WellChild.Activity.Utils"
        [ resolveFirstEncounterDateAfterMilestoneTests
        , resolveNextDateForECDVisitTests
        ]


resolveFirstEncounterDateAfterMilestoneTests : Test
resolveFirstEncounterDateAfterMilestoneTests =
    describe "resolveFirstEncounterDateAfterMilestone"
        [ test "picks the earliest encounter strictly AFTER the milestone, not an earlier one before it" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 15
                    , Date.fromCalendarDate 2024 Time.Sep 1
                    ]
                    |> Expect.equal (Just (Date.fromCalendarDate 2024 Time.Jul 15))
        , test "returns Nothing when every encounter is on or before the milestone" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 1
                    ]
                    |> Expect.equal Nothing
        , test "treats an encounter exactly on the milestone date as not after it" <|
            \_ ->
                resolveFirstEncounterDateAfterMilestone (Date.fromCalendarDate 2024 Time.Jul 1)
                    [ Date.fromCalendarDate 2024 Time.Jul 1 ]
                    |> Expect.equal Nothing
        ]


{-| The ECD sign group starting at 18 months means a child seen between 15 and
17 months should return at 18 months, not skip to 24 months.
-}
resolveNextDateForECDVisitTests : Test
resolveNextDateForECDVisitTests =
    let
        birthDate =
            Date.fromCalendarDate 2024 Time.Jan 1

        resolve current =
            resolveNextDateForECDVisit current birthDate True
    in
    describe "resolveNextDateForECDVisit"
        [ test "a 15-month-old is scheduled for the 18-month visit, not 24 months" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Apr 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "a 17-month-old is still scheduled for the 18-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jun 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "an 18-month-old moves on to the 24-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jul 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2026 Time.Jan 1))
        , test "the 15-month rung is unchanged for a younger child" <|
            \_ ->
                resolve (Date.fromCalendarDate 2024 Time.Nov 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Apr 1))
        , test "the years ladder is unchanged for a two-year-old" <|
            \_ ->
                resolve (Date.fromCalendarDate 2026 Time.Jan 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2027 Time.Jan 1))
        ]
