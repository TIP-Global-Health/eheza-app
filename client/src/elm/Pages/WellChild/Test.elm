module Pages.WellChild.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model exposing (VaccineDose(..), WellChildVaccineType(..))
import Date exposing (Unit(..))
import Expect
import Gizra.NominalDate exposing (diffMonths)
import List.Extra
import Pages.WellChild.Activity.Utils exposing (resolveNextDateForImmunisationVisit)
import Pages.WellChild.ProgressReport.View exposing (distributeByListIndex, resolveLastDayForMonthX)
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.WellChild"
        [ resolveNextDateForImmunisationVisitTests
        , immunizationBucketingTests
        ]


{-| The scenarios mirror issue #1896: an on-schedule child used to get
Nothing at essentially every encounter after 14 weeks of age, because
completed vaccine cycles still emit (vaccineType, Nothing) tuples and
the within-a-month filter had no fallback.
-}
resolveNextDateForImmunisationVisitTests : Test
resolveNextDateForImmunisationVisitTests =
    let
        -- An on-schedule child born 2024-01-01, at the 14 weeks encounter.
        fourteenWeeksDate =
            Date.fromCalendarDate 2024 Time.Apr 8

        nineMonthsDate =
            Date.fromCalendarDate 2024 Time.Oct 1

        fifteenMonthsDate =
            Date.fromCalendarDate 2025 Time.Apr 1
    in
    describe "resolveNextDateForImmunisationVisit"
        [ test "suggests the next milestone date when no dose is due within a month" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineBCG, Nothing )
                    , ( VaccineOPV, Nothing )
                    , ( VaccineDTP, Nothing )
                    , ( VaccinePCV13, Nothing )
                    , ( VaccineRotarix, Nothing )
                    , ( VaccineIPV, Just ( VaccineDoseSecond, nineMonthsDate ) )
                    , ( VaccineMR, Just ( VaccineDoseFirst, nineMonthsDate ) )
                    ]
                    |> Expect.equal (Just nineMonthsDate)
        , test "suggests the earliest MR/HPV date when only long-interval doses remain" <|
            \_ ->
                resolveNextDateForImmunisationVisit (Date.fromCalendarDate 2024 Time.Oct 5)
                    [ ( VaccineBCG, Nothing )
                    , ( VaccineOPV, Nothing )
                    , ( VaccineDTP, Nothing )
                    , ( VaccinePCV13, Nothing )
                    , ( VaccineRotarix, Nothing )
                    , ( VaccineIPV, Nothing )
                    , ( VaccineMR, Just ( VaccineDoseSecond, fifteenMonthsDate ) )
                    , ( VaccineHPV, Nothing )
                    ]
                    |> Expect.equal (Just fifteenMonthsDate)
        , test "falls back to the earliest pending dose across both interval groups" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineIPV, Just ( VaccineDoseSecond, Date.add Days 60 fourteenWeeksDate ) )
                    , ( VaccineMR, Just ( VaccineDoseFirst, Date.add Days 40 fourteenWeeksDate ) )
                    ]
                    |> Expect.equal (Just (Date.add Days 40 fourteenWeeksDate))
        , test "bundles doses due within a month on the latest of their dates" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineOPV, Just ( VaccineDoseThird, Date.add Days 10 fourteenWeeksDate ) )
                    , ( VaccineDTP, Just ( VaccineDoseThird, Date.add Days 20 fourteenWeeksDate ) )
                    , ( VaccineMR, Nothing )
                    ]
                    |> Expect.equal (Just (Date.add Days 20 fourteenWeeksDate))
        , test "clamps a passed long-interval suggestion to one interval from today" <|
            \_ ->
                resolveNextDateForImmunisationVisit (Date.fromCalendarDate 2025 Time.May 1)
                    [ ( VaccineOPV, Nothing )
                    , ( VaccineMR, Just ( VaccineDoseSecond, fifteenMonthsDate ) )
                    ]
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Nov 1))
        ]


{-| The Child Scorecard immunization row builds one value per age-in-months in
order, then keys them by list position. It used to re-derive the month from a
synthetic reference date (last day of month X after the birth month), which
mis-buckets children born on day 29/30/31 - the reference date lands in a
shorter month and `diffMonths` counts one month too few, collapsing a bucket.
-}
immunizationBucketingTests : Test
immunizationBucketingTests =
    let
        referenceDateBuckets birthDate =
            List.range 0 24
                |> List.map (\index -> diffMonths birthDate (resolveLastDayForMonthX index birthDate))
    in
    describe "immunization age-in-months bucketing"
        [ test "distributeByListIndex keys every one of the 25 months for a day-31 birth" <|
            \_ ->
                let
                    birthDate =
                        Date.fromCalendarDate 2024 Time.Jan 31

                    syntheticValues =
                        List.range 0 24
                            |> List.map (\index -> ( resolveLastDayForMonthX index birthDate, index ))
                in
                distributeByListIndex syntheticValues
                    |> Dict.keys
                    |> List.sort
                    |> Expect.equal (List.range 0 24)
        , test "the old reference-date bucketing collides for a day-31 birth (fewer than 25 distinct buckets)" <|
            \_ ->
                referenceDateBuckets (Date.fromCalendarDate 2024 Time.Jan 31)
                    |> List.Extra.unique
                    |> List.length
                    |> Expect.lessThan 25
        , test "a mid-month (day-15) birth does not collide - buckets are exactly 0..24" <|
            \_ ->
                referenceDateBuckets (Date.fromCalendarDate 2024 Time.Jan 15)
                    |> Expect.equal (List.range 0 24)
        ]
