module Utils.NominalDate exposing (..)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date exposing (Date)
import Time.Date exposing (year, month, day, delta, daysInMonth)
import Gizra.NominalDate exposing (NominalDate)
import Date.Extra exposing (fromParts, diff, Interval(Day))
import Date.Extra.Facts exposing (monthFromMonthNumber)
import Translate exposing (translate, Language)


{-| A wrapper for an integer representing days.
-}
type Days
    = Days Int


type Months
    = Months Int


{-| Converts a `NominalDate` to an Elm-core `Date`, with the supplied values
for hour, minute, second and milliseconds (in that order).

Uses `Date.Extra.fromParts` to do the conversion.

-}
toLocalDateTime : NominalDate -> Int -> Int -> Int -> Int -> Date
toLocalDateTime nominal hour minutes seconds milliseconds =
    fromParts
        (year nominal)
        (monthFromMonthNumber <| month nominal)
        (day nominal)
        hour
        minutes
        seconds
        milliseconds


{-| Difference in whole days between two dates.

The result is positive if the second parameter is after the first parameter.

-}
diffDays : NominalDate -> NominalDate -> Days
diffDays low high =
    -- delta gives us separate deltas for years, months and days ... so, for
    -- instance, for a difference of 2 years and 1 month, you'd get
    --
    -- { years : 2
    -- , months: 25
    -- , days: 760 -- roughly, depending on which months are involved
    -- }
    delta high low
        |> .days
        |> Days


{-| Like `diffDays`, but shows whole completed months.
-}
diffMonths : NominalDate -> NominalDate -> Months
diffMonths low high =
    delta high low
        |> .months
        |> Months


{-| Difference between two dates, in terms of months and days. This is based on
calendar months. So, if you're on the same day of the next month, you'd get
{ months : 1, days: 0 }. Now, you can't tell from this how many actual deys
it is, because the month might have 28, 30 or 31 days. So, if you need the
actual days, use `diffDays` instead.

The result will be positive if the second parameter is after the first parameter.

-}
diffCalendarMonthsAndDays : NominalDate -> NominalDate -> { months : Int, days : Int }
diffCalendarMonthsAndDays low high =
    let
        uncorrected =
            { days = day high - day low
            , months = (year high * 12 + month high) - (year low * 12 + month low)
            }
    in
        if uncorrected.days >= 0 then
            -- This is the easy case ... we're at the same day (or further
            -- along) in the target month than the original month, so we're
            -- done ... the answer is some number of full months (however
            -- long they were) and some number of additional days.
            uncorrected
        else
            -- This is the harder case. We're not as far along in our target
            -- month as we were in the original month. So, we need to subtract
            -- 1 from our months, and add something to the (negative) days.
            --
            -- Basically, we want to add however many days there were in the
            -- original month. We're "borrowing" that number of days, to use
            -- the language of subtraction-by-hand. And, it's the original
            -- month that is the "partial" month we're borrowing from ... all
            -- intervening months are full months, and the current month isn't
            -- finished, so it can't matter how many days it has.
            { months = uncorrected.months - 1
            , days = uncorrected.days + (Maybe.withDefault 0 (daysInMonth (year low) (month low)))
            }


{-| Shows the difference between the first date (the birthdate)
and the second date, formatted in months and days.
-}
renderAgeMonthsDays : Language -> NominalDate -> NominalDate -> String
renderAgeMonthsDays language birthDate now =
    let
        diff =
            diffCalendarMonthsAndDays birthDate now

        days =
            diff.days

        months =
            diff.months
    in
        if (days == 1 && months == 0) then
            translate language <| Translate.AgeSingleDayWithoutMonth months days
        else if (months == 0) then
            translate language <| Translate.AgeDays days
        else if (months == 1 && days == 0) then
            translate language <| Translate.AgeSingleMonthWithoutDay months
        else if (months > 1 && days == 0) then
            translate language <| Translate.AgeMonthsWithoutDay months
        else if (months == 1 && days == 1) then
            translate language <| Translate.AgeSingleBoth months days
        else if (days == 1) then
            translate language <| Translate.AgeSingleDayWithMonth months days
        else if (months == 1 && days /= 0) then
            translate language <| Translate.AgeSingleMonth months days
        else
            translate language <| Translate.Age months days


renderDateOfBirth : Language -> NominalDate -> String
renderDateOfBirth language birthDate =
    let
        day =
            Time.Date.day birthDate

        month =
            Time.Date.month birthDate
                |> monthFromMonthNumber
                |> Translate.ResolveMonth
                |> translate language

        year =
            Time.Date.year birthDate
    in
        (if day < 10 then
            "0" ++ toString day
         else
            toString day
        )
            ++ " "
            ++ month
            ++ " "
            ++ toString year
