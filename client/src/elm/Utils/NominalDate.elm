module Utils.NominalDate exposing (Days(..), Months(..), diffDays, diffMonths, endField, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate, setNominalDateRange, startField, validateNominalDate, validateNominalDateRange)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date.Extra exposing (Interval(..), diff, fromParts, numberToMonth)
import Form.Field exposing (Field)
import Form.Init exposing (setGroup, setString)
import Form.Validate as Validate exposing (Validation, field)
import Gizra.NominalDate exposing (NominalDate, NominalDateRange, diffCalendarMonthsAndDays, formatYYYYMMDD, fromLocalDateTime, toLocalDateTime)
import Html exposing (Html)
import Time.Date exposing (date, day, daysInMonth, delta, month, year)
import Translate exposing (Language, translate)


{-| A wrapper for an integer representing days.
-}
type Days
    = Days Int


{-| A wrapper for an integer representing months.
-}
type Months
    = Months Int


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
    Gizra.NominalDate.diffDays low high
        |> Days


{-| Like `diffDays`, but shows whole completed months.
-}
diffMonths : NominalDate -> NominalDate -> Months
diffMonths low high =
    delta high low
        |> .months
        |> Months


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
    if days == 1 && months == 0 then
        translate language <| Translate.AgeSingleDayWithoutMonth months days

    else if months == 0 then
        translate language <| Translate.AgeDays days

    else if months == 1 && days == 0 then
        translate language <| Translate.AgeSingleMonthWithoutDay months

    else if months > 1 && days == 0 then
        translate language <| Translate.AgeMonthsWithoutDay months

    else if months == 1 && days == 1 then
        translate language <| Translate.AgeSingleBoth months days

    else if days == 1 then
        translate language <| Translate.AgeSingleDayWithMonth months days

    else if months == 1 && days /= 0 then
        translate language <| Translate.AgeSingleMonth months days

    else
        translate language <| Translate.Age months days


renderAgeMonthsDaysAbbrev : Language -> NominalDate -> NominalDate -> String
renderAgeMonthsDaysAbbrev language birthDate now =
    let
        diff =
            diffCalendarMonthsAndDays birthDate now

        days =
            diff.days

        months =
            diff.months

        dayPart =
            if days == 0 then
                Nothing

            else if days == 1 then
                Just <|
                    "1 "
                        ++ translate language Translate.Day

            else
                Just <|
                    toString days
                        ++ " "
                        ++ translate language Translate.Days

        monthPart =
            if months == 0 then
                Nothing

            else
                Just <|
                    toString months
                        ++ " "
                        ++ translate language Translate.MonthAbbrev
    in
    [ monthPart, dayPart ]
        |> List.filterMap identity
        |> String.join " "


renderAgeMonthsDaysHtml : Language -> NominalDate -> NominalDate -> List (Html any)
renderAgeMonthsDaysHtml language birthDate now =
    let
        diff =
            diffCalendarMonthsAndDays birthDate now

        days =
            diff.days

        months =
            diff.months

        dayPart =
            if days == 0 then
                Nothing

            else if days == 1 then
                Just <|
                    "1 "
                        ++ translate language Translate.Day

            else
                Just <|
                    toString days
                        ++ " "
                        ++ translate language Translate.Days

        monthPart =
            if months == 0 then
                Nothing

            else
                Just <|
                    toString months
                        ++ " "
                        ++ translate language Translate.MonthAbbrev
    in
    [ monthPart, dayPart ]
        |> List.filterMap identity
        |> List.map Html.text
        |> List.intersperse (Html.br [] [])


renderDate : Language -> NominalDate -> String
renderDate language date =
    let
        day =
            Time.Date.day date

        month =
            Time.Date.month date
                |> numberToMonth
                |> Translate.ResolveMonth
                |> translate language

        year =
            Time.Date.year date
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


{-| Validates a NominalDate.
-}
validateNominalDate : Validation e NominalDate
validateNominalDate =
    -- It might be nice to do something more predictable than what `date` does,
    -- but it's certainly convenient.
    Validate.map fromLocalDateTime Validate.date


{-| Validates a `NominalDateRange`, on the assumption that it is represented
by a field group, which has the sub-fields `start` and `end`.
-}
validateNominalDateRange : Validation e NominalDateRange
validateNominalDateRange =
    Validate.succeed NominalDateRange
        |> Validate.andMap (field startField validateNominalDate)
        |> Validate.andMap (field endField validateNominalDate)


{-| The name of the field we use for the start date.
-}
startField : String
startField =
    "start"


{-| The name of the field we use for the end date.
-}
endField : String
endField =
    "end"


{-| For an initial grouped field. The string parameter is your name for the
grouped field ... we'll supply the `start` and `end` field names for the inner
fields.
-}
setNominalDateRange : String -> NominalDateRange -> ( String, Field )
setNominalDateRange fieldName range =
    setGroup fieldName
        -- I suppose this ought to be locale-dependent in some way, but we'll
        -- just start with YYYY-MM-DD
        [ setString startField (formatYYYYMMDD range.start)
        , setString endField (formatYYYYMMDD range.end)
        ]
