module Utils.NominalDate exposing (Days(..), Months(..), diffDays, diffMonths, endField, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate, startField)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date
import Form.Field exposing (Field)
import Form.Init exposing (setGroup, setString)
import Form.Validate as Validate exposing (Validation, field)
import Gizra.NominalDate exposing (NominalDate, NominalDateRange, diffCalendarMonthsAndDays)
import Html exposing (Html)
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


diffMonths : NominalDate -> NominalDate -> Months
diffMonths low high =
    Gizra.NominalDate.diffCalendarMonths low high
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
                    Debug.toString days
                        ++ " "
                        ++ translate language Translate.Days

        monthPart =
            if months == 0 then
                Nothing

            else
                Just <|
                    Debug.toString months
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
                    Debug.toString days
                        ++ " "
                        ++ translate language Translate.Days

        monthPart =
            if months == 0 then
                Nothing

            else
                Just <|
                    Debug.toString months
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
            Date.day date

        month =
            Date.month date
                |> Translate.ResolveMonth
                |> translate language

        year =
            Date.year date
    in
    (if day < 10 then
        "0" ++ String.fromInt day

     else
        String.fromInt day
    )
        ++ " "
        ++ month
        ++ " "
        ++ String.fromInt year


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
