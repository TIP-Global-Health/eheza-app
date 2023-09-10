module Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths, renderDate)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date
import Gizra.NominalDate exposing (NominalDate, diffCalendarMonthsAndDays, diffCalendarYearsAndMonths)
import Translate exposing (Language, translate)


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


renderAgeYearsMonths : Language -> NominalDate -> NominalDate -> String
renderAgeYearsMonths language birthDate now =
    let
        diff =
            diffCalendarYearsAndMonths birthDate now

        months =
            diff.months

        years =
            diff.years
    in
    case years of
        0 ->
            case months of
                1 ->
                    translate language <| Translate.AgeSingleMonthWithoutDay months

                _ ->
                    translate language <| Translate.AgeMonthsWithoutDay months

        1 ->
            case months of
                0 ->
                    translate language <| Translate.AgeOneYearOld

                1 ->
                    translate language <| Translate.AgeOneYearAndOneMonth

                _ ->
                    translate language <| Translate.AgeOneYearWithMonths months

        _ ->
            case months of
                0 ->
                    translate language <| Translate.YearsOld years

                1 ->
                    translate language <| Translate.AgeYearsWithSingleMonth years months

                _ ->
                    translate language <| Translate.AgeYearsAndMonths years months


renderDate : Language -> NominalDate -> String
renderDate language date =
    let
        day =
            Date.day date

        month =
            translate language <| Translate.ResolveMonth False (Date.month date)

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
