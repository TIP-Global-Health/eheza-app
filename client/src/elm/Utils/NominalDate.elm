module Utils.NominalDate exposing (..)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date
import Form.Field exposing (Field)
import Form.Init exposing (setGroup, setString)
import Form.Validate as Validate exposing (Validation, field)
import Gizra.NominalDate exposing (NominalDate, NominalDateRange, diffCalendarMonthsAndDays, diffCalendarYearsAndMonths)
import Html exposing (Html)
import Maybe.Extra
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
        translate language <| Translate.AgeSingleDayWithoutMonth days

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


renderAgeMonthsDaysParts : Language -> NominalDate -> NominalDate -> List (Maybe String)
renderAgeMonthsDaysParts language birthDate now =
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
                        ++ translate language Translate.DayAbbrev

            else
                Just <|
                    String.fromInt days
                        ++ " "
                        ++ translate language Translate.DaysAbbrev

        monthPart =
            if months == 0 then
                Nothing

            else
                Just <|
                    String.fromInt months
                        ++ " "
                        ++ translate language Translate.MonthAbbrev
    in
    [ monthPart, dayPart ]


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


renderAgeMonthsDaysAbbrev : Language -> NominalDate -> NominalDate -> String
renderAgeMonthsDaysAbbrev language birthDate now =
    renderAgeMonthsDaysParts language birthDate now
        |> Maybe.Extra.values
        |> String.join " "


renderAgeMonthsDaysHtml : Language -> NominalDate -> NominalDate -> List (Html any)
renderAgeMonthsDaysHtml language birthDate now =
    renderAgeMonthsDaysParts language birthDate now
        |> Maybe.Extra.values
        |> List.map Html.text
        |> List.intersperse (Html.br [] [])


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


sortTuplesByDate : ( NominalDate, a ) -> ( NominalDate, a ) -> Order
sortTuplesByDate =
    sortByDate Tuple.first


sortTuplesByDateDesc : ( NominalDate, a ) -> ( NominalDate, a ) -> Order
sortTuplesByDateDesc =
    sortByDateDesc Tuple.first


sortEncounterTuples : ( id, { e | startDate : NominalDate } ) -> ( id, { e | startDate : NominalDate } ) -> Order
sortEncounterTuples =
    sortByDate (Tuple.second >> .startDate)


sortEncounterTuplesDesc : ( id, { e | startDate : NominalDate } ) -> ( id, { e | startDate : NominalDate } ) -> Order
sortEncounterTuplesDesc =
    sortByDateDesc (Tuple.second >> .startDate)


sortByStartDateDesc : { e | startDate : NominalDate } -> { e | startDate : NominalDate } -> Order
sortByStartDateDesc =
    sortByDateDesc .startDate


sortDatesDesc : NominalDate -> NominalDate -> Order
sortDatesDesc =
    sortByDateDesc identity


sortByDate : (a -> NominalDate) -> a -> a -> Order
sortByDate getDateFunc entity1 entity2 =
    Date.compare (getDateFunc entity1) (getDateFunc entity2)


sortByDateDesc : (a -> NominalDate) -> a -> a -> Order
sortByDateDesc getDateFunc entity1 entity2 =
    Date.compare (getDateFunc entity2) (getDateFunc entity1)
