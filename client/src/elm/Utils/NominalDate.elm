module Utils.NominalDate exposing (..)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date exposing (Date)
import Time.Date exposing (year, month, day)
import Gizra.NominalDate exposing (NominalDate)
import Date.Extra exposing (fromParts, diff, Interval(Day))
import Date.Extra.Facts exposing (monthFromMonthNumber)


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

`Time.Date.delta` is awkward for this purpose, because it specifies years
and months without giving you a chance to know how many days are in those
years or months.

The result is positive if the second parameter is after the first parameter.

-}
diffDays : NominalDate -> NominalDate -> Int
diffDays a b =
    diff Day (toLocalDateTime a 12 0 0 0) (toLocalDateTime b 12 0 0 0)
