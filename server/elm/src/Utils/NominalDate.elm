module Utils.NominalDate exposing (..)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date
import Gizra.NominalDate exposing (NominalDate)


{-| The month a date falls in, as a value that can be compared or used as a
key. Anything that groups dates by month should say so with this, so that the
grouping and the comparison below cannot come to disagree.
-}
calendarMonth : NominalDate -> ( Int, Int )
calendarMonth date =
    ( Date.year date, Date.monthNumber date )


equalByYearAndMonth : NominalDate -> NominalDate -> Bool
equalByYearAndMonth first second =
    calendarMonth first == calendarMonth second
