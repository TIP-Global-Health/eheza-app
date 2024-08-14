module Utils.NominalDate exposing (..)

{-| An extra utility for elm-community/elm-time ... should integrate with
Gizra.NominalDate.
-}

import Date
import Gizra.NominalDate exposing (NominalDate)


equalByYearAndMonth : NominalDate -> NominalDate -> Bool
equalByYearAndMonth first second =
    (Date.year first == Date.year second)
        && (Date.month first == Date.month second)
