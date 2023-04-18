module Pages.StockManagement.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Fbf)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Pages.StockManagement.Model exposing (CorrectionEntryType(..))


correctionEntryTypeToString : CorrectionEntryType -> String
correctionEntryTypeToString value =
    case value of
        EntryAddition ->
            "addition"

        EntrySubstraction ->
            "substraction"


correctionEntryTypeFromString : String -> Maybe CorrectionEntryType
correctionEntryTypeFromString value =
    case value of
        "addition" ->
            Just EntryAddition

        "substraction" ->
            Just EntrySubstraction

        _ ->
            Nothing


dateToMonthYear : NominalDate -> ( Int, Int )
dateToMonthYear date =
    ( Date.monthNumber date
    , Date.year date
        |> modBy 1000
    )


compareMonthYear : ( Int, Int ) -> ( Int, Int ) -> Order
compareMonthYear ( m1, y1 ) ( m2, y2 ) =
    if y1 < y2 then
        LT

    else if y1 > y2 then
        GT

    else if m1 < m2 then
        LT

    else if m1 > m2 then
        GT

    else
        EQ


getPrevMonthYear : ( Int, Int ) -> ( Int, Int )
getPrevMonthYear ( month, year ) =
    if month > 1 then
        ( month - 1, year )

    else
        ( 12, year - 1 )
