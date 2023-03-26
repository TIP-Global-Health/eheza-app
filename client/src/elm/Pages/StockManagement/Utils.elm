module Pages.StockManagement.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Fbf)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Pages.StockManagement.Model exposing (CorrectionEntryType(..), MonthYear)


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


dateToMonthYear : NominalDate -> MonthYear
dateToMonthYear date =
    ( Date.monthNumber date
    , Date.year date
        |> modBy 1000
    )


compareMonthYear : MonthYear -> MonthYear -> Order
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


monthYearDiff : MonthYear -> MonthYear -> Int
monthYearDiff ( m1, y1 ) ( m2, y2 ) =
    if compareMonthYear ( m1, y1 ) ( m2, y2 ) == LT then
        monthYearDiff ( m2, y2 ) ( m1, y1 )

    else if y1 == y2 then
        m1 - m2

    else
        (y1 - y2) * 12 + m1 - m2


getPrevMonthYear : MonthYear -> MonthYear
getPrevMonthYear ( month, year ) =
    if month > 1 then
        ( month - 1, year )

    else
        ( 12, year - 1 )
