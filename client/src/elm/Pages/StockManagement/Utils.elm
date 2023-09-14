module Pages.StockManagement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Fbf)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Date
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Pages.StockManagement.Model exposing (AssembledData, CorrectionEntryType(..), MonthYear)
import RemoteData exposing (RemoteData(..))
import Utils.NominalDate exposing (sortByDate)


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
