module Pages.Reports.Utils exposing (..)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Reports.Model exposing (..)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (..)


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportDemographics ->
            "demographics"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "demographics" ->
            Just ReportDemographics

        _ ->
            Nothing
