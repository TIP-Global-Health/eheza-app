module Pages.Completion.Utils exposing (..)

import Backend.Completion.Model exposing (TakenBy(..))
import Pages.Completion.Model exposing (ReportType(..))


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportNutritionIndividual ->
            "nutrition-individual"

        ReportNutritionGroup ->
            "nutrition-group"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "nutrition-individual" ->
            Just ReportNutritionIndividual

        "nutrition-group" ->
            Just ReportNutritionGroup

        _ ->
            Nothing
