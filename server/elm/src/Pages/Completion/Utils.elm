module Pages.Completion.Utils exposing (..)

import Pages.Completion.Model exposing (ReportType(..))


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportNutritionIndividual ->
            "nutrition-individual"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "nutrition-individual" ->
            Just ReportNutritionIndividual

        _ ->
            Nothing
