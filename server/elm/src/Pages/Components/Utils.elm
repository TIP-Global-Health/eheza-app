module Pages.Components.Utils exposing (populationSelectionOptionFromString, populationSelectionOptionToString)

import Pages.Components.Types exposing (PopulationSelectionOption(..))


populationSelectionOptionToString : PopulationSelectionOption -> String
populationSelectionOptionToString selectionOption =
    case selectionOption of
        SelectionOptionGlobal ->
            "all"

        SelectionOptionDemographics ->
            "demographics"

        SelectionOptionHealthCenter ->
            "hc"


populationSelectionOptionFromString : String -> Maybe PopulationSelectionOption
populationSelectionOptionFromString selectionOption =
    case selectionOption of
        "all" ->
            Just SelectionOptionGlobal

        "demographics" ->
            Just SelectionOptionDemographics

        "hc" ->
            Just SelectionOptionHealthCenter

        _ ->
            Nothing
