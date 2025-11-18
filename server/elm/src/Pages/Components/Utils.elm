module Pages.Components.Utils exposing (..)

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


syncStatusAndProgress : List a -> Maybe Int -> ( String, String )
syncStatusAndProgress records =
    Maybe.map
        (\remainingForDownload ->
            let
                totalDownloaded =
                    List.length records
            in
            ( if remainingForDownload == 0 then
                "COMPLETED"

              else
                "IN PROCESS"
            , String.fromInt totalDownloaded ++ " / " ++ String.fromInt (totalDownloaded + remainingForDownload)
            )
        )
        >> Maybe.withDefault ( "PENDING", "0 / 0" )
