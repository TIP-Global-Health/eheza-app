module Pages.ReportsMenu.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Pages.Components.Utils exposing (populationSelectionOptionFromString)
import Pages.ReportsMenu.Model exposing (..)


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        SetPopulationSelection value ->
            PagesReturn
                { model | populationSelection = populationSelectionOptionFromString value }
                Cmd.none
                noError
                []

        SetGeoLocation updatedFunc value ->
            PagesReturn
                { model | selectedDemographics = updatedFunc value model.selectedDemographics }
                Cmd.none
                noError
                []

        SetHealthCenter value ->
            PagesReturn
                { model | selectedHealthCenter = String.toInt value }
                Cmd.none
                noError
                []

        SelectionMade ->
            PagesReturn
                { model | selected = True }
                Cmd.none
                noError
                []
