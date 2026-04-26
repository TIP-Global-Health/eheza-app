module Pages.ScoreboardMenu.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Pages.ScoreboardMenu.Model exposing (Model, Msg(..))


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        SetGeoLocation updatedFunc value ->
            PagesReturn
                { model | selectedDemographics = updatedFunc value model.selectedDemographics }
                Cmd.none
                noError
                []

        SelectionMade ->
            PagesReturn
                { model | selected = True }
                Cmd.none
                noError
                []
