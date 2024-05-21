module Pages.ScoreboardMenu.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Pages.ScoreboardMenu.Model exposing (..)


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        SetGeoLocation updatedFunc value ->
            PagesReturn
                (updatedFunc value model)
                Cmd.none
                noError
                []

        SelectionMade ->
            PagesReturn
                { model | selected = True }
                Cmd.none
                noError
                []
