module Pages.Scoreboard.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Pages.Scoreboard.Model exposing (Model, Msg(..))


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        ChaneYearGap step ->
            PagesReturn
                { model | yearSelectorGap = model.yearSelectorGap + step }
                Cmd.none
                noError
                []

        SetViewMode mode ->
            PagesReturn
                { model | viewMode = mode }
                Cmd.none
                noError
                []
