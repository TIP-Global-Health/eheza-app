module Pages.Scoreboard.Update exposing (update)

import App.Model exposing (PagesReturn)
import Backend.Model exposing (ModelBackend)
import Error.Utils exposing (noError)
import Pages.Scoreboard.Model exposing (Model, Msg(..))
import Pages.Scoreboard.Utils exposing (..)


update : ModelBackend -> Msg -> Model -> PagesReturn Model Msg
update modelBackend msg model =
    case msg of
        ChaneYearGap step ->
            PagesReturn
                { model | yearSelectorGap = model.yearSelectorGap + step }
                Cmd.none
                noError
                []
