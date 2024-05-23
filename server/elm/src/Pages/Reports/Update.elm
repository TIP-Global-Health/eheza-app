module Pages.Reports.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Pages.Reports.Model exposing (Model, Msg(..))


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        SetLimitDate value ->
            PagesReturn
                { model | limitDate = Just value }
                Cmd.none
                noError
                []

        SetLimitDateSelectorState state ->
            PagesReturn
                { model | dateSelectorPopupState = state }
                Cmd.none
                noError
                []
