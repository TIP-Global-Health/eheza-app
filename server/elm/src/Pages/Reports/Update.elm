module Pages.Reports.Update exposing (update)

import App.Model exposing (PagesReturn)
import Error.Utils exposing (noError)
import Maybe.Extra
import Pages.Reports.Model exposing (Model, Msg(..))
import Pages.Reports.Utils exposing (reportTypeFromString)


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        SetReportType value ->
            PagesReturn
                { model | reportType = reportTypeFromString value }
                Cmd.none
                noError
                []

        SetLimitDate value ->
            PagesReturn
                { model | limitDate = Just value }
                Cmd.none
                noError
                []

        SetLimitDateSelectorState state ->
            let
                defaultSelection =
                    Maybe.Extra.or model.limitDate (Maybe.andThen .dateDefault state)
            in
            PagesReturn
                { model | dateSelectorPopupState = state, limitDate = defaultSelection }
                Cmd.none
                noError
                []
