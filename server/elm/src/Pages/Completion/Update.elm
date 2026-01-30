module Pages.Completion.Update exposing (update)

import App.Model exposing (PagesReturn)
import Backend.Completion.Utils exposing (takenByFromString)
import Error.Utils exposing (noError)
import Maybe.Extra
import Pages.Completion.Model exposing (Model, Msg(..))
import Pages.Completion.Utils exposing (reportTypeFromString)


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        NoOp ->
            PagesReturn
                model
                Cmd.none
                noError
                []

        SetReportType value ->
            PagesReturn
                { model
                    | reportType = reportTypeFromString value
                    , takenBy = Nothing
                    , startDate = Nothing
                    , limitDate = Nothing
                }
                Cmd.none
                noError
                []

        SetTakenBy value ->
            PagesReturn
                { model | takenBy = takenByFromString value }
                Cmd.none
                noError
                []

        SetStartDate value ->
            PagesReturn
                { model | startDate = Just value }
                Cmd.none
                noError
                []

        SetStartDateSelectorState state ->
            let
                defaultSelection =
                    Maybe.Extra.or model.startDate (Maybe.andThen .dateDefault state)
            in
            PagesReturn
                { model | startDateSelectorPopupState = state, startDate = defaultSelection }
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
                { model | limitDateSelectorPopupState = state, limitDate = defaultSelection }
                Cmd.none
                noError
                []
