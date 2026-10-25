module Pages.Completion.Update exposing (update)

import App.Model exposing (PagesReturn)
import Backend.Completion.Utils exposing (takenByFromString)
import Error.Utils exposing (noError)
import Pages.Completion.Model exposing (Model, Msg(..))
import Pages.Completion.Utils exposing (reportTypeFromString)
import Pages.Utils exposing (dateSelectorDefault)


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
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
            PagesReturn
                { model | startDateSelectorPopupState = state, startDate = dateSelectorDefault model.startDate state }
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
            PagesReturn
                { model | limitDateSelectorPopupState = state, limitDate = dateSelectorDefault model.limitDate state }
                Cmd.none
                noError
                []
