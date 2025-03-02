module Pages.Completion.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import AssocList as Dict exposing (Dict)
import Backend.Completion.Utils exposing (takenByFromString)
import Backend.Model exposing (ModelBackend)
import Date exposing (Interval(..), Unit(..))
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.Completion.Model exposing (..)
import Pages.Completion.Utils exposing (..)
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)


update : NominalDate -> ModelBackend -> Msg -> Model -> PagesReturn Model Msg
update currentDate modelBackend msg model =
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
