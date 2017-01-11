module Pages.Patients.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Patients.Model exposing (Model, Msg(..))
import Patient.Model exposing (PatientsDict)


update : BackendUrl -> String -> User -> Msg -> PatientsDict -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg patients model =
    case msg of
        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            , Nothing
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            , Nothing
            )
