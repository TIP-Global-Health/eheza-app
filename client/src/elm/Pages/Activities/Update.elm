module Pages.Activities.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Activities.Model exposing (Msg(..))
import Patient.Model exposing (PatientsDict)


update : BackendUrl -> String -> User -> Msg -> PatientsDict -> ( Cmd Msg, Maybe Page )
update backendUrl accessToken user msg patients =
    case msg of
        SetRedirectPage page ->
            ( Cmd.none, Just page )
