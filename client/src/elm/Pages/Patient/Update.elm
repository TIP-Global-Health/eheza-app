module Pages.Patient.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Patient.Model exposing (Msg(..))
import Pusher.Model exposing (PusherEventData(..))
import Patient.Model exposing (Patient)


update : BackendUrl -> String -> User -> Msg -> Patient -> ( Patient, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg patient =
    case msg of
        HandlePusherEventData event ->
            case event of
                PatientUpdate newPatient ->
                    -- So, the idea is that we have a new or updated patient,
                    -- which has already been saved at the server. Note that
                    -- we may have just pushed this change ourselves, so it's
                    -- already reflected here.
                    ( newPatient
                    , Cmd.none
                    , Nothing
                    )

        SetRedirectPage page ->
            ( patient, Cmd.none, Just page )
