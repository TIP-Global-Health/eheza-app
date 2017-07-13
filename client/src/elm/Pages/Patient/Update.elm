module Pages.Patient.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Measurement.Model
import Measurement.Update
import Pages.Patient.Model exposing (Msg(..))
import Pusher.Model exposing (PusherEventData(..))
import Patient.Model exposing (Patient, PatientId)


type alias Measurements =
    Measurement.Model.Model


update : BackendUrl -> String -> User -> Msg -> ( PatientId, Patient ) -> Measurements -> ( Patient, Measurements, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg patient measurements =
    case msg of
        HandlePusherEventData event ->
            case event of
                PatientUpdate newPatient ->
                    -- So, the idea is that we have a new or updated patient,
                    -- which has already been saved at the server. Note that
                    -- we may have just pushed this change ourselves, so it's
                    -- already reflected here.
                    ( newPatient
                    , measurements
                    , Cmd.none
                    , Nothing
                    )

        MsgMeasurement subMsg ->
            let
                ( measurementsUpdated, cmds ) =
                    Measurement.Update.update backendUrl accessToken user ( patientId, patient ) subMsg measurements
            in
                ( patient
                , measurementsUpdated
                , Cmd.map MsgMeasurement cmds
                , Nothing
                )

        SetRedirectPage page ->
            ( patient, Cmd.none, Just page )
