port module Pages.Patient.Update exposing (update)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(ChildPicture))
import App.Model exposing (DropzoneConfig)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Measurement.Update
import Pages.Patient.Model exposing (Model, Msg(..))
import Pusher.Model exposing (PusherEventData(..))
import Patient.Model exposing (Patient, PatientId)


update : BackendUrl -> String -> User -> Msg -> ( PatientId, Patient ) -> Model -> ( Patient, Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg ( patientId, patient ) model =
    case msg of
        HandlePusherEventData event ->
            case event of
                PatientUpdate newPatient ->
                    -- So, the idea is that we have a new or updated patient,
                    -- which has already been saved at the server. Note that
                    -- we may have just pushed this change ourselves, so it's
                    -- already reflected here.
                    ( newPatient
                    , model
                    , Cmd.none
                    , Nothing
                    )

        MsgMeasurement subMsg ->
            let
                ( measurementsUpdated, cmds ) =
                    Measurement.Update.update backendUrl accessToken user ( patientId, patient ) subMsg model.measurements
            in
                ( patient
                , { model | measurements = measurementsUpdated }
                , Cmd.map MsgMeasurement cmds
                , Nothing
                )

        SetRedirectPage page ->
            ( patient, model, Cmd.none, Just page )

        SetSelectedActivity maybectivityType ->
            ( patient
            , { model | selectedActivity = maybectivityType }
            , setDropzone backendUrl maybectivityType
            , Nothing
            )

        SetSelectedTab tab ->
            ( patient
            , { model | selectedTab = tab }
            , Cmd.none
            , Nothing
            )


{-| Activate the dropzone on a specific activity type.
-}
setDropzone : String -> Maybe ActivityType -> Cmd Msg
setDropzone backendUrl activity =
    let
        isActive =
            case activity of
                Just (Child ChildPicture) ->
                    True

                _ ->
                    False

        config =
            { backendUrl = backendUrl
            , active = isActive
            }
    in
        dropzoneConfig config


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MsgMeasurement <| Measurement.Update.subscriptions model.measurements


{-| Send dropzone config to JS.
-}
port dropzoneConfig : DropzoneConfig -> Cmd msg
