module Measurement.Update exposing (update)

import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Measurement.Encoder exposing (encodeHeight, encodeMuac, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandleHeightSave (Ok ()) ->
            { model | status = Success () } ! []

        HandleHeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleHeightSave (Err)" False
            in
                { model | status = Failure err } ! []

        HandleMuacSave (Ok ()) ->
            { model | status = Success () } ! []

        HandleMuacSave (Err err) ->
            let
                _ =
                    Debug.log "HandleMuacSave (Err)" False
            in
                { model | status = Failure err } ! []

        HandleWeightSave (Ok ()) ->
            { model | status = Success () } ! []

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                { model | status = Failure err } ! []

        HeightSave ->
            postHeight backendUrl accessToken patientId model

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                { model | height = updatedHeight } ! []

        MuacSave ->
            postMuac backendUrl accessToken patientId model

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                { model | muac = updatedMuac } ! []

        WeightSave ->
            postWeight backendUrl accessToken patientId model

        WeightUpdate val ->
            let
                weight =
                    model.weight

                updatedWeight =
                    { weight | value = val }
            in
                { model | weight = updatedWeight } ! []


{-| Send new height of a child to the backend.
-}
postHeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg )
postHeight backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/heights")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeHeight childId model.height.value)
                |> send HandleHeightSave
    in
        ( { model | status = Loading }
        , command
        )


{-| Send new muac of a child to the backend.
-}
postMuac : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg )
postMuac backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/muacs")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeMuac childId model.muac.value)
                |> send HandleMuacSave
    in
        ( { model | status = Loading }
        , command
        )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg )
postWeight backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/weights")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeWeight childId model.weight.value)
                |> send HandleWeightSave
    in
        ( { model | status = Loading }
        , command
        )
