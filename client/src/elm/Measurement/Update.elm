module Measurement.Update exposing (update)

import Child.Model exposing (ChildId)
import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, withJsonBody, send, withQueryParams)
import Measurement.Encoder exposing (encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandleWeightSave (Ok ()) ->
            { model | status = Success () } ! []

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                { model | status = Failure err } ! []

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                { model | height = updatedHeight } ! []

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                { model | muac = updatedMuac } ! []

        WeightSave ->
            model ! []

        WeightUpdate val ->
            let
                weight =
                    model.weight

                updatedWeight =
                    { weight | value = val }
            in
                { model | weight = updatedWeight } ! []


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> ChildId -> Model -> ( Model, Cmd Msg )
postWeight backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/weights")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeWeight childId model.weight.value)
                |> send HandleWeightSave
    in
        ( model
        , command
        )
