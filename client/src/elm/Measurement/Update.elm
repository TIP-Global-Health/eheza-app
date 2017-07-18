module Measurement.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Json.Encode exposing (Value)
import Measurement.Encoder exposing (encodeHeight, encodeMuac, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandleHeightSave (Ok ()) ->
            ( { model | status = Success () }, Cmd.none, Just (Child Height) )

        HandleHeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleHeightSave (Err)" False
            in
                ( { model | status = Failure err }, Cmd.none, Nothing )

        HandleMuacSave (Ok ()) ->
            ( { model | status = Success () }, Cmd.none, Just (Child Muac) )

        HandleMuacSave (Err err) ->
            let
                _ =
                    Debug.log "HandleMuacSave (Err)" False
            in
                ( { model | status = Failure err }, Cmd.none, Nothing )

        HandleWeightSave (Ok ()) ->
            ( { model | status = Success () }, Cmd.none, Just (Child Weight) )

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                ( { model | status = Failure err }, Cmd.none, Nothing )

        HeightSave ->
            postHeight backendUrl accessToken patientId model

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                ( { model | height = updatedHeight }, Cmd.none, Nothing )

        MuacSave ->
            postMuac backendUrl accessToken patientId model

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                ( { model | muac = updatedMuac }, Cmd.none, Nothing )

        WeightSave ->
            postWeight backendUrl accessToken patientId model

        WeightUpdate val ->
            let
                weight =
                    model.weight

                updatedWeight =
                    { weight | value = val }
            in
                ( { model | weight = updatedWeight }, Cmd.none, Nothing )


{-| Enables posting of arbitrary values to the provided back end so long as the encoder matches the desired type
-}
postData : BackendUrl -> String -> PatientId -> Model -> String -> (Model -> value) -> (PatientId -> value -> Value) -> ( Model, Cmd Msg, Maybe ActivityType )
postData backendUrl accessToken childId model path getter encoder =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/" ++ path)
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encoder childId <| getter model)
                |> send HandleHeightSave
    in
        ( { model | status = Loading }
        , command
        , Nothing
        )


{-| Send new height of a child to the backend.
-}
postHeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postHeight backendUrl accessToken childId model =
    postData backendUrl accessToken childId model "heights" (\m -> m.height.value) encodeHeight


{-| Send new muac of a child to the backend.
-}
postMuac : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postMuac backendUrl accessToken childId model =
    postData backendUrl accessToken childId model "muacs" (\m -> m.muac.value) encodeMuac


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postWeight backendUrl accessToken childId model =
    postData backendUrl accessToken childId model "weights" (\m -> m.weight.value) encodeWeight
