module Measurement.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import Http
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Json.Encode exposing (Value)
import Measurement.Encoder exposing (encodePhoto, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


{-| This update section expects an additional activity type to be bubbled up, when appropriate, for completed activities to trigger completion mechanism in parent module
-}
update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandlePhotoSave (Ok ()) ->
            ( { model | status = Success () }, Cmd.none, Just (Child ChildPicture) )

        HandlePhotoSave (Err err) ->
            let
                _ =
                    Debug.log "HandlePhotoSave (Err)" False
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

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                ( { model | height = updatedHeight }, Cmd.none, Just (Child Height) )

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                ( { model | muac = updatedMuac }, Cmd.none, Just (Child Muac) )

        MuacSave ->
            ( model, Cmd.none, Just (Child Muac) )

        NutritionSignsSave ->
            ( model, Cmd.none, Nothing )

        PhotoSave ->
            postPhoto backendUrl accessToken patientId model

        WeightSave ->
            postWeight backendUrl accessToken patientId model

        HeightSave ->
            ( model, Cmd.none, Just (Child Height) )

        WeightUpdate val ->
            let
                weight =
                    model.weight

                updatedWeight =
                    { weight | value = val }
            in
                ( { model | weight = updatedWeight }, Cmd.none, Just (Child Weight) )


{-| Enables posting of arbitrary values to the provided back end so long as the encoder matches the desired type
-}
postData : BackendUrl -> String -> Model -> String -> value -> (value -> Value) -> (Result Http.Error () -> Msg) -> ( Model, Cmd Msg, Maybe ActivityType )
postData backendUrl accessToken model path value encoder handler =
    let
        builder : HttpBuilder.RequestBuilder () -> Cmd Msg
        builder =
            send handler

        commandDef =
            HttpBuilder.post (backendUrl ++ "/api/" ++ path)
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encoder value)

        command =
            builder commandDef
    in
        ( { model | status = Loading }
        , command
        , Nothing
        )


{-| Send new photo of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postPhoto backendUrl accessToken childId model =
    postData backendUrl accessToken model "photos" model.photo.value (encodePhoto childId) HandlePhotoSave


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postWeight backendUrl accessToken childId model =
    postData backendUrl accessToken model "weights" model.weight.value (encodeWeight childId) HandleWeightSave
