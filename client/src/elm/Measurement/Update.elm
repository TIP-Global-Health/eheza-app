module Measurement.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
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
                ( { model | height = updatedHeight }, Cmd.none, Nothing )

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                ( { model | muac = updatedMuac }, Cmd.none, Nothing )

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
                ( { model | weight = updatedWeight }, Cmd.none, Nothing )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
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
        , Nothing
        )


{-| Send new weight of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe ActivityType )
postPhoto backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/photos")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodePhoto childId model.photo.value)
                |> send HandlePhotoSave
    in
        ( { model | status = Loading }
        , command
        , Nothing
        )
