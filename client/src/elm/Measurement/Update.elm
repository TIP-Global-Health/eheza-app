port module Measurement.Update exposing (update, subscriptions)

import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Measurement.Encoder exposing (encodePhoto, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandleDropzoneUploadedFile fileId ->
            { model | photo = fileId } ! []

        HandlePhotoSave (Ok ()) ->
            { model | status = Success () } ! []

        HandlePhotoSave (Err err) ->
            let
                _ =
                    Debug.log "HandlePhotoSave (Err)" False
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

        MuacSave ->
            model ! []

        NutritionSignsSave ->
            model ! []

        PhotoSave ->
            postPhoto backendUrl accessToken patientId model

        WeightSave ->
            postWeight backendUrl accessToken patientId model

        HeightSave ->
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


{-| Send new photo of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg )
postPhoto backendUrl accessToken childId model =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/photos")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodePhoto childId model.photo)
                |> send HandlePhotoSave
    in
        ( { model | status = Loading }
        , command
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    dropzoneUploadedFile HandleDropzoneUploadedFile


{-| Get a singal if a file has been uploaded via the Dropzone.
-}
port dropzoneUploadedFile : (Int -> msg) -> Sub msg
