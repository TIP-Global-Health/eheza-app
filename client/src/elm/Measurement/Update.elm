module Measurement.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Activity.Model exposing (ActivityType)
import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Measurement.Encoder exposing (encodePhoto, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> Maybe ActivityType -> ( Model, Cmd Msg, Maybe ActivityType )
update backendUrl accessToken user ( patientId, patient ) msg model currentActivity =
    case msg of
        HandlePhotoSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| Child <| Weight
            )

        HandlePhotoSave (Err err) ->
            let
                _ =
                    Debug.log "HandlePhotoSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , currentActivity
                )

        HandleWeightSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| Child <| Height
            )

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , currentActivity
                )

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                ( { model | height = updatedHeight }
                , Cmd.none
                , currentActivity
                )

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                ( { model | muac = updatedMuac }
                , Cmd.none
                , currentActivity
                )

        NutritionSignsSave ->
            ( model
            , Cmd.none
            , currentActivity
            )

        PhotoSave ->
            postPhoto backendUrl accessToken patientId model currentActivity

        WeightSave ->
            postWeight backendUrl accessToken patientId model currentActivity

        WeightUpdate val ->
            let
                weight =
                    model.weight

                updatedWeight =
                    { weight | value = val }
            in
                ( { model | weight = updatedWeight }
                , Cmd.none
                , currentActivity
                )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> Maybe ActivityType -> ( Model, Cmd Msg, Maybe ActivityType )
postWeight backendUrl accessToken childId model currentActivity =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/weights")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeWeight childId model.weight.value)
                |> send HandleWeightSave
    in
        ( { model | status = Loading }
        , command
        , currentActivity
        )


{-| Send new weight of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> Maybe ActivityType -> ( Model, Cmd Msg, Maybe ActivityType )
postPhoto backendUrl accessToken childId model currentActivity =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/photos")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodePhoto childId model.photo.value)
                |> send HandlePhotoSave
    in
        ( { model | status = Loading }
        , command
        , currentActivity
        )
