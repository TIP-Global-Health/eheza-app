module Measurement.Update exposing (update)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(Weight))
import Activity.Model exposing (ActivityType)
import Config.Model exposing (BackendUrl)
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Measurement.Encoder exposing (encodePhoto, encodeWeight)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)


update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> Maybe ActivityType -> ( Maybe ActivityType, Model, Cmd Msg )
update backendUrl accessToken user ( patientId, patient ) msg model currentActivity =
    case msg of
        HandlePhotoSave (Ok ()) ->
            ( Just <| Child <| Weight
            , { model | status = Success () }
            , Cmd.none
            )

        HandlePhotoSave (Err err) ->
            let
                _ =
                    Debug.log "HandlePhotoSave (Err)" False
            in
                ( currentActivity
                , { model | status = Failure err }
                , Cmd.none
                )

        HandleWeightSave (Ok ()) ->
            ( Just <| Child <| Height
            , { model | status = Success () }
            , Cmd.none
            )

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                ( currentActivity
                , { model | status = Failure err }
                , Cmd.none
                )

        HeightUpdate val ->
            let
                height =
                    model.height

                updatedHeight =
                    { height | value = val }
            in
                ( currentActivity
                , { model | height = updatedHeight }
                , Cmd.none
                )

        MuacUpdate val ->
            let
                muac =
                    model.muac

                updatedMuac =
                    { muac | value = val }
            in
                ( currentActivity
                , { model | muac = updatedMuac }
                , Cmd.none
                )

        NutritionSignsSave ->
            ( currentActivity
            , model
            , Cmd.none
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
                ( currentActivity
                , { model | weight = updatedWeight }
                , Cmd.none
                )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> Maybe ActivityType -> ( Maybe ActivityType, Model, Cmd Msg )
postWeight backendUrl accessToken childId model currentActivity =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/weights")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodeWeight childId model.weight.value)
                |> send HandleWeightSave
    in
        ( currentActivity
        , { model | status = Loading }
        , command
        )


{-| Send new weight of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> Maybe ActivityType -> ( Maybe ActivityType, Model, Cmd Msg )
postPhoto backendUrl accessToken childId model currentActivity =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/photos")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encodePhoto childId model.photo.value)
                |> send HandlePhotoSave
    in
        ( currentActivity
        , { model | status = Loading }
        , command
        )
