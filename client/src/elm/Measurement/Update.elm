port module Measurement.Update exposing (update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import EveryDict exposing (EveryDict)
import Http
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Json.Encode exposing (Value)
import Measurement.Decoder exposing (decodePhotoFromResponse)
import Measurement.Encoder exposing (encodePhoto, encodeWeight)
import Measurement.Model exposing (CompletedAndRedirectToActivityTuple, Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)
import Utils.WebData exposing (sendWithHandler)


{-| Optionally, we bubble up two activity types in a tuple, which form to complete and which form is the next one.
-}
update : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Msg -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
update backendUrl accessToken user ( patientId, patient ) msg model =
    case msg of
        HandleDropzoneUploadedFile fileId ->
            ( { model | photo = ( Just fileId, Nothing ) }
            , Cmd.none
            , Nothing
            )

        HandlePhotoSave (Ok ( photoId, photo )) ->
            ( { model
                | status = Success ()
                , photo = ( Tuple.first model.photo, Just ( photoId, photo ) )
              }
            , Cmd.none
            , Just <| ( Child ChildPicture, Child Weight )
            )

        HandlePhotoSave (Err err) ->
            let
                _ =
                    Debug.log "HandlePhotoSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        HandleWeightSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| ( Child Weight, Child Height )
            )

        HandleWeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleWeightSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        HeightUpdate val ->
            ( { model | height = Just val }, Cmd.none, Nothing )

        MuacUpdate val ->
            ( { model | muac = Just val }, Cmd.none, Nothing )

        MuacSave ->
            ( model
            , Cmd.none
            , Just <| ( Child Muac, Child NutritionSigns )
            )

        NutritionSignsSave ->
            ( model
            , Cmd.none
            , Just <| ( Child NutritionSigns, Child ChildPicture )
            )

        NutritionSignsToggle nutritionSign ->
            let
                nutritionSignsUpdated =
                    if EveryDict.member nutritionSign model.nutritionSigns then
                        EveryDict.remove nutritionSign model.nutritionSigns
                    else
                        EveryDict.insert nutritionSign () model.nutritionSigns
            in
                ( { model | nutritionSigns = nutritionSignsUpdated }
                , Cmd.none
                , Nothing
                )

        PhotoSave ->
            postPhoto backendUrl accessToken patientId model

        WeightSave ->
            postWeight backendUrl accessToken patientId model

        HeightSave ->
            ( model
            , Cmd.none
            , Just <| ( Child Height, Child Muac )
            )

        WeightUpdate val ->
            ( { model | weight = Just val }, Cmd.none, Nothing )


{-| Enables posting of arbitrary values to the provided back end so long as the encoder matches the desired type
-}
postData : BackendUrl -> String -> Model -> String -> value -> (value -> Value) -> (Result Http.Error () -> Msg) -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postData backendUrl accessToken model path value encoder handler =
    let
        command =
            HttpBuilder.post (backendUrl ++ "/api/" ++ path)
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (encoder value)
                |> send handler
    in
        ( { model | status = Loading }
        , command
        , Nothing
        )


{-| Send new photo of a child to the backend.
-}
postPhoto : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postPhoto backendUrl accessToken childId model =
    case model.photo of
        ( Nothing, _ ) ->
            -- This shouldn't happen, but in case we don't have a file ID, we won't issue
            -- a POST request.
            ( model, Cmd.none, Nothing )

        ( Just fileId, _ ) ->
            let
                command =
                    HttpBuilder.post (backendUrl ++ "/api/photos")
                        |> withQueryParams [ ( "access_token", accessToken ) ]
                        |> withJsonBody (encodePhoto childId fileId)
                        |> sendWithHandler decodePhotoFromResponse HandlePhotoSave
            in
                ( { model | status = Loading }
                , command
                , Nothing
                )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postWeight backendUrl accessToken childId model =
    Maybe.map
        (\weight ->
            postData
                backendUrl
                accessToken
                model
                "weights"
                weight
                (encodeWeight childId)
                HandleWeightSave
        )
        model.weight
        |> Maybe.withDefault ( model, Cmd.none, Nothing )


subscriptions : Model -> Sub Msg
subscriptions model =
    dropzoneUploadedFile HandleDropzoneUploadedFile


{-| Get a singal if a file has been uploaded via the Dropzone.
-}
port dropzoneUploadedFile : (Int -> msg) -> Sub msg
