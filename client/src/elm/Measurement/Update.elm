port module Measurement.Update exposing (update, subscriptions)

import Activity.Model
    exposing
        ( ActivityType(..)
        , ChildActivityType(..)
        , ChildNutritionSign(..)
        , FamilyPlanningSign(..)
        , MotherActivityType(FamilyPlanning)
        )
import Config.Model exposing (BackendUrl)
import EveryDict exposing (EveryDict)
import Http
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Json.Encode exposing (Value)
import Measurement.Decoder exposing (decodePhotoFromResponse)
import Measurement.Encoder exposing (encodeFamilyPlanning, encodeHeight, encodeMuac, encodeNutritionSigns, encodePhoto, encodeWeight)
import Measurement.Model exposing (CompletedAndRedirectToActivityTuple, Model, Msg(..))
import Participant.Model exposing (Participant, ParticipantId)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (..)
import Utils.WebData exposing (sendWithHandler)


{-| Optionally, we bubble up two activity types in a tuple, which form to complete and which form is the next one.
-}
update : BackendUrl -> String -> User -> ( ParticipantId, Participant ) -> Msg -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
update backendUrl accessToken user ( participantId, participant ) msg model =
    case msg of
        FamilyPlanningSignsSave ->
            postFamilyPlanning backendUrl accessToken participantId model

        FamilyPlanningSignsToggle sign ->
            let
                signsUpdated =
                    if EveryDict.member sign model.familyPlanningSigns then
                        EveryDict.remove sign model.familyPlanningSigns
                    else
                        case sign of
                            -- 'None of these' checked. Need to empty all selected signs.
                            NoFamilyPlanning ->
                                EveryDict.insert sign () EveryDict.empty

                            -- Another option checked checked. Need to uncheck 'None of these', if it's checked.
                            _ ->
                                EveryDict.insert sign () <| EveryDict.remove NoFamilyPlanning model.familyPlanningSigns
            in
                ( { model | familyPlanningSigns = signsUpdated }
                , Cmd.none
                , Nothing
                )

        HandleDropzoneUploadedFile fileId ->
            ( { model | photo = ( Just fileId, Nothing ) }
            , Cmd.none
            , Nothing
            )

        HandleFamilyPlanningSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| ( Mother FamilyPlanning, Mother FamilyPlanning )
            )

        HandleFamilyPlanningSave (Err err) ->
            let
                _ =
                    Debug.log "HandleFamilyPlanningSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        HandleHeightSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| ( Child Height, Child Muac )
            )

        HandleHeightSave (Err err) ->
            let
                _ =
                    Debug.log "HandleHeightSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        HandleNutritionSignsSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| ( Child NutritionSigns, Child ChildPicture )
            )

        HandleNutritionSignsSave (Err err) ->
            let
                _ =
                    Debug.log "HandleNutritionSignsSave (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        HandleMuacSave (Ok ()) ->
            ( { model | status = Success () }
            , Cmd.none
            , Just <| ( Child Muac, Child NutritionSigns )
            )

        HandleMuacSave (Err err) ->
            let
                _ =
                    Debug.log "HandleMuacSave (Err)" False
            in
                ( { model | status = Failure err }
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
            postMuac backendUrl accessToken participantId model

        NutritionSignsSave ->
            postNutritionSigns backendUrl accessToken participantId model

        NutritionSignsToggle sign ->
            let
                nutritionSignsUpdated =
                    if EveryDict.member sign model.nutritionSigns then
                        EveryDict.remove sign model.nutritionSigns
                    else
                        case sign of
                            -- 'None of these' checked. Need to empty all selected signs.
                            None ->
                                EveryDict.insert sign () EveryDict.empty

                            -- Another option checked checked. Need to uncheck 'None of these', if it's checked.
                            _ ->
                                EveryDict.insert sign () <| EveryDict.remove None model.nutritionSigns
            in
                ( { model | nutritionSigns = nutritionSignsUpdated }
                , Cmd.none
                , Nothing
                )

        PhotoSave ->
            postPhoto backendUrl accessToken participantId model

        ResetDropZone ->
            ( model, dropzoneReset (), Nothing )

        WeightSave ->
            postWeight backendUrl accessToken participantId model

        HeightSave ->
            postHeight backendUrl accessToken participantId model

        WeightUpdate val ->
            ( { model | weight = Just val }, Cmd.none, Nothing )


{-| Send new family planning of a mother to the backend.
-}
postFamilyPlanning : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postFamilyPlanning backendUrl accessToken motherId model =
    if EveryDict.isEmpty model.familyPlanningSigns then
        ( model, Cmd.none, Nothing )
    else
        postData
            backendUrl
            accessToken
            model
            "family-plannings"
            model.familyPlanningSigns
            (encodeFamilyPlanning motherId)
            HandleFamilyPlanningSave


{-| Send new nutrition signs of a child to the backend.
-}
postNutritionSigns : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postNutritionSigns backendUrl accessToken childId model =
    if EveryDict.isEmpty model.nutritionSigns then
        ( model, Cmd.none, Nothing )
    else
        postData
            backendUrl
            accessToken
            model
            "nutritions"
            model.nutritionSigns
            (encodeNutritionSigns childId)
            HandleNutritionSignsSave


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
postPhoto : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
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
postWeight : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
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


{-| Send new height of a child to the backend.
-}
postHeight : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postHeight backendUrl accessToken childId model =
    Maybe.map
        (\height ->
            postData
                backendUrl
                accessToken
                model
                "heights"
                height
                (encodeHeight childId)
                HandleHeightSave
        )
        model.height
        |> Maybe.withDefault ( model, Cmd.none, Nothing )


{-| Send new MUAC of a child to the backend.
-}
postMuac : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
postMuac backendUrl accessToken childId model =
    Maybe.map
        (\muac ->
            postData
                backendUrl
                accessToken
                model
                "muacs"
                muac
                (encodeMuac childId)
                HandleMuacSave
        )
        model.muac
        |> Maybe.withDefault ( model, Cmd.none, Nothing )


subscriptions : Model -> Sub Msg
subscriptions model =
    dropzoneUploadedFile HandleDropzoneUploadedFile


{-| Get a singal if a file has been uploaded via the Dropzone.
-}
port dropzoneUploadedFile : (Int -> msg) -> Sub msg


{-| Cause the drop zone widget to clear it's image
-}
port dropzoneReset : () -> Cmd msg
