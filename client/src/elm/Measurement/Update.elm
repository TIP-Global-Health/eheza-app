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
import EverySet exposing (EverySet)
import Examination.Model exposing (Examination(..))
import Examination.Utils exposing (mapExaminationChild, mapExaminationMother, supplyMeasurement)
import Http
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams)
import Json.Encode exposing (Value)
import Measurement.Decoder exposing (decodePhotoFromResponse)
import Measurement.Encoder exposing (encodeFamilyPlanning, encodeHeight, encodeMuac, encodeNutritionSigns, encodePhoto, encodeWeight)
import Measurement.Model
    exposing
        ( CompletedAndRedirectToActivityTuple
        , Model
        , Msg(..)
        , getFloatInputValue
        , normalizeFloatFormInput
        , normalizeFloatInput
        )
import Participant.Model exposing (Participant, ParticipantId)
import RemoteData exposing (RemoteData(..))
import Utils.WebData exposing (sendWithHandler)


{-| Optionally, we bubble up two activity types in a tuple, which form to
complete and which form is the next one.

The strategy used here, for the moment, is that the `model` tracks the UI,
whereas the `examination` tracks the underlying data from the backend. So,
we'll update the `examination` only after data has actually been successfully
saved to the backend. (Ultimately, it will probably be better to use an
`EditableWebData` in the examination for each measurement, but this is
easier to implement for the moment). It is the caller's rsponsibility to
provided the relevant examination, and store it in the relevant structure
when it is returned.

It's possible that we ought to split this up into an `updateMother` and
`updateChild` (so we can specialize the examination), but perhaps not.

-}
update : BackendUrl -> String -> ParticipantId -> Msg -> Model -> Examination -> ( Model, Examination, Cmd Msg, Maybe CompletedAndRedirectToActivityTuple )
update backendUrl accessToken participantId msg model examination =
    let
        -- This processes a common case below, to supply an unchanged
        -- examination and no redirection
        unchangedExaminationNoRedirect ( updatedModel, cmd ) =
            ( updatedModel, examination, cmd, Nothing )
    in
        case msg of
            FamilyPlanningSignsSave ->
                postFamilyPlanning backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            FamilyPlanningSignsToggle sign ->
                let
                    signsUpdated =
                        if EverySet.member sign model.familyPlanningSigns then
                            EverySet.remove sign model.familyPlanningSigns
                        else
                            case sign of
                                -- 'None of these' checked. Need to empty all selected signs.
                                NoFamilyPlanning ->
                                    EverySet.insert sign EverySet.empty

                                -- Another option checked checked. Need to uncheck 'None of these', if it's checked.
                                _ ->
                                    EverySet.insert sign <|
                                        EverySet.remove NoFamilyPlanning model.familyPlanningSigns
                in
                    ( { model | familyPlanningSigns = signsUpdated }
                    , Cmd.none
                    )
                        |> unchangedExaminationNoRedirect

            HandleDropzoneUploadedFile fileId ->
                ( { model | photo = ( Just fileId, Nothing ) }
                , Cmd.none
                )
                    |> unchangedExaminationNoRedirect

            HandleFamilyPlanningSave (Ok ()) ->
                ( { model | status = Success () }
                , examination
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
                    )
                        |> unchangedExaminationNoRedirect

            HandleHeightSave value (Ok ()) ->
                ( { model | status = Success (), height = (normalizeFloatInput model.height) }
                , mapExaminationChild (\ex -> { ex | height = supplyMeasurement value ex.height }) examination
                , Cmd.none
                , Just <| ( Child Height, Child Muac )
                )

            HandleHeightSave value (Err err) ->
                let
                    _ =
                        Debug.log "HandleHeightSave (Err)" False
                in
                    ( { model | status = Failure err }
                    , Cmd.none
                    )
                        |> unchangedExaminationNoRedirect

            HandleNutritionSignsSave (Ok ()) ->
                ( { model | status = Success () }
                , examination
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
                    )
                        |> unchangedExaminationNoRedirect

            HandleMuacSave value (Ok ()) ->
                ( { model | status = Success (), muac = (normalizeFloatInput model.muac) }
                , mapExaminationChild (\ex -> { ex | muac = supplyMeasurement value ex.muac }) examination
                , Cmd.none
                , Just <| ( Child Muac, Child NutritionSigns )
                )

            HandleMuacSave value (Err err) ->
                let
                    _ =
                        Debug.log "HandleMuacSave (Err)" False
                in
                    ( { model | status = Failure err }
                    , Cmd.none
                    )
                        |> unchangedExaminationNoRedirect

            HandlePhotoSave (Ok ( photoId, photo )) ->
                ( { model
                    | status = Success ()
                    , photo = ( Tuple.first model.photo, Just ( photoId, photo ) )
                  }
                , examination
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
                    )
                        |> unchangedExaminationNoRedirect

            HandleWeightSave value (Ok ()) ->
                ( { model | status = Success (), weight = (normalizeFloatInput model.weight) }
                , mapExaminationChild (\ex -> { ex | weight = supplyMeasurement value ex.weight }) examination
                , Cmd.none
                , Just <| ( Child Weight, Child Height )
                )

            HandleWeightSave value (Err err) ->
                let
                    _ =
                        Debug.log "HandleWeightSave (Err)" False
                in
                    ( { model | status = Failure err }
                    , Cmd.none
                    )
                        |> unchangedExaminationNoRedirect

            HeightUpdate val ->
                ( { model | height = Just (normalizeFloatFormInput val) }
                , Cmd.none
                )
                    |> unchangedExaminationNoRedirect

            MuacUpdate val ->
                ( { model | muac = Just (normalizeFloatFormInput val) }
                , Cmd.none
                )
                    |> unchangedExaminationNoRedirect

            MuacSave ->
                postMuac backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            NutritionSignsSave ->
                postNutritionSigns backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            NutritionSignsToggle sign ->
                let
                    nutritionSignsUpdated =
                        if EverySet.member sign model.nutritionSigns then
                            EverySet.remove sign model.nutritionSigns
                        else
                            case sign of
                                -- 'None of these' checked. Need to empty all selected signs.
                                None ->
                                    EverySet.insert sign EverySet.empty

                                -- Another option checked checked. Need to uncheck 'None of these', if it's checked.
                                _ ->
                                    EverySet.insert sign <|
                                        EverySet.remove None model.nutritionSigns
                in
                    ( { model | nutritionSigns = nutritionSignsUpdated }
                    , Cmd.none
                    )
                        |> unchangedExaminationNoRedirect

            PhotoSave ->
                postPhoto backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            ResetDropZone ->
                ( model
                , dropzoneReset ()
                )
                    |> unchangedExaminationNoRedirect

            WeightSave ->
                postWeight backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            HeightSave ->
                postHeight backendUrl accessToken participantId model
                    |> unchangedExaminationNoRedirect

            WeightUpdate val ->
                ( { model | weight = Just (normalizeFloatFormInput val) }
                , Cmd.none
                )
                    |> unchangedExaminationNoRedirect


{-| Send new family planning of a mother to the backend.
-}
postFamilyPlanning : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postFamilyPlanning backendUrl accessToken motherId model =
    if EverySet.isEmpty model.familyPlanningSigns then
        ( model, Cmd.none )
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
postNutritionSigns : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postNutritionSigns backendUrl accessToken childId model =
    if EverySet.isEmpty model.nutritionSigns then
        ( model, Cmd.none )
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
postData : BackendUrl -> String -> Model -> String -> value -> (value -> Value) -> (Result Http.Error () -> Msg) -> ( Model, Cmd Msg )
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
        )


{-| Send new photo of a child to the backend.
-}
postPhoto : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postPhoto backendUrl accessToken childId model =
    case model.photo of
        ( Nothing, _ ) ->
            -- This shouldn't happen, but in case we don't have a file ID, we won't issue
            -- a POST request.
            ( model, Cmd.none )

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
                )


{-| Send new weight of a child to the backend.
-}
postWeight : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postWeight backendUrl accessToken childId model =
    Maybe.map
        (\weight ->
            postData
                backendUrl
                accessToken
                model
                "weights"
                (getFloatInputValue weight)
                (encodeWeight childId)
                (HandleWeightSave (getFloatInputValue weight))
        )
        model.weight
        |> Maybe.withDefault ( model, Cmd.none )


{-| Send new height of a child to the backend.
-}
postHeight : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postHeight backendUrl accessToken childId model =
    Maybe.map
        (\height ->
            postData
                backendUrl
                accessToken
                model
                "heights"
                (getFloatInputValue height)
                (encodeHeight childId)
                (HandleHeightSave (getFloatInputValue height))
        )
        model.height
        |> Maybe.withDefault ( model, Cmd.none )


{-| Send new MUAC of a child to the backend.
-}
postMuac : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
postMuac backendUrl accessToken childId model =
    Maybe.map
        (\muac ->
            postData
                backendUrl
                accessToken
                model
                "muacs"
                (getFloatInputValue muac)
                (encodeMuac childId)
                (HandleMuacSave (getFloatInputValue muac))
        )
        model.muac
        |> Maybe.withDefault ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    dropzoneUploadedFile HandleDropzoneUploadedFile


{-| Get a singal if a file has been uploaded via the Dropzone.
-}
port dropzoneUploadedFile : (Int -> msg) -> Sub msg


{-| Cause the drop zone widget to clear it's image
-}
port dropzoneReset : () -> Cmd msg
