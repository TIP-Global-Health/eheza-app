port module Measurement.Update exposing (update, subscriptions)

import Activity.Model
    exposing
        ( ActivityType(..)
        , ChildActivityType(..)
        , ChildNutritionSign(..)
        , FamilyPlanningSign(..)
        , MotherActivityType(FamilyPlanning)
        )
import Child.Model exposing (ChildId)
import Config.Model exposing (BackendUrl)
import EverySet exposing (EverySet)
import Examination.Model exposing (Examination(..))
import Examination.Utils exposing (mapExaminationChild, mapExaminationMother, supplyMeasurement, toExaminationChild, toExaminationMother)
import Http
import HttpBuilder exposing (get, send, withJsonBody, withQueryParams, withExpect)
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import Measurement.Decoder exposing (decodePhotoFromResponse, decodeWeight, decodeHeight, decodeMuac, decodeFamilyPlanning, decodeNutrition)
import Measurement.Encoder exposing (encodeFamilyPlanning, encodeHeight, encodeMuac, encodeNutritionSigns, encodePhoto, encodeWeight)
import Measurement.Model exposing (..)
import Participant.Model exposing (Participant, ParticipantId)
import RemoteData exposing (RemoteData(..))
import StorageKey exposing (StorageKey(..))
import Utils.Json exposing (decodeSingleDrupalEntity)
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
                let
                    -- We apply the value from the UI to the value stored in
                    -- the examination in order to send it to the backend.
                    -- But, we only update the examination itself when we get
                    -- the successful response from the backend.
                    cmd =
                        toExaminationMother examination
                            |> Maybe.map
                                (\ex ->
                                    if EverySet.isEmpty model.familyPlanningSigns then
                                        Cmd.none
                                    else
                                        upsert
                                            (upsertFamilyPlanning participantId)
                                            backendUrl
                                            accessToken
                                            (supplyMeasurement model.familyPlanningSigns (Just ex.familyPlanning))
                                )
                            |> Maybe.withDefault Cmd.none
                in
                    -- In principle, we shouldn't have a separate `status` ...
                    -- instead, the measurement itself ought to be an
                    -- `StorageEditableWebData` or something of the kind ...
                    -- but leaving it like this for now.
                    ( { model | status = Loading }
                    , cmd
                    )
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

            HandleFamilyPlanningSave (Ok value) ->
                ( { model
                    | status = Success ()
                    , familyPlanningSigns = Tuple.second value
                  }
                , mapExaminationMother (\ex -> { ex | familyPlanning = value }) examination
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

            HandleHeightSave (Ok value) ->
                ( { model
                    | status = Success ()
                    , height = Just (toString (Tuple.second value))
                  }
                , mapExaminationChild (\ex -> { ex | height = Just value }) examination
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
                    )
                        |> unchangedExaminationNoRedirect

            HandleNutritionSignsSave (Ok value) ->
                ( { model
                    | status = Success ()
                    , nutritionSigns = Tuple.second value
                  }
                , mapExaminationChild (\ex -> { ex | nutrition = value }) examination
                , Cmd.none
                , Just ( Child NutritionSigns, Child ChildPicture )
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

            HandleMuacSave (Ok value) ->
                ( { model
                    | status = Success ()
                    , muac = Just (toString (Tuple.second value))
                  }
                , mapExaminationChild (\ex -> { ex | muac = Just value }) examination
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

            HandleWeightSave (Ok value) ->
                ( { model
                    | status = Success ()
                    , weight = Just (toString (Tuple.second value))
                  }
                , mapExaminationChild (\ex -> { ex | weight = Just value }) examination
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
                let
                    -- We apply the value from the UI to the value stored in
                    -- the examination in order to send it to the backend.
                    -- But, we only update the examination itself when we get
                    -- the successful response from the backend.
                    cmd =
                        Maybe.map2
                            (\muac ex ->
                                upsert
                                    (upsertMuac participantId)
                                    backendUrl
                                    accessToken
                                    (supplyMeasurement (getFloatInputValue muac) ex.muac)
                            )
                            model.muac
                            (toExaminationChild examination)
                            |> Maybe.withDefault Cmd.none
                in
                    -- In principle, we shouldn't have a separate `status` ...
                    -- instead, the measurement itself ought to be an
                    -- `StorageEditableWebData` or something of the kind ...
                    -- but leaving it like this for now.
                    ( { model | status = Loading }
                    , cmd
                    )
                        |> unchangedExaminationNoRedirect

            NutritionSignsSave ->
                let
                    -- We apply the value from the UI to the value stored in
                    -- the examination in order to send it to the backend.
                    -- But, we only update the examination itself when we get
                    -- the successful response from the backend.
                    cmd =
                        toExaminationChild examination
                            |> Maybe.map
                                (\ex ->
                                    if EverySet.isEmpty model.nutritionSigns then
                                        Cmd.none
                                    else
                                        upsert
                                            (upsertNutrition participantId)
                                            backendUrl
                                            accessToken
                                            (supplyMeasurement model.nutritionSigns (Just ex.nutrition))
                                )
                            |> Maybe.withDefault Cmd.none
                in
                    -- In principle, we shouldn't have a separate `status` ...
                    -- instead, the measurement itself ought to be an
                    -- `StorageEditableWebData` or something of the kind ...
                    -- but leaving it like this for now.
                    ( { model | status = Loading }
                    , cmd
                    )
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
                let
                    -- We apply the value from the UI to the value stored in
                    -- the examination in order to send it to the backend.
                    -- But, we only update the examination itself when we get
                    -- the successful response from the backend.
                    cmd =
                        Maybe.map2
                            (\weight ex ->
                                upsert
                                    (upsertWeight participantId)
                                    backendUrl
                                    accessToken
                                    (supplyMeasurement (getFloatInputValue weight) ex.weight)
                            )
                            model.weight
                            (toExaminationChild examination)
                            |> Maybe.withDefault Cmd.none
                in
                    -- In principle, we shouldn't have a separate `status` ...
                    -- instead, the measurement itself ought to be an
                    -- `StorageEditableWebData` or something of the kind ...
                    -- but leaving it like this for now.
                    ( { model | status = Loading }
                    , cmd
                    )
                        |> unchangedExaminationNoRedirect

            HeightSave ->
                let
                    -- We apply the value from the UI to the value stored in
                    -- the examination in order to send it to the backend.
                    -- But, we only update the examination itself when we get
                    -- the successful response from the backend.
                    cmd =
                        Maybe.map2
                            (\height ex ->
                                upsert
                                    (upsertHeight participantId)
                                    backendUrl
                                    accessToken
                                    (supplyMeasurement (getFloatInputValue height) ex.height)
                            )
                            model.height
                            (toExaminationChild examination)
                            |> Maybe.withDefault Cmd.none
                in
                    -- In principle, we shouldn't have a separate `status` ...
                    -- instead, the measurement itself ought to be an
                    -- `StorageEditableWebData` or something of the kind ...
                    -- but leaving it like this for now.
                    ( { model | status = Loading }
                    , cmd
                    )
                        |> unchangedExaminationNoRedirect

            WeightUpdate val ->
                ( { model | weight = Just (normalizeFloatFormInput val) }
                , Cmd.none
                )
                    |> unchangedExaminationNoRedirect


{-| Some things that `upsert` needs to know.

The pattern here is that we try to isolate those parameters which are purely
dependennt on the types ... that is, which are static and will not vary. That
way, we can pre-construct a configuration for each combination of types we're
interested in. So, the moral equivalent of a type-class.

The `encodeStorage` function should check whether the record is `New` or
`Existing` and encode whatever is relevant.

-}
type alias UpsertConfig key value msg =
    { decodeStorage : Decoder ( StorageKey key, value )
    , encodeId : key -> String
    , encodeStorage : ( StorageKey key, value ) -> Value
    , handler : Result Http.Error ( StorageKey key, value ) -> msg
    , path : String
    }


{-| Actually, the `ChildId` ought to be part of the `Weight` type ... but
I'll leave that for now, as it may change depending on what will be done
with the `Examination` type.
-}
upsertWeight : ChildId -> UpsertConfig WeightId Float Msg
upsertWeight childId =
    { decodeStorage = decodeWeight
    , encodeId = \(WeightId id) -> toString id
    , encodeStorage = encodeWeight childId
    , handler = HandleWeightSave
    , path = "weights"
    }


upsertHeight : ChildId -> UpsertConfig HeightId Float Msg
upsertHeight childId =
    { decodeStorage = decodeHeight
    , encodeId = \(HeightId id) -> toString id
    , encodeStorage = encodeHeight childId
    , handler = HandleHeightSave
    , path = "heights"
    }


upsertMuac : ChildId -> UpsertConfig MuacId Float Msg
upsertMuac childId =
    { decodeStorage = decodeMuac
    , encodeId = \(MuacId id) -> toString id
    , encodeStorage = encodeMuac childId
    , handler = HandleMuacSave
    , path = "muacs"
    }


upsertFamilyPlanning : ChildId -> UpsertConfig FamilyPlanningId (EverySet FamilyPlanningSign) Msg
upsertFamilyPlanning childId =
    { decodeStorage = decodeFamilyPlanning
    , encodeId = \(FamilyPlanningId id) -> toString id
    , encodeStorage = encodeFamilyPlanning childId
    , handler = HandleFamilyPlanningSave
    , path = "family-plannings"
    }


upsertNutrition : ChildId -> UpsertConfig NutritionId (EverySet ChildNutritionSign) Msg
upsertNutrition childId =
    { decodeStorage = decodeNutrition
    , encodeId = \(NutritionId id) -> toString id
    , encodeStorage = encodeNutritionSigns childId
    , handler = HandleNutritionSignsSave
    , path = "nutritions"
    }


{-| If we have an `Existing` storage key, then update the backend via `patch`.

If we have a `New` storage key, insert it in the backend via `post`.

-}
upsert : UpsertConfig key value msg -> BackendUrl -> String -> ( StorageKey key, value ) -> Cmd msg
upsert config backendUrl accessToken ( key, value ) =
    case key of
        Existing id ->
            HttpBuilder.patch (backendUrl ++ "/api/" ++ config.path ++ "/" ++ config.encodeId id)
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (config.encodeStorage ( key, value ))
                |> withExpect (Http.expectJson (decodeSingleDrupalEntity config.decodeStorage))
                |> send config.handler

        New ->
            HttpBuilder.post (backendUrl ++ "/api/" ++ config.path)
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> withJsonBody (config.encodeStorage ( key, value ))
                |> withExpect (Http.expectJson (decodeSingleDrupalEntity config.decodeStorage))
                |> send config.handler


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


subscriptions : Model -> Sub Msg
subscriptions model =
    dropzoneUploadedFile HandleDropzoneUploadedFile


{-| Get a singal if a file has been uploaded via the Dropzone.
-}
port dropzoneUploadedFile : (Int -> msg) -> Sub msg


{-| Cause the drop zone widget to clear it's image
-}
port dropzoneReset : () -> Cmd msg
