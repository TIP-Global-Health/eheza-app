module Measurement.Decoder
    exposing
        ( decodeChildNutritionSign
        , decodeFamilyPlanning
        , decodeFamilyPlanningSign
        , decodeHeight
        , decodeMuac
        , decodeNutrition
        , decodePhotoFromResponse
        , decodeWeight
        )

import Backend.Measurement.Model exposing (FamilyPlanningSign(..), ChildNutritionSign(..))
import Drupal.Restful exposing (decodeId, decodeSingleEntity, decodeStorageTuple)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Measurement.Model exposing (FamilyPlanningId(..), Photo, PhotoId, HeightId(..), MuacId(..), NutritionId(..), WeightId(..))
import StorageKey exposing (StorageKey(..))
import Utils.Json exposing (decodeEverySet)


decodePhoto : Decoder Photo
decodePhoto =
    decode Photo
        |> requiredAt [ "photo", "styles", "thumbnail" ] string


decodePhotoTuple : Decoder ( PhotoId, Photo )
decodePhotoTuple =
    decode
        (,)
        |> required "id" decodeInt
        |> custom decodePhoto


decodePhotoFromResponse : Decoder ( PhotoId, Photo )
decodePhotoFromResponse =
    decodeSingleEntity decodePhotoTuple


decodeHeight : Decoder ( StorageKey HeightId, Float )
decodeHeight =
    decodeStorageTuple (decodeId HeightId) (field "height" decodeFloat)


decodeWeight : Decoder ( StorageKey WeightId, Float )
decodeWeight =
    decodeStorageTuple (decodeId WeightId) (field "weight" decodeFloat)


decodeMuac : Decoder ( StorageKey MuacId, Float )
decodeMuac =
    decodeStorageTuple (decodeId MuacId) (field "muac" decodeFloat)


decodeFamilyPlanning : Decoder ( StorageKey FamilyPlanningId, EverySet FamilyPlanningSign )
decodeFamilyPlanning =
    decodeStorageTuple (decodeId FamilyPlanningId) (field "family_planning_signs" (decodeEverySet decodeFamilyPlanningSign))


decodeNutrition : Decoder ( StorageKey NutritionId, EverySet ChildNutritionSign )
decodeNutrition =
    decodeStorageTuple (decodeId NutritionId) (field "nutrition_signs" (decodeEverySet decodeChildNutritionSign))


decodeChildNutritionSign : Decoder ChildNutritionSign
decodeChildNutritionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "abdominal-disortion" ->
                        succeed AbdominalDisortion

                    "apathy" ->
                        succeed Apathy

                    "brittle-hair" ->
                        succeed BrittleHair

                    "dry-skin" ->
                        succeed DrySkin

                    "edema" ->
                        succeed Edema

                    "none" ->
                        succeed None

                    "poor-appetite" ->
                        succeed PoorAppetite

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ChildNutritionSign"
            )


decodeFamilyPlanningSign : Decoder FamilyPlanningSign
decodeFamilyPlanningSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "pill" ->
                        succeed Pill

                    "condoms" ->
                        succeed Condoms

                    "iud" ->
                        succeed IUD

                    "injection" ->
                        succeed Injection

                    "necklace" ->
                        succeed Necklace

                    "none" ->
                        succeed NoFamilyPlanning

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FamilyPlanningSign"
            )
