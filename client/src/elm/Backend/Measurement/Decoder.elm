module Backend.Measurement.Decoder exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Drupal.Restful exposing (decodeEntityId, decodeSingleEntity, decodeStorageTuple, decodeEntity)
import EveryDict exposing (EveryDict)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Gizra.NominalDate
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed, value, decodeValue)
import Json.Decode.Extra exposing (fromResult)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEverySet)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeChildMeasurement : Decoder value -> Decoder (Measurement ChildId value)
decodeChildMeasurement =
    decodeMeasurement (field "child" decodeEntityId)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeMotherMeasurement : Decoder value -> Decoder (Measurement MotherId value)
decodeMotherMeasurement =
    decodeMeasurement (field "mother" decodeEntityId)


decodeMeasurement : Decoder participantId -> Decoder value -> Decoder (Measurement participantId value)
decodeMeasurement participantDecoder valueDecoder =
    decode Measurement
        |> custom participantDecoder
        |> required "session" (nullable decodeEntityId)
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> custom valueDecoder


{-| Decodes `Measurments` as sent by /api/offline_sessions/
-}
decodeMeasurements : Decoder Measurements
decodeMeasurements =
    decode Measurements
        |> requiredAt [ "participants", "mother_activity" ] decodeMotherMeasurements
        |> requiredAt [ "participants", "child_activity" ] decodeChildMeasurements


decodeMotherMeasurements : Decoder (EveryDict MotherId MotherMeasurements)
decodeMotherMeasurements =
    Debug.crash "implement"


decodeChildMeasurements : Decoder (EveryDict ChildId ChildMeasurements)
decodeChildMeasurements =
    Debug.crash "implement"


decodePhoto : Decoder Photo
decodePhoto =
    at [ "photo", "styles", "thumbnail" ] string
        |> map PhotoValue
        |> decodeChildMeasurement


decodeHeight : Decoder Height
decodeHeight =
    field "height" decodeFloat
        |> map HeightValue
        |> decodeChildMeasurement


decodeWeight : Decoder Weight
decodeWeight =
    field "weight" decodeFloat
        |> map WeightValue
        |> decodeChildMeasurement


decodeMuac : Decoder Muac
decodeMuac =
    field "muac" decodeFloat
        |> map MuacValue
        |> decodeChildMeasurement


decodeFamilyPlanning : Decoder FamilyPlanning
decodeFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeMotherMeasurement


decodeNutrition : Decoder ChildNutrition
decodeNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeChildMeasurement


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
