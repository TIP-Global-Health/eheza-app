module Measurement.Encoder exposing (..)

import Activity.Model exposing (ChildNutritionSign(..))
import Child.Model exposing (ChildId)
import EveryDict
import Json.Encode as Encoder exposing (Value, float, int, list, string)
import Measurement.Model exposing (EveryDictChildNutritionSign, FloatMeasurements(..))


encodeHeight : ChildId -> Float -> Value
encodeHeight childId value =
    encodeFloatMeasurement childId HeightFloat value


encodeMuac : ChildId -> Float -> Value
encodeMuac childId value =
    encodeFloatMeasurement childId MuacFloat value


encodeNutritionSign : ChildNutritionSign -> Value
encodeNutritionSign sign =
    case sign of
        AbdominalDisortion ->
            string "abdominal-disortion"

        Apathy ->
            string "apathy"

        BrittleHair ->
            string "brittle-hair"

        DrySkin ->
            string "dry-skin"

        Edema ->
            string "edema"

        None ->
            string "none"

        PoorAppetite ->
            string "poor-appetite"


encodeNutritionSigns : ChildId -> EveryDictChildNutritionSign -> Value
encodeNutritionSigns childId value =
    List.map (\sign -> encodeNutritionSign sign) (EveryDict.keys value)
        |> list


encodePhoto : ChildId -> Int -> Value
encodePhoto childId value =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "photo", Encoder.int value )
        ]


encodeWeight : ChildId -> Float -> Value
encodeWeight childId value =
    encodeFloatMeasurement childId WeightFloat value


encodeFloatMeasurement : ChildId -> FloatMeasurements -> Float -> Value
encodeFloatMeasurement childId floatMeasurement value =
    let
        key =
            case floatMeasurement of
                HeightFloat ->
                    "height"

                MuacFloat ->
                    "muac"

                WeightFloat ->
                    "weight"
    in
        Encoder.object <|
            [ ( "child", Encoder.int childId )
            , ( key, Encoder.float value )
            ]
