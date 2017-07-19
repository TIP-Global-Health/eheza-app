module Measurement.Encoder exposing (..)

import Child.Model exposing (ChildId)
import Json.Encode as Encoder exposing (Value, float, int)
import Measurement.Model exposing (FloatMeasurements(..))


encodeHeight : ChildId -> Float -> Value
encodeHeight childId value =
    encodeFloatMeasurement childId HeightFloat value


encodeMuac : ChildId -> Float -> Value
encodeMuac childId value =
    encodeFloatMeasurement childId MuacFloat value


encodePhoto : ChildId -> Int -> Value
encodePhoto childId value =
    let
        _ =
            Debug.log "encodePhoto" value
    in
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
