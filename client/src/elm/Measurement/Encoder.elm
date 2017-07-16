module Measurement.Encoder exposing (..)

import Child.Model exposing (ChildId)
import Json.Encode as Encoder exposing (Value, float, int)


encodeWeight : ChildId -> Float -> Value
encodeWeight childId weight =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "weight", Encoder.float weight )
        ]


encodePhoto : ChildId -> Float -> Value
encodePhoto childId weight =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "photo", Encoder.float weight )
        ]
