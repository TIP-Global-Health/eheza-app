module Measurement.Encoder exposing (..)

import Child.Model exposing (ChildId)
import Json.Encode as Encoder exposing (Value, float, int)


encodeHeight : ChildId -> Float -> Value
encodeHeight childId height =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "height", Encoder.float height )
        ]


encodeMuac : ChildId -> Float -> Value
encodeMuac childId muac =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "muac", Encoder.float muac )
        ]


encodeWeight : ChildId -> Float -> Value
encodeWeight childId weight =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "weight", Encoder.float weight )
        ]
