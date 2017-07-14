module PatientManager.Encoder exposing (..)

import Json.Encode as Encoder exposing (Value, float, int)
import PatientManager.Model exposing (PostWeightData)


encodeWeight : PostWeightData -> Value
encodeWeight weightData =
    Encoder.object <|
        [ ( "child", Encoder.int weightData.child )
        , ( "weight", Encoder.float weightData.weight )
        ]
