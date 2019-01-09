module Device.Encoder exposing (encode)

import Device.Model exposing (..)
import Json.Encode exposing (..)


encode : Device -> Value
encode device =
    object
        [ ( "id", int device.id )
        , ( "label", string device.name )
        , ( "access_token", string device.accessToken )
        ]
