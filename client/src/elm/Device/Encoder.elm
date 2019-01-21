module Device.Encoder exposing (encode)

import Device.Model exposing (..)
import Json.Encode exposing (..)


encode : Device -> Value
encode device =
    object
        [ ( "access_token", string device.accessToken )
        , ( "refresh_token", string device.refreshToken )
        ]
