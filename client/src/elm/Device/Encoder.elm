module Device.Encoder exposing (encode)

import Device.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)


encode : Device -> Value
encode device =
    object
        [ ( "access_token", string device.accessToken )
        , ( "refresh_token", string device.refreshToken )
        , ( "backend_url", string device.backendUrl )
        , ( "device_id", maybe int device.deviceId )
        ]
