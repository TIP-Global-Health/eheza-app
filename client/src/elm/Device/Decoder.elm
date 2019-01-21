module Device.Decoder exposing (decode)

import Device.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decode : Decoder Device
decode =
    succeed Device
        |> required "access_token" string
        |> required "refresh_token" string
