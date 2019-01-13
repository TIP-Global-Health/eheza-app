module Device.Decoder exposing (decode)

import Device.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decode : Decoder Device
decode =
    succeed Device
        |> required "id" decodeInt
        |> required "label" string
        |> required "access_token" string
