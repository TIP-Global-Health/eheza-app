module Device.Decoder exposing (decode)

import Device.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decode : String -> Decoder Device
decode defaultBackendUrl =
    succeed Device
        |> required "access_token" string
        |> required "refresh_token" string
        |> optional "backend_url" string defaultBackendUrl
        |> optional "device_id" (nullable decodeInt) Nothing
