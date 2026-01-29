module Device.Decoder exposing (decode)

import Device.Model exposing (Device)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decode : String -> Decoder Device
decode defaultBackendUrl =
    succeed Device
        |> required "access_token" string
        |> required "refresh_token" string
        |> optional "backend_url" string defaultBackendUrl
        |> optional "device_id" (nullable decodeInt) Nothing
