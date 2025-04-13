module Backend.Components.Decoder exposing (decodeHealthCenterData)

import Backend.Components.Model exposing (HealthCenterData)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeHealthCenterData : Decoder HealthCenterData
decodeHealthCenterData =
    succeed HealthCenterData
        |> required "id" decodeInt
        |> required "name" string
