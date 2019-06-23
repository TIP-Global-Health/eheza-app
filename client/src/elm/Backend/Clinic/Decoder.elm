module Backend.Clinic.Decoder exposing (decodeClinic)

import Backend.Clinic.Model exposing (..)
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeClinic : Decoder Clinic
decodeClinic =
    succeed Clinic
        |> required "label" string
        |> required "health_center" decodeEntityUuid
