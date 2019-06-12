module Backend.Clinic.Decoder exposing (decodeClinic)

import Backend.Clinic.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeClinic : Decoder Clinic
decodeClinic =
    decode Clinic
        |> required "label" string
        |> required "health_center" decodeEntityUuid
