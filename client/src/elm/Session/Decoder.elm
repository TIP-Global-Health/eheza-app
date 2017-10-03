module Session.Decoder exposing (..)

import Clinic.Model exposing (ClinicId(..))
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Session.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)


decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "label" string
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> required "clinic" (map ClinicId int)
