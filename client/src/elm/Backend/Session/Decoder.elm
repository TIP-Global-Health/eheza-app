module Backend.Session.Decoder exposing (..)

import Backend.Entities exposing (..)
import Backend.Session.Model exposing (..)
import Drupal.Restful exposing (decodeNodeId)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)


decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> required "clinic" decodeNodeId
