module Backend.Session.Decoder exposing (decodeSession)

import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


{-| Decodes the JSON sent by /api/sessions
-}
decodeSession : Decoder Session
decodeSession =
    succeed Session
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> custom
            (oneOf
                -- Work with "full_view" true or false, or with the
                -- structure we encode for the cache.
                [ field "clinic" decodeEntityUuid
                , field "clinic_id" decodeEntityUuid
                , at [ "clinic", "id" ] decodeEntityUuid
                ]
            )
        |> optional "closed" bool False
        |> optional "training" bool False
