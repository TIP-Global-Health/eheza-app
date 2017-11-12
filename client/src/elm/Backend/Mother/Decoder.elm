module Backend.Mother.Decoder
    exposing
        ( decodeMother
        )

import Backend.Mother.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, at, oneOf, andThen, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityId)
import Utils.Json exposing (decodeDate, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        -- The default avatar comes from SASS , not from the Model.
        -- And, we accommodate the JSON from the server or from the cache
        |> custom
            (oneOf
                [ at [ "avatar", "styles", "patient-photo" ] string
                , field "avatar" string
                , succeed ""
                ]
            )
        |> required "children" (oneOf [ list decodeEntityId, decodeNullAsEmptyArray ])
        |> required "date_birth" decodeYYYYMMDD
