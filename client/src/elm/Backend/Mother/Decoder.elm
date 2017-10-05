module Backend.Mother.Decoder
    exposing
        ( decodeMother
        )

import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Backend.Mother.Model exposing (..)
import Drupal.Restful exposing (decodeNodeId)
import Utils.Json exposing (decodeDate, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        -- The default avatar comes from SASS , not from the Model.
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string ""
        |> required "children" (oneOf [ list decodeNodeId, decodeNullAsEmptyArray ])
        |> hardcoded []
        |> required "date_birth" decodeDate
