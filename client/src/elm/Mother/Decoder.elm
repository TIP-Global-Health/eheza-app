module Mother.Decoder
    exposing
        ( decodeMother
        )

import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Mother.Model exposing (..)
import Utils.Json exposing (decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "large" ] string "http://placehold.it/350x150"
        |> required "children" (oneOf [ list string, decodeNullAsEmptyArray ])
