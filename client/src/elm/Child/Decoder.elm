module Child.Decoder
    exposing
        ( decodeChild
        )

import Child.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "large" ] string "http://placehold.it/350x150"
        |> required "mother" string
