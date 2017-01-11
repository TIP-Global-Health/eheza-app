module Mother.Decoder
    exposing
        ( decodeMother
        )

import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Mother.Model exposing (..)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        |> optionalAt [ "image", "styles", "large" ] string "http://placehold.it/350x150"
        |> required "children" (list string)
