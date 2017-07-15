module Mother.Decoder
    exposing
        ( decodeMother
        )

import Activity.Decoder exposing (decodeMotherActivityDates)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Mother.Model exposing (..)
import Utils.Json exposing (decodeIntAsString, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string "http://placehold.it/200x200"
        |> required "children" (oneOf [ list int, decodeNullAsEmptyArray ])
        |> custom decodeMotherActivityDates
