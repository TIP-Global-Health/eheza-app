module Child.Decoder
    exposing
        ( decodeChild
        )

import Activity.Decoder exposing (decodeChildActivityDates)
import Child.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeInt)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string "http://placehold.it/200x200"
        |> required "mother" (nullable decodeInt)
        |> required "lastExamination" (nullable decodeInt)
        |> custom decodeChildActivityDates
