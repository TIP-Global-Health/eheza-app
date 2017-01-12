module Child.Decoder
    exposing
        ( decodeChild
        )

import Activity.Decoder exposing (decodeChildActivityDates)
import Child.Model exposing (..)
import Date exposing (Date)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeIntAsString)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "large" ] string "http://placehold.it/350x150"
        |> required "mother" (nullable decodeIntAsString)
        |> custom decodeChildActivityDates
