module Measurement.Decoder
    exposing
        ( decodeWeight
        , decodeWeightFromResponse
        )

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeFloat)


decodeWeight : Decoder Float
decodeWeight =
    decodeFloat


decodeWeightFromResponse : Decoder Float
decodeWeightFromResponse =
    at [ "data", "0" ] decodeFloat
