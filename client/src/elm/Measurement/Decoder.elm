module Measurement.Decoder exposing (..)

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Measurement.Model exposing (PostWeightResponse)


decodeWeight : Decoder PostWeightResponse
decodeWeight =
    decode PostWeightResponse
        |> required "weight"
            (string
                |> andThen
                    (\val ->
                        case String.toFloat val of
                            Ok value ->
                                succeed value

                            Err message ->
                                fail message
                    )
            )


decodeWeightList : Decoder (List PostWeightResponse)
decodeWeightList =
    list decodeWeight


decodeWeightFromResponse : Decoder PostWeightResponse
decodeWeightFromResponse =
    decodeWeight


decodeWeightListFromResponse : Decoder (List PostWeightResponse)
decodeWeightListFromResponse =
    at [ "data" ] decodeWeightList
