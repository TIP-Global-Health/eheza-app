module Measurement.Decoder
    exposing
        ( decodePhotoFromResponse
        , decodeWeight
        , decodeWeightFromResponse
        )

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Measurement.Model exposing (Photo, PhotoId)
import Utils.Json exposing (decodeFloat, decodeInt)


decodePhoto : Decoder Photo
decodePhoto =
    decode Photo
        |> requiredAt [ "photo", "styles", "thumbnail" ] string


decodePhotoTuple : Decoder ( PhotoId, Photo )
decodePhotoTuple =
    decode
        (,)
        |> required "id" decodeInt
        |> custom decodePhoto


decodePhotoFromResponse : Decoder ( PhotoId, Photo )
decodePhotoFromResponse =
    at [ "data", "0" ] decodePhotoTuple


decodeWeight : Decoder Float
decodeWeight =
    decodeFloat


decodeWeightFromResponse : Decoder Float
decodeWeightFromResponse =
    at [ "data", "0" ] decodeFloat
