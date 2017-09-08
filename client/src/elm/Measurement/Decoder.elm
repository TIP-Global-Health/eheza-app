module Measurement.Decoder
    exposing
        ( decodePhotoFromResponse
        , decodeHeight
        , decodeMuac
        , decodeWeight
        )

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Measurement.Model exposing (Photo, PhotoId, HeightId(..), MuacId(..), WeightId(..))
import StorageKey exposing (StorageKey(..))
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


decodeHeight : Decoder ( StorageKey HeightId, Float )
decodeHeight =
    decodeStorageTuple (decodeId HeightId) (field "height" decodeFloat)


decodeWeight : Decoder ( StorageKey WeightId, Float )
decodeWeight =
    decodeStorageTuple (decodeId WeightId) (field "weight" decodeFloat)


decodeMuac : Decoder ( StorageKey MuacId, Float )
decodeMuac =
    decodeStorageTuple (decodeId MuacId) (field "muac" decodeFloat)


{-| Convenience for the pattern where you have a field called "id",
and you want to wrap the result in a type (e.g. PersonId Int). You can
just use `decodeId PersonId`.
-}
decodeId : (Int -> a) -> Decoder a
decodeId wrapper =
    map wrapper (field "id" decodeInt)


{-| Convenience for the case where you have a decoder for the ID,
a decoder for the value, and you want to decode a tuple of StorageKey and
value.
-}
decodeStorageTuple : Decoder key -> Decoder value -> Decoder ( StorageKey key, value )
decodeStorageTuple keyDecoder valueDecoder =
    map2 (,)
        (map Existing keyDecoder)
        valueDecoder
