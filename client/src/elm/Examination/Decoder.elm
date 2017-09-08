module Examination.Decoder exposing (..)

import Examination.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import StorageKey exposing (StorageKey(..))
import Utils.Json exposing (decodeFloat, decodeInt)


decodeExaminationChild : Decoder ExaminationChild
decodeExaminationChild =
    decode ExaminationChild
        |> optional "height" (map Just heightDecoder) Nothing
        |> optional "muac" (map Just muacDecoder) Nothing
        |> hardcoded Nothing
        |> optional "weight" (map Just weightDecoder) Nothing


heightDecoder : Decoder ( StorageKey HeightId, Float )
heightDecoder =
    decodeStorageTuple (decodeId HeightId) (field "height" decodeFloat)


weightDecoder : Decoder ( StorageKey WeightId, Float )
weightDecoder =
    decodeStorageTuple (decodeId WeightId) (field "weight" decodeFloat)


muacDecoder : Decoder ( StorageKey MuacId, Float )
muacDecoder =
    decodeStorageTuple (decodeId MuacId) (field "muac" decodeFloat)


decodeExaminationMother : Decoder ExaminationMother
decodeExaminationMother =
    decode ExaminationMother


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
