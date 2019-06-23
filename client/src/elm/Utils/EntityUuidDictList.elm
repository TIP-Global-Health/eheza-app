module Utils.Dict exposing (Dict, concat, decodeArray, decodeArray2, decodeKeysAndValues, decodeWithKeys, empty, fromList, fromListBy, groupBy, singleton)

import AssocList as Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid)


{-| An `Dict` is like a `DictList`, but the key can be any
`EntityUuid`-based type. It's actually a specialization of `Dict.. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`Dict`. Once you've done that, you can use all the other
`Dict. methods in the usual way -- they are not repeated here.
-}
type alias Dict k v =
    Dict.k v String


empty : Dict (EntityUuid k) v
empty =
    Dict.empty fromEntityUuid


singleton : EntityUuid k -> v -> Dict (EntityUuid k) v
singleton =
    Dict.singleton fromEntityUuid


fromList : List ( EntityUuid k, v ) -> Dict (EntityUuid k) v
fromList =
    Dict.fromList fromEntityUuid


groupBy : (v -> EntityUuid k) -> List v -> Dict (EntityUuid k) (List v)
groupBy =
    Dict.groupBy fromEntityUuid


fromListBy : (v -> EntityUuid k) -> List v -> Dict (EntityUuid k) v
fromListBy =
    Dict.fromListBy fromEntityUuid


decodeWithKeys : List (EntityUuid k) -> (EntityUuid k -> Decoder v) -> Decoder (Dict (EntityUuid k) v)
decodeWithKeys =
    Dict.decodeWithKeys fromEntityUuid


decodeKeysAndValues : Decoder (List (EntityUuid k)) -> (EntityUuid k -> Decoder v) -> Decoder (Dict (EntityUuid k) v)
decodeKeysAndValues =
    Dict.decodeKeysAndValues fromEntityUuid


decodeArray : (v -> EntityUuid k) -> Decoder v -> Decoder (Dict (EntityUuid k) v)
decodeArray =
    Dict.decodeArray fromEntityUuid


decodeArray2 : Decoder (EntityUuid k) -> Decoder v -> Decoder (Dict (EntityUuid k) v)
decodeArray2 =
    Dict.decodeArray2 fromEntityUuid


concat : List (Dict (EntityUuid k) v) -> Dict (EntityUuid k) v
concat =
    Dict.concat fromEntityUuid
