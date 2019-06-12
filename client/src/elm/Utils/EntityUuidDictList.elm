module Utils.EntityUuidDictList exposing (EntityUuidDictList, concat, decodeArray, decodeArray2, decodeKeysAndValues, decodeWithKeys, empty, fromList, fromListBy, groupBy, singleton)

import AllDictList exposing (AllDictList)
import Json.Decode exposing (Decoder)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid)


{-| An `EntityUuidDictList` is like a `DictList`, but the key can be any
`EntityUuid`-based type. It's actually a specialization of `AllDictList`. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityUuidDictList`. Once you've done that, you can use all the other
`AllDictList` methods in the usual way -- they are not repeated here.
-}
type alias EntityUuidDictList k v =
    AllDictList k v String


empty : EntityUuidDictList (EntityUuid k) v
empty =
    AllDictList.empty fromEntityUuid


singleton : EntityUuid k -> v -> EntityUuidDictList (EntityUuid k) v
singleton =
    AllDictList.singleton fromEntityUuid


fromList : List ( EntityUuid k, v ) -> EntityUuidDictList (EntityUuid k) v
fromList =
    AllDictList.fromList fromEntityUuid


groupBy : (v -> EntityUuid k) -> List v -> EntityUuidDictList (EntityUuid k) (List v)
groupBy =
    AllDictList.groupBy fromEntityUuid


fromListBy : (v -> EntityUuid k) -> List v -> EntityUuidDictList (EntityUuid k) v
fromListBy =
    AllDictList.fromListBy fromEntityUuid


decodeWithKeys : List (EntityUuid k) -> (EntityUuid k -> Decoder v) -> Decoder (EntityUuidDictList (EntityUuid k) v)
decodeWithKeys =
    AllDictList.decodeWithKeys fromEntityUuid


decodeKeysAndValues : Decoder (List (EntityUuid k)) -> (EntityUuid k -> Decoder v) -> Decoder (EntityUuidDictList (EntityUuid k) v)
decodeKeysAndValues =
    AllDictList.decodeKeysAndValues fromEntityUuid


decodeArray : (v -> EntityUuid k) -> Decoder v -> Decoder (EntityUuidDictList (EntityUuid k) v)
decodeArray =
    AllDictList.decodeArray fromEntityUuid


decodeArray2 : Decoder (EntityUuid k) -> Decoder v -> Decoder (EntityUuidDictList (EntityUuid k) v)
decodeArray2 =
    AllDictList.decodeArray2 fromEntityUuid


concat : List (EntityUuidDictList (EntityUuid k) v) -> EntityUuidDictList (EntityUuid k) v
concat =
    AllDictList.concat fromEntityUuid
