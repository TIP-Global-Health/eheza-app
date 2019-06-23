module Utils.EntityIdDictList exposing (EntityIdDictList, concat, decodeArray, decodeArray2, decodeKeysAndValues, decodeWithKeys, empty, fromList, fromListBy, groupBy, singleton)

import AssocList as Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Restful.Endpoint exposing (EntityId, fromEntityId)


{-| An `EntityIdDictList` is like a `DictList`, but the key can be any
`EntityId`-based type. It's actually a specialization of `Dict.. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityIdDictList`. Once you've done that, you can use all the other
`Dict. methods in the usual way -- they are not repeated here.
-}
type alias EntityIdDictList k v =
    Dict.k v Int


empty : EntityIdDictList (EntityId k) v
empty =
    Dict.empty fromEntityId


singleton : EntityId k -> v -> EntityIdDictList (EntityId k) v
singleton =
    Dict.singleton fromEntityId


fromList : List ( EntityId k, v ) -> EntityIdDictList (EntityId k) v
fromList =
    Dict.fromList fromEntityId


groupBy : (v -> EntityId k) -> List v -> EntityIdDictList (EntityId k) (List v)
groupBy =
    Dict.groupBy fromEntityId


fromListBy : (v -> EntityId k) -> List v -> EntityIdDictList (EntityId k) v
fromListBy =
    Dict.fromListBy fromEntityId


decodeWithKeys : List (EntityId k) -> (EntityId k -> Decoder v) -> Decoder (EntityIdDictList (EntityId k) v)
decodeWithKeys =
    Dict.decodeWithKeys fromEntityId


decodeKeysAndValues : Decoder (List (EntityId k)) -> (EntityId k -> Decoder v) -> Decoder (EntityIdDictList (EntityId k) v)
decodeKeysAndValues =
    Dict.decodeKeysAndValues fromEntityId


decodeArray : (v -> EntityId k) -> Decoder v -> Decoder (EntityIdDictList (EntityId k) v)
decodeArray =
    Dict.decodeArray fromEntityId


decodeArray2 : Decoder (EntityId k) -> Decoder v -> Decoder (EntityIdDictList (EntityId k) v)
decodeArray2 =
    Dict.decodeArray2 fromEntityId


concat : List (EntityIdDictList (EntityId k) v) -> EntityIdDictList (EntityId k) v
concat =
    Dict.concat fromEntityId
