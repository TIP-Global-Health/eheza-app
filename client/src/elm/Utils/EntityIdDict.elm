module Utils.EntityIdDict exposing (EntityIdDict, empty, fromList, singleton)

import AssocList as Dict exposing (Dict)
import Restful.Endpoint exposing (EntityId, fromEntityId)


{-| An `EntityIdDict` is like a `Dict`, but the key can be any
`EntityId`-based type. It's actually a specialization of `Dict.. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityIdDict`. Once you've done that, you can use all the other `Dict.
methods in the usual way -- they are not repeated here.
-}
type alias EntityIdDict k v =
    Dict.k v Int


empty : EntityIdDict (EntityId k) v
empty =
    Dict.empty fromEntityId


singleton : EntityId k -> v -> EntityIdDict (EntityId k) v
singleton =
    Dict.singleton fromEntityId


fromList : List ( EntityId k, v ) -> EntityIdDict (EntityId k) v
fromList =
    Dict.fromList fromEntityId
