module Utils.EntityIdDict exposing (EntityIdDict, empty, fromList, singleton)

import AllDict exposing (AllDict)
import Restful.Endpoint exposing (EntityId, fromEntityId)


{-| An `EntityIdDict` is like a `Dict`, but the key can be any
`EntityId`-based type. It's actually a specialization of `AllDict`. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityIdDict`. Once you've done that, you can use all the other `AllDict`
methods in the usual way -- they are not repeated here.
-}
type alias EntityIdDict k v =
    AllDict k v Int


empty : EntityIdDict (EntityId k) v
empty =
    AllDict.empty fromEntityId


singleton : EntityId k -> v -> EntityIdDict (EntityId k) v
singleton =
    AllDict.singleton fromEntityId


fromList : List ( EntityId k, v ) -> EntityIdDict (EntityId k) v
fromList =
    AllDict.fromList fromEntityId
