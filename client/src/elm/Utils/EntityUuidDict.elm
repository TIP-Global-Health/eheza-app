module Utils.EntityUuidDict exposing (EntityUuidDict, empty, fromList, get, singleton)

import AllDict exposing (AllDict)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid)


{-| An `EntityUuidDict` is like a `Dict`, but the key can be any
`EntityUUID`-based type. It's actually a specialization of `AllDict`. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityUuidDict`. Once you've done that, you can use all the other `AllDict`
methods in the usual way -- they are not repeated here.
-}
type alias EntityUuidDict k v =
    AllDict k v String


empty : EntityUuidDict (EntityUuid k) v
empty =
    AllDict.empty fromEntityUuid


singleton : EntityUuid k -> v -> EntityUuidDict (EntityUuid k) v
singleton =
    AllDict.singleton fromEntityUuid


fromList : List ( EntityUuid k, v ) -> EntityUuidDict (EntityUuid k) v
fromList =
    AllDict.fromList fromEntityUuid


get : EntityUuid k -> EntityUuidDict (EntityUuid k) v -> Maybe v
get k dict =
    AllDict.get k dict
