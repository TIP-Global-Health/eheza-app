module Utils.EntityUuidDict exposing (EntityUuidDict, empty, fromList, singleton)

import AssocList as Dict exposing (Dict)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid)


{-| An `EntityUuidDict` is like a `Dict`, but the key can be any
`EntityUUID`-based type. It's actually a specialization of `Dict.. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`EntityUuidDict`. Once you've done that, you can use all the other `Dict.
methods in the usual way -- they are not repeated here.
-}
type alias EntityUuidDict k v =
    Dict.k v String


empty : EntityUuidDict (EntityUuid k) v
empty =
    Dict.empty fromEntityUuid


singleton : EntityUuid k -> v -> EntityUuidDict (EntityUuid k) v
singleton =
    Dict.singleton fromEntityUuid


fromList : List ( EntityUuid k, v ) -> EntityUuidDict (EntityUuid k) v
fromList =
    Dict.fromList fromEntityUuid
