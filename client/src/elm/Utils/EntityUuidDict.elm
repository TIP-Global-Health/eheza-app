module Utils.Dict exposing (Dict, empty, fromList, singleton)

import AssocList as Dict exposing (Dict)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid)


{-| An `Dict` is like a `Dict`, but the key can be any
`EntityUUID`-based type. It's actually a specialization of `Dict.. As a
convenience, you can use `empty`, `singleton` and `fromList` to construct an
`Dict`. Once you've done that, you can use all the other `Dict.
methods in the usual way -- they are not repeated here.
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
