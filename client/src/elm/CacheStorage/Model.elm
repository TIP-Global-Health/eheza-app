module CacheStorage.Model exposing (Model, Msg(..), Request(..), Response(..), Url, cachePhotos, checkCachedPhotos, clearCachedPhotos, emptyModel)

{-| Wraps access to the Javascript `CacheStorage` APIs.

It might be nice, at some point, to actually do a proper implementation of the
`CacheStorage` APIs as a native Elm module ... this would allow more of the
actual logic to be in Elm. For the moment, we'll just define what we
actually want here, and use ports to implement.

-}

import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..), WebData)


type alias Url =
    String


type alias Model =
    -- Which URLs have we successfully cached? If `Loading`, there is a
    -- request in progress to modify the cache.
    { cachedPhotos : WebData (List Url)
    }


emptyModel : Model
emptyModel =
    { cachedPhotos = NotAsked
    }


type Msg
    = SendRequest Request
    | HandleResponse Value


cachePhotos : List Url -> Msg
cachePhotos =
    SendRequest << CachePhotos


clearCachedPhotos : Msg
clearCachedPhotos =
    SendRequest ClearCachedPhotos


checkCachedPhotos : Msg
checkCachedPhotos =
    SendRequest CheckCachedPhotos


type Request
    = CachePhotos (List Url)
    | CheckCachedPhotos
    | ClearCachedPhotos


type Response
    = SetCachedPhotos (List Url)
