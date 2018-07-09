module Restful.Endpoint
    exposing
        ( (</>)
        , AccessToken
        , Backend
        , BackendUrl
        , CrudOperations
        , CrudRequest
        , EndPoint
        , EntityId
        , EntityUuid
        , Offset
        , QueryResult
        , Range
        , ReadOnly
        , ReadOnlyEndPoint
        , ReadWrite
        , ReadWriteEndPoint
        , TokenStrategy
        , applyAccessToken
        , applyBackendUrl
        , backend
        , decodeEntityId
        , decodeEntityUuid
        , decodeSingleDrupalEntity
        , delete
        , drupalBackend
        , drupalEndpoint
        , encodeEntityId
        , encodeEntityUuid
        , endpoint
        , fromEntityId
        , fromEntityUuid
        , get
        , getMany
        , modifyRequest
        , patch
        , patchAny
        , patchFull
        , post
        , put
        , select
        , selectRange
        , toCmd
        , toCmdMaybe
        , toEntityId
        , toEntityUuid
        , toTask
        , toTaskMaybe
        , tokenHeader
        , tokenUrlParam
        , withAccessToken
        , withBackend
        , withCountDecoder
        , withCreatedEncoder
        , withDrupalCountDecoder
        , withDrupalItems
        , withErrorDecoder
        , withItems
        , withKeyEncoder
        , withManyKeys
        , withOffsetParam
        , withParamsEncoder
        , withPath
        , withPlainItems
        , withRangeParam
        , withTokenStrategy
        , withValueEncoder
        , withoutDecoder
        )

{-| These functions and types are intended to facilitate CRUD operations upon
backend entities exposed through a Restful HTTP API.


## Backends

@docs Backend, backend, drupalBackend
@docs withItems, withDrupalItems, withPlainItems
@docs withCountDecoder, withDrupalCountDecoder
@docs withOffsetParam, withRangeParam
@docs withManyKeys
@docs withTokenStrategy


## Endpoints

@docs ReadOnly, ReadWrite
@docs EndPoint, ReadOnlyEndPoint, ReadWriteEndPoint
@docs drupalEndpoint, endpoint
@docs withBackend, withKeyEncoder, withValueEncoder, withCreatedEncoder, withParamsEncoder, withErrorDecoder, withPath


## Access Tokens

@docs AccessToken, TokenStrategy, tokenHeader, tokenUrlParam


## CRUD Operations

@docs BackendUrl, Offset, Range, QueryResult
@docs get, getMany, select, selectRange, patch, patchAny, patchFull, post, put, delete
@docs CrudOperations, applyBackendUrl, applyAccessToken


# Requests

@docs CrudRequest, withAccessToken, withoutDecoder, modifyRequest
@docs toTask, toTaskMaybe, toCmd, toCmdMaybe


## EntityId

@docs EntityId, decodeEntityId, encodeEntityId, fromEntityId, toEntityId


## EntityUuid

@docs EntityUuid, decodeEntityUuid, encodeEntityUuid, fromEntityUuid, toEntityUuid


## Helpers

@docs (</>), decodeSingleDrupalEntity

-}

import Gizra.Json exposing (decodeInt)
import Http exposing (Error(..), expectJson)
import HttpBuilder exposing (..)
import Json.Decode as JD exposing (Decoder, field, index, list)
import Json.Encode exposing (Value)
import Task exposing (Task)


{-| The base URL for a backend (i.e. the part that doesn't vary from
one endpoint to another).
-}
type alias BackendUrl =
    String


{-| An access token.
-}
type alias AccessToken =
    String


{-| A type which flags an `EndPoint` as being read-only.
-}
type ReadOnly
    = ReadOnly


{-| A type which flags an `EndPoint` as being read-only.
-}
type ReadWrite
    = ReadWrite


{-| The `EndPoint` type represents a Restful JSON endpoint, with at least
a path for URLs, and a decoder for the JSON it emits.

The structure is somewhat specialized for a headless Drupal backend using its
Restful module. However, it should be adaptable for use in other REST
environments (let us know if any changes are needed to handle your case).

The type parameters have the following significance.

| Type | Significance |
| ---- | ------------ |
| `writeable` | A type which flags whether this endpoint is read-only or read-write. |
| `error` | Your error type. <p>If you don't want to do something special with errors, then it can just be `Http.Error` |
| `key` | Your ID type. We usually use some kind of `EntityId`, but you can use something else if you like. |
| `value` | Your value type. |
| `created` | The type you would use in POST requests, when creating a new value. May be missing some information from `value` which the backend will supply. May be the same as `value` if POST isn't special. |
| `params` | A type for the query params that this endpoint uses. <p>If your endpoint doesn't take params, just use `()` (or, a phantom type variable, if you like). |

In function signatures, we sometimes abbreviate these type parameters to something like
`EndPoint w e p k v c` ... at least, for the type parameters that aren't relevant to
that particular function. Also, we use the type aliases `ReadOnlyEndPoint` and `ReadWriteEndPoint`
to simplify some function signatures.

To create an `EndPoint`, start with `drupalEndpoint` (or `endpoint`), and then use the various
`with...` functions to customize it as needed.

To use your endpoint, see functions like `get`, `put`, `post`, `delete`, etc.

-}
type EndPoint writeable error key value created params
    = EndPoint (EndPointRec error key value created params)


type alias EndPointRec error key value created params =
    { backend : BackendRec ( key, value )
    , decodeKey : Decoder key
    , decodeValue : Decoder value
    , encodeCreatedValue : created -> Value
    , encodeParams : params -> List ( String, String )
    , encodeValue : value -> Value
    , keyToUrlPart : key -> String
    , mapError : Error -> error
    , path : String
    }


{-| A type alias which represents a read-only `EndPoint`.
-}
type alias ReadOnlyEndPoint error key value params =
    EndPoint ReadOnly error key value value params


{-| A type alias which represents a read-write `EndPoint`.
-}
type alias ReadWriteEndPoint error key value created params =
    EndPoint ReadWrite error key value created params


{-| Common configuration for endpoints connected to a particular backend.

You might wonder why the `BackendUrl` could not be specified as part of the
`Backend`, rather than asking for it with each CRUD request. The reason is
that, in our setups, the `BackendUrl` is typically provided a run-time,
whereas the rest of the information needed to construct the `Backend` (or an
`EndPoint`) is known at compile-time. So, it's convenient to construct the
`Backend` and `EndPoint` values statically, without requiring parameters.

You can pre-apply a `BackendUrl` (or `AccessToken`), once known, using
`applyBackendUrl` and `applyAccessToken`.

-}
type Backend a
    = Backend (BackendRec a)


type alias BackendRec a =
    { decodeCount : Decoder Int
    , decodeMultipleItems : Decoder a -> Decoder (List a)
    , decodeSingleItem : Decoder a -> Decoder a
    , manyKeys : List String -> String
    , offsetParam : String
    , rangeParam : String
    , tokenStrategy : TokenStrategy
    }


unwrapBackend : Backend a -> BackendRec a
unwrapBackend (Backend backend) =
    backend


{-| Constructs a default `Backend`.

This is a starting point for further configuration -- it won't
work for your backend out of the box.

For use with Drupal's Restful module, you can use `drupalBackend` (or simply
use `drupalEndpoint`).

-}
backend : Backend a
backend =
    Backend
        { decodeSingleItem = identity
        , decodeMultipleItems = JD.list
        , decodeCount = decodeDrupalCount
        , manyKeys = String.join ","
        , offsetParam = "offset"
        , rangeParam = "range"
        , tokenStrategy = tokenUrlParam "access_token"
        }


decodeDrupalCount : Decoder Int
decodeDrupalCount =
    field "count" decodeInt


{-| A `Backend` which decodes the kind of responses a Drupal backend sends
when using the Restful module.
-}
drupalBackend : Backend a
drupalBackend =
    withDrupalItems backend


{-| Specify how to unwrap items sent by the backend, before applying the
decoders for the `key` and `value`.

  - The first parameter is used for functions like `get`, which return only one
    value. So, the question is: given what the backend sends, what do we need to
    decode to get an item to which we can apply the `key` or `value` decoders?

    If the decoders actually operate on exactly what the backend returns, you
    could supply `identity`.

  - The second parameter is used for functions like `select`, which return a
    list of values. So, the question is: given what the backend sends, what do
    we need to decode to get a list of items to which we can apply the `key` or
    `value` decoders?

    If the backend actually returns just a JSON array of the things the decoders
    can handle, you could just supply `Json.Decode.list`.

For a pre-built version that handles how Drupal sends responses with its
Restful module, see `withDrupalItems`.

-}
withItems : (Decoder a -> Decoder a) -> (Decoder a -> Decoder (List a)) -> Backend b -> Backend a
withItems decodeSingleItem decodeMultipleItems (Backend backend) =
    Backend
        { backend
            | decodeSingleItem = decodeSingleItem
            , decodeMultipleItems = decodeMultipleItems
        }


{-| When doing a GET via `getMany`, for multiple keys, how do we combine the
keys for the URL?

By default, an `endpoint` combines keys using `String.join ","` ... that is, by
putting a comma between each key.

-}
withManyKeys : (List String -> String) -> Backend a -> Backend a
withManyKeys manyKeys (Backend backend) =
    Backend { backend | manyKeys = manyKeys }


{-| Unwrap items the Drupal way, as sent by its Restful module.

  - Single items are sent as the first element of a JSON array, inside a
    field called "data".

  - Multiple items are sent as a JSON array, inside a field called "data".

So, this is equivalent to something like:

    withItems
        (field "data" << index 0)
        (field "data" << list)

-}
withDrupalItems : Backend a -> Backend b
withDrupalItems =
    withItems decodeSingleDrupalEntity decodeDrupalList


{-| Unwrap items in the simplest possible way:

  - Single items are sent in a way that your decoders can handle directly.

  - Multiple items are sent as a JSON array of things your decoders can
    handle directly.

So, this is equivalent to:

    withItems identity Json.Decode.list

-}
withPlainItems : Backend a -> Backend b
withPlainItems =
    withItems identity list


{-| Given the JSON your backend returns for queries, how can we decode the
total count of all the items on the backend? (They may not all necessarily have
been returned, due to paging).
-}
withCountDecoder : Decoder Int -> Backend a -> Backend a
withCountDecoder decodeCount (Backend backend) =
    Backend { backend | decodeCount = decodeCount }


{-| Decode the item count the Drupal way, by looking at a field named `count`.
-}
withDrupalCountDecoder : Backend a -> Backend a
withDrupalCountDecoder =
    withCountDecoder decodeDrupalCount


{-| What is the name of the query parameter this backend uses to specify an
offset for queries?

By default, we use "offset".

-}
withOffsetParam : String -> Backend a -> Backend a
withOffsetParam offsetParam (Backend backend) =
    Backend { backend | offsetParam = offsetParam }


{-| What is the name of the query parameter this backend uses to specify how
many items you want at once?

By default, we use "range".

-}
withRangeParam : String -> Backend a -> Backend a
withRangeParam rangeParam (Backend backend) =
    Backend { backend | rangeParam = rangeParam }


{-| Use the supplied token strategy for this backend.

You can use `tokenHeader` or `tokenUrlParam` to construct a `TokenStrategy`.

-}
withTokenStrategy : TokenStrategy -> Backend a -> Backend a
withTokenStrategy tokenStrategy (Backend backend) =
    Backend { backend | tokenStrategy = tokenStrategy }


{-| Use the supplied backend with the endpoint.
-}
withBackend : Backend ( k, v ) -> EndPoint w e k v c p -> EndPoint w e k v c p
withBackend (Backend backend) (EndPoint endpoint) =
    EndPoint { endpoint | backend = backend }


{-| Use the supplied function to convert your `params` type into something we can feed to
`HttpBuilder.withQueryParams`. So, you get type-safety for the params!

This is how you would implement sorting or filtering, by:

  - making up a type which describes the query
  - telling us how to convert that to parameters for the URL

`endpoint` and `drupalEndpoint` both default this to `always []` (i.e. no params)

-}
withParamsEncoder : (params -> List ( String, String )) -> EndPoint w e k v c p -> EndPoint w e k v c params
withParamsEncoder encodeParams (EndPoint endpoint) =
    EndPoint { endpoint | encodeParams = encodeParams }


{-| Use the supplied function to convert your `key` type to a string we can append
to the URL for a `get` request. By default, we just use `toString`, but you may need
to something else, depending on your `key` type.
-}
withKeyEncoder : (key -> String) -> EndPoint w e key v c p -> EndPoint w e key v c p
withKeyEncoder keyToUrlPart (EndPoint endpoint) =
    EndPoint { endpoint | keyToUrlPart = keyToUrlPart }


{-| Use the supplied function to encode your `value` type when writing to the backend.

If you need a special `created` type for `POST` requests, apply `withCreatedEncoder`
after this function. Otherwise, we'll use this function for both `PUT` and `POST`.

-}
withValueEncoder : (value -> Value) -> ReadOnlyEndPoint e k value p -> ReadWriteEndPoint e k value value p
withValueEncoder encodeValue (EndPoint endpoint) =
    EndPoint
        { endpoint
            | encodeValue = encodeValue
            , encodeCreatedValue = encodeValue
        }


{-| Use the supplied function to encode new `created` values for the endpoint,
for use in POST requests.

This is for cases where some values are supplied by the backend after the
entity is created. So, they are part of your `value` type, but you can't send
them as part of a POST request.

If you don't use a special `created` type, then we'll POST with your `value`
encoder.

-}
withCreatedEncoder : (created -> Value) -> ReadWriteEndPoint e k v c p -> ReadWriteEndPoint e k v created p
withCreatedEncoder encodeCreatedValue (EndPoint endpoint) =
    EndPoint { endpoint | encodeCreatedValue = encodeCreatedValue }


{-| Use the supplied function to convert an `Http.Error` to your desired
`error` type.

By default, we use `identity` (that is, by default, our error type is
`Http.Error`). However, you may have a way to turn that into a more
helpful type.

-}
withErrorDecoder : (Error -> error) -> EndPoint w e k v c p -> EndPoint w error k v c p
withErrorDecoder mapError (EndPoint endpoint) =
    EndPoint { endpoint | mapError = mapError }


{-| Use the supplied `path` for this endpoint.

The path is appenend to whatever you supply for the `BackendUrl` for a request.

-}
withPath : String -> EndPoint w e k v c p -> EndPoint w e k v c p
withPath path (EndPoint endpoint) =
    EndPoint { endpoint | path = path }


{-| Construct a Drupal-oriented endpoint, with as many defaults filled in as possible.

  - The first parameter is the `path` to the endpoint (which will be appended to the
    `BackendUrl` you provide for requests).

  - The second parameter is a decoder for your `value` type.

Yes, just two parameters! We'll supplement that with various Drupal-oriented defaults:

  - The `key` is some kind of `EntityId`, and it can be found in an `id` field
    in the JSON.

  - Multiple values are returned as a JSON array inside a `data` field.
    But you can change that using `withBackend`.

  - Single values are returned as a single-elmeent JSON array, inside a `data` field.
    But you can change that using `withBackend`.

  - Your endpoint doesn't use any URL params.
    But you can change that using `withParamsEncoder`.

  - You're not using a custom error type.
    But you can change that using `withErrorDecoder`.

If you need a different `key` type, you can start with `endpoint` instead, and supply
`drupalBackend` to it as a parameter to get the rest of the defaults.

This initially produces a `ReadOnlyEndPoint`. To obtain a `ReadWriteEndpoint`,
apply `withValueEncoder` to the result.

-}
drupalEndpoint : String -> Decoder value -> ReadOnlyEndPoint Error (EntityId a) value p
drupalEndpoint path decodeValue =
    endpoint path (decodeDrupalId toEntityId) decodeValue drupalBackend
        |> withKeyEncoder (fromEntityId >> toString)


{-| Produces an `EndPoint` which you may then customize further.

  - The first parameter is the `path` to the endpoint (which will be appended
    to the `BackendUrl` you provide for requests).

  - The second parameter is a decoder for your `key` type.

  - The third parameter is a decoder for your `value` type.

  - The fourth parameter is your backend.

To turn this into a `ReadWriteEndpoint`, apply `withValueEncoder` to the result.

-}
endpoint : String -> Decoder key -> Decoder value -> Backend ( key, value ) -> ReadOnlyEndPoint Error key value p
endpoint path decodeKey decodeValue backend =
    EndPoint
        { backend = unwrapBackend backend
        , decodeKey = decodeKey
        , decodeValue = decodeValue
        , encodeCreatedValue = encodeEmptyObject
        , encodeParams = encodeEmptyParams
        , encodeValue = encodeEmptyObject
        , keyToUrlPart = toString
        , mapError = identity
        , path = path
        }


encodeEmptyParams : p -> List ( String, String )
encodeEmptyParams p =
    []


encodeEmptyObject : a -> Value
encodeEmptyObject a =
    Json.Encode.object []


{-| We can use two strategies to send an `AccessToken` to the backend -- either an
HTTP header (with a key and value), or a param for the URL (with key and value).
Use `tokenHeader` or `tokenUrlParam` to construct.
-}
type TokenStrategy
    = TokenHeader String
    | TokenUrlParam String


{-| Send an `AccessToken` to the backend using the specified HTTP header.
-}
tokenHeader : String -> TokenStrategy
tokenHeader =
    TokenHeader


{-| Send an `AccessToken` to the backend using the specified parameter in the URL.
-}
tokenUrlParam : String -> TokenStrategy
tokenUrlParam =
    TokenUrlParam


{-| Appends the second parameter to the first, joining them with a "/", but avoiding "//".

    "http://www.apple.com"  </> "path"  --> "http://www.apple.com/path"

    "http://www.apple.com"  </> "/path" --> "http://www.apple.com/path"

    "http://www.apple.com/" </> "path"  --> "http://www.apple.com/path"

    "http://www.apple.com/" </> "/path" --> "http://www.apple.com/path"

-}
(</>) : String -> String -> String
(</>) left right =
    case ( String.endsWith "/" left, String.startsWith "/" right ) of
        ( False, False ) ->
            left ++ "/" ++ right

        ( True, True ) ->
            left ++ String.dropLeft 1 right

        _ ->
            left ++ right


decodeItemList : EndPoint w e k v c p -> Decoder (List ( k, v ))
decodeItemList (EndPoint endpoint) =
    JD.map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.backend.decodeMultipleItems


expectMany : EndPoint w e k v c p -> RequestBuilder a -> RequestBuilder (List ( k, v ))
expectMany ((EndPoint endpoint) as ep) =
    decodeItemList ep
        |> expectJson
        |> withExpect


expectMultiple : EndPoint w e k v c p -> p -> Int -> RequestBuilder a -> RequestBuilder (QueryResult k v p)
expectMultiple ((EndPoint endpoint) as ep) params offset =
    JD.map2 (QueryResult params offset) (decodeItemList ep) endpoint.backend.decodeCount
        |> expectJson
        |> withExpect


expectSingle : EndPoint w e k v c p -> RequestBuilder a -> RequestBuilder ( k, v )
expectSingle (EndPoint endpoint) =
    JD.map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.backend.decodeSingleItem
        |> expectJson
        |> withExpect


expectSingleWithKey : EndPoint w e k v c p -> k -> RequestBuilder a -> RequestBuilder v
expectSingleWithKey (EndPoint endpoint) key =
    JD.map2 (,) (JD.succeed key) endpoint.decodeValue
        |> endpoint.backend.decodeSingleItem
        |> JD.map Tuple.second
        |> expectJson
        |> withExpect


{-| A type representing a CRUD request. The `err` type is the kind of result you'll
get back for errors. The `ok` type is the kind of result you'll get back if the
request succeeds.

  - You can construct requests with `select`, `get`, etc.

  - You can use requests via `toTask` or `toCmd`.

-}
type CrudRequest err ok
    = CrudRequest (Error -> err) TokenStrategy (RequestBuilder ok)


{-| Supply an `AccessToken` to be used with the request.
-}
withAccessToken : AccessToken -> CrudRequest err ok -> CrudRequest err ok
withAccessToken token ((CrudRequest _ strategy _) as req) =
    let
        func =
            case strategy of
                TokenHeader header ->
                    withHeader header token

                TokenUrlParam param ->
                    withQueryParams [ ( param, token ) ]
    in
    modifyRequest func req


{-| Despite all the fine work and careful thought which has gone into this
package, perhaps there is some modification you'd like to make to the HTTP
request before you send it. Under the hood, we're using the very fine
`RequestBuilder` from
[`lukewestby/elm-http-builder`](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/5.1.0)
to construct the HTTP requests. So, once you're got a `CrudRequest`, you
can use `modifyRequest` to alter the request. But consider filing a bug
report if it's something we could handle in the package itself!
-}
modifyRequest : (RequestBuilder a -> RequestBuilder b) -> CrudRequest e a -> CrudRequest e b
modifyRequest func (CrudRequest mapError strategy builder) =
    CrudRequest mapError strategy (func builder)


{-| Sometimes we want to send a request, but we're not interested in the
response, just whether there was an error. In that case, you can use
`withoutDecoder` to omit parsing the response.
-}
withoutDecoder : CrudRequest e a -> CrudRequest e ()
withoutDecoder =
    JD.succeed ()
        |> expectJson
        |> withExpect
        |> modifyRequest


{-| Convert a `CrudRequest` into a `Cmd`. You provide a tagger which indicates
which `Msg` should handle the result.

If you'd prefer to get a `Task`, you can use `toTask` instead.

-}
toCmd : (Result err ok -> msg) -> CrudRequest err ok -> Cmd msg
toCmd tagger request =
    Task.attempt tagger (toTask request)


{-| Like `toCmd`, but treats a 404 error specially. Instead of handling it as
an error, it is treating as a successful result, returning `Nothing`. So, the
success type is now wrapped in a `Maybe`. Other errors are still treated as an
error.
-}
toCmdMaybe : (Result err (Maybe ok) -> msg) -> CrudRequest err ok -> Cmd msg
toCmdMaybe tagger request =
    Task.attempt tagger (toTaskMaybe request)


{-| Convert a `CrudRequest` into a `Task`.

If you'd prefer to go directly to a `Cmd`, see `toCmd`.

-}
toTask : CrudRequest err ok -> Task err ok
toTask (CrudRequest mapError _ builder) =
    HttpBuilder.toTask builder
        |> Task.mapError mapError


{-| Like `toTask`, but treats a 404 error specially. Instead of handling it as
an error, it is treating as a successful result, returning `Nothing`. So, the
success type is now wrapped in a `Maybe`. Other errors are still treated as an
error.
-}
toTaskMaybe : CrudRequest err ok -> Task err (Maybe ok)
toTaskMaybe (CrudRequest mapError _ builder) =
    HttpBuilder.toTask builder
        |> Task.map Just
        |> Task.onError
            (\err ->
                case err of
                    BadStatus response ->
                        if response.status.code == 404 then
                            Task.succeed Nothing
                        else
                            Task.fail (mapError err)

                    _ ->
                        Task.fail (mapError err)
            )


{-| When we're querying the backend, our result consists of a list of items,
the count of how many items there are on the backend, and the offset on the
backend that our list starts at. We also remember what params we supplied,
since that will affect the meaning of the total and the offset.
-}
type alias QueryResult key value params =
    { params : params
    , offset : Int
    , items : List ( key, value )
    , count : Int
    }


{-| Select entities from an endpoint.
-}
select : BackendUrl -> EndPoint w error key value c params -> params -> CrudRequest error (QueryResult key value params)
select backendUrl ((EndPoint endpoint) as ep) params =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> expectMultiple ep params 0
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| A zero-based offset that you would like the backend to start from.
-}
type alias Offset =
    Int


{-| How many items would you like the backend to return?
-}
type alias Range =
    Int


withOffsetAndRange : BackendRec a -> Offset -> Maybe Range -> RequestBuilder b -> RequestBuilder b
withOffsetAndRange backend offset range =
    let
        offsetParam =
            if offset == 0 then
                Nothing
            else
                Just ( backend.offsetParam, toString offset )

        rangeParam =
            Maybe.map (\r -> ( backend.rangeParam, toString r )) range
    in
    [ offsetParam, rangeParam ]
        |> List.filterMap identity
        |> withQueryParams


{-| Like `select`, but you specify an offset and the number of items you want to fetch at once.

If you don't supply the range, we'll start at the offset, but not specify any particular number
of items to fetch.

-}
selectRange : BackendUrl -> EndPoint w error key value c params -> params -> Offset -> Maybe Range -> CrudRequest error (QueryResult key value params)
selectRange backendUrl ((EndPoint endpoint) as ep) params offset range =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> withOffsetAndRange endpoint.backend offset range
        |> expectMultiple ep params offset
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Gets a entity from the backend via its `key`.

Sometimes you'd like to treat a 404 error specially, since the request
essentially succeeded ... it's just that there was no result. To do that, you
can use `toTaskMaybe` or `toCmdMaybe` with the resulting `CrudRequest`.

-}
get : BackendUrl -> EndPoint w error key value c p -> key -> CrudRequest error value
get backendUrl ((EndPoint endpoint) as ep) key =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> expectSingleWithKey ep key
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Gets several entities from the backend via multiple `key`s.
-}
getMany : BackendUrl -> EndPoint w error key value c p -> List key -> CrudRequest error (List ( key, value ))
getMany backendUrl ((EndPoint endpoint) as ep) keys =
    urlForManyKeys backendUrl ep keys
        |> HttpBuilder.get
        |> expectMany ep
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Sends a `POST` request to create the specified value.
-}
post : BackendUrl -> ReadWriteEndPoint error key value created p -> created -> CrudRequest error ( key, value )
post backendUrl ((EndPoint endpoint) as ep) created =
    (backendUrl </> endpoint.path)
        |> HttpBuilder.post
        |> expectSingle ep
        |> withJsonBody (endpoint.encodeCreatedValue created)
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Sends a `PUT` request to create the specified value.

If you want to ignore the JSON the server sends back for the value, you
can apply `withoutDecoder` to the result.

If you want to send the full value, but use the `PATCH` HTTP method, see
`patchFull`.

-}
put : BackendUrl -> ReadWriteEndPoint error key value c p -> key -> value -> CrudRequest error value
put backendUrl ((EndPoint endpoint) as ep) key value =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> expectSingleWithKey ep key
        |> withJsonBody (endpoint.encodeValue value)
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Sends a `PATCH` request for the specified key and value, using the encoder
you supply.

  - If you want to send a `PATCH` with your usual encoder, then you can use
    `patchFull` insteed. (The result will be just like `put`, but it will use
    a PATCH request instead of PUT).

  - If you want to send an arbitrary JSON `Value` that doesn't depend on your
    `value`, then you can use `patchAny`.

If you want to ignore the JSON the server sends back for the value, you
can apply `withoutDecoder` to the result.

-}
patch : BackendUrl -> ReadWriteEndPoint error key value c p -> key -> value -> (value -> Value) -> CrudRequest error value
patch backendUrl endpoint key value encoder =
    patchAny backendUrl endpoint key (encoder value)


{-| Sends a `PATCH` request for the specified key and value, using the encoder
defined for this endpoint. So, this is just like a `put`, except it uses an
HTTP PATCH request.

  - If you want to send a `PATCH` with just some fields, you can use `patch`
    instead, and provide an alternate encoder for your `value`.

  - If you want to send an arbitrary JSON `Value` that doesn't depend on your
    `value`, then you can use `patchAny`.

If you want to ignore the JSON the server sends back for the value, you
can apply `withoutDecoder` to the result.

-}
patchFull : BackendUrl -> ReadWriteEndPoint error key value c p -> key -> value -> CrudRequest error value
patchFull backendUrl ((EndPoint endpoint) as ep) key value =
    patch backendUrl ep key value endpoint.encodeValue


{-| Sends a `PATCH` request for the specified key, using an arbitrary JSON `Value`
that you supply.

  - If you have one of your `value` entities, and an encoder, you can use
    `patch` instead.

  - If you want to send a `PATCH` with your usual encoder, then you can use
    `patchFull` insteed. (The result will be just like `put`, but it will use
    a PATCH request instead of PUT).

If you want to ignore the JSON the server sends back for the value, you
can apply `withoutDecoder` to the result.

-}
patchAny : BackendUrl -> ReadWriteEndPoint error key value c p -> key -> Value -> CrudRequest error value
patchAny backendUrl ((EndPoint endpoint) as ep) key json =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> expectSingleWithKey ep key
        |> withJsonBody json
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


{-| Delete entity.

If you want to treat a 404 result as a success, use `toTaskMaybe` or `toCmdMaybe`
on the resulting `CrudRequest`.

-}
delete : BackendUrl -> EndPoint w error key v c p -> key -> CrudRequest error ()
delete backendUrl ((EndPoint endpoint) as ep) key =
    urlForKey backendUrl ep key
        |> HttpBuilder.delete
        |> CrudRequest endpoint.mapError endpoint.backend.tokenStrategy


decodeDrupalId : (Int -> a) -> Decoder a
decodeDrupalId wrapper =
    JD.map wrapper (field "id" decodeInt)


decodeDrupalData : Decoder a -> Decoder a
decodeDrupalData =
    field "data"


{-| Given a decoder for an item, applies that decoder to the kind of
response Drupal sends for a single item (as the only element in a JSON
array in a "data" field).
-}
decodeSingleDrupalEntity : Decoder a -> Decoder a
decodeSingleDrupalEntity =
    decodeDrupalData << index 0


decodeDrupalList : Decoder a -> Decoder (List a)
decodeDrupalList =
    decodeDrupalData << list


{-| A record with alll the functions for CRUD requests, with the `BackendUrl`
partially applied (and, optionally, the `AccessToken`).

So, at the top of an `update` function, you might do something like:

    let
        ops =
            applyBackendUrl backendUrl
                |> applyAccessToken accessToken

Then, in the rest of the `update` function, you can say thtings like:

    ops.select ...

... without having to supply the `backendUrl` over and over, and without having
to say `withAccessToken accessToken` each time.

Or, your `update` function might just ask for a `CrudOperations` as a parameter,
rather than asking for a `BackendUrl` or `AccessToken` at all.

-}
type alias CrudOperations w e k v c p =
    { delete : EndPoint w e k v c p -> k -> CrudRequest e ()
    , get : EndPoint w e k v c p -> k -> CrudRequest e v
    , getMany : EndPoint w e k v c p -> List k -> CrudRequest e (List ( k, v ))
    , patch : ReadWriteEndPoint e k v c p -> k -> v -> (v -> Value) -> CrudRequest e v
    , patchAny : ReadWriteEndPoint e k v c p -> k -> Value -> CrudRequest e v
    , patchFull : ReadWriteEndPoint e k v c p -> k -> v -> CrudRequest e v
    , post : ReadWriteEndPoint e k v c p -> c -> CrudRequest e ( k, v )
    , put : ReadWriteEndPoint e k v c p -> k -> v -> CrudRequest e v
    , select : EndPoint w e k v c p -> p -> CrudRequest e (QueryResult k v p)
    , selectRange : EndPoint w e k v c p -> p -> Offset -> Maybe Range -> CrudRequest e (QueryResult k v p)
    }


{-| Returns a `CrudOperations` record that has the `BackendUrl` applied to each function.
-}
applyBackendUrl : BackendUrl -> CrudOperations w e k v c p
applyBackendUrl backendUrl =
    { delete = delete backendUrl
    , get = get backendUrl
    , getMany = getMany backendUrl
    , patch = patch backendUrl
    , patchAny = patchAny backendUrl
    , patchFull = patchFull backendUrl
    , post = post backendUrl
    , put = put backendUrl
    , select = select backendUrl
    , selectRange = selectRange backendUrl
    }


{-| Applies the given `AccessToken` to each of the functions, so you don't need
to do it every time.
-}
applyAccessToken : AccessToken -> CrudOperations w e k v c p -> CrudOperations w e k v c p
applyAccessToken accessToken ops =
    let
        addAccessToken =
            withAccessToken accessToken

        apply2 func =
            \a -> func a >> addAccessToken

        apply3 func =
            \a b -> func a b >> addAccessToken

        apply4 func =
            \a b c -> func a b c >> addAccessToken
    in
    { delete = apply2 ops.delete
    , get = apply2 ops.get
    , getMany = apply2 ops.getMany
    , patch = apply4 ops.patch
    , patchAny = apply3 ops.patchAny
    , patchFull = apply3 ops.patchFull
    , post = apply2 ops.post
    , put = apply3 ops.put
    , select = apply2 ops.select
    , selectRange = apply4 ops.selectRange
    }


{-| This is a wrapper for an `Int` id. It takes a "phantom" type variable
in order to gain type-safety about what kind of entity it is an ID for.
So, to specify that you have an id for a clinic, you would say:

    type ClinicId
        = ClinicId

    clinidId : EntityId ClinicId

-}
type EntityId a
    = EntityId Int


{-| This is how you create an `EntityId`, if you have an `Int`. You can create
any kind of `EntityId` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed `EntityIds`.
-}
toEntityId : Int -> EntityId a
toEntityId =
    EntityId


{-| This is how you get an `Int` back from a `EntityId`. You should only use
this in boundary situations, where you need to send the id out in an untyped
way. Normally, you should just pass around the `EntityId` itself, to retain
type-safety.
-}
fromEntityId : EntityId a -> Int
fromEntityId (EntityId a) =
    a


{-| Decodes a EntityId.

This just turns JSON int (or string that is an int) to an `EntityId`. You need
to supply the `field "id"` yourself, if necessary, since id's could be present
in other fields as well.

This decodes any kind of `EntityId` you like (since there is fundamentally no type
information in the JSON iself, of course). So, you need to verify that the type
is correct yourself.

-}
decodeEntityId : Decoder (EntityId a)
decodeEntityId =
    JD.map toEntityId decodeInt


{-| Encodes any kind of `EntityId` as a JSON int.
-}
encodeEntityId : EntityId a -> Value
encodeEntityId =
    Json.Encode.int << fromEntityId


{-| This is a wrapper for an UUID.
-}
type EntityUuid a
    = EntityUuid String


{-| This is how you create a `EntityUuid`, if you have a `String`. You can create
any kind of `EntityUuid` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed `EntityUuid`s.
-}
toEntityUuid : String -> EntityUuid a
toEntityUuid =
    EntityUuid


{-| This is how you get a `String` back from an `EntityUuid`. You should only use
this in boundary situations, where you need to send the UUID out in an untyped
way. Normally, you should just pass around the `EntityUuid` itself, to retain
type-safety.
-}
fromEntityUuid : EntityUuid a -> String
fromEntityUuid (EntityUuid a) =
    a


{-| Decodes an `EntityUuid`.
-}
decodeEntityUuid : Decoder (EntityUuid a)
decodeEntityUuid =
    JD.map toEntityUuid JD.string


{-| Encodes any kind of `EntityUuid` as a JSON string.
-}
encodeEntityUuid : EntityUuid a -> Value
encodeEntityUuid =
    Json.Encode.string << fromEntityUuid


urlForKey : BackendUrl -> EndPoint w e k v c p -> k -> String
urlForKey backendUrl (EndPoint endpoint) key =
    backendUrl </> endpoint.path </> endpoint.keyToUrlPart key


urlForManyKeys : BackendUrl -> EndPoint w e k v c p -> List k -> String
urlForManyKeys backendUrl (EndPoint endpoint) keys =
    let
        ids =
            List.map endpoint.keyToUrlPart keys
                |> endpoint.backend.manyKeys
    in
    backendUrl </> endpoint.path </> ids
