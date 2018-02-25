module Restful.Endpoint
    exposing
        ( (</>)
        , AccessToken
        , Backend
        , BackendUrl
        , CrudRequest
        , EndPoint
        , EntityId
        , EntityUuid
        , Offset
        , Range
        , TokenStrategy
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
        , modifyRequest
        , patch
        , patch_
        , post
        , put
        , put_
        , select
        , selectRange
        , toCmd
        , toCmd404
        , toEntityId
        , toEntityUuid
        , toTask
        , toTask404
        , tokenHeader
        , tokenUrlParam
        , withAccessToken
        , withBackend
        , withCountDecoder
        , withCreatedType
        , withDrupalCountDecoder
        , withDrupalItems
        , withErrorType
        , withItems
        , withKeyType
        , withOffsetParam
        , withParamsType
        , withPath
        , withPlainItems
        , withRangeParam
        , withTokenStrategy
        , withValueType
        )

{-| These functions and types are intended to facilitate CRUD operations upon
backend entities exposed through a Restful HTTP API.


## Backends

@docs Backend, backend, drupalBackend
@docs withItems, withDrupalItems, withPlainItems
@docs withCountDecoder, withDrupalCountDecoder
@docs withOffsetParam, withRangeParam
@docs withTokenStrategy


## Endpoints

@docs EndPoint, drupalEndpoint, endpoint
@docs withBackend, withKeyType, withValueType, withParamsType, withCreatedType, withErrorType, withPath


## Access Tokens

@docs AccessToken, TokenStrategy, tokenHeader, tokenUrlParam


## CRUD Operations

@docs BackendUrl, Offset, Range
@docs get, select, selectRange, patch, patch_, post, put, put_, delete


# Requests

@docs CrudRequest, withAccessToken, modifyRequest, toTask, toTask404, toCmd, toCmd404


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


{-| This represents an idiom for dealing with Restful JSON endpoints.
The basic idea is to include in the `EndPoint` type all those things about an
endpoint which don't change. For instance, we know the path to the endpoint,
what kind of JSON it emits, etc. -- that never varies.

The structure is somewhat specialized for a headless Drupal backend using its
Restful module. However, it should be adaptable for use in other REST
environments (let us know if any changes are needed to handle your case).

The type parameters have the following significance.

| Type | Significance |
| ---- | ------------ |
| `error` | Your error type. <p>If you don't want to do something special with errors, then it can just be `Http.Error` |
| `params` | A type for the query params that this endpoint uses. <p>If your endpoint doesn't take params, just use `()` (or, a phantom type variable, if you like). |
| `key` | Your ID type. We usually use some kind of `EntityId`, but you can use something else if you like. |
| `value` | Your value type. |
| `created` | The type you would use in POST requests, when creating a new value. May be missing some information from `value` which the backend will supply. May be the same as `value` if POST isn't special. |

To create an `EndPoint`, start with `drupalEndpoint` (or `endpoint`), and then use the various
`with...` functions to customize it as needed.

-}
type EndPoint error key value created params
    = EndPoint
        -- Ideally, `decodeSingleItem` and `decodeMultipleItems` would remember that
        -- their "real" type signature is the more general:
        --
        -- , decodeMultipleItems : forall a. Decoder a -> Decoder (List a)
        -- , decodeSingleItem : forall a. Decoder a -> Decoder a
        --
        -- ... but Elm doesn't have Rank-N types, so there is no way to
        -- remember that they can operate on any type. (We could add an `a`
        -- type to `EndPoint`, but that doesn't help because the compiler would
        -- fix it as `(key, value)` anyway, through type inference.)
        --
        -- To work around that, we define `decodeMultipleItems` and `decodeSingleItem`
        -- in their more polymorphic form in a separate `Backend` type,
        -- and require that to be supplied to several configuration functions,
        -- even if unchanged.
        { decodeCount : Decoder Int
        , decodeKey : Decoder key
        , decodeMultipleItems : Decoder ( key, value ) -> Decoder (List ( key, value ))
        , decodeSingleItem : Decoder ( key, value ) -> Decoder ( key, value )
        , decodeValue : Decoder value
        , encodeCreatedValue : created -> Value
        , encodeParams : params -> List ( String, String )
        , encodeValue : value -> Value
        , keyToUrlPart : key -> String
        , mapError : Error -> error
        , offsetParam : String
        , path : String
        , rangeParam : String
        , tokenStrategy : TokenStrategy
        }


{-| Common configuration for endpoints connected to a particular backend.

You might wonder why the `BackendUrl` could not be specified as part of the
`Backend`, rather than asking for it with each CRUD request. The reason is
that, in our setups, the `BackendUrl` is typically provided a run-time,
whereas the rest of the information needed to construct the `Backend` or an
`EndPoint` is known at compile-time. So, it's convenient to construct the
`Backend` and `EndPoint` values statically, without requiring parameters.

-}
type Backend a
    = Backend
        { decodeCount : Decoder Int
        , decodeMultipleItems : Decoder a -> Decoder (List a)
        , decodeSingleItem : Decoder a -> Decoder a
        , offsetParam : String
        , rangeParam : String
        , tokenStrategy : TokenStrategy
        }


{-| Constructs a default `Backend`, which decodes responses via `withPlainResponses`.
-}
backend : Backend a
backend =
    Backend
        { decodeSingleItem = identity
        , decodeMultipleItems = JD.list
        , decodeCount = decodeDrupalCount
        , offsetParam = "offset"
        , rangeParam = "range"
        , tokenStrategy = tokenUrlParam "access_token"
        }


decodeDrupalCount : Decoder Int
decodeDrupalCount =
    field "count" decodeInt


{-| A `Backend` which decodes the kind of responses a Drupal backend sends.
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

For a pre-built version that handles how Drupal sends responses, see `withDrupalResponses`.

-}
withItems : (Decoder a -> Decoder a) -> (Decoder a -> Decoder (List a)) -> Backend b -> Backend a
withItems decodeSingleItem decodeMultipleItems (Backend backend) =
    Backend
        { backend
            | decodeSingleItem = decodeSingleItem
            , decodeMultipleItems = decodeMultipleItems
        }


{-| Unwrap items the Drupal way.

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


{-| What is the name of the query parameter this backend uses to specify an offset for queries?
-}
withOffsetParam : String -> Backend a -> Backend a
withOffsetParam offsetParam (Backend backend) =
    Backend { backend | offsetParam = offsetParam }


{-| What is the name of the query parameter this backend uses to specify how many items you want at once?
-}
withRangeParam : String -> Backend a -> Backend a
withRangeParam rangeParam (Backend backend) =
    Backend { backend | rangeParam = rangeParam }


{-| Use the supplied token strategy for this backend.

You can use `tokenHeader` or `tokenUrlParam` to construct a `TokenStrategy.

-}
withTokenStrategy : TokenStrategy -> Backend a -> Backend a
withTokenStrategy tokenStrategy (Backend backend) =
    Backend { backend | tokenStrategy = tokenStrategy }


{-| Use the supplied backend with the endpoint.
-}
withBackend : Backend ( k, v ) -> EndPoint e k v c p -> EndPoint e k v c p
withBackend (Backend backend) (EndPoint endpoint) =
    -- Ordinary, we'd store the whole backend in the `EndPoint` type. However,
    -- we're trying to avoid the extra `a` type parameter, to work around Elm's
    -- lack of Rank-N types. So, we copy what we need, specializing the
    -- actually polymorhpic `a` to our `(k, v)` type.
    EndPoint
        { endpoint
            | decodeCount = backend.decodeCount
            , decodeSingleItem = backend.decodeSingleItem
            , decodeMultipleItems = backend.decodeMultipleItems
            , offsetParam = backend.offsetParam
            , rangeParam = backend.rangeParam
            , tokenStrategy = backend.tokenStrategy
        }


{-| Use the specified `key` type with this endpoint.

The first parameter is a decoder for your `key` type, given the JSON the
backend returns for each item. If you're using a kind of `EntityId`, for the
`key`, then you can just supply `decodeEntityId`.

The second parameter helps construct the URL for cases where the `key` is
included in the URL (e.g. PUT, PATCH or DELETE). Given your `key`, what should
we put after the endpoint's `path`?

The third parameter must be provided even if it hasn't changed, for complicated
reasons that I'll blog about someday (the lack of Rank-N types).

-}
withKeyType : Decoder key -> (key -> String) -> Backend ( key, v ) -> EndPoint e k v c p -> EndPoint e key v c p
withKeyType decodeKey keyToUrlPart (Backend backend) (EndPoint endpoint) =
    EndPoint
        { endpoint
            | decodeSingleItem = backend.decodeSingleItem
            , decodeMultipleItems = backend.decodeMultipleItems
            , decodeKey = decodeKey
            , keyToUrlPart = keyToUrlPart
        }


{-| Use the specified `value` type with this endpoint.

The first parameter is a decoder for your `value` type, given the JSON the
backend returns for each item.

The second parameter is an encoder for your `value` type, for use in `PUT`
requests.

The third parameter must be provided even if it hasn't changed, for complicated
reasons that I'll blog about someday (the lack of Rank-N types).

-}
withValueType : Decoder value -> (value -> Value) -> Backend ( k, value ) -> EndPoint e k v c p -> EndPoint e k value c p
withValueType decodeValue encodeValue (Backend backend) (EndPoint endpoint) =
    EndPoint
        { endpoint
            | decodeSingleItem = backend.decodeSingleItem
            , decodeMultipleItems = backend.decodeMultipleItems
            , decodeValue = decodeValue
            , encodeValue = encodeValue
        }


{-| Use the supplied function to convert your `params` type into something we can feed to
`HttpBuilder.withQueryParams`. So, you get type-safety for the params!

`endpoint` and `drupalEndpoint` both default this to `always []` (i.e. no params)

-}
withParamsType : (params -> List ( String, String )) -> EndPoint e k v c p -> EndPoint e k v c params
withParamsType encodeParams (EndPoint endpoint) =
    EndPoint { endpoint | encodeParams = encodeParams }


{-| Use the supplied function to encode new `created` values for the endpoint,
for use in POST requests.

This is for cases where some values are supplied by the backend after the
entity is created. So, they are part of your `value` type, but you can't send
them as part of a POST request.

You can just use the same encoder as for `withValueType` if POST is not
special.

-}
withCreatedType : (created -> Value) -> EndPoint e k v c p -> EndPoint e k v created p
withCreatedType encodeCreatedValue (EndPoint endpoint) =
    EndPoint { endpoint | encodeCreatedValue = encodeCreatedValue }


{-| Use the supplied function to convert an `Http.Error` to your desired `error` type.
-}
withErrorType : (Error -> error) -> EndPoint e k v c p -> EndPoint error k v c p
withErrorType mapError (EndPoint endpoint) =
    EndPoint { endpoint | mapError = mapError }


{-| Use the supplied `path` for this endpoint.

The path is appenend to whatever you supply for the `BackendUrl` for a request.

-}
withPath : String -> EndPoint e k v c p -> EndPoint e k v c p
withPath path (EndPoint endpoint) =
    EndPoint { endpoint | path = path }


{-| Construct a Drupal-oriented endpoint, with as many defaults filled in as possible.

  - The first parameter is the `path` to the endpoint (which will be appended to the
    `BackendUrl` you provide for requests).
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

Yes, just three parameters! We'll supplement that with various Drupal-oriented defaults:

  - The `key` is some kind of `EntityId`, and it can be found in an `id` field in the JSON.
    But you can change that using `withKeyType`.

  - You create values with the full `value` type (not a partial `created` type).
    But you can change that using `withCreatedType`.

  - Multiple values are returned as a JSON array inside a `data` field.
    But you can change that using `withBackend`.

  - Single values are returned as a single-elmeent JSON array, inside a `data` field.
    But you can change that using `withBackend`.

  - Your endpoint doesn't use any URL params.
    But you can change that using `withParamsType`.

  - You're not using a custom error type.
    But you can change that using `withErrorType`.

-}
drupalEndpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error (EntityId a) value value p
drupalEndpoint path decodeValue encodeValue =
    EndPoint
        { decodeCount = decodeDrupalCount
        , decodeKey = decodeDrupalId toEntityId
        , decodeMultipleItems = decodeDrupalList
        , decodeSingleItem = decodeSingleDrupalEntity
        , decodeValue = decodeValue
        , encodeCreatedValue = encodeValue
        , encodeParams = always []
        , encodeValue = encodeValue
        , keyToUrlPart = fromEntityId >> toString
        , mapError = identity
        , offsetParam = "offset"
        , path = path
        , rangeParam = "range"
        , tokenStrategy = TokenUrlParam "access_token"
        }


{-| Produces an `EndPoint` with very basic defaults ... it will need
customization to actually work with your endpoint.

  - The first parameter is the `path` to the endpoint (which will be appended to the
    `BackendUrl` you provide for requests).
  - The second parameter is a decoder for your `value` type.
  - The third parameter is an encoder for your `value` type.

-}
endpoint : String -> Decoder value -> (value -> Value) -> EndPoint Error Int value value p
endpoint path decodeValue encodeValue =
    EndPoint
        { decodeCount = decodeDrupalCount
        , decodeKey = decodeDrupalId identity
        , decodeMultipleItems = list
        , decodeSingleItem = identity
        , decodeValue = decodeValue
        , encodeCreatedValue = encodeValue
        , encodeParams = always []
        , encodeValue = encodeValue
        , keyToUrlPart = toString
        , mapError = identity
        , offsetParam = "offset"
        , path = path
        , rangeParam = "range"
        , tokenStrategy = TokenUrlParam "access_token"
        }


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


decodeItemList : EndPoint e k v c p -> Decoder (List ( k, v ))
decodeItemList (EndPoint endpoint) =
    JD.map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.decodeMultipleItems


expectMultiple : EndPoint e key value c params -> params -> Int -> RequestBuilder a -> RequestBuilder (QueryResult key value params)
expectMultiple ((EndPoint endpoint) as ep) params offset =
    JD.map2 (QueryResult params offset) (decodeItemList ep) endpoint.decodeCount
        |> expectJson
        |> withExpect


expectSingle : EndPoint e key value c p -> RequestBuilder a -> RequestBuilder ( key, value )
expectSingle (EndPoint endpoint) =
    JD.map2 (,) endpoint.decodeKey endpoint.decodeValue
        |> endpoint.decodeSingleItem
        |> expectJson
        |> withExpect


{-| We could avoid this if Elm had Rank-N types, because in that case
`EndPoint.decodeSingleItem` could remember that it is a polymorphic function.
Without that, we need to fulfill the more specific type signature in
`Endpoint.decodeSingleItem` ... fortunately, in the cases we need that, we
actually know the key!
-}
expectSingleWithKey : EndPoint e key value c p -> key -> RequestBuilder a -> RequestBuilder value
expectSingleWithKey (EndPoint endpoint) key =
    JD.map2 (,) (JD.succeed key) endpoint.decodeValue
        |> endpoint.decodeSingleItem
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
toCmd404 : (Result err (Maybe ok) -> msg) -> CrudRequest err ok -> Cmd msg
toCmd404 tagger request =
    Task.attempt tagger (toTask404 request)


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
toTask404 : CrudRequest err ok -> Task err (Maybe ok)
toTask404 (CrudRequest mapError _ builder) =
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
select : BackendUrl -> EndPoint error key value c params -> params -> CrudRequest error (QueryResult key value params)
select backendUrl ((EndPoint endpoint) as ep) params =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> expectMultiple ep params 0
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| A zero-based offset that you would like the backend to start from.
-}
type alias Offset =
    Int


{-| How many items would you like the backend to return?
-}
type alias Range =
    Int


withOffsetAndRange : EndPoint e k v c p -> Offset -> Maybe Range -> RequestBuilder a -> RequestBuilder a
withOffsetAndRange (EndPoint endpoint) offset range =
    let
        offsetParam =
            if offset == 0 then
                Nothing
            else
                Just ( endpoint.offsetParam, toString offset )

        rangeParam =
            Maybe.map (\r -> ( endpoint.rangeParam, toString r )) range
    in
        [ offsetParam, rangeParam ]
            |> List.filterMap identity
            |> withQueryParams


{-| Like `select`, but you specify an offset and the number of items you want to fetch at once.

If you don't supply the range, we'll start at the offset, but not specify any particular number
of items to fetch.

-}
selectRange : BackendUrl -> EndPoint error key value c params -> params -> Offset -> Maybe Range -> CrudRequest error (QueryResult key value params)
selectRange backendUrl ((EndPoint endpoint) as ep) params offset range =
    HttpBuilder.get (backendUrl </> endpoint.path)
        |> withQueryParams (endpoint.encodeParams params)
        |> withOffsetAndRange ep offset range
        |> expectMultiple ep params offset
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Gets a entity from the backend via its `key`.

Sometimes you'd like to treat a 404 error specially, since the request
essentially succeeded ... it's just that there was no result. To do that, you
can use `toTask404` or `toCmd404` with the resulting `CrudRequest`.

-}
get : BackendUrl -> EndPoint error key value c p -> key -> CrudRequest error ( key, value )
get backendUrl ((EndPoint endpoint) as ep) key =
    urlForKey backendUrl ep key
        |> HttpBuilder.get
        |> expectSingle ep
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Sends a `POST` request to create the specified value.
-}
post : BackendUrl -> EndPoint error key value created p -> created -> CrudRequest error ( key, value )
post backendUrl ((EndPoint endpoint) as ep) created =
    (backendUrl </> endpoint.path)
        |> HttpBuilder.post
        |> expectSingle ep
        |> withJsonBody (endpoint.encodeCreatedValue created)
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Sends a `PUT` request to create the specified value.

Assumes that the backend will respond with the full value. If that's not true, you
can use `put_` instead.

-}
put : BackendUrl -> EndPoint error key value c p -> key -> value -> CrudRequest error value
put backendUrl ((EndPoint endpoint) as ep) key value =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> expectSingleWithKey ep key
        |> withJsonBody (endpoint.encodeValue value)
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Like `put`, but ignores any value sent by the backend back ... just interprets errors.
-}
put_ : BackendUrl -> EndPoint error key value c p -> key -> value -> CrudRequest error ()
put_ backendUrl ((EndPoint endpoint) as ep) key value =
    urlForKey backendUrl ep key
        |> HttpBuilder.put
        |> withJsonBody (endpoint.encodeValue value)
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Sends a `PATCH` request for the specified key and value.

Now, the point of a `PATCH` request is that you're not sending the **full** value,
but some subset. So, you supply your own JSON value, rather than using the one that
the endpoint would create use for PUT or POST. (We could have a separate config for
each kind of PATCH, which would contribute to type-safety, but is possibly overkill).

This function assumes that the backend will send the full value back. If it won't, then
you can use `patch_` instead.

-}
patch : BackendUrl -> EndPoint error key value c p -> key -> Value -> CrudRequest error value
patch backendUrl ((EndPoint endpoint) as ep) key value =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> expectSingleWithKey ep key
        |> withJsonBody value
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Like `patch`, but doesn't try to decode the response ... just reports errors.
-}
patch_ : BackendUrl -> EndPoint error key v c p -> key -> Value -> CrudRequest error ()
patch_ backendUrl ((EndPoint endpoint) as ep) key value =
    urlForKey backendUrl ep key
        |> HttpBuilder.patch
        |> withJsonBody value
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


{-| Delete entity.

If you want to treat a 404 result as a success, use `toTask404` or `toCmd404`
on the resulting `CrudRequest`.

-}
delete : BackendUrl -> EndPoint error key v c p -> key -> CrudRequest error ()
delete backendUrl ((EndPoint endpoint) as ep) key =
    urlForKey backendUrl ep key
        |> HttpBuilder.delete
        |> CrudRequest endpoint.mapError endpoint.tokenStrategy


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


{-| This is a wrapper for an `Int` id. It takes a "phantom" type variable
in order to gain type-safety about what kind of entity it is an ID for.
So, to specify that you have an id for a clinic, you would say:

    clinidId : EntityId ClinicId

-}
type EntityId a
    = EntityId Int


{-| This is how you create a EntityId, if you have an `Int`. You can create
any kind of `EntityId` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed EntityIds.
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

This just turns JSON int (or string that is an int) to a EntityId. You need
to supply the `field "id"` yourself, if necessary, since id's could be present
in other fields as well.

This decodes any kind of EntityId you like (since there is fundamentally no type
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


{-| This is how you create a EntityUuid, if you have a `String`. You can create
any kind of `EntityUuid` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed EntityUuids.
-}
toEntityUuid : String -> EntityUuid a
toEntityUuid =
    EntityUuid


{-| This is how you get a `String` back from a `EntityUuid`. You should only use
this in boundary situations, where you need to send the UUID out in an untyped
way. Normally, you should just pass around the `EntityUuid` itself, to retain
type-safety.
-}
fromEntityUuid : EntityUuid a -> String
fromEntityUuid (EntityUuid a) =
    a


{-| Decodes a EntityUuid.
-}
decodeEntityUuid : Decoder (EntityUuid a)
decodeEntityUuid =
    JD.map toEntityUuid JD.string


{-| Encodes any kind of `EntityUuid` as a JSON string.
-}
encodeEntityUuid : EntityUuid a -> Value
encodeEntityUuid =
    Json.Encode.string << fromEntityUuid


urlForKey : BackendUrl -> EndPoint e key v c p -> key -> String
urlForKey backendUrl (EndPoint endpoint) key =
    backendUrl </> endpoint.path </> endpoint.keyToUrlPart key
