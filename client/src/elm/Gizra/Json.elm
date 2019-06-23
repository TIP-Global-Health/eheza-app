module Gizra.Json exposing
    ( encodeDict, encodeAnyDict, decodeIntDict
    , decodeInt, decodeIntToString, decodeFloat
    , decodeEmptyArrayAs
    , decodeJsonInString
    )

{-| Utilities for dealing with JSON.


## Dictionaries

@docs encodeDict, encodeAnyDict, decodeIntDict


## Numbers

@docs decodeInt, decodeIntToString, decodeFloat


## Arrays

@docs decodeEmptyArrayAs


## Strings

@docs decodeJsonInString

-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (dict2)
import Json.Encode exposing (object)
import String


{-| Given a function which encodes a dict's values, encode the `Dict` as a JSON
object, using the dict's keys as the keys for the JSON object.
-}
encodeDict : (v -> Value) -> Dict String v -> Value
encodeDict =
    encodeAnyDict identity


{-| Like `encodeDict`, but you also supply a way of turning the keys into
strings.
-}
encodeAnyDict : (comparable -> String) -> (v -> Value) -> Dict comparable v -> Value
encodeAnyDict keyFunc valueFunc =
    Dict.toList
        >> List.map (\( key, value ) -> ( keyFunc key, valueFunc value ))
        >> object


{-| Decodes an `Int`, but if given a `String`, will convert it to an `Int`
if possible.

    import Json.Decode exposing (..)
    import Result exposing (Result(..), mapError)

    decodeString decodeInt """ "7" """ --> Ok 7

    decodeString decodeInt """ 7 """ --> Ok 7

    """ "not an int" """
        |> decodeString decodeInt
        |> mapError (always "")
    --> Err ""

-}
decodeInt : Decoder Int
decodeInt =
    oneOf
        [ int
        , string
            |> andThen
                (\s ->
                    case String.toInt s of
                        Just value ->
                            succeed value

                        Nothing ->
                            fail "Not an integer"
                )
        ]


{-| Decodes a `String`, but if given an `Int` will convert it to a `String`.

    decodeString decodeIntToString """ "7" """ --> Ok "7"

    decodeString decodeIntToString """ 7 """ --> Ok "7"

    decodeString decodeIntToString """ "other string" """ --> Ok "other string"

-}
decodeIntToString : Decoder String
decodeIntToString =
    oneOf
        [ string
        , int |> andThen (\v -> succeed (String.fromInt v))
        ]


{-| Decodes a `Float` -- but, if given a `String`, will convert it to a
`Float` if possible.

    decodeString decodeFloat """ "7.1" """ --> Ok 7.1

    decodeString decodeFloat """ 7.1 """ --> Ok 7.1

    """ "not a float" """
        |> decodeString decodeFloat
        |> mapError (always "")
    --> Err ""

-}
decodeFloat : Decoder Float
decodeFloat =
    oneOf
        [ float
        , string
            |> andThen
                (\s ->
                    case String.toFloat s of
                        Just value ->
                            succeed value

                        Nothing ->
                            fail "Not a float"
                )
        ]


{-| Given a decoder for the values, decode a dictionary that has integer keys.
The resulting decoder will fail if any of the keys can't be converted to an `Int`.

    import Dict

    """ { "27" : "value" } """
        |> decodeString (decodeIntDict string)
    --> Ok <| Dict.fromList [ (27, "value") ]

-}
decodeIntDict : Decoder value -> Decoder (Dict Int value)
decodeIntDict =
    dict2 decodeInt


{-| If given an empty array, decodes it as the given value. Otherwise, fail.

    """ [] """
        |> decodeString (decodeEmptyArrayAs "empty")
    --> Ok "empty"

    """ [27] """
        |> decodeString (decodeEmptyArrayAs "empty")
        |> mapError (always "error")
    --> Err "error"

-}
decodeEmptyArrayAs : a -> Decoder a
decodeEmptyArrayAs default =
    list value
        |> andThen
            (\list ->
                let
                    length =
                        List.length list
                in
                if length == 0 then
                    succeed default

                else
                    fail <| "Expected an empty array, not an array with length: " ++ String.fromInt length
            )


{-| This is for JSON which is embedded as a string value. Given a decoder, this
will produce a decoder that first decodes a string, and then run the supplied
decoder on that JSON string.

So, in the example below, note how the `[27, 32]` is wrapped as a string, rather
than being "normal" JSON. With `decodeJsonInString`, we can "unwrap" it.

    """ { "wrappedJson": "[27, 32]" } """
        |> decodeString (field "wrappedJson" (decodeJsonInString (list int)))
    --> Ok [27, 32]

-}
decodeJsonInString : Decoder a -> Decoder a
decodeJsonInString decoder =
    string
        |> andThen
            (\s ->
                case decodeString decoder s of
                    Ok a ->
                        succeed a

                    Err err ->
                        fail <| errorToString err
            )
