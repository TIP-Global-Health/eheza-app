module Utils.Json exposing
    ( decodeArray2
    , decodeEmptyArrayAsEmptyDict
    , decodeError
    , decodeEverySet
    , decodeNullAsEmptyArray
    , decodeWithDefault
    , encodeIfExists
    )

import AssocList as Dict exposing (Dict)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, decodeString, dict, fail, field, float, index, int, list, map, map2, nullable, oneOf, string, succeed, value)
import Json.Encode exposing (Value)


decodeArray2 : Decoder k -> Decoder v -> Decoder (Dict k v)
decodeArray2 keyDecoder valueDecoder =
    Json.Decode.map2 (\k v -> ( k, v )) keyDecoder valueDecoder
        |> Json.Decode.list
        |> Json.Decode.map Dict.fromList


decodeEmptyArrayAsEmptyDict : Decoder (Dict.Dict k v)
decodeEmptyArrayAsEmptyDict =
    list value
        |> andThen
            (\list ->
                let
                    length =
                        List.length list
                in
                if length == 0 then
                    succeed Dict.empty

                else
                    fail <| "Expected an empty array, not an array with length: " ++ String.fromInt length
            )


decodeError : Decoder String
decodeError =
    field "title" string


decodeNullAsEmptyArray : Decoder (List a)
decodeNullAsEmptyArray =
    (nullable <| list value)
        |> andThen
            (\val ->
                case val of
                    Nothing ->
                        succeed []

                    Just res ->
                        fail <| "Expected Null, not an array with length: " ++ (String.fromInt <| List.length res)
            )


{-| Given a decoder, decodes a JSON list of that type, and then
turns it into an `EverySet`.
-}
decodeEverySet : Decoder a -> Decoder (EverySet a)
decodeEverySet decoder =
    oneOf
        [ map EverySet.fromList <| list decoder
        , succeed EverySet.empty
        ]


decodeWithDefault : a -> Decoder a -> Decoder a
decodeWithDefault default decoder =
    oneOf [ decoder, succeed default ]


encodeIfExists : String -> Maybe a -> (a -> Value) -> List ( String, Value )
encodeIfExists name maybeVal encoder =
    maybeVal
        |> Maybe.map (\val -> [ ( name, encoder val ) ])
        |> Maybe.withDefault []
