module Utils.Json exposing
    ( decodeArray2
    , decodeEmptyArrayAsEmptyDict
    , decodeError
    , decodeEverySet
    , decodeNullAsEmptyArray
    )

import AssocList as Dict exposing (Dict)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, decodeString, dict, fail, field, float, index, int, list, map, map2, nullable, oneOf, string, succeed, value)


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
                    fail <| "Expected an empty array, not an array with length: " ++ Debug.toString length
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
                        fail <| "Expected Null, not an array with length: " ++ (Debug.toString <| List.length res)
            )


{-| Given a decoder, decodes a JSON list of that type, and then
turns it into an `EverySet`.
-}
decodeEverySet : Decoder a -> Decoder (EverySet a)
decodeEverySet =
    map EverySet.fromList << list
