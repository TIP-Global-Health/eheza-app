module Utils.Json exposing
    ( decodeDate
    , decodeEmptyArrayAsEmptyDict
    , decodeError
    , decodeEverySet
    , decodeListAsDict
    , decodeListAsDictByProperty
    , decodeListAsIntDict
    , decodeListAsIntDictByProperty
    , decodeNullAsEmptyArray
    )

import Date exposing (Date)
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, float, index, int, list, map, map2, nullable, oneOf, string, succeed, value)
import Json.Decode.Extra exposing (date)


decodeDate : Decoder Date
decodeDate =
    string
        |> andThen (\val -> date)


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


decodeListAsDict : Decoder a -> Decoder (Dict String a)
decodeListAsDict decoder =
    decodeListAsDictByProperty "id" decodeInt decoder Debug.toString


decodeListAsIntDict : Decoder a -> Decoder (Dict Int a)
decodeListAsIntDict decoder =
    decodeListAsIntDictByProperty "id" decodeInt decoder identity


decodeListAsDictByProperty : String -> Decoder a -> Decoder v -> (a -> comparable) -> Decoder (Dict String v)
decodeListAsDictByProperty property keyDecoder valDecoder stringFunc =
    list (map2 (\a b -> ( a, b )) (field property keyDecoder) valDecoder)
        |> andThen
            (\valList ->
                List.map (\( id, value ) -> ( stringFunc id, value )) valList
                    |> Dict.fromList
                    |> succeed
            )


decodeListAsIntDictByProperty : String -> Decoder a -> Decoder v -> (a -> comparable) -> Decoder (Dict Int v)
decodeListAsIntDictByProperty property keyDecoder valDecoder stringFunc =
    list (map2 (\a b -> ( a, b )) (field property keyDecoder) valDecoder)
        |> andThen
            (\valList ->
                List.map (\( id, value ) -> ( stringFunc id, value )) valList
                    |> Dict.fromList
                    |> succeed
            )


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
