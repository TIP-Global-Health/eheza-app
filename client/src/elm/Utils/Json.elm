module Utils.Json
    exposing
        ( decodeDate
        , decodeEmptyArrayAsEmptyDict
        , decodeError
        , decodeFloat
        , decodeInt
        , decodeIntAsString
        , decodeListAsDict
        , decodeListAsDictByProperty
        , decodeListAsIntDict
        , decodeListAsIntDictByProperty
        , decodeNullAsEmptyArray
        , decodeSingleDrupalEntity
        )

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, float, index, int, list, map, map2, nullable, oneOf, string, succeed, value)
import Json.Decode.Extra exposing (date)
import String


{-| Given a decoder for a Drupal entity, applies it to the kind of response Drupal
sends when you do a PUT, POST, or PATCH.

For instance, if you POST an entity, Drupal will send back the JSON for that entity,
as the single element of an array, then wrapped in a `data` field, e.g.:

    { data :
        [
            {
                id: 27,
                label: "The label",
                ...
            }
        ]
    }

To decode this, write a decoder for the "inner" part (the actual entity), and then
supply that as a parameter to `decodeSingleDrupalEntity`.

-}
decodeSingleDrupalEntity : Decoder a -> Decoder a
decodeSingleDrupalEntity =
    field "data" << index 0


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
                        fail <| "Expected an empty array, not an array with length: " ++ toString length
            )


decodeError : Decoder String
decodeError =
    field "title" string


decodeFloat : Decoder Float
decodeFloat =
    oneOf
        [ float
        , string
            |> andThen
                (\val ->
                    case String.toFloat val of
                        Ok int ->
                            succeed int

                        Err _ ->
                            fail "Cannot convert string to float"
                )
        ]


{-| Cast String to Int.
-}
decodeInt : Decoder Int
decodeInt =
    oneOf
        [ int
        , string
            |> andThen
                (\val ->
                    case String.toInt val of
                        Ok int ->
                            succeed int

                        Err _ ->
                            fail "Cannot convert string to integer"
                )
        ]


{-| Cast Int to String.
-}
decodeIntAsString : Decoder String
decodeIntAsString =
    oneOf
        [ string
        , int |> andThen (\val -> succeed <| toString val)
        ]


decodeListAsDict : Decoder a -> Decoder (Dict String a)
decodeListAsDict decoder =
    decodeListAsDictByProperty "id" decodeInt decoder toString


decodeListAsIntDict : Decoder a -> Decoder (Dict Int a)
decodeListAsIntDict decoder =
    decodeListAsIntDictByProperty "id" decodeInt decoder identity


decodeListAsDictByProperty : String -> Decoder a -> Decoder v -> (a -> comparable) -> Decoder (Dict String v)
decodeListAsDictByProperty property keyDecoder valDecoder stringFunc =
    list (map2 (,) (field property keyDecoder) valDecoder)
        |> andThen
            (\valList ->
                List.map (\( id, value ) -> ( stringFunc id, value )) valList
                    |> Dict.fromList
                    |> succeed
            )


decodeListAsIntDictByProperty : String -> Decoder a -> Decoder v -> (a -> comparable) -> Decoder (Dict Int v)
decodeListAsIntDictByProperty property keyDecoder valDecoder stringFunc =
    list (map2 (,) (field property keyDecoder) valDecoder)
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
                        fail <| "Expected Null, not an array with length: " ++ (toString <| List.length res)
            )
