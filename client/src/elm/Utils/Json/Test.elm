module Utils.Json.Test exposing (all)

import EverySet
import Expect
import Json.Decode exposing (Decoder, andThen, decodeString, fail, string, succeed)
import Test exposing (Test, describe, test)
import Utils.Json exposing (decodeEverySet, decodeListDroppingUnknown)


{-| A small enum decoder standing in for the real clinical enum decoders (e.g.
`decodeDangerSign`/`decodePrenatalDiagnosis`): it recognizes a fixed set of
tokens and `fail`s on anything else, exactly like the `_ -> fail ...` branch
they all use.
-}
decodeSign : Decoder String
decodeSign =
    string
        |> andThen
            (\s ->
                case s of
                    "vaginal-bleeding" ->
                        succeed s

                    "convulsions" ->
                        succeed s

                    "fever" ->
                        succeed s

                    _ ->
                        fail <| s ++ " is not a recognized sign"
            )


{-| Decode through `decodeListDroppingUnknown`, preserving element order.
-}
decodeAsList : String -> Result Json.Decode.Error (List String)
decodeAsList json =
    decodeString (decodeListDroppingUnknown decodeSign) json


{-| Decode through `decodeEverySet`; sort the result so assertions don't depend
on `EverySet`'s internal iteration order (a set has no order, and comparing the
`AssocList` backing is itself order-sensitive, so sorting is the robust choice).
-}
decodeAsSet : String -> Result Json.Decode.Error (List String)
decodeAsSet json =
    decodeString (decodeEverySet decodeSign) json
        |> Result.map (EverySet.toList >> List.sort)


all : Test
all =
    describe "Utils.Json tolerant decoders"
        [ describe "decodeEverySet"
            [ test "keeps all recognized values" <|
                \_ ->
                    decodeAsSet """["vaginal-bleeding","convulsions"]"""
                        |> Expect.equal (Ok [ "convulsions", "vaginal-bleeding" ])
            , test "drops only the unknown element and keeps the recognized ones" <|
                -- The regression at the heart of #1829: previously a single
                -- unknown value collapsed the WHOLE set to empty.
                \_ ->
                    decodeAsSet """["vaginal-bleeding","some-future-sign","convulsions"]"""
                        |> Expect.equal (Ok [ "convulsions", "vaginal-bleeding" ])
            , test "an all-unknown list decodes to empty rather than failing" <|
                \_ ->
                    decodeAsSet """["future-a","future-b"]"""
                        |> Expect.equal (Ok [])
            , test "an empty list decodes to the empty set" <|
                \_ ->
                    decodeAsSet "[]"
                        |> Expect.equal (Ok [])
            , test "a non-list value falls back to the empty set" <|
                \_ ->
                    decodeAsSet "null"
                        |> Expect.equal (Ok [])
            ]
        , describe "decodeListDroppingUnknown"
            [ test "keeps all recognized values, in order" <|
                \_ ->
                    decodeAsList """["vaginal-bleeding","convulsions"]"""
                        |> Expect.equal (Ok [ "vaginal-bleeding", "convulsions" ])
            , test "drops only the unrecognized element and keeps the rest" <|
                -- #1833: an unknown value must be dropped, not coerced into a
                -- "no-finding" sentinel.
                \_ ->
                    decodeAsList """["vaginal-bleeding","some-future-sign","convulsions"]"""
                        |> Expect.equal (Ok [ "vaginal-bleeding", "convulsions" ])
            , test "an all-unknown list decodes to []" <|
                \_ ->
                    decodeAsList """["future-a","future-b"]"""
                        |> Expect.equal (Ok [])
            , test "a non-list value falls back to []" <|
                \_ ->
                    decodeAsList "null"
                        |> Expect.equal (Ok [])
            ]
        ]
