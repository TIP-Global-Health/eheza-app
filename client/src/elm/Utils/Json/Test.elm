module Utils.Json.Test exposing (all)

import EverySet
import Expect
import Json.Decode exposing (Decoder, andThen, decodeString, fail, string, succeed)
import Test exposing (Test, describe, test)
import Utils.Json exposing (decodeEverySetDroppingUnknown, decodeListDroppingUnknown)


{-| A small enum decoder standing in for the real clinical enum decoders (e.g.
`decodePrenatalDiagnosis`): it recognizes a fixed set of tokens and `fail`s on
anything else, exactly like the `_ -> fail ...` branch they all use.
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


decodeAsList : String -> Result Json.Decode.Error (List String)
decodeAsList json =
    decodeString (decodeListDroppingUnknown decodeSign) json


decodeAsSet : String -> Result Json.Decode.Error (List String)
decodeAsSet json =
    decodeString (decodeEverySetDroppingUnknown decodeSign) json
        |> Result.map (EverySet.toList >> List.sort)


all : Test
all =
    describe "Utils.Json drop-unknown decoders"
        [ describe "decodeListDroppingUnknown"
            [ test "keeps all recognized values, in order" <|
                \_ ->
                    decodeAsList """["vaginal-bleeding","convulsions"]"""
                        |> Expect.equal (Ok [ "vaginal-bleeding", "convulsions" ])
            , test "drops only the unrecognized element and keeps the rest" <|
                -- The bug this fix addresses: previously an unknown value was
                -- coerced into a "no-finding" sentinel rather than dropped.
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
        , describe "decodeEverySetDroppingUnknown"
            [ test "drops the unrecognized element and keeps the recognized ones" <|
                \_ ->
                    decodeAsSet """["vaginal-bleeding","some-future-sign","convulsions"]"""
                        |> Expect.equal (Ok [ "convulsions", "vaginal-bleeding" ])
            , test "a non-list value falls back to the empty set" <|
                \_ ->
                    decodeAsSet "null"
                        |> Expect.equal (Ok [])
            ]
        ]
