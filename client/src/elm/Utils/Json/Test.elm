module Utils.Json.Test exposing (all)

import EverySet
import Expect
import Json.Decode exposing (andThen, decodeString, fail, string, succeed)
import Test exposing (Test, describe, test)
import Utils.Json exposing (decodeEverySet)


{-| A small enum decoder standing in for the real clinical sign decoders
(e.g. `decodeDangerSign`): it recognizes a fixed set of tokens and fails on
anything else, exactly like the `_ -> fail ...` branch they all use.
-}
decodeSign : Json.Decode.Decoder String
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


decodeStringSet : String -> Result Json.Decode.Error (List String)
decodeStringSet json =
    decodeString (decodeEverySet decodeSign) json
        |> Result.map EverySet.toList


all : Test
all =
    describe "Utils.Json.decodeEverySet"
        [ test "keeps all recognized values" <|
            \_ ->
                decodeStringSet """["vaginal-bleeding","convulsions"]"""
                    |> Expect.equal (Ok [ "convulsions", "vaginal-bleeding" ])
        , test "drops only the unknown element and keeps the recognized ones" <|
            -- The regression at the heart of this fix: previously a single
            -- unknown value collapsed the WHOLE set to empty.
            \_ ->
                decodeStringSet """["vaginal-bleeding","some-future-sign","convulsions"]"""
                    |> Expect.equal (Ok [ "convulsions", "vaginal-bleeding" ])
        , test "an all-unknown list decodes to empty rather than failing" <|
            \_ ->
                decodeStringSet """["future-a","future-b"]"""
                    |> Expect.equal (Ok [])
        , test "an empty list decodes to the empty set" <|
            \_ ->
                decodeStringSet "[]"
                    |> Expect.equal (Ok [])
        , test "a non-list value falls back to the empty set" <|
            \_ ->
                decodeStringSet "null"
                    |> Expect.equal (Ok [])
        ]
