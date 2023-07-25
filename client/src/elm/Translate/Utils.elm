module Translate.Utils exposing (decodeLanguage, encodeLanguage, languageFromCode, languageFromString, languageToCode, selectLanguage)

{-| Utilities related to the translation types.

Actual translations can be found in `Translate.elm`

-}

import Json.Decode exposing (Decoder, fail, succeed)
import Json.Encode exposing (Value)
import Translate.Model exposing (..)


selectLanguage : Language -> TranslationSet a -> a
selectLanguage lang set =
    let
        optinal resolveSetFunc =
            resolveSetFunc set
                |> Maybe.withDefault set.english
    in
    case lang of
        English ->
            set.english

        Kinyarwanda ->
            optinal .kinyarwanda

        Kirundi ->
            optinal .kirundi


languageFromString : String -> Result String Language
languageFromString str =
    case str of
        "English" ->
            Ok English

        "Kinyarwanda" ->
            Ok Kinyarwanda

        "Kirundi" ->
            Ok Kirundi

        _ ->
            Err "Not a language"


languageFromCode : String -> Result String Language
languageFromCode str =
    case str of
        "en" ->
            Ok English

        "rw" ->
            Ok Kinyarwanda

        "bu" ->
            Ok Kirundi

        _ ->
            Err "Not a language"


languageToCode : Language -> String
languageToCode lang =
    case lang of
        English ->
            "en"

        Kinyarwanda ->
            "rw"

        Kirundi ->
            "bu"


decodeLanguage : Decoder Language
decodeLanguage =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case languageFromCode s of
                    Ok language ->
                        succeed language

                    Err err ->
                        fail err
            )


encodeLanguage : Language -> Value
encodeLanguage =
    languageToCode >> Json.Encode.string
