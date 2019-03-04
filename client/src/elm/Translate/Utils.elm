module Translate.Utils exposing (decodeLanguage, encodeLanguage, languageFromCode, languageFromString, languageToCode, selectLanguage)

{-| Utilities related to the translation types.

Actual translations can be found in `Translate.elm`

-}

import Json.Decode exposing (Decoder, fail, succeed)
import Json.Encode exposing (Value)
import Translate.Model exposing (..)


selectLanguage : Language -> TranslationSet a -> a
selectLanguage lang set =
    case lang of
        English ->
            set.english

        Kinyarwanda ->
            case set.kinyarwanda of
                Just trans ->
                    trans

                Nothing ->
                    set.english


languageFromString : String -> Result String Language
languageFromString str =
    case str of
        "English" ->
            Ok English

        "Kinyarwanda" ->
            Ok Kinyarwanda

        _ ->
            Err "Not a language"


languageFromCode : String -> Result String Language
languageFromCode str =
    case str of
        "en" ->
            Ok English

        "rw" ->
            Ok Kinyarwanda

        _ ->
            Err "Not a language"


languageToCode : Language -> String
languageToCode lang =
    case lang of
        English ->
            "en"

        Kinyarwanda ->
            "rw"


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
