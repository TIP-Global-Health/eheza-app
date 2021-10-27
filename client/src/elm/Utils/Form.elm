module Utils.Form exposing (..)

import Form exposing (..)
import Form.Error exposing (..)
import Form.Field exposing (asBool, asString)
import Form.Input exposing (..)
import Form.Validate exposing (..)
import Html exposing (Html, li, text, ul)
import Json.Decode exposing (Decoder, errorToString)
import Maybe.Extra exposing (isNothing, unwrap)
import Translate exposing (Language, ValidationError, translate)


viewFormError : Language -> ( String, ErrorValue ValidationError ) -> Html msg
viewFormError language ( path, error ) =
    li []
        [ text <| translate language <| Translate.FormField path
        , text " "
        , text <| translate language <| Translate.FormError error
        ]


{-| Possibly recover from an error.
-}
onError : (Error x -> Validation y a) -> Validation x a -> Validation y a
onError callback validation field =
    case validation field of
        Ok a ->
            Ok a

        Err err ->
            callback err field


isFormFieldSet : Form.FieldState e String -> Bool
isFormFieldSet field =
    case field.value of
        Nothing ->
            False

        Just "" ->
            False

        _ ->
            True


isFormFieldValid : Form.FieldState e String -> Bool
isFormFieldValid field =
    isNothing field.error


{-| Validates an empty field as `Nothing`. If the field is not empty, applies
the provided validation and returns a `Just` if it succeeds, and fails if it
fails.
-}
nullable : Validation e a -> Validation e (Maybe a)
nullable validation field =
    case asString field of
        Just s ->
            if String.isEmpty (String.trim s) then
                Ok Nothing

            else
                validation field
                    |> Result.map Just

        Nothing ->
            case asBool field of
                Just b ->
                    validation field
                        |> Result.map Just

                Nothing ->
                    Ok Nothing


{-| Applies a JSON Decoder to a string field. If the decoder succeeds, returns
the result of decoding. If the decoder fails, wraps the failure in the provided
error tag.
-}
fromDecoder : (String -> e) -> Maybe e -> Decoder a -> Validation e a
fromDecoder errorTag maybeRequiredTag decoder field =
    let
        decoded =
            string field

        decoderResult =
            maybeRequiredTag
                |> unwrap
                    decoded
                    -- Decoder will fail when no value is provided, and
                    -- this indicates that required field was not set.
                    (\requiredTag -> decoded |> Result.mapError (\_ -> customError requiredTag))
    in
    decoderResult
        |> Result.andThen
            (\s ->
                let
                    -- We assume we're getting a bare string, which we need to
                    -- wrap in quotes to get a valid JSON string.  If we ever
                    -- need to decode objects here, we'll need to provide a
                    -- different method, or a way to figure out whether we're
                    -- getting a string or an object.
                    json =
                        "\"" ++ String.trim s ++ "\""
                in
                Json.Decode.decodeString decoder json
                    |> Result.mapError (customError << errorTag << errorToString)
            )


{-| An `<input>` with `type=date`. The value provided must be in YYYY-MM-DD
format, and the value returned will also be in that format.
-}
dateInput : Input e String
dateInput =
    baseInput "date" Form.Field.String Text


getValueAsInt : Form.FieldState e String -> Maybe Int
getValueAsInt field =
    Maybe.andThen String.toInt field.value
