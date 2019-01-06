module Utils.Form exposing (isFormFieldSet, isFormFieldValid, onError)

import Form exposing (..)
import Form.Error exposing (..)
import Form.Validate exposing (..)
import Maybe.Extra exposing (isNothing)


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
