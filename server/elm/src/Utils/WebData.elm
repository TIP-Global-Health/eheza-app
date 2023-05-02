module Utils.WebData exposing
    ( viewError
    , whenNotAsked
    )

import App.Types exposing (Language)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (decodeString)
import RemoteData exposing (..)
import Translate as Trans exposing (TranslationId(..), translate)


{-| Get Error message as `String`.
-}
errorString : Language -> Http.Error -> String
errorString language error =
    case error of
        Http.BadUrl _ ->
            translate language <| HttpError Trans.ErrorBadUrl

        Http.BadPayload message _ ->
            translate language <| HttpError <| Trans.ErrorBadPayload message

        Http.NetworkError ->
            translate language <| HttpError Trans.ErrorNetworkError

        Http.Timeout ->
            translate language <| HttpError Trans.ErrorTimeout

        Http.BadStatus response ->
            translate language <|
                HttpError <|
                    Trans.ErrorBadStatus <|
                        case decodeString (Json.Decode.field "title" Json.Decode.string) response.body of
                            Ok err ->
                                err

                            Err _ ->
                                response.status.message


{-| Provide some `Html` to view an error message.
-}
viewError : Language -> Http.Error -> Html any
viewError language error =
    div [ class "alert alert-danger" ] [ text <| errorString language error ]


{-| Return `Just msg` if we're `NotAsked`, otherwise `Nothing`. Sort of the
opposite of `map`. We use this in order to kick off some process if we're
`NotAsked`, but not otherwise.
-}
whenNotAsked : msg -> RemoteData e a -> Maybe msg
whenNotAsked msg data =
    case data of
        NotAsked ->
            Just msg

        _ ->
            Nothing
