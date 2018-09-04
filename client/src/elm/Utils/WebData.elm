module Utils.WebData exposing (resetError, resetSuccess, sendWithHandler, viewError, viewOrFetch, whenNotAsked)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson)
import HttpBuilder exposing (..)
import Json.Decode exposing (Decoder, field, list)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)


{-| Represents the "body" sent by Drupal with an HTTP error.
-}
type alias DrupalError =
    { type_ : String
    , title : String
    , status : Int
    }


decodeDrupalError : Decoder DrupalError
decodeDrupalError =
    Json.Decode.map3 DrupalError
        (field "type" Json.Decode.string)
        (field "title" Json.Decode.string)
        (field "status" Json.Decode.int)


{-| Provide some `Html` to view an error message.
-}
viewError : Language -> Http.Error -> Html any
viewError language error =
    case error of
        Http.BadUrl message ->
            div [] [ text <| translate language <| Translate.HttpError error ]

        Http.BadPayload message _ ->
            div []
                [ p [] [ text <| translate language <| Translate.HttpError error ]
                , p [] [ text message ]
                ]

        Http.NetworkError ->
            div [] [ text <| translate language <| Translate.HttpError error ]

        Http.Timeout ->
            div [] [ text <| translate language <| Translate.HttpError error ]

        Http.BadStatus response ->
            let
                decodedBody =
                    case Json.Decode.decodeString decodeDrupalError response.body of
                        Ok decoded ->
                            decoded.title

                        Err err ->
                            response.body
            in
            div []
                [ p [] [ text <| translate language <| Translate.HttpError error ]
                , p [] [ text response.status.message ]
                , p [] [ text decodedBody ]
                ]


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


{-| Given:

  - some WebData
  - a messages that would kick off a fetch
  - a function that would produce some HTML
  - a function that would wrap error HTML

... return some HTML that will use a `Success` to make HTML, or provide
some HTML to kick off the fetch.

-}
viewOrFetch : Language -> msg -> (a -> List (Html msg)) -> (List (Html msg) -> List (Html msg)) -> WebData a -> List (Html msg)
viewOrFetch language fetch view wrapError data =
    case data of
        NotAsked ->
            wrapError
                [ div
                    [ class "ui message" ]
                    [ div
                        [ class "ui primary button"
                        , onClick fetch
                        ]
                        [ text <| translate language Translate.Fetch ]
                    ]
                ]

        Loading ->
            wrapError
                [ spinner ]

        Failure err ->
            wrapError
                [ div
                    [ class "ui message error" ]
                    [ viewError language err ]
                , button
                    [ class "ui primary button"
                    , onClick fetch
                    ]
                    [ text <| translate language Translate.Retry ]
                ]

        Success a ->
            view a


resetError : RemoteData e a -> RemoteData e a
resetError data =
    case data of
        Failure _ ->
            NotAsked

        _ ->
            data


resetSuccess : RemoteData e a -> RemoteData e a
resetSuccess data =
    case data of
        Success _ ->
            NotAsked

        _ ->
            data


sendWithHandler : Decoder a -> (Result Http.Error a -> msg) -> RequestBuilder a1 -> Cmd msg
sendWithHandler decoder tagger builder =
    builder
        |> withExpect (Http.expectJson decoder)
        |> send tagger
