module Utils.WebData exposing (sendWithHandler, resetError, viewError, viewOrFetch, whenNotAsked)

import Json.Decode exposing (Decoder, list)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Http exposing (Error, expectJson)
import HttpBuilder exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)


{-| Provide some `Html` to view an error message.
-}
viewError : Language -> Http.Error -> Html any
viewError language error =
    case error of
        Http.BadUrl message ->
            div [] [ text <| translate language Translate.ErrorBadUrl ]

        Http.BadPayload message _ ->
            div []
                [ p [] [ text <| translate language Translate.ErrorBadPayload ]
                , p [] [ text message ]
                ]

        Http.NetworkError ->
            div [] [ text <| translate language Translate.ErrorNetworkError ]

        Http.Timeout ->
            div [] [ text <| translate language Translate.ErrorTimeout ]

        Http.BadStatus response ->
            div []
                [ div [] [ text <| translate language Translate.ErrorBadStatus ]
                , div [] [ text response.status.message ]
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


sendWithHandler : Decoder a -> (Result Http.Error a -> msg) -> RequestBuilder a1 -> Cmd msg
sendWithHandler decoder tagger builder =
    builder
        |> withExpect (Http.expectJson decoder)
        |> send tagger
