module Utils.WebData exposing (sendWithHandler, viewError)

import Json.Decode exposing (Decoder, list)
import Html exposing (..)
import Http exposing (Error, expectJson)
import HttpBuilder exposing (..)
import Translate as Trans exposing (Language, translate)


{-| Provide some `Html` to view an error message.
-}
viewError : Language -> Http.Error -> Html any
viewError language error =
    case error of
        Http.BadUrl message ->
            div [] [ text <| translate language Trans.ErrorBadUrl ]

        Http.BadPayload message _ ->
            div []
                [ p [] [ text <| translate language Trans.ErrorBadPayload ]
                , p [] [ text message ]
                ]

        Http.NetworkError ->
            div [] [ text <| translate language Trans.ErrorNetworkError ]

        Http.Timeout ->
            div [] [ text <| translate language Trans.ErrorTimeout ]

        Http.BadStatus response ->
            div []
                [ div [] [ text <| translate language Trans.ErrorBadStatus ]
                , div [] [ text response.status.message ]
                ]


sendWithHandler : Decoder a -> (Result Http.Error a -> msg) -> RequestBuilder a1 -> Cmd msg
sendWithHandler decoder tagger builder =
    builder
        |> withExpect (Http.expectJson decoder)
        |> send tagger
