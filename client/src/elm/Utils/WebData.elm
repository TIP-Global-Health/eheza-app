module Utils.WebData exposing
    ( isNetworkError
    , viewError
    , viewErrorForRollbar
    , viewWebData
    )

import Html exposing (..)
import Html.Attributes exposing (class)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field)
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
        Http.BadUrl _ ->
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

                        Err _ ->
                            response.body
            in
            div []
                [ p [] [ text <| translate language <| Translate.HttpError error ]
                , p [] [ text response.status.message ]
                , p [] [ text decodedBody ]
                ]


viewErrorForRollbar : Http.Error -> String
viewErrorForRollbar error =
    case error of
        Http.BadUrl url ->
            "Http.BadUrl: " ++ url

        Http.BadPayload message _ ->
            "Http.BadPayload: " ++ message

        Http.NetworkError ->
            "Http.NetworkError"

        Http.Timeout ->
            "Http.Timeout"

        Http.BadStatus response ->
            let
                decodedBody =
                    case Json.Decode.decodeString decodeDrupalError response.body of
                        Ok decoded ->
                            decoded.title

                        Err _ ->
                            response.body
            in
            "Http.BadStatus - message: " ++ response.status.message ++ ", body: " ++ decodedBody


{-| Given:

  - some WebData
  - a function that would produce some HTML
  - a function that would wrap error HTML

... return some HTML that will use a `Success` to make HTML, show a spinner
if the webdata is loading, or show the error if it failed.

-}
viewWebData : Language -> (a -> Html msg) -> (Html msg -> Html msg) -> WebData a -> Html msg
viewWebData language view wrapError data =
    case data of
        NotAsked ->
            wrapError spinner

        Loading ->
            wrapError spinner

        Failure err ->
            wrapError
                (div
                    [ class "ui message error" ]
                    [ viewError language err ]
                )

        Success a ->
            view a


isNetworkError : Http.Error -> Bool
isNetworkError error =
    case error of
        NetworkError ->
            True

        _ ->
            False
