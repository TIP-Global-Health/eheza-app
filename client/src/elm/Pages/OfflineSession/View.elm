module Pages.OfflineSession.View exposing (view)

{-| This is the entry point for interacting with a Session for
data-entry while offline.
-}

import Backend.Entities exposing (..)
import Backend.Session.Model exposing (OfflineSession)
import Drupal.Restful exposing (Entity)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.OfflineSession.Model exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Translate exposing (Language)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


view : Language -> WebData (Maybe (Entity SessionId OfflineSession)) -> Html Msg
view language data =
    div
        [ class "ui full segment" ]
        [ h3 [] [ text "Offline Session" ]
        , viewData language data
        ]


viewData : Language -> WebData (Maybe (Entity SessionId OfflineSession)) -> Html Msg
viewData language data =
    case data of
        NotAsked ->
            p []
                [ text "We haven't loaded an offline session" ]

        Loading ->
            spinner

        Failure err ->
            viewError language err

        Success offlineSession ->
            viewOfflineSession language offlineSession


viewOfflineSession : Language -> Maybe (Entity SessionId OfflineSession) -> Html Msg
viewOfflineSession language data =
    case data of
        Just session ->
            p [] [ text "We have a session" ]

        Nothing ->
            p [] [ text "There was no session" ]
