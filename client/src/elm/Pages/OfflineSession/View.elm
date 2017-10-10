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
import RemoteData exposing (WebData)
import Translate exposing (Language)


view : Language -> WebData (Maybe (Entity SessionId OfflineSession)) -> Html Msg
view language data =
    div
        [ class "ui full segment" ]
        [ text "Offline Session" ]
