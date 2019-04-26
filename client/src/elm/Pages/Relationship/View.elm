module Pages.Relationship.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EveryDict
import EveryDictList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language, TranslationId, translate)


view : Language -> PersonId -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language id1 id2 db =
    div
        [ class "page-relationship" ]
        [ viewHeader language
        , div
            [ class "ui full segment blue" ]
            []
        ]


viewHeader : Language -> Html App.Model.Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.EditRelationship ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]
