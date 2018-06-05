module Pages.Admin.View exposing (view)

import EverySet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import App.Model exposing (Msg(..))
import Pages.Page exposing (..)
import Translate exposing (Language, translate)
import User.Model exposing (..)


view : Language -> User -> Html Msg
view language user =
    let
        content =
            if EverySet.member Administrator user.roles then
                contentForAdmin language
            else
                contentForOthers language
    in
        div [ class "wrap wrap-alt-2" ]
            [ div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Translate.Admin ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage LoginPage
                    ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]
                ]
            , div [ class "ui basic segment" ] [ content ]
            ]


contentForAdmin : Language -> Html Msg
contentForAdmin language =
    div [] [ text "You are an admin." ]


contentForOthers : Language -> Html Msg
contentForOthers language =
    div [ class "ui basic segment" ]
        [ text <| translate language Translate.YouAreNotAnAdmin ]
