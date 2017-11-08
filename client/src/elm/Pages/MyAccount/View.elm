module Pages.MyAccount.View exposing (view)

import Html exposing (a, div, h2, i, p, text, img, Html)
import Html.Attributes exposing (class, href, src)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (..)


view : Language -> User -> Html a
view language user =
    div [ class "ui centered card" ]
        [ div [ class "image" ] [ img [ src user.avatarUrl ] [] ]
        , div [ class "content" ]
            [ div [ class "header" ] [ text <| translate language <| Trans.WelcomeUser user.name ]
            ]
        ]
