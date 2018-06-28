module Pages.MyAccount.View exposing (view)

import Html exposing (Html, a, div, h2, i, img, p, text)
import Html.Attributes exposing (class, href, src)
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)


view : Language -> User -> Html a
view language user =
    div [ class "ui centered card" ]
        [ div [ class "image" ] [ img [ src user.avatarUrl ] [] ]
        , div [ class "content" ]
            [ div [ class "header" ] [ text <| translate language <| Trans.WelcomeUser user.name ]
            ]
        ]
