module Pages.MyAccount.View exposing (view)

import Html exposing (a, div, h2, i, p, text, img, Html)
import Html.Attributes exposing (class, href, src)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (..)


-- VIEW


view : Language -> WebData User -> Html a
view language user =
    let
        ( name, avatar ) =
            case user of
                Success val ->
                    ( val.name, img [ src val.avatarUrl ] [] )

                _ ->
                    ( "", div [] [] )
    in
        div [ class "ui centered card" ]
            [ div [ class "image" ] [ avatar ]
            , div [ class "content" ]
                [ div [ class "header" ] [ text <| translate language <| Trans.WelcomeUser name ]
                ]
            ]
