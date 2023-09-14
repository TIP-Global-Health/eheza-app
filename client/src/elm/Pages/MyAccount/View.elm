module Pages.MyAccount.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Translate as Trans exposing (Language, translate)


view : Language -> ( NurseId, Nurse ) -> Html a
view language ( nurseId, nurse ) =
    div [ class "ui centered card" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ text <| translate language <| Trans.WelcomeUser nurse.name ]
            ]
        ]
