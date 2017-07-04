module Config.View exposing (..)

import Html exposing (div, h2, text, Html)
import Html.Attributes exposing (class)
import Translate as Trans exposing (translate, Language)


-- A plain function that always returns the error message


view : Language -> Html msg
view language =
    div
        [ class "config-error" ]
        [ h2 [] [ text "Configuration error" ]
        , div [] [ text "Check your LocalConfig.elm file and make sure you have defined the enviorement properly" ]
        ]
