module Config.View exposing (..)

import Html exposing (div, h2, text, Html)
import Html.Attributes exposing (class)
import Translate as Trans exposing (translate, Language)


-- A plain function that always returns the error message


view : Language -> Html msg
view language =
    div
        [ class "config-error" ]
        [ h2 [] [ text <| translate language Trans.ErrorConfigurationError ]
        , div [] [ text <| translate language Trans.ErrorCheckLocalConfig ]
        ]
