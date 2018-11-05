module Config.View exposing (view)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Translate as Trans exposing (Language, translate)



-- A plain function that always returns the error message


view : Language -> Html msg
view language =
    div
        [ class "config-error" ]
        [ h2 [] [ text <| translate language Trans.ErrorConfigurationError ]
        , div [] [ text <| translate language Trans.ErrorCheckLocalConfig ]
        ]
