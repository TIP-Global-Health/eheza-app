module Config.View exposing (view)

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (class)
import Translate as Trans exposing (Language, translate)



-- A plain function that always returns the error message


view : Language -> String -> Html msg
view language err =
    div
        [ class "ui segment" ]
        [ h2 [] [ text <| translate language Trans.ErrorConfigurationError ]
        , p [] [ text <| translate language Trans.ErrorCheckLocalConfig ]
        , p [] [ text err ]
        ]
