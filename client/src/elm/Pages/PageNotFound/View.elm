module Pages.PageNotFound.View exposing (view)

import Html exposing (a, div, h2, text, Html)
import Html.Attributes exposing (class, href)
import Translate as Trans exposing (translate, Language)


view : Language -> String -> Html a
view language url =
    -- TODO: Mention the url. Also, needs some links and styling
    div [ class "ui segment center aligned" ]
        [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
        ]
