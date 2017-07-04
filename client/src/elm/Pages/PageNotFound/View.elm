module Pages.PageNotFound.View exposing (view)

import Html exposing (a, div, h2, text, Html)
import Html.Attributes exposing (class, href)
import Translate as Trans exposing (translate, Language)


-- VIEW


view : Language -> Html a
view language =
    div [ class "ui segment center aligned" ]
        [ h2 [] [ text "Sorry, nothing found in this URL." ]
        ]
