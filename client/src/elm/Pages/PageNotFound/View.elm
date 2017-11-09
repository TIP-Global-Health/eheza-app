module Pages.PageNotFound.View exposing (view, viewPage)

import Html exposing (a, div, h2, h4, text, Html)
import Html.Attributes exposing (class, href)
import Pages.Page exposing (Page(..))
import Translate as Trans exposing (translate, Language)


{-| Shows page not found message for a URL we could not interpret.
-}
view : Language -> String -> Html a
view language url =
    div [ class "ui segment center aligned" ]
        [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
        , h4 [] [ text url ]
        ]


{-| Shows page not found message where we could interpret the URL,
but the `Page` doesn't exist.
-}
viewPage : Language -> Page -> Html a
viewPage language page =
    div [ class "ui segment center aligned" ]
        [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
        , h4 [] [ text <| translate language <| Trans.ActivePage page ]
        ]
