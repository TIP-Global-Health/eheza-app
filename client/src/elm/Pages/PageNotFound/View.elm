module Pages.PageNotFound.View exposing (view, viewPage)

import App.Model exposing (Msg(..))
import Html exposing (a, div, h2, h4, text, button, Html)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..))
import Translate as Trans exposing (translate, Language)


{-| Shows page not found message for a URL we could not interpret.
-}
view : Language -> String -> Html Msg
view language url =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui segment center aligned" ]
            [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
            , h4 [] [ text url ]
            , button
                [ class "ui fluid button"
                , onClick <| SetActivePage LoginPage
                ]
                [ text <| translate language Trans.GoHome ]
            ]
        ]


{-| Shows page not found message where we could interpret the URL,
but the `Page` doesn't exist.
-}
viewPage : Language -> Page -> Html Msg
viewPage language page =
    div
        [ class "wrap wrap-alt-2" ]
        [ div [ class "ui segment center aligned" ]
            [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
            , h4 [] [ text <| translate language <| Trans.ActivePage page ]
            , button
                [ class "ui fluid button"
                , onClick <| SetActivePage LoginPage
                ]
                [ text <| translate language Trans.GoHome ]
            ]
        ]
