module Pages.PageNotFound.View exposing (view, viewPage)

import App.Model exposing (Msg(..))
import Html exposing (Html, button, div, h2, h4, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..))
import Translate as Trans exposing (Language, translate)


{-| Shows page not found message for a URL we could not interpret.
-}
view : Language -> String -> Html Msg
view language url =
    div
        [ class "wrap wrap-alt-2" ]
        [ div [ class "ui segment center aligned" ]
            [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
            , h4 [] [ text url ]
            , button
                [ class "ui fluid button"
                , onClick <| SetActivePage PinCodePage
                ]
                [ text <| translate language Trans.GoHome ]
            ]
        ]


{-| Shows page not found message where we could interpret the URL,
but the `Page` doesn't exist.

The `msg` should be a msg that goes "home" (i.e. somewhere useful).

-}
viewPage : Language -> msg -> Page -> Html msg
viewPage language msg page =
    div
        [ class "wrap wrap-alt-2" ]
        [ div [ class "ui segment center aligned" ]
            [ h2 [] [ text <| translate language Trans.PageNotFoundMsg ]
            , h4 [] [ text <| translate language <| Trans.ActivePage page ]
            , button
                [ class "ui fluid button"
                , onClick msg
                ]
                [ text <| translate language Trans.GoHome ]
            ]
        ]
