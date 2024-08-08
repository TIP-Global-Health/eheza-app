module Pages.PageNotFound.View exposing (view)

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
