module Utils.Html
    exposing
        ( debugView
        , divider
        , emptyNode
        , showIf
        , showMaybe
        , tabItem
        , thumbnailImage
        )

import Config.Model exposing (Model)
import Html exposing (Html, a, div, h5, img, text)
import Html.Attributes exposing (attribute, class, classList, id, src, style)
import Html.Events exposing (onClick)


{-| Produces an empty text node in the DOM.
-}
emptyNode : Html msg
emptyNode =
    text ""


{-| Conditionally show Html. A bit cleaner than using if expressions in middle
of an html block:
showIf True <| text "I'm shown"
showIf False <| text "I'm not shown"
-}
showIf : Bool -> Html msg -> Html msg
showIf condition html =
    if condition then
        html
    else
        emptyNode


{-| Show Maybe Html if Just, or empty node if Nothing.
-}
showMaybe : Maybe (Html msg) -> Html msg
showMaybe =
    Maybe.withDefault emptyNode


{-| Displays a debugging segment if debugging is enabled, otherwise renders
nothing.
-}
debugView : Model -> Html msg -> Html msg
debugView config html =
    showIf config.debug <|
        div [ class "ui tertiary segment" ]
            [ h5 [ class "ui right aligned header" ] [ text "Debug" ]
            , html
            ]


divider : Html msg
divider =
    div [ class "ui divider" ] []


tabItem : String -> Bool -> String -> msg -> Html msg
tabItem title active taId action =
    a
        [ classList [ ( "item", True ), ( "active", active ) ]
        , onClick action
        , id <| taId ++ "-tab"
        ]
        [ text title ]


thumbnailImage : String -> String -> Int -> Int -> Html any
thumbnailImage source label height width =
    img
        [ src source
        , attribute "alt" label
        , style
            [ ( "height", (toString height) ++ "px" )
            , ( "width", (toString width) ++ "px" )
            ]
        ]
        []
