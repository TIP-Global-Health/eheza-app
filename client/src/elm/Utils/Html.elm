module Utils.Html exposing
    ( debugView
    , divider
    , spinner
    , tabItem
    , thumbnailImage
    , viewLoading
    , viewLogo
    , viewModal
    , wrapPage
    )

import Config.Model exposing (Model)
import Gizra.Html exposing (showIf, showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, translate)


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


spinner : Html any
spinner =
    div []
        [ i [ class "icon loading spinner" ] []
        ]


tabItem : String -> Bool -> String -> msg -> Html msg
tabItem title active taId action =
    a
        [ classList [ ( "item", True ), ( "active", active ) ]
        , onClick action
        , id <| taId ++ "-tab"
        ]
        [ text title ]


thumbnailImage : String -> Maybe String -> String -> Int -> Int -> Html any
thumbnailImage subClass maybeAvatarUrl label height width =
    case maybeAvatarUrl of
        Nothing ->
            span
                [ class <| "icon-participant " ++ subClass
                , style "height" (Debug.toString height ++ "px")
                , style "width" (Debug.toString width ++ "px")
                ]
                []

        Just source ->
            img
                [ src source
                , attribute "alt" label
                , style "height" (Debug.toString height ++ "px")
                , style "width" (Debug.toString width ++ "px")
                , class <| "photo-participant " ++ subClass
                ]
                []


{-| Just show a generic loading indicator, for cases that will resolve soon,
where we don't need to show any progress.
-}
viewLoading : Html any
viewLoading =
    div
        [ class "ui basic segment" ]
        [ spinner ]


{-| Takes some HTML with a "modal" class, and puts it in an overlay
which dims the background and centers the modal vertically in the
viewport.

Or, if nothing, shows an emptyNode.

-}
viewModal : Maybe (Html msg) -> Html msg
viewModal =
    showMaybe << Maybe.map (\modal -> div [ class "overlay" ] [ modal ])


{-| Take some HTML and wrap it in some standard HTML to make a page that
works with our CSS. Meant for cases where we just want to show a quick
error message or something where we don't have a full design.
-}
wrapPage : List (Html a) -> Html a
wrapPage html =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic segment" ]
            html
        ]


viewLogo : Language -> Html any
viewLogo language =
    let
        appName =
            translate language Translate.AppName
    in
    div
        [ class "logo" ]
        [ img
            [ alt appName
            , class "img-logo"
            , height 245
            , width 245
            , src "assets/images/logo-app.svg"
            ]
            []
        , br [] []
        , text appName
        ]
