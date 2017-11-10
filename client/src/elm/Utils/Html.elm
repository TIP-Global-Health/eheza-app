module Utils.Html
    exposing
        ( debugView
        , divider
        , viewModal
        , spinner
        , tabItem
        , thumbnailImage
        )

import Config.Model exposing (Model)
import Gizra.Html exposing (showIf, showMaybe)
import Html exposing (Html, a, div, h5, img, span, text, i)
import Html.Attributes exposing (attribute, class, classList, id, src, style)
import Html.Events exposing (onClick)
import Participant.Model exposing (Participant(..))


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


thumbnailImage : Participant -> String -> String -> Int -> Int -> Html any
thumbnailImage participant source label height width =
    let
        subClass =
            case participant of
                ParticipantMother _ ->
                    "mother"

                ParticipantChild _ ->
                    "child"
    in
        if String.isEmpty source then
            span
                [ class <| "icon-participant " ++ subClass
                , style
                    [ ( "height", (toString height) ++ "px" )
                    , ( "width", (toString width) ++ "px" )
                    ]
                ]
                []
        else
            img
                [ src source
                , attribute "alt" label
                , style
                    [ ( "height", (toString height) ++ "px" )
                    , ( "width", (toString width) ++ "px" )
                    ]
                , class <| "photo-participant" ++ subClass
                ]
                []


{-| Takes some HTML with a "modal" class, and puts it in an overlay
which dims the background and centers the modal vertically in the
viewport.

Or, if nothing, shows an emptyNode.

-}
viewModal : Maybe (Html msg) -> Html msg
viewModal =
    showMaybe << Maybe.map (\modal -> div [ class "overlay" ] [ modal ])
