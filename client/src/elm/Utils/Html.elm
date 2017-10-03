module Utils.Html
    exposing
        ( debugView
        , divider
        , tabItem
        , thumbnailImage
        )

import Config.Model exposing (Model)
import Gizra.Html exposing (showIf)
import Html exposing (Html, a, div, h5, img, span, text)
import Html.Attributes exposing (attribute, class, classList, id, src, style)
import Html.Events exposing (onClick)
import Participant.Model exposing (ParticipantType(..))


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


thumbnailImage : ParticipantType -> String -> String -> Int -> Int -> Html any
thumbnailImage participantType source label height width =
    let
        subClass =
            case participantType of
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
