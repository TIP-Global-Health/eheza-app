module Pages.Utils exposing (viewNameFilter)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, translate)


viewNameFilter : Language -> String -> (String -> msg) -> Html msg
viewNameFilter language currentFilter setFilterMsg =
    let
        normalizedFilter =
            currentFilter
                |> String.toLower
                |> String.trim
    in
    div
        [ class "ui action input small" ]
        [ input
            [ placeholder <| translate language Translate.FilterByName
            , type_ "text"
            , onInput setFilterMsg
            , value currentFilter
            ]
            []
        , button
            [ classList
                [ ( "ui button primary", True )
                , ( "disabled", String.isEmpty normalizedFilter )
                ]
            , onClick <| setFilterMsg ""
            ]
            [ text <| translate language Translate.ShowAll ]
        ]
