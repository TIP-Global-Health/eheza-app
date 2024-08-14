module Utils.Html exposing (..)

import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


emptyNode : Html msg
emptyNode =
    text ""


{-| Takes some HTML with a "modal" class, and puts it in an overlay
which dims the background and centers the modal vertically in the
viewport.

Or, if nothing, shows an emptyNode.

-}
viewModal : Maybe (Html msg) -> Html msg
viewModal =
    viewCustomModal []


viewCustomModal : List String -> Maybe (Html msg) -> Html msg
viewCustomModal extraClasses =
    let
        classes =
            "overlay"
                :: extraClasses
                |> String.join " "
    in
    showMaybe << Maybe.map (\modal -> div [ class classes ] [ modal ])
