module Error.View exposing (view, viewError)

import Error.Model exposing (Error, ErrorType(..))
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Json.Encode
import Translate as Trans exposing (Language, translate)
import Utils.WebData


view : Language -> List Error -> Html msg
view language errors =
    if List.isEmpty errors then
        emptyNode

    else
        details
            [ class "ui segment elm-errors alert debug-errors"
            , style "min-height" "100px"
            , style "max-height" "500px"
            , style "overflow" "auto"
            , property "open" (Json.Encode.bool True)
            ]
            [ summary [] [ text "Error log" ]
            , div [] [ text "Ordered descending (first error is the newest one)" ]
            , ol [] (List.map (viewError language) (List.reverse errors))
            ]


viewError : Language -> Error -> Html msg
viewError language error =
    let
        apply str =
            li []
                [ text <| error.module_ ++ "." ++ error.location ++ ": "
                , str
                ]
    in
    case error.error of
        Decoder err ->
            Json.Decode.errorToString err
                |> text
                |> apply

        Http err ->
            Utils.WebData.viewError language err
                |> apply

        Plain txt ->
            text txt
                |> apply
