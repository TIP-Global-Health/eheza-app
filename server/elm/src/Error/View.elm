module Error.View exposing (view)

import App.Types exposing (Language)
import Error.Model exposing (Error, ErrorType(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Utils.Html exposing (emptyNode)
import Utils.WebData


view : Language -> List Error -> Html msg
view language errors =
    if List.isEmpty errors then
        emptyNode

    else
        div [ class "elm-errors alert debug-errors" ]
            [ ul [] (List.map (viewError language) errors)
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
