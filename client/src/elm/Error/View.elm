module Error.View exposing (view)

import App.Model exposing (ConfiguredModel)
import Backend.Nurse.Model exposing (Role(..))
import Error.Model exposing (Error, ErrorType(..))
import EverySet
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Json.Encode
import RemoteData exposing (RemoteData)
import Translate exposing (Language)
import Utils.WebData


view : Language -> RemoteData String ConfiguredModel -> List Error -> Html any
view language configuredModel errors =
    RemoteData.toMaybe configuredModel
        |> Maybe.map
            (\configured ->
                let
                    nurseIsAdmin =
                        RemoteData.toMaybe configured.loggedIn
                            |> Maybe.map (.nurse >> Tuple.second >> .roles >> EverySet.member RoleAdministrator)
                            |> Maybe.withDefault False
                in
                if configured.config.debug || nurseIsAdmin then
                    -- Show Errors log when at debug, or nurse got Admin role.
                    viewErrors language errors

                else
                    emptyNode
            )
        -- When not logged in, do not show Errors log.
        |> Maybe.withDefault (viewErrors language errors)


viewErrors : Language -> List Error -> Html any
viewErrors language errors =
    if List.isEmpty errors then
        emptyNode

    else
        details
            [ class "error-log"
            , property "open" (Json.Encode.bool False)
            ]
            [ summary [] [ text "Error log" ]
            , ol [] (List.map (viewError language) (List.reverse errors))
            ]


viewError : Language -> Error -> Html any
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
