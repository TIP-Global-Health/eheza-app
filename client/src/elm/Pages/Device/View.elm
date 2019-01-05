module Pages.Device.View exposing (view)

import Device.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewError)


{-| We call this if we have an active service worker. If the device is authorized,
we show its status. Otherwise, we show a UI that allows for authorization.
-}
view : Language -> WebData Device -> Html msg
view language device =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.DeviceStatus ]
            ]
        , div
            [ class "ui basic segment" ]
            [ viewContent language device
            ]
        ]


viewContent : Language -> WebData Device -> Html msg
viewContent language device =
    case device of
        NotAsked ->
            text "NotAsked"

        Loading ->
            text "Loading"

        Failure err ->
            viewError language err

        Success device ->
            text "Success"
