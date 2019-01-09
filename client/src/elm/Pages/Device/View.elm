module Pages.Device.View exposing (view)

import Device.Model exposing (..)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Device.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| We call this if we have an active service worker. If the device is authorized,
we show its status. Otherwise, we show a UI that allows for authorization.
-}
view : Language -> WebData Device -> Model -> Html Msg
view language device model =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.DeviceStatus ]
            ]
        , div
            [ class "ui segment" ]
            [ viewDeviceStatus language device model
            ]
        ]


viewDeviceStatus : Language -> WebData Device -> Model -> Html Msg
viewDeviceStatus language device model =
    case device of
        Success device ->
            p []
                [ text <| translate language Translate.Device
                , text " "
                , text <| toString device.id
                , text " "
                , text <| device.name
                ]

        _ ->
            viewPairingForm language device model


viewPairingForm : Language -> WebData Device -> Model -> Html Msg
viewPairingForm language device model =
    let
        isLoading =
            RemoteData.isLoading device

        ( disableSubmitButton, formAttr ) =
            if isLoading || model.code == "" then
                ( True, [] )

            else
                ( False, [ onSubmit HandlePairClicked ] )

        formState =
            case device of
                NotAsked ->
                    ""

                Loading ->
                    "loading"

                Failure _ ->
                    "error"

                Success _ ->
                    "success"

        error =
            case device of
                Failure err ->
                    div [ class "ui message error" ]
                        [ viewError language err ]

                _ ->
                    emptyNode
    in
    Html.form
        (action "javascript:void(0);" :: formAttr)
        [ div
            [ class "ui form"
            , class formState
            ]
            [ div
                [ class "ui messsage" ]
                [ text <| translate language Translate.DeviceNotAuthorized ]
            , p [] []
            , div
                [ class "ui input" ]
                [ input
                    [ placeholder <| translate language Translate.EnterPairingCode
                    , type_ "text"
                    , name "pairing-code"
                    , onInput SetCode
                    , value model.code
                    , autofocus True
                    ]
                    []
                ]
            , p [] []
            , button
                [ class "ui fluid primary button"
                , disabled disableSubmitButton
                , type_ "submit"
                ]
                [ span
                    [ hidden <| not isLoading ]
                    [ spinner ]
                , span
                    [ hidden isLoading ]
                    [ text <| translate language Translate.SubmitPairingCode ]
                ]
            , p [] []
            , error
            ]
        ]
