module Pages.Device.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Device.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Device.Model exposing (..)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)


{-| We organize our SyncManager by health center. However, there is also a bunch
of nodes that we get no matter which health center we're interesting in. So,
this is the "magic" UUID that represents "all the health centers" (or, "no
health center", depending on how you look at it).
-}
nodesUuid : HealthCenterId
nodesUuid =
    toEntityUuid "78cf21d1-b3f4-496a-b312-d8ae73041f09"


{-| We call this if we have an active service worker. If the device is authorized,
we show its status. Otherwise, we show a UI that allows for authorization.
-}
view : Language -> WebData Device -> App.Model.Model -> Model -> Html Msg
view language device app model =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.DeviceStatus ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage PinCodePage
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui segment" ]
            [ viewDeviceStatus language device app model
            ]
        ]


viewDeviceStatus : Language -> WebData Device -> App.Model.Model -> Model -> Html Msg
viewDeviceStatus language device app model =
    case device of
        Success _ ->
            div [ class "device-status" ]
                [ viewStorageStatus language app
                ]

        _ ->
            viewPairingForm language device model


viewStorageStatus : Language -> App.Model.Model -> Html Msg
viewStorageStatus language app =
    let
        viewPersistent persistent =
            li [ class "persistence" ]
                [ text <| translate language <| Translate.PersistentStorage persistent ]

        viewMemoryQuota quota =
            li [ class "memory" ]
                [ text <| translate language <| Translate.MemoryQuota quota ]

        viewStorageQuota quota =
            li [ class "storage" ]
                [ text <| translate language <| Translate.StorageQuota quota ]
    in
    [ Maybe.map viewStorageQuota app.storageQuota
    , Maybe.map viewPersistent app.persistentStorage
    , Maybe.map viewMemoryQuota app.memoryQuota
    ]
        |> List.filterMap identity
        |> ul [ class "storage-dashboard" ]


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
                    , class "pairing-code"
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
            ]
        ]
