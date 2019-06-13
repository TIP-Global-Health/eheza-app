module Pages.Device.View exposing (view)

import AllDictList
import App.Model
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import Backend.SyncData.Model exposing (SyncData)
import Device.Model exposing (..)
import Gizra.Html exposing (emptyNode, showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Device.Model exposing (..)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| We organize our SyncData by health center. However, there is also a bunch
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
        Success device ->
            div [ class "device-status" ]
                [ button
                    [ class "ui fluid primary button"
                    , onClick TrySyncing
                    ]
                    [ text <| translate language Translate.TrySyncing ]
                , viewStorageStatus language app
                , viewNodes language app.indexedDb
                , viewHealthCenters language app.indexedDb
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


viewNodes : Language -> ModelIndexedDb -> Html Msg
viewNodes language db =
    case db.syncData of
        Success syncData ->
            syncData
                |> AllDictList.get nodesUuid
                |> Maybe.map
                    (\data ->
                        div [ class "general-sync" ]
                            [ h2 [] [ text <| translate language Translate.SyncGeneral ]
                            , viewSyncData language data
                            ]
                    )
                |> showMaybe

        Failure err ->
            viewError language err

        Loading ->
            spinner

        NotAsked ->
            spinner


viewSyncData : Language -> SyncData -> Html Msg
viewSyncData language data =
    div [ class "general-status" ] [ text <| toString data ]


viewHealthCenters : Language -> ModelIndexedDb -> Html Msg
viewHealthCenters language db =
    db.healthCenters
        |> RemoteData.map
            (\data ->
                data
                    |> AllDictList.sortBy .name
                    |> AllDictList.map (viewHealthCenter language db)
                    |> AllDictList.values
                    |> div [ class "health-centers" ]
            )
        |> RemoteData.withDefault spinner


viewHealthCenter : Language -> ModelIndexedDb -> HealthCenterId -> HealthCenter -> Html Msg
viewHealthCenter language db uuid model =
    let
        sync =
            db.syncData
                |> RemoteData.map
                    (\syncData ->
                        case AllDictList.get uuid syncData of
                            Just data ->
                                div [ class "health-center-info" ]
                                    [ text <| toString data
                                    , button
                                        [ class "ui button"
                                        , onClick (SetSyncing uuid False)
                                        ]
                                        [ text <| translate language Translate.StopSyncing ]
                                    ]

                            Nothing ->
                                button
                                    [ class "ui button"
                                    , onClick (SetSyncing uuid True)
                                    ]
                                    [ text <| translate language Translate.StartSyncing ]
                    )
                |> RemoteData.toMaybe
                |> showMaybe
    in
    div [ class "health-center" ]
        [ h2 [] [ text <| model.name ]
        , sync
        ]


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
