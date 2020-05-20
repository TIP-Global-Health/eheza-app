module Pages.Device.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import DataManager.Model exposing (SyncData)
import Date
import Device.Model exposing (..)
import Gizra.Html exposing (emptyNode, showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Device.Model exposing (..)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Time
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| We organize our DataManager by health center. However, there is also a bunch
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
                |> Dict.get nodesUuid
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
    let
        viewDateTime time =
            let
                normalize number =
                    if number < 10 then
                        "0" ++ String.fromInt number

                    else
                        String.fromInt number

                year =
                    Time.toYear Time.utc time |> String.fromInt

                month =
                    Time.toMonth Time.utc time
                        |> Translate.ResolveMonth
                        |> translate language

                day =
                    Time.toDay Time.utc time |> normalize

                hour =
                    Time.toHour Time.utc time |> normalize

                minute =
                    Time.toMinute Time.utc time |> normalize

                second =
                    Time.toSecond Time.utc time |> normalize
            in
            day ++ " " ++ month ++ " " ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"

        viewAttempt attempt =
            case attempt of
                DataManager.Model.NotAsked ->
                    "NotAsked"

                DataManager.Model.Downloading _ _ ->
                    "Downloading"

                DataManager.Model.Uploading _ ->
                    "Uploading"

                DataManager.Model.Failure time error ->
                    "Failure " ++ viewDateTime time ++ " " ++ Debug.toString error

                DataManager.Model.Success ->
                    "Success"

        ( lastSuccessfulContact, remainingForDownload ) =
            data.downloadStatus
                |> Maybe.map
                    (\downloadStatus ->
                        ( viewDateTime downloadStatus.lastSuccessfulContact, downloadStatus.remaining |> String.fromInt )
                    )
                |> Maybe.withDefault ( "NA", "NA" )

        remainingForUpload =
            data.uploadStatus
                |> Maybe.map (.remaining >> String.fromInt)
                |> Maybe.withDefault "NA"
    in
    div [ class "general-status" ]
        [ div [] [ text <| translate language Translate.LastSuccesfulContactLabel ++ ": " ++ lastSuccessfulContact ]
        , div [] [ text <| translate language Translate.RemainingForUploadLabel ++ ": " ++ remainingForUpload ]
        , div [] [ text <| translate language Translate.RemainingForDownloadLabel ++ ": " ++ remainingForDownload ]
        , div [] [ text <| translate language Translate.StatusLabel ++ ": " ++ viewAttempt data.attempt ]
        ]


viewHealthCenters : Language -> ModelIndexedDb -> Html Msg
viewHealthCenters language db =
    db.healthCenters
        |> RemoteData.map
            (\data ->
                data
                    |> Dict.toList
                    |> List.sortBy (Tuple.second >> .name)
                    |> List.map (viewHealthCenter language db)
                    |> div [ class "health-centers" ]
            )
        |> RemoteData.withDefault spinner


viewHealthCenter : Language -> ModelIndexedDb -> ( HealthCenterId, HealthCenter ) -> Html Msg
viewHealthCenter language db ( uuid, model ) =
    let
        sync =
            db.syncData
                |> RemoteData.map
                    (\syncData ->
                        case Dict.get uuid syncData of
                            Just data ->
                                div [ class "health-center-info" ]
                                    [ viewSyncData language data
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
            , error
            ]
        ]
