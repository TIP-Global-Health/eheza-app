module Pages.Device.View exposing (view)

import App.Model
import App.Utils exposing (getLoggedInData)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import Device.Model exposing (..)
import EverySet
import Gizra.Html exposing (showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra
import Pages.Device.Model exposing (..)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (DownloadPhotosMode(..), DownloadPhotosStatus(..), SyncInfoAuthorityZipper, SyncInfoGeneral, SyncInfoStatus, SyncStatus(..))
import SyncManager.Utils exposing (syncInfoStatusToString)
import Time
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner)


{-| We call this if we have an active service worker. If the device is authorized,
we show its status. Otherwise, we show a UI that allows for authorization.
-}
view : Language -> WebData Device -> App.Model.Model -> Model -> Html Msg
view language device app model =
    div [ class "wrap wrap-alt-2" ]
        [ div [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.DeviceStatus ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage PinCodePage
                ]
                [ span [ class "icon-back" ] [] ]
            ]
        , div [ class "ui segment" ]
            [ viewDeviceStatus language device app model
            ]
        ]


viewDeviceStatus : Language -> WebData Device -> App.Model.Model -> Model -> Html Msg
viewDeviceStatus language device app model =
    case device of
        Success _ ->
            div [ class "device-status" ]
                [ button
                    [ classList
                        [ ( "ui fluid primary button", True )
                        , ( "disabled", app.syncManager.syncStatus /= SyncManager.Model.SyncIdle )
                        ]
                    , onClick <| MsgSyncManager SyncManager.Model.TrySyncing
                    ]
                    [ text <| translate language Translate.TrySyncing ]
                , viewStorageStatus language app
                , div [ class "general-sync" ]
                    [ h2 [] [ text <| translate language Translate.SyncGeneral ]
                    , viewSyncInfo language app.syncManager.syncInfoGeneral
                    , viewPhotosTransferInfo language app.syncManager.syncStatus app.syncManager.downloadPhotosStatus
                    ]
                , viewHealthCenters language app
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
        |> Maybe.Extra.values
        |> ul [ class "storage-dashboard" ]


viewSyncInfo : Language -> { a | lastFetchedRevisionId : Int, lastSuccesfulContact : Int, remainingToUpload : Int, remainingToDownload : Int, status : SyncInfoStatus } -> Html Msg
viewSyncInfo language info =
    let
        viewDateTime time =
            if Time.posixToMillis time == 0 then
                translate language Translate.Never

            else
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
                            |> Translate.ResolveMonth True
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

        lastSuccessfulContact =
            viewDateTime (Time.millisToPosix info.lastSuccesfulContact)
    in
    div [ class "sync-status" ]
        [ div [] [ text <| translate language Translate.LastSuccesfulContactLabel ++ ": " ++ lastSuccessfulContact ]
        , div [] [ text <| translate language Translate.RemainingForUploadLabel ++ ": " ++ String.fromInt info.remainingToUpload ]
        , div [] [ text <| translate language Translate.RemainingForDownloadLabel ++ ": " ++ String.fromInt info.remainingToDownload ]
        , div [] [ text <| translate language Translate.StatusLabel ++ ": " ++ syncInfoStatusToString info.status ]
        ]


viewPhotosTransferInfo : Language -> SyncStatus -> DownloadPhotosStatus -> Html Msg
viewPhotosTransferInfo language syncStatus status =
    let
        statusHtml =
            case syncStatus of
                SyncUploadPhotoAuthority _ _ ->
                    div [] [ text <| translate language Translate.Uploading ]

                _ ->
                    case status of
                        DownloadPhotosIdle ->
                            div [] [ text <| translate language Translate.IdleWaitingForSync ]

                        DownloadPhotosInProcess DownloadPhotosNone ->
                            div [] [ text <| translate language Translate.Disabled ]

                        DownloadPhotosInProcess (DownloadPhotosBatch rect) ->
                            let
                                remaining =
                                    case rect.indexDbRemoteData of
                                        RemoteData.Success (Just result) ->
                                            String.fromInt result.remaining

                                        _ ->
                                            ""
                            in
                            div []
                                [ text <|
                                    String.fromInt (rect.batchCounter + 1)
                                        ++ " / "
                                        ++ String.fromInt rect.batchSize
                                        ++ " , "
                                , text <|
                                    translate language Translate.RemainingForDownloadLabel
                                        ++ ": "
                                        ++ remaining
                                ]

                        DownloadPhotosInProcess (DownloadPhotosAll rect) ->
                            let
                                remaining =
                                    case rect.indexDbRemoteData of
                                        RemoteData.Success (Just result) ->
                                            String.fromInt result.remaining

                                        _ ->
                                            ""
                            in
                            div []
                                [ text <|
                                    translate language Translate.RemainingForDownloadLabel
                                        ++ ": "
                                        ++ remaining
                                ]
    in
    div
        [ class "transfer-photos" ]
        [ h2 [] [ text <| translate language Translate.PhotosTransferStatus ]
        , statusHtml
        ]


viewHealthCenters : Language -> App.Model.Model -> Html Msg
viewHealthCenters language app =
    getLoggedInData app
        |> Maybe.map
            (\( _, loggedInModel ) ->
                let
                    allowedHealthCenters =
                        Tuple.second loggedInModel.nurse
                            |> .healthCenters
                in
                app.indexedDb.healthCenters
                    |> RemoteData.map
                        (Dict.toList
                            >> List.filter (\( healthCenterId, _ ) -> EverySet.member healthCenterId allowedHealthCenters)
                            >> List.sortBy (Tuple.second >> .name)
                            >> List.map (viewHealthCenter language app.syncManager.syncInfoAuthorities)
                            >> div [ class "health-centers" ]
                        )
                    |> RemoteData.withDefault spinner
            )
        |> Maybe.withDefault
            (button
                [ class "ui fluid primary button"
                , onClick <| SetActivePage PinCodePage
                ]
                [ text <| translate language <| Translate.LoginPhrase Translate.LoginToSyncHealthCenters ]
            )


viewHealthCenter : Language -> SyncInfoAuthorityZipper -> ( HealthCenterId, HealthCenter ) -> Html Msg
viewHealthCenter language zipper ( healthCenterId, healthCenter ) =
    let
        viewNotSyncedHealthCenter uuid =
            button
                [ class "ui button"
                , onClick <| MsgSyncManager <| SyncManager.Model.RevisionIdAuthorityAdd uuid
                ]
                [ text <| translate language Translate.StartSyncing ]

        viewSyncedAuthority authorityInfo =
            div [ class "health-center-info" ]
                [ viewSyncInfo language authorityInfo
                , button
                    [ class "ui button"
                    , onClick <| MsgSyncManager <| SyncManager.Model.RevisionIdAuthorityRemove (toEntityUuid authorityInfo.uuid)
                    ]
                    [ text <| translate language Translate.StopSyncing ]
                ]

        content =
            zipper
                |> Maybe.map
                    (Zipper.toList
                        >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid healthCenterId)
                        >> Maybe.map viewSyncedAuthority
                        >> Maybe.withDefault (viewNotSyncedHealthCenter healthCenterId)
                    )
                |> Maybe.withDefault (viewNotSyncedHealthCenter healthCenterId)
    in
    div [ class "health-center" ]
        [ h2 [] [ text <| healthCenter.name ]
        , content
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
