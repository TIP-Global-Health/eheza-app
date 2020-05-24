module DataManager.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import DataManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , DownloadPhotos(..)
        , DownloadPhotosBatchRec
        , DownloadSyncResponse
        , Model
        , Msg(..)
        , RevisionIdPerAuthorityZipper
        , SyncStatus(..)
        )
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Json.Encode
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra exposing (isJust)
import RemoteData exposing (WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, translate)
import Url
import Utils.Html exposing (spinner)
import Utils.WebData


view : Language -> ModelIndexedDb -> Model -> Html Msg
view language db model =
    let
        htmlContent =
            details [ property "open" (Json.Encode.bool True) ]
                [ viewSyncStatusControl model

                -- button [ onClick <| DataManager.Model.FetchFromIndexDb DataManager.Model.IndexDbQueryHealthCenters ] [ text "Fetch Health Centers" ]
                , div [] [ text <| "Sync status: " ++ Debug.toString model.syncStatus ]
                , case model.syncStatus of
                    SyncDownloadGeneral webData ->
                        viewSyncDownloadGeneral model webData

                    SyncDownloadAuthority webData ->
                        viewSyncDownloadAuthority model webData

                    SyncDownloadPhotos (DownloadPhotosBatch deferredPhoto) ->
                        viewDownloadPhotosBatch model deferredPhoto

                    _ ->
                        emptyNode
                ]
    in
    div []
        [ pre [ class "ui segment", style "min-height" "240px" ] [ htmlContent ]
        , viewHealthCentersForSync language db model
        ]


viewSyncStatusControl : Model -> Html Msg
viewSyncStatusControl model =
    div []
        [ input
            [ type_ "checkbox"
            , checked model.syncStatusRotateAutomatic
            , onCheck SetSyncStatusRotateAutomatic
            ]
            []
        , label [] [ text "Automatic Sync status control" ]
        ]


viewSyncDownloadGeneral : Model -> WebData (DownloadSyncResponse BackendGeneralEntity) -> Html Msg
viewSyncDownloadGeneral model webData =
    div []
        [ div [] [ text <| "Trying to fetch `General` from revision ID " ++ String.fromInt model.lastFetchedRevisionIdGeneral ]
        , button [ onClick <| DataManager.Model.SetLastFetchedRevisionIdGeneral 0 ] [ text "Reset revision ID to 0" ]
        , case webData of
            RemoteData.Success data ->
                div []
                    [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                    , if List.isEmpty data.entities then
                        div [] [ text "No content fetched in last HTTP request" ]

                      else
                        div []
                            [ div [] [ text <| "Here is the content we've fetched in the last HTTP request:" ]
                            , ol [] (List.map viewGeneralEntity data.entities)
                            ]
                    ]

            RemoteData.Failure error ->
                text <| Debug.toString error

            RemoteData.Loading ->
                spinner

            RemoteData.NotAsked ->
                emptyNode
        ]


viewGeneralEntity : BackendGeneralEntity -> Html msg
viewGeneralEntity backendGeneralEntity =
    li []
        [ case backendGeneralEntity of
            BackendGeneralCatchmentArea _ _ entity ->
                text <| "Catchment area (" ++ entity.name ++ ")"

            BackendGeneralHealthCenter _ _ entity ->
                text <| "Health Center (" ++ entity.name ++ ")"

            BackendGeneralNurse _ _ entity ->
                text <| "Nurse " ++ entity.name

            BackendGeneralPerson _ _ entity ->
                text <| "Person (" ++ entity.name ++ ")"

            BackendGeneralPmtctParticipant _ _ entity ->
                text <| "Pmtct Participant for child ID (" ++ fromEntityUuid entity.child ++ ")"

            BackendGeneralRelationship _ _ entity ->
                text <| "Relationship for person ID (" ++ fromEntityUuid entity.person ++ ")"

            BackendGeneralEntityUnknown type_ _ ->
                text <| type_ ++ " (we still don't decode it)"
        ]


viewSyncDownloadAuthority : Model -> WebData (DownloadSyncResponse BackendAuthorityEntity) -> Html Msg
viewSyncDownloadAuthority model webData =
    case model.revisionIdPerAuthorityZipper of
        Nothing ->
            emptyNode

        Just zipper ->
            let
                currentZipper =
                    Zipper.current zipper
            in
            div []
                [ div [] [ text <| "Trying to fetch `Authority` from UUID " ++ currentZipper.uuid ++ " revision ID " ++ String.fromInt currentZipper.revisionId ]
                , button [ onClick <| DataManager.Model.SetLastFetchedRevisionIdAuthority zipper 0 ] [ text "Reset revision ID to 0" ]
                , case webData of
                    RemoteData.Success data ->
                        div []
                            [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                            , if List.isEmpty data.entities then
                                div [] [ text "No content fetched in last HTTP request" ]

                              else
                                div []
                                    [ div [] [ text <| "Here is the content we've fetched in the last HTTP request:" ]
                                    , ol [] (List.map viewAuthorityEntity data.entities)
                                    ]
                            ]

                    RemoteData.Failure error ->
                        text <| Debug.toString error

                    RemoteData.Loading ->
                        spinner

                    RemoteData.NotAsked ->
                        emptyNode
                ]


viewAuthorityEntity : BackendAuthorityEntity -> Html msg
viewAuthorityEntity backendAuthorityEntity =
    li []
        [ case backendAuthorityEntity of
            BackendAuthorityAttendance _ _ entity ->
                text <| "Attendance for person ID" ++ fromEntityUuid entity.participantId

            BackendAuthorityPhoto _ _ entity ->
                text <| "Photo for person ID" ++ fromEntityUuid entity.participantId

            BackendAuthorityWeight _ _ entity ->
                text <| "Weight for person ID" ++ fromEntityUuid entity.participantId

            BackendAuthorityEntityUnknown type_ _ ->
                text <| type_ ++ " (we still don't decode it)"
        ]


viewDownloadPhotosBatch : Model -> DownloadPhotosBatchRec -> Html Msg
viewDownloadPhotosBatch model deferredPhoto =
    case deferredPhoto.indexDbRemoteData of
        RemoteData.Success (Just result) ->
            let
                fileName =
                    result.photo
                        |> Url.fromString
                        |> Maybe.andThen
                            (\url ->
                                url.path
                                    |> String.split "/"
                                    |> List.Extra.last
                            )
                        |> Maybe.withDefault ""
            in
            div []
                [ text <| "Attempt " ++ String.fromInt (result.attempts + 1) ++ " to download "
                , a [ href result.photo, target "_blank" ] [ text fileName ]
                ]

        _ ->
            emptyNode


{-| Show a list of Authorities that allow syncing from.
-}
viewHealthCentersForSync : Language -> ModelIndexedDb -> Model -> Html Msg
viewHealthCentersForSync language db model =
    let
        -- The Health centers that are synced.
        selectedHealthCentersUuid =
            case model.revisionIdPerAuthorityZipper of
                Just zipper ->
                    Zipper.toList zipper
                        |> List.map (\row -> row.uuid)

                Nothing ->
                    []
    in
    case db.healthCenters of
        RemoteData.Success healthCenters ->
            if Dict.isEmpty healthCenters then
                div [ class "segment ui health-center" ] [ text "No health centers synced yet" ]

            else
                div
                    [ class "segment ui health-center" ]
                    [ ul []
                        (List.map
                            (\( healthCenterId, healthCenter ) ->
                                let
                                    isSynced =
                                        List.Extra.find (\selectedUuid -> selectedUuid == fromEntityUuid healthCenterId) selectedHealthCentersUuid
                                            |> isJust
                                in
                                viewHealthCenter language ( healthCenterId, healthCenter ) isSynced
                            )
                            (Dict.toList healthCenters)
                        )
                    ]

        RemoteData.Failure error ->
            Utils.WebData.viewError language error

        RemoteData.Loading ->
            spinner

        RemoteData.NotAsked ->
            emptyNode


viewHealthCenter : Language -> ( HealthCenterId, HealthCenter ) -> Bool -> Html Msg
viewHealthCenter language ( healthCenterId, healthCenter ) isSynced =
    let
        ( syncLabel, syncMsg ) =
            if isSynced then
                ( "Remove from Sync list", RevisionIdAuthorityRemove healthCenterId )

            else
                ( "Add to Sync list", RevisionIdAuthorityAdd healthCenterId )
    in
    li []
        [ text <| healthCenter.name
        , button [ onClick syncMsg ] [ text syncLabel ]
        ]



--    sync =
--        db.syncData
--            |> RemoteData.map
--                (\syncData ->
--                    case Dict.get uuid syncData of
--                        Just data ->
--                            div [ class "health-center-info" ]
--                                [ viewSyncData language data
--                                , button
--                                    [ class "ui button"
--                                    , onClick (SetSyncing uuid False)
--                                    ]
--                                    [ text <| translate language Translate.StopSyncing ]
--                                ]
--
--                        Nothing ->
--                            button
--                                [ class "ui button"
--                                , onClick (SetSyncing uuid True)
--                                ]
--                                [ text <| translate language Translate.StartSyncing ]
--                )
--            |> RemoteData.toMaybe
--            |> showMaybe
--in
--div [ class "health-center" ]
--    [ h2 [] [ text <| model.name ]
--    , sync
--    ]
