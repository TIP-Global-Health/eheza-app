module DataManager.View exposing (viewDebugSync)

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
import RemoteData exposing (WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import Url
import Utils.Html exposing (spinner)



-- @todo: Debug for now


viewDebugSync : Model -> Html Msg
viewDebugSync model =
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
    pre [ class "ui segment", style "min-height" "240px" ] [ htmlContent ]


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
            BackendAuthorityPhoto _ _ entity ->
                text <| "Photo " ++ Debug.toString entity

            BackendAuthorityWeight _ _ entity ->
                text <| "Weight " ++ fromEntityUuid entity.participantId

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
