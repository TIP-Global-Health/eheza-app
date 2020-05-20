module DataManager.View exposing (viewDebugSync)

import DataManager.Model exposing (BackendGeneralEntity(..), DownloadSyncResponse, Model, Msg, SyncStatus(..))
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode
import RemoteData exposing (WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import Utils.Html exposing (spinner)



-- @todo: Debug for now


viewDebugSync : Model -> Html Msg
viewDebugSync model =
    let
        htmlContent =
            details [ property "open" (Json.Encode.bool True) ]
                [ div [] [ text <| "Sync status: " ++ Debug.toString model.syncStatus ]
                , case model.syncStatus of
                    SyncDownloadGeneral webData ->
                        viewSyncDownloadGeneral model webData

                    _ ->
                        emptyNode
                ]
    in
    pre [ class "ui segment" ] [ htmlContent ]


viewSyncDownloadGeneral : Model -> WebData DownloadSyncResponse -> Html Msg
viewSyncDownloadGeneral model webData =
    div []
        [ div [] [ text <| "Trying to fetch `General` from revision ID " ++ String.fromInt model.lastFetchedRevisionIdGeneral ]
        , button [ onClick <| DataManager.Model.SetLastFetchedRevisionIdGeneral 0 ] [ text "Reset revision ID to 0" ]
        , case webData of
            RemoteData.Success data ->
                div []
                    [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                    , if List.isEmpty data.backendGeneralEntities then
                        div [] [ text "No content fetched in last HTTP request" ]

                      else
                        div []
                            [ div [] [ text <| "Here is the content we've fetched in the last HTTP request:" ]
                            , ol [] (List.map viewGeneralEntity data.backendGeneralEntities)
                            ]
                    ]

            RemoteData.Failure error ->
                text <| Debug.toString error

            _ ->
                spinner
        ]


viewGeneralEntity : BackendGeneralEntity -> Html msg
viewGeneralEntity backendGeneralEntity =
    li []
        [ case backendGeneralEntity of
            BackendGeneralHealthCenter _ _ healthCenter ->
                text <| "Health Center (" ++ healthCenter.name ++ ")"

            BackendGeneralPerson _ _ entity ->
                text <| "Person (" ++ entity.name ++ ")"

            BackendGeneralPmtctParticipant _ _ entity ->
                text <| "Pmtct Participant for child ID (" ++ fromEntityUuid entity.child ++ ")"

            BackendGeneralEntityUnknown type_ _ ->
                text <| type_ ++ " (we still don't decode it)"



                        ]
