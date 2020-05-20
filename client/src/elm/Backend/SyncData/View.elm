module Backend.SyncData.View exposing (viewDebugSync)

import Backend.SyncData.Model exposing (BackendGeneralEntity(..), Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import RemoteData
import Utils.Html exposing (spinner)



-- @todo: Debug for now


viewDebugSync : Model -> Html msg
viewDebugSync model =
    let
        htmlContent =
            case model.downloadSyncResponse of
                RemoteData.Success data ->
                    details [ property "open" (Json.Encode.bool True) ]
                        [ div [] [ text <| "We still have " ++ String.fromInt data.revisionCount ++ " items left to download" ]
                        , div [] [ text <| "Here is the content we've fetched from revision ID " ++ String.fromInt model.lastFetchedRevisionIdGeneral ++ ":" ]
                        , ol [] (List.map viewGeneralEntity data.backendGeneralEntities)
                        ]

                RemoteData.Failure error ->
                    text <| Debug.toString error

                _ ->
                    spinner
    in
    pre [ class "ui segment" ] [ htmlContent ]


viewGeneralEntity : BackendGeneralEntity -> Html msg
viewGeneralEntity backendGeneralEntity =
    li []
        [ case backendGeneralEntity of
            BackendGeneralEntityPerson _ person _ ->
                text <| "Person (" ++ person.name ++ ")"

            BackendGeneralEntityUnknown type_ _ ->
                text <| type_ ++ " (we still don't decode it)"
        ]
