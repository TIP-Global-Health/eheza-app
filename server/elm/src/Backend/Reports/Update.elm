module Backend.Reports.Update exposing (update)

import Backend.Components.Sync as Sync
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Decoder exposing (decodeReportsData, decodeSyncResponse)
import Backend.Reports.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)


update : String -> String -> Msg -> ModelBackend -> BackendReturn Msg
update backendUrl csrfToken msg model =
    let
        config =
            { appType = "reports"
            , backendUrl = backendUrl
            , csrfToken = csrfToken
            , dataDecoder = decodeReportsData
            , syncResponseDecoder = decodeSyncResponse
            , getData = .reportsData
            , setData = \v m -> { m | reportsData = v }
            , getParams = .params
            , mergeResponse =
                \response data ->
                    { data
                        | records = data.records ++ response.records
                        , remainingForDownload = Just response.totalRemaining
                    }
            , getRemaining = .totalRemaining
            , getLastIdSynced = .lastIdSynced
            , wrapHandleResponse = HandleSyncResponse
            }
    in
    case msg of
        SetData value ->
            Sync.handleSetData config value model

        SendSyncRequest fromPersonId ->
            Sync.handleSendRequest config fromPersonId model

        HandleSyncResponse data ->
            Sync.handleSyncResponse config data model
