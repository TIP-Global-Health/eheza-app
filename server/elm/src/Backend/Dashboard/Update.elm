module Backend.Dashboard.Update exposing (update)

import Backend.Components.Model exposing (ReportParams)
import Backend.Components.Sync as Sync
import Backend.Dashboard.Decoder exposing (decodeDashboardData, decodeSyncResponse)
import Backend.Dashboard.Model exposing (Msg(..))
import Backend.Model exposing (ModelBackend)
import Backend.Types exposing (BackendReturn)


update : String -> String -> Msg -> ModelBackend -> BackendReturn Msg
update backendUrl csrfToken msg model =
    let
        config =
            { appType = "dashboard"
            , backendUrl = backendUrl
            , csrfToken = csrfToken
            , dataDecoder = decodeDashboardData
            , syncResponseDecoder = decodeSyncResponse
            , getData = .dashboardData
            , setData = \v m -> { m | dashboardData = v }

            -- Every site's records load, since the Facility dashboard
            -- compares its site with the others.
            , getParams = always allSites
            , mergeResponse = Sync.mergeSyncResponse
            , getRemaining = .totalRemaining
            , getLastIdSynced = .lastIdSynced
            , wrapHandleResponse = HandleSyncResponse
            }
    in
    case msg of
        HandleSyncResponse data ->
            Sync.handleSyncResponse config data model

        SendSyncRequest fromId ->
            Sync.handleSendRequest config fromId model

        SetData value ->
            Sync.handleSetData config value model


allSites : ReportParams
allSites =
    ReportParams Nothing Nothing Nothing Nothing Nothing Nothing
