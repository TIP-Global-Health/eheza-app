module Backend.Scoreboard.Update exposing (update)

import Backend.Components.Sync as Sync
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData, decodeSyncResponse)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Gizra.NominalDate exposing (NominalDate)


update : NominalDate -> String -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl csrfToken msg model =
    let
        config =
            { appType = "scoreboard"
            , backendUrl = backendUrl
            , csrfToken = csrfToken
            , dataDecoder = decodeScoreboardData
            , syncResponseDecoder = decodeSyncResponse currentDate
            , getData = .scoreboardData
            , setData = \v m -> { m | scoreboardData = v }
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
