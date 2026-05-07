module Backend.Completion.Update exposing (update)

import Backend.Completion.Decoder exposing (decodeCompletionData, decodeSyncResponse)
import Backend.Completion.Model exposing (Msg(..))
import Backend.Components.Sync as Sync
import Backend.Model exposing (ModelBackend)
import Backend.Types exposing (BackendReturn)


update : String -> String -> Msg -> ModelBackend -> BackendReturn Msg
update backendUrl csrfToken msg model =
    let
        config =
            { appType = "completion"
            , backendUrl = backendUrl
            , csrfToken = csrfToken
            , dataDecoder = decodeCompletionData
            , syncResponseDecoder = decodeSyncResponse
            , getData = .completionData
            , setData = \v m -> { m | completionData = v }
            , getParams = .params
            , mergeResponse =
                \response data ->
                    { data
                        | acuteIllnessData = data.acuteIllnessData ++ response.acuteIllnessData
                        , childScoreboardData = data.childScoreboardData ++ response.childScoreboardData
                        , hivData = data.hivData ++ response.hivData
                        , homeVisitData = data.homeVisitData ++ response.homeVisitData
                        , ncdData = data.ncdData ++ response.ncdData
                        , nutritionIndividualData = data.nutritionIndividualData ++ response.nutritionIndividualData
                        , nutritionGroupData = data.nutritionGroupData ++ response.nutritionGroupData
                        , prenatalData = data.prenatalData ++ response.prenatalData
                        , tuberculosisData = data.tuberculosisData ++ response.tuberculosisData
                        , wellChildData = data.wellChildData ++ response.wellChildData
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
