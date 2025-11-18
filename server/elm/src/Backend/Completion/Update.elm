module Backend.Completion.Update exposing (update)

import Backend.Completion.Decoder exposing (decodeCompletionData, decodeSyncResponse)
import Backend.Completion.Model exposing (Msg(..))
import Backend.Components.Encoder exposing (encodeReportParams)
import Backend.Model exposing (ModelBackend)
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (object, string)
import RemoteData


update : NominalDate -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | completionData = Just <| decodeValue decodeCompletionData value }
            in
            update currentDate backendUrl (SendSyncRequest 0) modelUpdated

        SendSyncRequest fromPersonId ->
            let
                cmd =
                    let
                        geoParams =
                            Maybe.andThen Result.toMaybe model.completionData
                                |> Maybe.map (.params >> encodeReportParams)
                                |> Maybe.withDefault []

                        params =
                            [ ( "app_type", string "completion" )
                            , ( "base_revision", string (String.fromInt fromPersonId) )
                            ]
                                ++ geoParams
                    in
                    HttpBuilder.post (backendUrl ++ "/api/reports-data")
                        |> withJsonBody (object params)
                        |> withExpectJson (decodeSyncResponse currentDate)
                        |> HttpBuilder.send (RemoteData.fromResult >> HandleSyncResponse)
            in
            BackendReturn model cmd noError []

        HandleSyncResponse data ->
            RemoteData.toMaybe data
                |> Maybe.map
                    (\response ->
                        let
                            modelUpdated =
                                Maybe.andThen Result.toMaybe model.completionData
                                    |> Maybe.map
                                        (\completionData ->
                                            let
                                                completionDataUpdated =
                                                    { completionData
                                                        | acuteIllnessData = completionData.acuteIllnessData ++ response.acuteIllnessData
                                                        , childScoreboardData = completionData.childScoreboardData ++ response.childScoreboardData
                                                        , hivData = completionData.hivData ++ response.hivData
                                                        , homeVisitData = completionData.homeVisitData ++ response.homeVisitData
                                                        , ncdData = completionData.ncdData ++ response.ncdData
                                                        , nutritionIndividualData = completionData.nutritionIndividualData ++ response.nutritionIndividualData
                                                        , nutritionGroupData = completionData.nutritionGroupData ++ response.nutritionGroupData
                                                        , prenatalData = completionData.prenatalData ++ response.prenatalData
                                                        , tuberculosisData = completionData.tuberculosisData ++ response.tuberculosisData
                                                        , wellChildData = completionData.wellChildData ++ response.wellChildData
                                                        , remainingForDownload = Just response.totalRemaining
                                                    }
                                            in
                                            { model | completionData = Just (Ok completionDataUpdated) }
                                        )
                                    |> Maybe.withDefault model
                        in
                        if response.totalRemaining == 0 then
                            BackendReturn modelUpdated Cmd.none noError []

                        else
                            update currentDate backendUrl (SendSyncRequest response.lastIdSynced) modelUpdated
                    )
                |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
