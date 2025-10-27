module Backend.Scoreboard.Update exposing (update)

import Backend.Components.Encoder exposing (encodeReportParams)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData, decodeSyncResponse)
import Backend.Scoreboard.Model exposing (Msg(..))
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
                    { model | scoreboardData = Just <| decodeValue (decodeScoreboardData currentDate) value }
            in
            update currentDate backendUrl (SendSyncRequest 0) modelUpdated

        SendSyncRequest fromPersonId ->
            let
                cmd =
                    let
                        geoParams =
                            Maybe.andThen Result.toMaybe model.scoreboardData
                                |> Maybe.map (.params >> encodeReportParams)
                                |> Maybe.withDefault []

                        params =
                            [ ( "app_type", string "scoreboard" )
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
                                Maybe.andThen Result.toMaybe model.scoreboardData
                                    |> Maybe.map
                                        (\scoreboardData ->
                                            let
                                                scoreboardDataUpdated =
                                                    { scoreboardData | records = recordsUpdated, remainingForDownload = Just response.totalRemaining }

                                                recordsUpdated =
                                                    scoreboardData.records ++ response.records

                                                totalRecordsUpdated =
                                                    List.length recordsUpdated
                                            in
                                            { model | scoreboardData = Just (Ok scoreboardDataUpdated) }
                                        )
                                    |> Maybe.withDefault model
                        in
                        update currentDate backendUrl (SendSyncRequest response.lastIdSynced) modelUpdated
                    )
                |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
