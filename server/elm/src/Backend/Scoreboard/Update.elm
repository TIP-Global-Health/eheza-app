module Backend.Scoreboard.Update exposing (update)

import Backend.Components.Encoder exposing (encodeReportParams)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData, decodeSyncResponse)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withHeader, withJsonBody)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (object, string)
import RemoteData


update : NominalDate -> String -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl csrfToken msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | scoreboardData = Just <| decodeValue decodeScoreboardData value }
            in
            update currentDate backendUrl csrfToken (SendSyncRequest 0) modelUpdated

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
                        |> withHeader "X-CSRF-Token" csrfToken
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
                                                    { scoreboardData
                                                        | records = scoreboardData.records ++ response.records
                                                        , remainingForDownload = Just response.totalRemaining
                                                    }
                                            in
                                            { model | scoreboardData = Just (Ok scoreboardDataUpdated) }
                                        )
                                    |> Maybe.withDefault model
                        in
                        if response.totalRemaining == 0 then
                            BackendReturn modelUpdated Cmd.none noError []

                        else
                            update currentDate backendUrl csrfToken (SendSyncRequest response.lastIdSynced) modelUpdated
                    )
                |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
