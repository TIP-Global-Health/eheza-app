module Backend.Reports.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Reports.Decoder exposing (decodeReportsData, decodeSyncResponse)
import Backend.Reports.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode
import RemoteData


update : NominalDate -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | reportsData = Just <| decodeValue decodeReportsData value }
            in
            update currentDate backendUrl (SendSyncRequest 0) modelUpdated

        SendSyncRequest fromPersonId ->
            let
                cmd =
                    let
                        params =
                            [ ( "app_type", Json.Encode.string "reports" )
                            , ( "base_revision", Json.Encode.string (String.fromInt fromPersonId) )
                            ]
                    in
                    HttpBuilder.post (backendUrl ++ "/api/reports-data")
                        |> withJsonBody (Json.Encode.object params)
                        |> withExpectJson decodeSyncResponse
                        |> HttpBuilder.send (RemoteData.fromResult >> HandleSyncResponse)
            in
            BackendReturn model cmd noError []

        HandleSyncResponse data ->
            RemoteData.toMaybe data
                |> Maybe.map
                    (\response ->
                        let
                            _ =
                                Debug.log "" response.lastIdSynced

                            modelUpdated =
                                Maybe.andThen Result.toMaybe model.reportsData
                                    |> Maybe.map
                                        (\reportsData ->
                                            let
                                                reportsDataUpdated =
                                                    { reportsData | records = reportsData.records ++ response.records }
                                            in
                                            { model | reportsData = Just (Ok reportsDataUpdated) }
                                        )
                                    |> Maybe.withDefault model
                        in
                        update currentDate backendUrl (SendSyncRequest response.lastIdSynced) model
                    )
                |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
