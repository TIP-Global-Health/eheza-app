module Backend.Reports.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Reports.Decoder exposing (decodeReportsData, decodeSyncResponse)
import Backend.Reports.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (int, object, string)
import Maybe.Extra
import RemoteData


update : NominalDate -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl msg model =
    case msg of
        SetData value ->
            let
                decodedValue =
                    decodeValue decodeReportsData value

                modelUpdated =
                    { model | reportsData = Just decodedValue }
            in
            update currentDate backendUrl (SendSyncRequest 0) modelUpdated

        SendSyncRequest fromPersonId ->
            let
                cmd =
                    let
                        geoParams =
                            Maybe.andThen Result.toMaybe model.reportsData
                                |> Maybe.map
                                    (\reportsData ->
                                        [ Maybe.map (\value -> ( "province", string value )) reportsData.params.province
                                        , Maybe.map (\value -> ( "district", string value )) reportsData.params.district
                                        , Maybe.map (\value -> ( "sector", string value )) reportsData.params.sector
                                        , Maybe.map (\value -> ( "cell", string value )) reportsData.params.cell
                                        , Maybe.map (\value -> ( "village", string value )) reportsData.params.village
                                        , Maybe.map (\value -> ( "health_center", int value )) reportsData.params.healthCenter
                                        ]
                                            |> Maybe.Extra.values
                                    )
                                |> Maybe.withDefault []

                        _ =
                            Debug.log "geoParams" geoParams

                        params =
                            [ ( "app_type", string "reports" )
                            , ( "base_revision", string (String.fromInt fromPersonId) )
                            ]
                                ++ geoParams
                    in
                    HttpBuilder.post (backendUrl ++ "/api/reports-data")
                        |> withJsonBody (object params)
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
                        update currentDate backendUrl (SendSyncRequest response.lastIdSynced) modelUpdated
                    )
                |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
