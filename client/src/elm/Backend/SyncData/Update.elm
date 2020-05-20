module Backend.SyncData.Update exposing (update)

import App.Model exposing (SubModelReturn)
import Backend.SyncData.Decoder exposing (decodeDownloadSyncResponse)
import Backend.SyncData.Model exposing (Model, Msg(..))
import Device.Model exposing (Device)
import Error.Utils exposing (maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import RemoteData


update : NominalDate -> Device -> Msg -> Model -> SubModelReturn Model Msg
update currentDate device msg model =
    let
        noChange =
            SubModelReturn model Cmd.none noError []

        -- @todo: Move has hardcoded in flags, or keep here?
        dbVersion =
            9
    in
    case msg of
        BackendGeneralFetch ->
            if RemoteData.isNotAsked model.downloadSyncResponse then
                let
                    cmd =
                        HttpBuilder.get (device.backendUrl ++ "/api/sync")
                            |> withQueryParams
                                [ ( "access_token", device.accessToken )
                                , ( "db_version", String.fromInt dbVersion )
                                , ( "base_revision", String.fromInt model.lastFetchedRevisionIdGeneral )
                                ]
                            |> withExpectJson decodeDownloadSyncResponse
                            |> HttpBuilder.send (RemoteData.fromResult >> BackendGeneralFetchHandle)
                in
                SubModelReturn
                    { model | downloadSyncResponse = RemoteData.Loading }
                    cmd
                    noError
                    []

            else
                -- @todo: Handle other cases.
                noChange

        BackendGeneralFetchHandle webData ->
            SubModelReturn
                { model | downloadSyncResponse = webData }
                Cmd.none
                (maybeHttpError webData "Backend.SyncData.Update" "BackendGeneralFetchHandle")
                []
