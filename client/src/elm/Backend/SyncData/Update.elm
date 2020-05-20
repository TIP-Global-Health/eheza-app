module Backend.SyncData.Update exposing (update)

import Backend.SyncData.Decoder exposing (decodeBackendGeneralEntityList)
import Backend.SyncData.Model exposing (Model, Msg(..))
import Backend.SyncData.Types exposing (SyncDataReturn)
import Device.Model exposing (Device)
import Error.Utils exposing (maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import RemoteData


update : NominalDate -> Device -> Msg -> Model -> SyncDataReturn Msg
update currentDate device msg model =
    let
        noChange =
            SyncDataReturn model Cmd.none noError []

        -- @todo: Move has hardcoded in flags, or keep here?
        dbVersion =
            7
    in
    case msg of
        BackendGeneralFetch lastFetchedRevisionId ->
            if RemoteData.isNotAsked model.backendGeneralEntities then
                let
                    cmd =
                        HttpBuilder.get (device.backendUrl ++ "/api/sync")
                            |> withQueryParams
                                [ ( "access_token", device.accessToken )
                                , ( "db_version", String.fromInt dbVersion )
                                ]
                            |> withExpectJson decodeBackendGeneralEntityList
                            |> HttpBuilder.send (RemoteData.fromResult >> BackendGeneralFetchHandle lastFetchedRevisionId)
                in
                SyncDataReturn
                    { model | backendGeneralEntities = RemoteData.Loading }
                    cmd
                    noError
                    []

            else
                -- @todo: Handle other cases.
                noChange

        BackendGeneralFetchHandle lastFetchedRevisionId webData ->
            let
                modelUpdated =
                    if RemoteData.isSuccess webData then
                        { model | lastFetchedRevisionIdGeneral = lastFetchedRevisionId }

                    else
                        model
            in
            SyncDataReturn
                { modelUpdated | backendGeneralEntities = webData }
                Cmd.none
                (maybeHttpError webData "Backend.SyncData.Update" "BackendGeneralFetchHandle")
                []
