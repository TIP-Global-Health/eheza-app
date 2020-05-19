module Backend.SyncData.Update exposing (update)

import Backend.SyncData.Decoder exposing (decodeBackendGeneralEntity)
import Backend.SyncData.Model exposing (Model, Msg(..))
import Device.Model exposing (Device)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import RemoteData


update : NominalDate -> Device -> Msg -> Model -> ( Model, Cmd Msg )
update currentDate device msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        BackendGeneralFetch lastFetchedRevisionId ->
            let
                cmd =
                    HttpBuilder.get (device.backendUrl ++ "/api/sync")
                        |> withQueryParams [ ( "access_token", device.accessToken ) ]
                        |> HttpBuilder.toTask
                        |> withExpectJson decodeBackendGeneralEntity
                        |> HttpBuilder.send (RemoteData.fromResult >> BackendGeneralFetchHandle lastFetchedRevisionId)
            in
            noChange

        BackendGeneralFetchHandle lastFetchedRevisionId webData ->
            noChange

        SetbackendGeneralLastFetchedRevisionId lastFetchedRevisionId ->
            ( { model | backendGeneralLastFetchedRevisionId = lastFetchedRevisionId }
            , Cmd.none
            )
