port module DataManager.Update exposing (update)

import App.Model exposing (SubModelReturn)
import Backend.HealthCenter.Encoder
import Backend.Person.Encoder
import Backend.PmtctParticipant.Encoder
import DataManager.Decoder exposing (decodeDownloadSyncResponse)
import DataManager.Model exposing (BackendGeneralEntity(..), Model, Msg(..), SyncStatus(..))
import DataManager.Utils
import Device.Model exposing (Device)
import Error.Utils exposing (maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import Json.Encode
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
        BackendAuthorityFetch ->
            noChange

        BackendAuthorityFetchHandle webData ->
            noChange

        BackendGeneralFetch ->
            case model.syncStatus of
                SyncDownloadGeneral webData ->
                    if RemoteData.isLoading webData then
                        -- We are already loading, or not in correct sync status.
                        noChange

                    else
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
                            { model | syncStatus = SyncDownloadGeneral RemoteData.Loading }
                            cmd
                            noError
                            []

                _ ->
                    -- @todo: Probably needs to be `noChange`?
                    SubModelReturn
                        (DataManager.Utils.determineSyncStatus model)
                        Cmd.none
                        noError
                        []

        BackendGeneralFetchHandle webData ->
            let
                cmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            data.backendGeneralEntities
                                |> List.foldl
                                    (\entity accum ->
                                        let
                                            doEncode uuid vid val =
                                                Json.Encode.object
                                                    [ ( "uuid", Json.Encode.string uuid )
                                                    , ( "entity", val )
                                                    , ( "vid", Json.Encode.int vid )
                                                    ]
                                                    |> Json.Encode.encode 0
                                        in
                                        case entity of
                                            BackendGeneralHealthCenter uuid vid entity_ ->
                                                doEncode uuid vid (Backend.HealthCenter.Encoder.encodeHealthCenter entity_)
                                                    :: accum

                                            BackendGeneralPerson uuid vid entity_ ->
                                                doEncode uuid vid (Backend.Person.Encoder.encodePerson entity_)
                                                    :: accum

                                            BackendGeneralPmtctParticipant uuid vid entity_ ->
                                                doEncode uuid vid (Backend.PmtctParticipant.Encoder.encodePmtctParticipant entity_)
                                                    :: accum

                                            BackendGeneralEntityUnknown _ _ ->
                                                -- Filter out the unknown entities.
                                                accum
                                    )
                                    []
                                |> List.reverse
                                |> sendSyncedDataToIndexDb

                        Nothing ->
                            Cmd.none

                lastFetchedRevisionIdGeneral =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            -- Get the last item.
                            data.backendGeneralEntities
                                |> List.reverse
                                |> List.head
                                |> Maybe.map
                                    (\entity ->
                                        case entity of
                                            BackendGeneralHealthCenter _ vid _ ->
                                                vid

                                            BackendGeneralPerson _ vid _ ->
                                                vid

                                            BackendGeneralPmtctParticipant _ vid _ ->
                                                vid

                                            BackendGeneralEntityUnknown _ vid ->
                                                vid
                                    )
                                |> Maybe.withDefault model.lastFetchedRevisionIdGeneral

                        Nothing ->
                            model.lastFetchedRevisionIdGeneral

                modelWithSyncStatus =
                    DataManager.Utils.determineSyncStatus { model | syncStatus = SyncDownloadGeneral webData }
            in
            SubModelReturn
                { modelWithSyncStatus | lastFetchedRevisionIdGeneral = lastFetchedRevisionIdGeneral }
                (Cmd.batch [ cmd, sendLastFetchedRevisionIdGeneral lastFetchedRevisionIdGeneral ])
                (maybeHttpError webData "Backend.DataManager.Update" "BackendGeneralFetchHandle")
                []

        SetLastFetchedRevisionIdGeneral revisionId ->
            SubModelReturn
                { model | lastFetchedRevisionIdGeneral = revisionId }
                Cmd.none
                noError
                []

        FetchFromIndexDb indexDbQueryType ->
            SubModelReturn
                model
                Cmd.none
                noError
                []


{-| Send to JS data we have synced (e.g. `person`, `health center`, etc.
-}
port sendSyncedDataToIndexDb : List String -> Cmd msg


{-| Send to JS the last revision ID used to download General.
-}
port sendLastFetchedRevisionIdGeneral : Int -> Cmd msg


{-| Send to JS a list with the last revision ID used to download Authority,
along with their UUID.
-}
port sendRevisionIdPerAuthority : List ( String, Int ) -> Cmd msg


{-| Ask JS to send us data from IndexDB. We send the query type.
-}
port askFromIndexDb : String -> Cmd msg


{-| Get data requested from IndexDB.

For now we don't care who asked for the data, we just fill it in where
needed.

-}
port getFromIndexDb : (String -> msg) -> Sub msg
