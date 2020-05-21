port module DataManager.Update exposing (subscriptions, update)

import App.Model exposing (SubModelReturn)
import Backend.HealthCenter.Encoder
import Backend.Measurement.Encoder
import Backend.Model
import Backend.Person.Encoder
import Backend.PmtctParticipant.Encoder
import DataManager.Decoder exposing (decodeDownloadSyncResponseAuthority, decodeDownloadSyncResponseGeneral)
import DataManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , FetchFromIndexDbQueryType(..)
        , IndexDbQueryTypeResult(..)
        , Model
        , Msg(..)
        , SyncStatus(..)
        )
import DataManager.Utils
import Device.Model exposing (Device)
import Error.Utils exposing (decodeError, maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import Json.Decode exposing (Value, decodeString, decodeValue)
import Json.Encode
import List.Zipper as Zipper
import RemoteData
import Time


update : NominalDate -> Device -> Msg -> Model -> SubModelReturn Model Msg
update currentDate device msg model =
    let
        noChange =
            SubModelReturn model Cmd.none noError []

        returnDetermineSyncStatus =
            SubModelReturn
                (DataManager.Utils.determineSyncStatus model)
                Cmd.none
                noError
                []

        -- @todo: Move has hardcoded in flags, or keep here?
        dbVersion =
            9
    in
    case msg of
        BackendAuthorityFetch ->
            case model.syncStatus of
                SyncDownloadAuthority webData ->
                    case model.revisionIdPerAuthorityZipper of
                        Nothing ->
                            -- No zipper, means not subscribed yet to any
                            -- authority. `determineSyncStatus` will take care of
                            -- rotating if we're not on automatic sync.
                            returnDetermineSyncStatus

                        Just zipper ->
                            if RemoteData.isLoading webData then
                                -- We are already loading.
                                noChange

                            else
                                let
                                    currentZipper =
                                        Zipper.current zipper

                                    cmd =
                                        HttpBuilder.get (device.backendUrl ++ "/api/sync/" ++ currentZipper.uuid)
                                            |> withQueryParams
                                                [ ( "access_token", device.accessToken )
                                                , ( "db_version", String.fromInt dbVersion )
                                                , ( "base_revision", String.fromInt currentZipper.revisionId )
                                                ]
                                            |> withExpectJson decodeDownloadSyncResponseAuthority
                                            |> HttpBuilder.send (RemoteData.fromResult >> BackendAuthorityFetchHandle zipper)
                                in
                                SubModelReturn
                                    { model | syncStatus = SyncDownloadAuthority RemoteData.Loading }
                                    cmd
                                    noError
                                    []

                _ ->
                    returnDetermineSyncStatus

        BackendAuthorityFetchHandle zipper webData ->
            let
                currentZipper =
                    Zipper.current zipper

                cmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            data.entities
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
                                            BackendAuthorityPhoto uuid vid entity_ ->
                                                -- @todo: Encoder is probably wrong.
                                                -- need to have `type=photo`.
                                                doEncode uuid vid (Json.Encode.object <| Backend.Measurement.Encoder.encodePhoto entity_)
                                                    :: accum

                                            BackendAuthorityEntityUnknown _ _ ->
                                                -- Filter out the unknown entities.
                                                accum
                                    )
                                    []
                                |> List.reverse
                                |> sendSyncedDataToIndexDb

                        Nothing ->
                            Cmd.none

                lastFetchedRevisionId =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            -- Get the last item.
                            data.entities
                                |> List.reverse
                                |> List.head
                                |> Maybe.map
                                    (\entity ->
                                        case entity of
                                            BackendAuthorityPhoto _ vid _ ->
                                                vid

                                            BackendAuthorityEntityUnknown _ vid ->
                                                vid
                                    )
                                |> Maybe.withDefault currentZipper.revisionId

                        Nothing ->
                            currentZipper.revisionId

                zipperUpdated =
                    Zipper.mapCurrent (\old -> { old | revisionId = lastFetchedRevisionId }) zipper

                modelWithSyncStatus =
                    DataManager.Utils.determineSyncStatus { model | syncStatus = SyncDownloadAuthority webData }
            in
            SubModelReturn
                { modelWithSyncStatus | revisionIdPerAuthorityZipper = Just zipperUpdated }
                (Cmd.batch
                    [ cmd

                    -- Send to JS the updated revision ID. We send the entire
                    -- list.
                    , sendRevisionIdPerAuthority (Zipper.toList zipperUpdated)
                    ]
                )
                (maybeHttpError webData "Backend.DataManager.Update" "BackendGeneralFetchHandle")
                []

        BackendFetchMain ->
            case model.syncStatus of
                SyncIdle ->
                    returnDetermineSyncStatus

                SyncUpload ->
                    returnDetermineSyncStatus

                SyncDownloadGeneral _ ->
                    update
                        currentDate
                        device
                        BackendGeneralFetch
                        model

                SyncDownloadAuthority _ ->
                    update
                        currentDate
                        device
                        BackendAuthorityFetch
                        model

                SyncDownloadPhotos _ ->
                    returnDetermineSyncStatus

        BackendGeneralFetch ->
            case model.syncStatus of
                SyncDownloadGeneral webData ->
                    if RemoteData.isLoading webData then
                        -- We are already loading.
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
                                    |> withExpectJson decodeDownloadSyncResponseGeneral
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
                            data.entities
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
                            data.entities
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

        SetLastFetchedRevisionIdAuthority zipper revisionId ->
            let
                zipperUpdated =
                    Zipper.mapCurrent (\old -> { old | revisionId = revisionId }) zipper
            in
            SubModelReturn
                { model | revisionIdPerAuthorityZipper = Just zipperUpdated }
                Cmd.none
                noError
                []

        SetLastFetchedRevisionIdGeneral revisionId ->
            SubModelReturn
                { model | lastFetchedRevisionIdGeneral = revisionId }
                Cmd.none
                noError
                []

        SetSyncStatusRotateAutomatic status ->
            SubModelReturn
                { model | syncStatusRotateAutomatic = status }
                Cmd.none
                noError
                []

        FetchFromIndexDb indexDbQueryType ->
            let
                indexDbQueryTypeAsString =
                    case indexDbQueryType of
                        IndexDbQueryHealthCenters ->
                            "IndexDbQueryHealthCenters"
            in
            SubModelReturn
                model
                (askFromIndexDb indexDbQueryTypeAsString)
                noError
                []

        FetchFromIndexDbHandle val ->
            case decodeValue DataManager.Decoder.decodeIndexDbQueryTypeResult val of
                Ok indexDbQueryTypeResult ->
                    case indexDbQueryTypeResult of
                        IndexDbQueryHealthCentersResult dict ->
                            -- Send info back
                            SubModelReturn
                                model
                                Cmd.none
                                noError
                                [ Backend.Model.HandleFetchedHealthCenters (RemoteData.Success dict)
                                    |> App.Model.MsgIndexedDb
                                ]

                Err error ->
                    SubModelReturn
                        model
                        Cmd.none
                        (decodeError "Backend.DataManager.Update" "BackendGeneralFetchHandle" error)
                        []


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Time.every 1000 (\_ -> BackendFetchMain)
        , getFromIndexDb FetchFromIndexDbHandle
        ]


{-| Send to JS data we have synced (e.g. `person`, `health center`, etc.
-}
port sendSyncedDataToIndexDb : List String -> Cmd msg


{-| Send to JS the last revision ID used to download General.
-}
port sendLastFetchedRevisionIdGeneral : Int -> Cmd msg


{-| Send to JS a list with the last revision ID used to download Authority,
along with their UUID.
-}
port sendRevisionIdPerAuthority : List { uuid : String, revisionId : Int } -> Cmd msg


{-| Ask JS to send us data from IndexDB. We send the query type.
-}
port askFromIndexDb : String -> Cmd msg


{-| Get data requested from IndexDB.

For now we don't care who asked for the data, we just fill it in where
needed.

-}
port getFromIndexDb : (Value -> msg) -> Sub msg
