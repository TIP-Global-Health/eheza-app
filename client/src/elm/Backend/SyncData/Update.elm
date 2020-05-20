port module Backend.SyncData.Update exposing (update)

import App.Model exposing (SubModelReturn)
import Backend.Person.Encoder
import Backend.SyncData.Decoder exposing (decodeDownloadSyncResponse)
import Backend.SyncData.Model exposing (BackendGeneralEntity(..), Model, Msg(..))
import Device.Model exposing (Device)
import Error.Utils exposing (maybeHttpError, noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (withExpectJson, withQueryParams)
import Json.Encode
import RemoteData
import Restful.Endpoint exposing (encodeEntityUuid)


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
            let
                cmd =
                    case RemoteData.toMaybe webData of
                        Just data ->
                            data.backendGeneralEntities
                                |> List.foldl
                                    (\entity accum ->
                                        let
                                            doEncode uuid val =
                                                Json.Encode.object
                                                    [ ( "uuid", encodeEntityUuid uuid )
                                                    , ( "entity", val )
                                                    ]
                                                    |> Json.Encode.encode 0
                                        in
                                        case entity of
                                            BackendGeneralEntityPerson uuid person ->
                                                doEncode uuid (Backend.Person.Encoder.encodePerson person)
                                                    :: accum

                                            BackendGeneralEntityUnknown type_ ->
                                                -- Filter out the unknown entities.
                                                accum
                                    )
                                    []
                                |> List.reverse
                                |> sendSyncedDataToIndexDb

                        Nothing ->
                            Cmd.none
            in
            SubModelReturn
                { model | downloadSyncResponse = webData }
                cmd
                (maybeHttpError webData "Backend.SyncData.Update" "BackendGeneralFetchHandle")
                []


{-| Send to JS data we have synced (e.g. `person`, `health center`, etc.
-}
port sendSyncedDataToIndexDb : List String -> Cmd msg
