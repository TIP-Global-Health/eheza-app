module DataManager.Decoder exposing (decodeDownloadSyncResponse, decodeSyncData)

import Backend.Person.Decoder exposing (decodePerson)
import Backend.PmtctParticipant.Decoder
import DataManager.Model exposing (BackendGeneralEntity(..), DownloadStatus, DownloadSyncResponse, SyncAttempt(..), SyncData, SyncError(..), UploadStatus)
import Gizra.Date exposing (decodeDate)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)
import Time


decodeDownloadSyncResponse : Decoder DownloadSyncResponse
decodeDownloadSyncResponse =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list decodeBackendGeneralEntity)
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
        )


decodeBackendGeneralEntity : Decoder BackendGeneralEntity
decodeBackendGeneralEntity =
    (succeed (\a b c -> ( a, b, c ))
        |> required "type" string
        |> required "uuid" string
        |> required "vid" decodeInt
    )
        |> andThen
            (\( type_, uuid, vid ) ->
                case type_ of
                    "person" ->
                        Backend.Person.Decoder.decodePerson
                            |> andThen (\entity -> succeed (BackendGeneralEntityPerson uuid vid entity))

                    "pmtct_participant" ->
                        Backend.PmtctParticipant.Decoder.decodePmtctParticipant
                            |> andThen (\entity -> succeed (BackendGeneralPmtctParticipant uuid vid entity))

                    _ ->
                        succeed (BackendGeneralEntityUnknown type_ vid)
            )


decodeSyncData : Decoder SyncData
decodeSyncData =
    succeed SyncData
        |> optional "download" (nullable decodeDownloadStatus) Nothing
        |> optional "upload" (nullable decodeUploadStatus) Nothing
        |> required "attempt" decodeSyncAttempt


decodeDownloadStatus : Decoder DownloadStatus
decodeDownloadStatus =
    succeed DownloadStatus
        |> custom (decodeTimeField "last_contact")
        |> required "last_timestamp" decodeInt
        |> required "remaining" decodeInt


decodeUploadStatus : Decoder UploadStatus
decodeUploadStatus =
    succeed UploadStatus
        |> optional "first_timestamp" (nullable decodeInt) Nothing
        |> required "remaining" decodeInt


decodeSyncAttempt : Decoder SyncAttempt
decodeSyncAttempt =
    field "tag" string
        |> andThen
            (\s ->
                case s of
                    "NotAsked" ->
                        succeed NotAsked

                    "Success" ->
                        succeed Success

                    "DatabaseError" ->
                        succeed DatabaseError
                            |> required "message" string
                            |> decodeFailure

                    "NetworkError" ->
                        succeed NetworkError
                            |> required "message" string
                            |> decodeFailure

                    "ImageNotFound" ->
                        succeed ImageNotFound
                            |> required "url" string
                            |> decodeFailure

                    "NoCredentials" ->
                        succeed NoCredentials
                            |> decodeFailure

                    "BadResponse" ->
                        succeed BadResponse
                            |> required "status" decodeInt
                            |> required "statusText" string
                            |> decodeFailure

                    "BadJson" ->
                        succeed BadJson
                            |> decodeFailure

                    "Loading" ->
                        succeed Downloading
                            |> custom (decodeTimeField "timestamp")
                            |> required "revision" decodeInt

                    "Uploading" ->
                        succeed Uploading
                            |> custom (decodeTimeField "timestamp")

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized SyncAttempt tag"
            )


decodeFailure : Decoder SyncError -> Decoder SyncAttempt
decodeFailure =
    map2 DataManager.Model.Failure (decodeTimeField "timestamp")


decodeTimeField : String -> Decoder Time.Posix
decodeTimeField fieldName =
    map Time.millisToPosix (field fieldName decodeInt)
