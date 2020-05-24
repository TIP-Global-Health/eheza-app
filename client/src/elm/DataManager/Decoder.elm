module DataManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    , decodeSyncData
    )

import AssocList as Dict
import Backend.HealthCenter.Decoder
import Backend.Measurement.Decoder
import Backend.Nurse.Decoder
import Backend.Person.Decoder
import Backend.PmtctParticipant.Decoder
import Backend.Relationship.Decoder
import DataManager.Model exposing (BackendAuthorityEntity(..), BackendGeneralEntity(..), DownloadStatus, DownloadSyncResponse, IndexDbQueryDeferredPhotoResultRecord, IndexDbQueryTypeResult(..), SyncAttempt(..), SyncData, SyncError(..), UploadStatus)
import Gizra.Date exposing (decodeDate)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)
import Time


decodeIndexDbQueryTypeResult : Decoder IndexDbQueryTypeResult
decodeIndexDbQueryTypeResult =
    field "queryType" string
        |> andThen
            (\queryType ->
                case queryType of
                    "IndexDbQueryHealthCentersResult" ->
                        field "data" decodeIndexDbQueryHealthCentersResult
                            |> andThen (\list_ -> succeed (IndexDbQueryHealthCentersResult (Dict.fromList list_)))

                    "IndexDbQueryDeferredPhotoResult" ->
                        oneOf
                            [ field "data" decodeIndexDbQueryDeferredPhotoResult
                                |> andThen (\record -> succeed (IndexDbQueryDeferredPhotoResult (Just record)))

                            -- In case we have no deferred photo.
                            , succeed (IndexDbQueryDeferredPhotoResult Nothing)
                            ]

                    _ ->
                        fail <| queryType ++ " is not a recognized IndexDbQueryTypeResult"
            )


decodeIndexDbQueryHealthCentersResult =
    (succeed (\a b -> ( a, b ))
        |> required "uuid" decodeEntityUuid
        |> custom Backend.HealthCenter.Decoder.decodeHealthCenter
    )
        |> list


decodeIndexDbQueryDeferredPhotoResult : Decoder IndexDbQueryDeferredPhotoResultRecord
decodeIndexDbQueryDeferredPhotoResult =
    succeed IndexDbQueryDeferredPhotoResultRecord
        |> requiredAt [ "0", "uuid" ] string
        |> requiredAt [ "0", "photo" ] string
        |> requiredAt [ "0", "attempts" ] int


decodeDownloadSyncResponseGeneral : Decoder (DownloadSyncResponse BackendGeneralEntity)
decodeDownloadSyncResponseGeneral =
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
                    "catchment_area" ->
                        Backend.HealthCenter.Decoder.decodeCatchmentArea
                            |> andThen (\entity -> succeed (BackendGeneralCatchmentArea uuid vid entity))

                    "health_center" ->
                        Backend.HealthCenter.Decoder.decodeHealthCenter
                            |> andThen (\entity -> succeed (BackendGeneralHealthCenter uuid vid entity))

                    "nurse" ->
                        Backend.Nurse.Decoder.decodeNurse
                            |> andThen (\entity -> succeed (BackendGeneralNurse uuid vid entity))

                    "person" ->
                        Backend.Person.Decoder.decodePerson
                            |> andThen (\entity -> succeed (BackendGeneralPerson uuid vid entity))

                    "pmtct_participant" ->
                        Backend.PmtctParticipant.Decoder.decodePmtctParticipant
                            |> andThen (\entity -> succeed (BackendGeneralPmtctParticipant uuid vid entity))

                    "relationship" ->
                        Backend.Relationship.Decoder.decodeRelationship
                            |> andThen (\entity -> succeed (BackendGeneralRelationship uuid vid entity))

                    _ ->
                        succeed (BackendGeneralEntityUnknown type_ vid)
            )


decodeDownloadSyncResponseAuthority : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthority =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list decodeBackendAuthorityEntity)
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
        )


decodeBackendAuthorityEntity : Decoder BackendAuthorityEntity
decodeBackendAuthorityEntity =
    (succeed (\a b c -> ( a, b, c ))
        |> required "type" string
        |> required "uuid" string
        |> required "vid" decodeInt
    )
        |> andThen
            (\( type_, uuid, vid ) ->
                case type_ of
                    "photo" ->
                        Backend.Measurement.Decoder.decodePhoto
                            |> andThen (\entity -> succeed (BackendAuthorityPhoto uuid vid entity))

                    "weight" ->
                        Backend.Measurement.Decoder.decodeWeight
                            |> andThen (\entity -> succeed (BackendAuthorityWeight uuid vid entity))

                    _ ->
                        succeed (BackendAuthorityEntityUnknown type_ vid)
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
