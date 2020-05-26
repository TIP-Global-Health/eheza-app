module DataManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    )

import Backend.HealthCenter.Decoder
import Backend.Measurement.Decoder
import Backend.Nurse.Decoder
import Backend.Person.Decoder
import Backend.PmtctParticipant.Decoder
import Backend.Relationship.Decoder
import DataManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , DownloadSyncResponse
        , IndexDbQueryDeferredPhotoResultRecord
        , IndexDbQueryTypeResult(..)
        , IndexDbQueryUploadPhotoResultRecord
        )
import Gizra.Date exposing (decodeDate)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time


decodeIndexDbQueryTypeResult : Decoder IndexDbQueryTypeResult
decodeIndexDbQueryTypeResult =
    field "queryType" string
        |> andThen
            (\queryType ->
                case queryType of
                    "IndexDbQueryUploadPhotoGeneralResult" ->
                        oneOf
                            [ field "data" decodeIndexDbQueryUploadPhotoResultRecord
                                |> andThen (\record -> succeed (IndexDbQueryUploadPhotoGeneralResult (Just record)))

                            -- In case we have no photos to upload.
                            , succeed (IndexDbQueryUploadPhotoGeneralResult Nothing)
                            ]

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


decodeIndexDbQueryUploadPhotoResultRecord : Decoder IndexDbQueryUploadPhotoResultRecord
decodeIndexDbQueryUploadPhotoResultRecord =
    succeed IndexDbQueryUploadPhotoResultRecord
        |> requiredAt [ "0", "uuid" ] string
        |> requiredAt [ "0", "photo" ] string
        |> requiredAt [ "0", "localId" ] int
        |> optionalAt [ "0", "fileId" ] (nullable int) Nothing


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
                    "attendance" ->
                        Backend.Measurement.Decoder.decodeAttendance
                            |> andThen (\entity -> succeed (BackendAuthorityAttendance uuid vid entity))

                    "photo" ->
                        Backend.Measurement.Decoder.decodePhoto
                            |> andThen (\entity -> succeed (BackendAuthorityPhoto uuid vid entity))

                    "weight" ->
                        Backend.Measurement.Decoder.decodeWeight
                            |> andThen (\entity -> succeed (BackendAuthorityWeight uuid vid entity))

                    _ ->
                        succeed (BackendAuthorityEntityUnknown type_ vid)
            )



-- @todo: Needed? Move to utils.


decodeTimeField : String -> Decoder Time.Posix
decodeTimeField fieldName =
    map Time.millisToPosix (field fieldName decodeInt)
