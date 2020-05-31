module DataManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    )

import AssocList as Dict
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
        , IndexDbQueryUploadGeneralResultRecord
        , IndexDbQueryUploadPhotoResultRecord
        , UploadMethod(..)
        , UploadPhotoError(..)
        )
import Gizra.Date exposing (decodeDate)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import RemoteData exposing (RemoteData)
import Time


decodeIndexDbQueryTypeResult : Decoder IndexDbQueryTypeResult
decodeIndexDbQueryTypeResult =
    field "queryType" string
        |> andThen
            (\queryType ->
                case queryType of
                    "IndexDbQueryUploadPhotoGeneralResult" ->
                        decodeIndexDbQueryUploadPhotoResultRecordRemoteData
                            |> andThen (\val -> succeed (IndexDbQueryUploadPhotoGeneralResult val))

                    "IndexDbQueryUploadGeneralResult" ->
                        oneOf
                            [ field "data" decodeIndexDbQueryUploadGeneralResultRecord
                                |> andThen (\record -> succeed (IndexDbQueryUploadGeneralResult (Just record)))

                            -- In case we have no entities to upload.
                            , succeed (IndexDbQueryUploadGeneralResult Nothing)
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


decodeIndexDbQueryUploadPhotoResultRecordRemoteData : Decoder (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
decodeIndexDbQueryUploadPhotoResultRecordRemoteData =
    at [ "data", "tag" ] string
        |> andThen
            (\tag ->
                case tag of
                    "Success" ->
                        oneOf
                            [ field "data" decodeIndexDbQueryUploadPhotoResultRecord
                                |> andThen (\record -> succeed (RemoteData.Success (Just record)))

                            -- In case we have no photos to upload.
                            , succeed (RemoteData.Success Nothing)
                            ]

                    "Error" ->
                        (succeed (\a b -> ( a, b ))
                            |> requiredAt [ "data", "error" ] string
                            |> optionalAt [ "data", "reason" ] (nullable string) Nothing
                        )
                            |> andThen
                                (\( error, maybeReason ) ->
                                    case error of
                                        "PhotoNotFoundOnCacheStorage" ->
                                            succeed (RemoteData.Failure PhotoNotFoundOnCacheStorage)

                                        "FetchError" ->
                                            succeed (RemoteData.Failure <| FetchError (Maybe.withDefault "" maybeReason))

                                        "BadJson" ->
                                            succeed (RemoteData.Failure <| BadJson (Maybe.withDefault "" maybeReason))

                                        _ ->
                                            fail <| error ++ " is not a recognized Error tag IndexDbQueryUploadGeneralResultRecord"
                                )

                    _ ->
                        fail <| tag ++ " is not a recognized Error for decodeIndexDbQueryUploadPhotoResultRecordRemoteData"
            )


decodeIndexDbQueryUploadPhotoResultRecord : Decoder IndexDbQueryUploadPhotoResultRecord
decodeIndexDbQueryUploadPhotoResultRecord =
    succeed IndexDbQueryUploadPhotoResultRecord
        |> required "uuid" string
        |> required "photo" string
        |> required "localId" int
        |> optional "fileId" (nullable int) Nothing


decodeIndexDbQueryUploadGeneralResultRecord : Decoder IndexDbQueryUploadGeneralResultRecord
decodeIndexDbQueryUploadGeneralResultRecord =
    succeed IndexDbQueryUploadGeneralResultRecord
        |> required "entities" (list decodeBackendGeneralEntityAndUploadMethod)
        |> required "uploadPhotos"
            (list decodeIndexDbQueryUploadPhotoResultRecord
                |> andThen
                    (\list_ ->
                        -- Convert list to a dict.
                        list_
                            |> List.map (\row -> ( row.localId, row ))
                            |> Dict.fromList
                            |> succeed
                    )
            )


{-| We need to get the localId, which is wrapping the data. It looks something like:

{ type: "person"
, uuid: "some-uuid"
, localId: 1
, data: {label: "My name", gender: "female"
...
}

So we grab the `localId` and `uuid`, and feed them to the decodeBackendGeneralEntity.

-}
decodeBackendGeneralEntityAndUploadMethod : Decoder ( BackendGeneralEntity, UploadMethod )
decodeBackendGeneralEntityAndUploadMethod =
    (succeed (\a b -> ( a, b ))
        |> required "uuid" string
        |> required "localId" int
    )
        |> andThen
            (\( uuid, localId ) ->
                succeed (\c d -> ( c, d ))
                    |> required "data" (decodeBackendGeneralEntity (hardcoded uuid) (hardcoded localId))
                    |> required "method" decodeUploadMethod
            )


decodeUploadMethod : Decoder UploadMethod
decodeUploadMethod =
    string
        |> andThen
            (\str ->
                case str of
                    "POST" ->
                        succeed UploadMethodCreate

                    "PATCH" ->
                        succeed UploadMethodUpdate

                    _ ->
                        fail <| str ++ " is not a recognized UploadMethod"
            )


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
            |> required "batch" (list <| decodeBackendGeneralEntity (required "uuid" string) (required "vid" decodeInt))
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
        )



--decodeBackendGeneralEntity : Decoder (number -> BackendGeneralEntity) -> Decoder BackendGeneralEntity


decodeBackendGeneralEntity uuidDecoder identifierDecoder =
    (succeed (\a b c -> ( a, b, c ))
        |> required "type" string
        |> uuidDecoder
        |> identifierDecoder
    )
        |> andThen
            (\( type_, uuid, identifier_ ) ->
                case type_ of
                    "catchment_area" ->
                        Backend.HealthCenter.Decoder.decodeCatchmentArea
                            |> andThen (\entity -> succeed (BackendGeneralCatchmentArea uuid identifier_ entity))

                    "health_center" ->
                        Backend.HealthCenter.Decoder.decodeHealthCenter
                            |> andThen (\entity -> succeed (BackendGeneralHealthCenter uuid identifier_ entity))

                    "nurse" ->
                        Backend.Nurse.Decoder.decodeNurse
                            |> andThen (\entity -> succeed (BackendGeneralNurse uuid identifier_ entity))

                    "person" ->
                        Backend.Person.Decoder.decodePerson
                            |> andThen (\entity -> succeed (BackendGeneralPerson uuid identifier_ entity))

                    "pmtct_participant" ->
                        Backend.PmtctParticipant.Decoder.decodePmtctParticipant
                            |> andThen (\entity -> succeed (BackendGeneralPmtctParticipant uuid identifier_ entity))

                    "relationship" ->
                        Backend.Relationship.Decoder.decodeRelationship
                            |> andThen (\entity -> succeed (BackendGeneralRelationship uuid identifier_ entity))

                    _ ->
                        succeed (BackendGeneralEntityUnknown type_ identifier_)
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
                let
                    doDecode decoder tag =
                        decoder
                            |> andThen (\entity -> succeed (tag uuid vid entity))
                in
                case type_ of
                    "attendance" ->
                        doDecode Backend.Measurement.Decoder.decodeAttendance BackendAuthorityAttendance

                    "breast_exam" ->
                        doDecode Backend.Measurement.Decoder.decodeBreastExam BackendAuthorityBreastExam

                    "nutrition_photo" ->
                        doDecode Backend.Measurement.Decoder.decodeNutritionPhoto BackendAuthorityNutritionPhoto

                    "photo" ->
                        doDecode Backend.Measurement.Decoder.decodePhoto BackendAuthorityPhoto

                    "weight" ->
                        doDecode Backend.Measurement.Decoder.decodeWeight BackendAuthorityWeight

                    _ ->
                        succeed (BackendAuthorityEntityUnknown type_ vid)
            )



-- @todo: Needed? Move to utils.


decodeTimeField : String -> Decoder Time.Posix
decodeTimeField fieldName =
    map Time.millisToPosix (field fieldName decodeInt)
