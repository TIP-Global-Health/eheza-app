module DataManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    )

import AssocList as Dict
import Backend.Clinic.Decoder
import Backend.Counseling.Decoder
import Backend.HealthCenter.Decoder
import Backend.IndividualEncounterParticipant.Decoder
import Backend.Measurement.Decoder
import Backend.Nurse.Decoder
import Backend.NutritionEncounter.Decoder
import Backend.ParticipantConsent.Decoder
import Backend.Person.Decoder
import Backend.PmtctParticipant.Decoder
import Backend.PrenatalEncounter.Decoder
import Backend.Relationship.Decoder
import Backend.Session.Decoder
import Backend.Village.Decoder
import DataManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , DownloadSyncResponse
        , IndexDbQueryDeferredPhotoResultRecord
        , IndexDbQueryTypeResult(..)
        , IndexDbQueryUploadAuthorityResultRecord
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
                    "IndexDbQueryUploadPhotoAuthorityResult" ->
                        decodeIndexDbQueryUploadPhotoResultRecordRemoteData
                            |> andThen (\val -> succeed (IndexDbQueryUploadPhotoAuthorityResult val))

                    "IndexDbQueryUploadAuthorityResult" ->
                        oneOf
                            [ field "data" decodeIndexDbQueryUploadAuthorityResultRecord
                                |> andThen (\record -> succeed (IndexDbQueryUploadAuthorityResult (Just record)))

                            -- In case we have no entities to upload.
                            , succeed (IndexDbQueryUploadAuthorityResult Nothing)
                            ]

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
        |> required "entities" (list <| decodeBackendEntityAndUploadMethod (\uuid localId -> decodeBackendGeneralEntity (hardcoded uuid) (hardcoded localId)))


decodeIndexDbQueryUploadAuthorityResultRecord : Decoder IndexDbQueryUploadAuthorityResultRecord
decodeIndexDbQueryUploadAuthorityResultRecord =
    succeed IndexDbQueryUploadAuthorityResultRecord
        |> required "entities" (list <| decodeBackendEntityAndUploadMethod (\uuid localId -> decodeBackendAuthorityEntity (hardcoded uuid) (hardcoded localId)))
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



--decodeBackendGeneralEntityAndUploadMethod : a -> Decoder ( BackendGeneralEntity, UploadMethod )


decodeBackendEntityAndUploadMethod func =
    (succeed (\a b -> ( a, b ))
        |> required "uuid" string
        |> required "localId" int
    )
        |> andThen
            (\( uuid, localId ) ->
                succeed (\c d -> ( c, d ))
                    |> required "data" (func uuid localId)
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
            (\( type_, uuid, revisionIdentifier ) ->
                let
                    doDecode decoder tag =
                        decoder
                            |> andThen
                                (\entity ->
                                    let
                                        backendEntity =
                                            { uuid = uuid
                                            , revision = revisionIdentifier
                                            , entity = entity
                                            }
                                    in
                                    succeed (tag backendEntity)
                                )
                in
                case type_ of
                    "catchment_area" ->
                        doDecode Backend.HealthCenter.Decoder.decodeCatchmentArea BackendGeneralCatchmentArea

                    "counseling_schedule" ->
                        doDecode Backend.Counseling.Decoder.decodeCounselingSchedule BackendGeneralCounselingSchedule

                    "counseling_topic" ->
                        doDecode Backend.Counseling.Decoder.decodeCounselingTopic BackendGeneralCounselingTopic

                    "health_center" ->
                        doDecode Backend.HealthCenter.Decoder.decodeHealthCenter BackendGeneralHealthCenter

                    "nurse" ->
                        doDecode Backend.Nurse.Decoder.decodeNurse BackendGeneralNurse

                    "participant_form" ->
                        doDecode Backend.ParticipantConsent.Decoder.decodeParticipantForm BackendGeneralParticipantForm

                    "village" ->
                        doDecode Backend.Village.Decoder.decodeVillage BackendGeneralVillage

                    _ ->
                        succeed (BackendGeneralEntityUnknown type_ revisionIdentifier)
            )


decodeDownloadSyncResponseAuthority : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthority =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendAuthorityEntity (required "uuid" string) (required "vid" decodeInt))
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
        )



--decodeBackendAuthorityEntity : Decoder BackendAuthorityEntity


decodeBackendAuthorityEntity uuidDecoder identifierDecoder =
    (succeed (\a b c -> ( a, b, c ))
        |> required "type" string
        |> uuidDecoder
        |> identifierDecoder
    )
        |> andThen
            (\( type_, uuid, revisionIdentifier ) ->
                let
                    doDecode decoder tag =
                        decoder
                            |> andThen
                                (\entity ->
                                    let
                                        backendEntity =
                                            { uuid = uuid
                                            , revision = revisionIdentifier
                                            , entity = entity
                                            }
                                    in
                                    succeed (tag backendEntity)
                                )
                in
                case type_ of
                    "attendance" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAttendance
                            BackendAuthorityAttendance

                    "child_fbf" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFbf
                            BackendAuthorityChildFbf

                    "breast_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeBreastExam
                            BackendAuthorityBreastExam

                    "clinic" ->
                        doDecode
                            Backend.Clinic.Decoder.decodeClinic
                            BackendAuthorityClinic

                    "counseling_session" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeCounselingSession
                            BackendAuthorityCounselingSession

                    "core_physical_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeCorePhysicalExam
                            BackendAuthorityCorePhysicalExam

                    "danger_signs" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeDangerSigns
                            BackendAuthorityDangerSigns

                    "family_planning" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFamilyPlanning
                            BackendAuthorityFamilyPlanning

                    "height" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHeight
                            BackendAuthorityHeight

                    "individual_participant" ->
                        doDecode
                            Backend.IndividualEncounterParticipant.Decoder.decodeIndividualEncounterParticipant
                            BackendAuthorityIndividualParticipant

                    "lactation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeLactation
                            BackendAuthorityLactation

                    "last_menstrual_period" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeLastMenstrualPeriod
                            BackendAuthorityLastMenstrualPeriod

                    "medical_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMedicalHistory
                            BackendAuthorityMedicalHistory

                    "medication" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMedication
                            BackendAuthorityMedication

                    "mother_fbf" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFbf
                            BackendAuthorityMotherFbf

                    "muac" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMuac
                            BackendAuthorityMuac

                    "nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutrition
                            BackendAuthorityNutrition

                    "nutrition_encounter" ->
                        doDecode
                            Backend.NutritionEncounter.Decoder.decodeNutritionEncounter
                            BackendAuthorityNutritionEncounter

                    "nutrition_height" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionHeight
                            BackendAuthorityNutritionHeight

                    "nutrition_muac" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionMuac
                            BackendAuthorityNutritionMuac

                    "nutrition_nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionNutrition
                            BackendAuthorityNutritionNutrition

                    "nutrition_photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionPhoto
                            BackendAuthorityNutritionPhoto

                    "nutrition_weight" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionWeight
                            BackendAuthorityNutritionWeight

                    "obstetric_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeObstetricHistory
                            BackendAuthorityObstetricHistory

                    "obstetric_history_step2" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeObstetricHistoryStep2
                            BackendAuthorityObstetricHistoryStep2

                    "obstetrical_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeObstetricalExam
                            BackendAuthorityObstetricalExam

                    "participant_consent" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeParticipantConsent
                            BackendAuthorityParticipantConsent

                    "person" ->
                        doDecode
                            Backend.Person.Decoder.decodePerson
                            BackendAuthorityPerson

                    "photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePhoto
                            BackendAuthorityPhoto

                    "prenatal_photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalPhoto
                            BackendAuthorityPrenatalPhoto

                    "pmtct_participant" ->
                        doDecode Backend.PmtctParticipant.Decoder.decodePmtctParticipant BackendAuthorityPmtctParticipant

                    "prenatal_family_planning" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalFamilyPlanning
                            BackendAuthorityPrenatalFamilyPlanning

                    "prenatal_nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalNutrition
                            BackendAuthorityPrenatalNutrition

                    "prenatal_encounter" ->
                        doDecode
                            Backend.PrenatalEncounter.Decoder.decodePrenatalEncounter
                            BackendAuthorityPrenatalEncounter

                    "relationship" ->
                        doDecode Backend.Relationship.Decoder.decodeRelationship BackendAuthorityRelationship

                    "resource" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeResource
                            BackendAuthorityResource

                    "session" ->
                        doDecode
                            Backend.Session.Decoder.decodeSession
                            BackendAuthoritySession

                    "social_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSocialHistory
                            BackendAuthoritySocialHistory

                    "weight" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWeight
                            BackendAuthorityWeight

                    _ ->
                        succeed (BackendAuthorityEntityUnknown type_ revisionIdentifier)
            )



-- @todo: Needed? Move to utils.


decodeTimeField : String -> Decoder Time.Posix
decodeTimeField fieldName =
    map Time.millisToPosix (field fieldName decodeInt)
