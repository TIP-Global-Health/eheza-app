module SyncManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseAuthorityStats
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    )

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Decoder
import Backend.Clinic.Decoder
import Backend.Counseling.Decoder
import Backend.Dashboard.Decoder
import Backend.HealthCenter.Decoder
import Backend.HomeVisitEncounter.Decoder
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
import Gizra.Date exposing (decodeDate)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import RemoteData exposing (RemoteData)
import SyncManager.Model
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

                    "IndexDbQueryGetTotalEntriesToUploadResult" ->
                        field "data" decodeInt
                            |> andThen (\val -> succeed (IndexDbQueryGetTotalEntriesToUploadResult val))

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
                            [ at [ "data", "result" ] decodeIndexDbQueryUploadPhotoResultRecord
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
                                        "BadJson" ->
                                            succeed (RemoteData.Failure <| BadJson (Maybe.withDefault "" maybeReason))

                                        "NetworkError" ->
                                            succeed (RemoteData.Failure <| NetworkError (Maybe.withDefault "" maybeReason))

                                        "UploadError" ->
                                            succeed (RemoteData.Failure <| UploadError (Maybe.withDefault "" maybeReason))

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
        |> required "remaining" decodeInt


decodeIndexDbQueryUploadAuthorityResultRecord : Decoder IndexDbQueryUploadAuthorityResultRecord
decodeIndexDbQueryUploadAuthorityResultRecord =
    succeed IndexDbQueryUploadAuthorityResultRecord
        |> required "entities" (list <| decodeBackendEntityAndUploadMethod (\uuid localId -> decodeBackendAuthorityEntity (hardcoded uuid) (hardcoded localId)))
        |> required "remaining" decodeInt
        |> optional "uploadPhotos"
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
            Dict.empty


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
        |> requiredAt [ "0", "remaining" ] int


decodeDownloadSyncResponseGeneral : Decoder (DownloadSyncResponse BackendGeneralEntity)
decodeDownloadSyncResponseGeneral =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendGeneralEntity (required "uuid" string) (required "vid" decodeInt))
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
            |> optional "device_name" string ""
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
                        fail <| type_ ++ " is unknown BackendGeneralEntity"
            )


decodeDownloadSyncResponseAuthority : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthority =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendAuthorityEntity (required "uuid" string) (required "vid" decodeInt))
            |> required "last_timestamp" decodeDate
            |> required "revision_count" decodeInt
            |> hardcoded ""
        )


decodeDownloadSyncResponseAuthorityStats : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthorityStats =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendAuthorityEntity (required "uuid" string) (required "vid" decodeInt))
            |> hardcoded (Time.millisToPosix 0)
            |> hardcoded 0
            |> hardcoded ""
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
                    "acute_findings" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteFindings
                            BackendAuthorityAcuteFindings

                    "acute_illness_danger_signs" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessDangerSigns
                            BackendAuthorityAcuteIllnessDangerSigns

                    "acute_illness_encounter" ->
                        doDecode
                            Backend.AcuteIllnessEncounter.Decoder.decodeAcuteIllnessEncounter
                            BackendAuthorityAcuteIllnessEncounter

                    "acute_illness_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessFollowUp
                            BackendAuthorityAcuteIllnessFollowUp

                    "acute_illness_muac" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessMuac
                            BackendAuthorityAcuteIllnessMuac

                    "acute_illness_nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessNutrition
                            BackendAuthorityAcuteIllnessNutrition

                    "acute_illness_vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessVitals
                            BackendAuthorityAcuteIllnessVitals

                    "attendance" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAttendance
                            BackendAuthorityAttendance

                    "call_114" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeCall114
                            BackendAuthorityCall114

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

                    "contributing_factors" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeContributingFactors
                            BackendAuthorityContributingFactors

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

                    "exposure" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeExposure
                            BackendAuthorityExposure

                    "family_planning" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFamilyPlanning
                            BackendAuthorityFamilyPlanning

                    "follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFollowUp
                            BackendAuthorityFollowUp

                    "group_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeGroupHealthEducation
                            BackendAuthorityGroupHealthEducation

                    "group_send_to_hc" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeGroupSendToHC
                            BackendAuthorityGroupSendToHC

                    "health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHealthEducation
                            BackendAuthorityHealthEducation

                    "hc_contact" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHCContact
                            BackendAuthorityHCContact

                    "height" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHeight
                            BackendAuthorityHeight

                    "home_visit_encounter" ->
                        doDecode
                            Backend.HomeVisitEncounter.Decoder.decodeHomeVisitEncounter
                            BackendAuthorityHomeVisitEncounter

                    "individual_participant" ->
                        doDecode
                            Backend.IndividualEncounterParticipant.Decoder.decodeIndividualEncounterParticipant
                            BackendAuthorityIndividualParticipant

                    "isolation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeIsolation
                            BackendAuthorityIsolation

                    "lactation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeLactation
                            BackendAuthorityLactation

                    "last_menstrual_period" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeLastMenstrualPeriod
                            BackendAuthorityLastMenstrualPeriod

                    "malaria_testing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMalariaTesting
                            BackendAuthorityMalariaTesting

                    "medical_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMedicalHistory
                            BackendAuthorityMedicalHistory

                    "medication" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMedication
                            BackendAuthorityMedication

                    "medication_distribution" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMedicationDistribution
                            BackendAuthorityMedicationDistribution

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

                    "nutrition_caring" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionCaring
                            BackendAuthorityNutritionCaring

                    "nutrition_contributing_factors" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionContributingFactors
                            BackendAuthorityNutritionContributingFactors

                    "nutrition_encounter" ->
                        doDecode
                            Backend.NutritionEncounter.Decoder.decodeNutritionEncounter
                            BackendAuthorityNutritionEncounter

                    "nutrition_feeding" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionFeeding
                            BackendAuthorityNutritionFeeding

                    "nutrition_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionFollowUp
                            BackendAuthorityNutritionFollowUp

                    "nutrition_food_security" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionFoodSecurity
                            BackendAuthorityNutritionFoodSecurity

                    "nutrition_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionHealthEducation
                            BackendAuthorityNutritionHealthEducation

                    "nutrition_height" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionHeight
                            BackendAuthorityNutritionHeight

                    "nutrition_hygiene" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionHygiene
                            BackendAuthorityNutritionHygiene

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

                    "nutrition_send_to_hc" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionSendToHC
                            BackendAuthorityNutritionSendToHC

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

                    "pregnancy_testing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePregnancyTesting
                            BackendAuthorityPregnancyTesting

                    "prenatal_photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalPhoto
                            BackendAuthorityPrenatalPhoto

                    "pmtct_participant" ->
                        doDecode
                            Backend.PmtctParticipant.Decoder.decodePmtctParticipant
                            BackendAuthorityPmtctParticipant

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

                    "send_to_hc" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSendToHC
                            BackendAuthoritySendToHC

                    "session" ->
                        doDecode
                            Backend.Session.Decoder.decodeSession
                            BackendAuthoritySession

                    "social_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSocialHistory
                            BackendAuthoritySocialHistory

                    "statistics" ->
                        doDecode
                            Backend.Dashboard.Decoder.decodeDashboardStats
                            BackendAuthorityDashboardStats

                    "symptoms_general" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSymptomsGeneral
                            BackendAuthoritySymptomsGeneral

                    "symptoms_gi" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSymptomsGI
                            BackendAuthoritySymptomsGI

                    "symptoms_respiratory" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeSymptomsRespiratory
                            BackendAuthoritySymptomsRespiratory

                    "travel_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTravelHistory
                            BackendAuthorityTravelHistory

                    "treatment_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTreatmentReview
                            BackendAuthorityTreatmentReview

                    "treatment_ongoing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTreatmentOngoing
                            BackendAuthorityTreatmentOngoing

                    "vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeVitals
                            BackendAuthorityVitals

                    "weight" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWeight
                            BackendAuthorityWeight

                    _ ->
                        fail <| type_ ++ " is unknown BackendAuthorityEntity"
            )
