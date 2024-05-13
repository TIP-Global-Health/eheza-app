module SyncManager.Decoder exposing
    ( decodeDownloadSyncResponseAuthority
    , decodeDownloadSyncResponseAuthorityStats
    , decodeDownloadSyncResponseGeneral
    , decodeIndexDbQueryTypeResult
    , decodeIndexDbSaveResult
    )

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Decoder
import Backend.ChildScoreboardEncounter.Decoder
import Backend.Clinic.Decoder
import Backend.Counseling.Decoder
import Backend.Dashboard.Decoder
import Backend.EducationSession.Decoder
import Backend.HIVEncounter.Decoder
import Backend.HealthCenter.Decoder
import Backend.HomeVisitEncounter.Decoder
import Backend.IndividualEncounterParticipant.Decoder
import Backend.Measurement.Decoder
import Backend.NCDEncounter.Decoder
import Backend.Nurse.Decoder
import Backend.NutritionEncounter.Decoder
import Backend.ParticipantConsent.Decoder
import Backend.Person.Decoder
import Backend.PmtctParticipant.Decoder
import Backend.PrenatalEncounter.Decoder
import Backend.Relationship.Decoder
import Backend.ResilienceMessage.Decoder
import Backend.ResilienceSurvey.Decoder
import Backend.Session.Decoder
import Backend.StockUpdate.Decoder
import Backend.TuberculosisEncounter.Decoder
import Backend.Village.Decoder
import Backend.WellChildEncounter.Decoder
import Components.ReportToWhatsAppDialog.Decoder exposing (decodeReportType)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import RemoteData exposing (RemoteData)
import SyncManager.Model exposing (..)
import SyncManager.Utils exposing (siteFeaturesFromString, siteFromString)
import Translate.Utils exposing (decodeLanguage)


decodeIndexDbQueryTypeResult : Decoder IndexDbQueryTypeResult
decodeIndexDbQueryTypeResult =
    field "queryType" string
        |> andThen
            (\queryType ->
                case queryType of
                    "IndexDbQueryUploadPhotoResult" ->
                        decodeIndexDbQueryUploadPhotoResultRecordRemoteData
                            |> andThen (\val -> succeed (IndexDbQueryUploadPhotoResult val))

                    "IndexDbQueryUploadScreenshotResult" ->
                        decodeIndexDbQueryUploadScreenshotResultRecordRemoteData
                            |> andThen (\val -> succeed (IndexDbQueryUploadScreenshotResult val))

                    "IndexDbQueryUploadAuthorityResult" ->
                        field "data" decodeIndexDbQueryUploadAuthorityResultRecord
                            |> andThen (\record -> succeed (IndexDbQueryUploadAuthorityResult (Just record)))

                    "IndexDbQueryUploadGeneralResult" ->
                        field "data" decodeIndexDbQueryUploadGeneralResultRecord
                            |> andThen (\record -> succeed (IndexDbQueryUploadGeneralResult (Just record)))

                    "IndexDbQueryUploadWhatsAppResult" ->
                        field "data" decodeIndexDbQueryUploadWhatsAppResultRecord
                            |> andThen (\record -> succeed (IndexDbQueryUploadWhatsAppResult (Just record)))

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

                    "IndexDbQueryGetShardsEntityByUuidResult" ->
                        field "data" string
                            |> andThen (\val -> succeed (IndexDbQueryGetShardsEntityByUuidResult val))

                    _ ->
                        fail <| queryType ++ " is not a recognized IndexDbQueryTypeResult"
            )


decodeIndexDbQueryUploadPhotoResultRecordRemoteData : Decoder (RemoteData UploadFileError (Maybe IndexDbQueryUploadPhotoResultRecord))
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
                                            fail <| error ++ " is not a recognized Error tag IndexDbQueryUploadPhotoResultRecord"
                                )

                    _ ->
                        fail <| tag ++ " is not a recognized Error for decodeIndexDbQueryUploadPhotoResultRecordRemoteData"
            )


decodeIndexDbQueryUploadScreenshotResultRecordRemoteData : Decoder (RemoteData UploadFileError (Maybe IndexDbQueryUploadFileResultRecord))
decodeIndexDbQueryUploadScreenshotResultRecordRemoteData =
    at [ "data", "tag" ] string
        |> andThen
            (\tag ->
                case tag of
                    "Success" ->
                        oneOf
                            [ at [ "data", "result" ] decodeIndexDbQueryUploadFileResultRecord
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


decodeIndexDbQueryUploadFileResultRecord : Decoder IndexDbQueryUploadFileResultRecord
decodeIndexDbQueryUploadFileResultRecord =
    succeed IndexDbQueryUploadFileResultRecord
        |> required "localId" int
        |> optional "fileId" (nullable int) Nothing


decodeIndexDbQueryUploadGeneralResultRecord : Decoder IndexDbQueryUploadGeneralResultRecord
decodeIndexDbQueryUploadGeneralResultRecord =
    succeed IndexDbQueryUploadGeneralResultRecord
        |> required "entities" (list <| decodeBackendEntityAndUploadMethod (\uuid localId -> decodeBackendGeneralEntity (hardcoded uuid) (hardcoded localId)))
        |> required "remaining" decodeInt


decodeIndexDbQueryUploadWhatsAppResultRecord : Decoder IndexDbQueryUploadWhatsAppResultRecord
decodeIndexDbQueryUploadWhatsAppResultRecord =
    succeed IndexDbQueryUploadWhatsAppResultRecord
        |> required "entities" (list decodeBackendWhatsAppEntity)
        |> required "remaining" decodeInt


decodeBackendWhatsAppEntity : Decoder BackendWhatsAppEntity
decodeBackendWhatsAppEntity =
    succeed BackendWhatsAppEntity
        |> required "localId" decodeInt
        |> required "person" string
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "language" decodeLanguage
        |> required "report_type" decodeReportType
        |> required "phone_number" string
        |> required "fileId" decodeInt


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
            |> required "revision_count" decodeInt
            |> optional "device_name" string ""
            |> optional "rollbar_token" string ""
            |> optional "site" decodeSite SiteRwanda
            |> optional "features" decodeSiteFeatures EverySet.empty
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

                    "resilience_survey" ->
                        doDecode Backend.ResilienceSurvey.Decoder.decodeResilienceSurvey BackendGeneralResilienceSurvey

                    _ ->
                        fail <| type_ ++ " is unknown BackendGeneralEntity"
            )


decodeSite : Decoder Site
decodeSite =
    string
        |> andThen (siteFromString >> succeed)


decodeSiteFeatures : Decoder (EverySet SiteFeature)
decodeSiteFeatures =
    string
        |> andThen (siteFeaturesFromString >> succeed)


decodeDownloadSyncResponseAuthority : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthority =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendAuthorityEntity (required "uuid" string) (required "vid" decodeInt))
            |> required "revision_count" decodeInt
            |> hardcoded ""
            |> hardcoded ""
            |> hardcoded SiteUnknown
            |> hardcoded EverySet.empty
        )


decodeDownloadSyncResponseAuthorityStats : Decoder (DownloadSyncResponse BackendAuthorityEntity)
decodeDownloadSyncResponseAuthorityStats =
    field "data"
        (succeed DownloadSyncResponse
            |> required "batch" (list <| decodeBackendAuthorityEntity (required "uuid" string) (required "vid" decodeInt))
            |> hardcoded 0
            |> hardcoded ""
            |> hardcoded ""
            |> hardcoded SiteUnknown
            |> hardcoded EverySet.empty
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

                    "acute_illness_contacts_tracing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessContactsTracing
                            BackendAuthorityAcuteIllnessContactsTracing

                    "acute_illness_core_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessCoreExam
                            BackendAuthorityAcuteIllnessCoreExam

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

                    "acute_illness_trace_contact" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessTraceContact
                            BackendAuthorityAcuteIllnessTraceContact

                    "acute_illness_vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAcuteIllnessVitals
                            BackendAuthorityAcuteIllnessVitals

                    "appointment_confirmation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAppointmentConfirmation
                            BackendAuthorityAppointmentConfirmation

                    "attendance" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeAttendance
                            BackendAuthorityAttendance

                    "birth_plan" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeBirthPlan
                            BackendAuthorityBirthPlan

                    "breast_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeBreastExam
                            BackendAuthorityBreastExam

                    "call_114" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeCall114
                            BackendAuthorityCall114

                    "child_fbf" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeFbf
                            BackendAuthorityChildFbf

                    "child_scoreboard_encounter" ->
                        doDecode
                            Backend.ChildScoreboardEncounter.Decoder.decodeChildScoreboardEncounter
                            BackendAuthorityChildScoreboardEncounter

                    "child_scoreboard_bcg_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardBCGImmunisation
                            BackendAuthorityChildScoreboardBCGImmunisation

                    "child_scoreboard_dtp_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardDTPImmunisation
                            BackendAuthorityChildScoreboardDTPImmunisation

                    "child_scoreboard_dtp_sa_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardDTPStandaloneImmunisation
                            BackendAuthorityChildScoreboardDTPStandaloneImmunisation

                    "child_scoreboard_ipv_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardIPVImmunisation
                            BackendAuthorityChildScoreboardIPVImmunisation

                    "child_scoreboard_mr_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardMRImmunisation
                            BackendAuthorityChildScoreboardMRImmunisation

                    "child_scoreboard_ncda" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardNCDA
                            BackendAuthorityChildScoreboardNCDA

                    "child_scoreboard_opv_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardOPVImmunisation
                            BackendAuthorityChildScoreboardOPVImmunisation

                    "child_scoreboard_pcv13_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardPCV13Immunisation
                            BackendAuthorityChildScoreboardPCV13Immunisation

                    "child_scoreboard_rotarix_iz" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeChildScoreboardRotarixImmunisation
                            BackendAuthorityChildScoreboardRotarixImmunisation

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

                    "covid_testing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeCovidTesting
                            BackendAuthorityCovidTesting

                    "danger_signs" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeDangerSigns
                            BackendAuthorityDangerSigns

                    "education_session" ->
                        doDecode
                            Backend.EducationSession.Decoder.decodeEducationSession
                            BackendAuthorityEducationSession

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

                    "group_ncda" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeGroupNCDA
                            BackendAuthorityGroupNCDA

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

                    "hiv_diagnostics" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVDiagnostics
                            BackendAuthorityHIVDiagnostics

                    "hiv_encounter" ->
                        doDecode
                            Backend.HIVEncounter.Decoder.decodeHIVEncounter
                            BackendAuthorityHIVEncounter

                    "hiv_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVFollowUp
                            BackendAuthorityHIVFollowUp

                    "hiv_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVHealthEducation
                            BackendAuthorityHIVHealthEducation

                    "hiv_medication" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVMedication
                            BackendAuthorityHIVMedication

                    "hiv_referral" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVReferral
                            BackendAuthorityHIVReferral

                    "hiv_symptom_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVSymptomReview
                            BackendAuthorityHIVSymptomReview

                    "hiv_treatment_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeHIVTreatmentReview
                            BackendAuthorityHIVTreatmentReview

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

                    "ncd_co_morbidities" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDCoMorbidities
                            BackendAuthorityNCDCoMorbidities

                    "ncd_core_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDCoreExam
                            BackendAuthorityNCDCoreExam

                    "ncd_creatinine_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDCreatinineTest
                            BackendAuthorityNCDCreatinineTest

                    "ncd_danger_signs" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDDangerSigns
                            BackendAuthorityNCDDangerSigns

                    "ncd_encounter" ->
                        doDecode
                            Backend.NCDEncounter.Decoder.decodeNCDEncounter
                            BackendAuthorityNCDEncounter

                    "ncd_family_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDFamilyHistory
                            BackendAuthorityNCDFamilyHistory

                    "ncd_family_planning" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDFamilyPlanning
                            BackendAuthorityNCDFamilyPlanning

                    "ncd_hba1c_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDHbA1cTest
                            BackendAuthorityNCDHbA1cTest

                    "ncd_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDHealthEducation
                            BackendAuthorityNCDHealthEducation

                    "ncd_hiv_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDHIVTest
                            BackendAuthorityNCDHIVTest

                    "ncd_labs_results" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDLabsResults
                            BackendAuthorityNCDLabsResults

                    "ncd_lipid_panel_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDLipidPanelTest
                            BackendAuthorityNCDLipidPanelTest

                    "ncd_liver_function_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDLiverFunctionTest
                            BackendAuthorityNCDLiverFunctionTest

                    "ncd_medication_distribution" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDMedicationDistribution
                            BackendAuthorityNCDMedicationDistribution

                    "ncd_medication_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDMedicationHistory
                            BackendAuthorityNCDMedicationHistory

                    "ncd_outside_care" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDOutsideCare
                            BackendAuthorityNCDOutsideCare

                    "ncd_pregnancy_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDPregnancyTest
                            BackendAuthorityNCDPregnancyTest

                    "ncd_random_blood_sugar_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDRandomBloodSugarTest
                            BackendAuthorityNCDRandomBloodSugarTest

                    "ncd_referral" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDReferral
                            BackendAuthorityNCDReferral

                    "ncd_social_history" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDSocialHistory
                            BackendAuthorityNCDSocialHistory

                    "ncd_symptom_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDSymptomReview
                            BackendAuthorityNCDSymptomReview

                    "ncd_urine_dipstick_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDUrineDipstickTest
                            BackendAuthorityNCDUrineDipstickTest

                    "ncd_vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNCDVitals
                            BackendAuthorityNCDVitals

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

                    "nutrition_ncda" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeNutritionNCDA
                            BackendAuthorityNutritionNCDA

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

                    "pmtct_participant" ->
                        doDecode
                            Backend.PmtctParticipant.Decoder.decodePmtctParticipant
                            BackendAuthorityPmtctParticipant

                    "photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePhoto
                            BackendAuthorityPhoto

                    "pregnancy_testing" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePregnancyTest
                            BackendAuthorityPregnancyTest

                    "prenatal_blood_gprs_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalBloodGpRsTest
                            BackendAuthorityPrenatalBloodGpRsTest

                    "prenatal_breastfeeding" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalBreastfeeding
                            BackendAuthorityPrenatalBreastfeeding

                    "prenatal_encounter" ->
                        doDecode
                            Backend.PrenatalEncounter.Decoder.decodePrenatalEncounter
                            BackendAuthorityPrenatalEncounter

                    "prenatal_family_planning" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalFamilyPlanning
                            BackendAuthorityPrenatalFamilyPlanning

                    "prenatal_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalFollowUp
                            BackendAuthorityPrenatalFollowUp

                    "prenatal_gu_exam" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalGUExam
                            BackendAuthorityPrenatalGUExam

                    "prenatal_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalHealthEducation
                            BackendAuthorityPrenatalHealthEducation

                    "prenatal_hemoglobin_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalHemoglobinTest
                            BackendAuthorityPrenatalHemoglobinTest

                    "prenatal_hepatitis_b_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalHepatitisBTest
                            BackendAuthorityPrenatalHepatitisBTest

                    "prenatal_hiv_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalHIVTest
                            BackendAuthorityPrenatalHIVTest

                    "prenatal_hiv_pcr_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalHIVPCRTest
                            BackendAuthorityPrenatalHIVPCRTest

                    "prenatal_labs_results" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalLabsResults
                            BackendAuthorityPrenatalLabsResults

                    "prenatal_malaria_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalMalariaTest
                            BackendAuthorityPrenatalMalariaTest

                    "prenatal_medication_distribution" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalMedicationDistribution
                            BackendAuthorityPrenatalMedicationDistribution

                    "prenatal_mental_health" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalMentalHealth
                            BackendAuthorityPrenatalMentalHealth

                    "prenatal_nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalNutrition
                            BackendAuthorityPrenatalNutrition

                    "prenatal_outside_care" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalOutsideCare
                            BackendAuthorityPrenatalOutsideCare

                    "prenatal_partner_hiv_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalPartnerHIVTest
                            BackendAuthorityPrenatalPartnerHIVTest

                    "prenatal_photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalPhoto
                            BackendAuthorityPrenatalPhoto

                    "prenatal_random_blood_sugar_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalRandomBloodSugarTest
                            BackendAuthorityPrenatalRandomBloodSugarTest

                    "prenatal_send_to_hc" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalSendToHc
                            BackendAuthorityPrenatalSendToHC

                    "prenatal_speciality_care" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalSpecialityCare
                            BackendAuthorityPrenatalSpecialityCare

                    "prenatal_symptom_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalSymptomReview
                            BackendAuthorityPrenatalSymptomReview

                    "prenatal_syphilis_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalSyphilisTest
                            BackendAuthorityPrenatalSyphilisTest

                    "prenatal_tetanus_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalTetanusImmunisation
                            BackendAuthorityPrenatalTetanusImmunisation

                    "prenatal_urine_dipstick_test" ->
                        doDecode
                            Backend.Measurement.Decoder.decodePrenatalUrineDipstickTest
                            BackendAuthorityPrenatalUrineDipstickTest

                    "relationship" ->
                        doDecode Backend.Relationship.Decoder.decodeRelationship BackendAuthorityRelationship

                    "resource" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeMalariaPrevention
                            BackendAuthorityMalariaPrevention

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
                            Backend.Dashboard.Decoder.decodeDashboardStatsRaw
                            BackendAuthorityDashboardStats

                    "stock_update" ->
                        doDecode
                            Backend.StockUpdate.Decoder.decodeStockUpdate
                            BackendAuthorityStockUpdate

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

                    "tuberculosis_diagnostics" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisDiagnostics
                            BackendAuthorityTuberculosisDiagnostics

                    "tuberculosis_dot" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisDOT
                            BackendAuthorityTuberculosisDOT

                    "tuberculosis_encounter" ->
                        doDecode
                            Backend.TuberculosisEncounter.Decoder.decodeTuberculosisEncounter
                            BackendAuthorityTuberculosisEncounter

                    "tuberculosis_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisFollowUp
                            BackendAuthorityTuberculosisFollowUp

                    "tuberculosis_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisHealthEducation
                            BackendAuthorityTuberculosisHealthEducation

                    "tuberculosis_medication" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisMedication
                            BackendAuthorityTuberculosisMedication

                    "tuberculosis_referral" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisReferral
                            BackendAuthorityTuberculosisReferral

                    "tuberculosis_symptom_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisSymptomReview
                            BackendAuthorityTuberculosisSymptomReview

                    "tuberculosis_treatment_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeTuberculosisTreatmentReview
                            BackendAuthorityTuberculosisTreatmentReview

                    "vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeVitals
                            BackendAuthorityVitals

                    "weight" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWeight
                            BackendAuthorityWeight

                    "well_child_albendazole" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildAlbendazole
                            BackendAuthorityWellChildAlbendazole

                    "well_child_bcg_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildBCGImmunisation
                            BackendAuthorityWellChildBCGImmunisation

                    "well_child_caring" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildCaring
                            BackendAuthorityWellChildCaring

                    "well_child_contributing_factors" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildContributingFactors
                            BackendAuthorityWellChildContributingFactors

                    "well_child_dtp_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildDTPImmunisation
                            BackendAuthorityWellChildDTPImmunisation

                    "well_child_dtp_sa_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildDTPStandaloneImmunisation
                            BackendAuthorityWellChildDTPStandaloneImmunisation

                    "well_child_ecd" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildECD
                            BackendAuthorityWellChildECD

                    "well_child_encounter" ->
                        doDecode
                            Backend.WellChildEncounter.Decoder.decodeWellChildEncounter
                            BackendAuthorityWellChildEncounter

                    "well_child_feeding" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildFeeding
                            BackendAuthorityWellChildFeeding

                    "well_child_follow_up" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildFollowUp
                            BackendAuthorityWellChildFollowUp

                    "well_child_food_security" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildFoodSecurity
                            BackendAuthorityWellChildFoodSecurity

                    "well_child_head_circumference" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildHeadCircumference
                            BackendAuthorityWellChildHeadCircumference

                    "well_child_health_education" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildHealthEducation
                            BackendAuthorityWellChildHealthEducation

                    "well_child_height" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildHeight
                            BackendAuthorityWellChildHeight

                    "well_child_hygiene" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildHygiene
                            BackendAuthorityWellChildHygiene

                    "well_child_hpv_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildHPVImmunisation
                            BackendAuthorityWellChildHPVImmunisation

                    "well_child_ipv_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildIPVImmunisation
                            BackendAuthorityWellChildIPVImmunisation

                    "well_child_mebendezole" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildMebendezole
                            BackendAuthorityWellChildMebendezole

                    "well_child_mr_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildMRImmunisation
                            BackendAuthorityWellChildMRImmunisation

                    "well_child_muac" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildMuac
                            BackendAuthorityWellChildMuac

                    "well_child_ncda" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildNCDA
                            BackendAuthorityWellChildNCDA

                    "well_child_next_visit" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildNextVisit
                            BackendAuthorityWellChildNextVisit

                    "well_child_nutrition" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildNutrition
                            BackendAuthorityWellChildNutrition

                    "well_child_opv_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildOPVImmunisation
                            BackendAuthorityWellChildOPVImmunisation

                    "well_child_pcv13_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildPCV13Immunisation
                            BackendAuthorityWellChildPCV13Immunisation

                    "well_child_photo" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildPhoto
                            BackendAuthorityWellChildPhoto

                    "well_child_pregnancy_summary" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildPregnancySummary
                            BackendAuthorityWellChildPregnancySummary

                    "well_child_rotarix_immunisation" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildRotarixImmunisation
                            BackendAuthorityWellChildRotarixImmunisation

                    "well_child_send_to_hc" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildSendToHC
                            BackendAuthorityWellChildSendToHC

                    "well_child_symptoms_review" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildSymptomsReview
                            BackendAuthorityWellChildSymptomsReview

                    "well_child_vitals" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildVitals
                            BackendAuthorityWellChildVitals

                    "well_child_vitamin_a" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildVitaminA
                            BackendAuthorityWellChildVitaminA

                    "well_child_weight" ->
                        doDecode
                            Backend.Measurement.Decoder.decodeWellChildWeight
                            BackendAuthorityWellChildWeight

                    _ ->
                        fail <| type_ ++ " is unknown BackendAuthorityEntity"
            )


decodeIndexDbSaveResult : Decoder IndexDbSaveResult
decodeIndexDbSaveResult =
    succeed IndexDbSaveResult
        |> required "table" decodeIndexDbSaveResultTable
        |> required "status" decodeIndexDbSaveStatus
        |> required "timestamp" string


decodeIndexDbSaveResultTable : Decoder IndexDbSaveResultTable
decodeIndexDbSaveResultTable =
    string
        |> andThen
            (\table ->
                case table of
                    "Authority" ->
                        succeed IndexDbSaveResultTableAutority

                    "AuthorityStats" ->
                        succeed IndexDbSaveResultTableAuthorityStats

                    "DeferredPhotos" ->
                        succeed IndexDbSaveResultTableDeferredPhotos

                    "General" ->
                        succeed IndexDbSaveResultTableGeneral

                    _ ->
                        fail <| table ++ " is not a recognized IndexDbSaveResultTable"
            )


decodeIndexDbSaveStatus : Decoder IndexDbSaveStatus
decodeIndexDbSaveStatus =
    string
        |> andThen
            (\status ->
                case status of
                    "Success" ->
                        succeed IndexDbSaveSuccess

                    "Failure" ->
                        succeed IndexDbSaveFailure

                    _ ->
                        fail <| status ++ " is not a recognized IndexDbSaveStatus"
            )
