module Backend.Endpoints exposing (ChildParams, MotherParams, NurseParams, SessionParams(..), attendanceEndpoint, childEndpoint, childMeasurementListEndpoint, clinicEndpoint, counselingScheduleEndpoint, counselingSessionEndpoint, counselingTopicEndpoint, encodeChildParams, encodeMotherParams, encodeNurseParams, encodeSessionParams, familyPlanningEndpoint, healthCenterEndpoint, heightEndpoint, motherEndpoint, motherMeasurementListEndpoint, muacEndpoint, nurseEndpoint, nutritionEndpoint, participantConsentEndpoint, participantFormEndpoint, photoEndpoint, sessionEndpoint, swEndpoint, syncDataEndpoint, trainingSessionsEndpoint, weightEndpoint)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Child.Encoder exposing (encodeChild)
import Backend.Child.Model exposing (Child)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.Counseling.Encoder exposing (encodeCounselingSchedule, encodeCounselingTopic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Decoder exposing (decodeHealthCenter)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Measurement.Decoder exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (TrainingSessionRequest)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Mother.Encoder exposing (encodeMother)
import Backend.Mother.Model exposing (Mother)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.ParticipantConsent.Encoder exposing (encodeParticipantForm)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Session.Decoder exposing (decodeSession, decodeTrainingSessionRequest)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId, encodeSession, encodeTrainingSessionRequest)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.SyncData.Decoder exposing (decodeSyncData)
import Backend.SyncData.Encoder exposing (encodeSyncData)
import Backend.SyncData.Model exposing (SyncData)
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, succeed)
import Json.Encode exposing (Value, object)
import Maybe.Extra exposing (toList)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)


{-| Construct an endpoint that talks to our local service worker in terms of UUIDs.
-}
swEndpoint : String -> Decoder value -> ReadOnlyEndPoint Error (EntityUuid a) value p
swEndpoint path decodeValue =
    let
        decodeKey =
            Json.Decode.map toEntityUuid (field "uuid" Json.Decode.string)
    in
    endpoint path decodeKey decodeValue drupalBackend
        |> withKeyEncoder fromEntityUuid


childEndpoint : ReadWriteEndPoint Error ChildId Child Child ChildParams
childEndpoint =
    swEndpoint "nodes/child" decodeChild
        |> withValueEncoder (object << encodeChild)
        |> withParamsEncoder encodeChildParams


type alias ChildParams =
    { session : Maybe SessionId
    }


encodeChildParams : ChildParams -> List ( String, String )
encodeChildParams params =
    params.session
        |> Maybe.map (\id -> ( "session", fromEntityUuid id ))
        |> Maybe.Extra.toList


motherEndpoint : ReadWriteEndPoint Error MotherId Mother Mother MotherParams
motherEndpoint =
    swEndpoint "nodes/mother" decodeMother
        |> withValueEncoder (object << encodeMother)
        |> withParamsEncoder encodeMotherParams


type alias MotherParams =
    { session : Maybe SessionId
    }


encodeMotherParams : MotherParams -> List ( String, String )
encodeMotherParams params =
    params.session
        |> Maybe.map (\id -> ( "session", fromEntityUuid id ))
        |> Maybe.Extra.toList


healthCenterEndpoint : ReadOnlyEndPoint Error HealthCenterId HealthCenter ()
healthCenterEndpoint =
    swEndpoint "nodes/health_center" decodeHealthCenter


syncDataEndpoint : ReadWriteEndPoint Error HealthCenterId SyncData SyncData ()
syncDataEndpoint =
    swEndpoint "nodes/syncmetadata" decodeSyncData
        |> withValueEncoder encodeSyncData


clinicEndpoint : ReadWriteEndPoint Error ClinicId Clinic Clinic ()
clinicEndpoint =
    swEndpoint "nodes/clinic" decodeClinic
        |> withValueEncoder (object << encodeClinic)


attendanceEndpoint : ReadWriteEndPoint Error AttendanceId Attendance Attendance ()
attendanceEndpoint =
    swEndpoint "nodes/attendance" decodeAttendance
        |> withValueEncoder (object << encodeAttendance)


heightEndpoint : ReadWriteEndPoint Error HeightId Height Height ()
heightEndpoint =
    swEndpoint "nodes/height" decodeHeight
        |> withValueEncoder (object << encodeHeight)


weightEndpoint : ReadWriteEndPoint Error WeightId Weight Weight ()
weightEndpoint =
    swEndpoint "nodes/weight" decodeWeight
        |> withValueEncoder (object << encodeWeight)


muacEndpoint : ReadWriteEndPoint Error MuacId Muac Muac ()
muacEndpoint =
    swEndpoint "nodes/muac" decodeMuac
        |> withValueEncoder (object << encodeMuac)


counselingSessionEndpoint : ReadWriteEndPoint Error CounselingSessionId CounselingSession CounselingSession ()
counselingSessionEndpoint =
    swEndpoint "nodes/counseling_session" decodeCounselingSession
        |> withValueEncoder (object << encodeCounselingSession)


nutritionEndpoint : ReadWriteEndPoint Error ChildNutritionId ChildNutrition ChildNutrition ()
nutritionEndpoint =
    swEndpoint "nodes/nutrition" decodeNutrition
        |> withValueEncoder (object << encodeNutrition)


photoEndpoint : ReadWriteEndPoint Error PhotoId Photo Photo ()
photoEndpoint =
    swEndpoint "nodes/photo" decodePhoto
        |> withValueEncoder (object << encodePhoto)


familyPlanningEndpoint : ReadWriteEndPoint Error FamilyPlanningId FamilyPlanning FamilyPlanning ()
familyPlanningEndpoint =
    swEndpoint "nodes/family_planning" decodeFamilyPlanning
        |> withValueEncoder (object << encodeFamilyPlanning)


participantConsentEndpoint : ReadWriteEndPoint Error ParticipantConsentId ParticipantConsent ParticipantConsent ()
participantConsentEndpoint =
    swEndpoint "nodes/participant_consent" decodeParticipantConsent
        |> withValueEncoder (object << encodeParticipantConsent)


counselingScheduleEndpoint : ReadWriteEndPoint Error CounselingScheduleId CounselingSchedule CounselingSchedule ()
counselingScheduleEndpoint =
    swEndpoint "nodes/counseling_schedule" decodeCounselingSchedule
        |> withValueEncoder encodeCounselingSchedule


counselingTopicEndpoint : ReadWriteEndPoint Error CounselingTopicId CounselingTopic CounselingTopic ()
counselingTopicEndpoint =
    swEndpoint "nodes/counseling_topic" decodeCounselingTopic
        |> withValueEncoder (object << encodeCounselingTopic)


participantFormEndpoint : ReadWriteEndPoint Error ParticipantFormId ParticipantForm ParticipantForm ()
participantFormEndpoint =
    swEndpoint "nodes/participant_form" decodeParticipantForm
        |> withValueEncoder (object << encodeParticipantForm)


nurseEndpoint : ReadOnlyEndPoint Error NurseId Nurse NurseParams
nurseEndpoint =
    swEndpoint "nodes/nurse" decodeNurse
        |> withParamsEncoder encodeNurseParams


type alias NurseParams =
    { pinCode : Maybe String
    }


encodeNurseParams : NurseParams -> List ( String, String )
encodeNurseParams params =
    params.pinCode
        |> Maybe.map (\code -> ( "pin_code", code ))
        |> Maybe.Extra.toList


motherMeasurementListEndpoint : ReadOnlyEndPoint Error MotherId MotherMeasurementList ()
motherMeasurementListEndpoint =
    swEndpoint "nodes/mother-measurements" decodeMotherMeasurementList


childMeasurementListEndpoint : ReadOnlyEndPoint Error ChildId ChildMeasurementList ()
childMeasurementListEndpoint =
    swEndpoint "nodes/child-measurements" decodeChildMeasurementList


{-| Type-safe params ... how nice!
-}
type SessionParams
    = AllSessions
    | ForClinic ClinicId
    | ForChild ChildId


encodeSessionParams : SessionParams -> List ( String, String )
encodeSessionParams params =
    case params of
        AllSessions ->
            []

        ForClinic clinic ->
            [ ( "clinic", fromEntityUuid clinic ) ]

        ForChild child ->
            [ ( "child", fromEntityUuid child ) ]


sessionEndpoint : ReadWriteEndPoint Error SessionId Session Session SessionParams
sessionEndpoint =
    swEndpoint "nodes/session" decodeSession
        |> withValueEncoder (object << encodeSession)
        |> withParamsEncoder encodeSessionParams


trainingSessionsEndpoint : ReadWriteEndPoint Error () TrainingSessionRequest TrainingSessionRequest ()
trainingSessionsEndpoint =
    -- This one is a little different because we're not expecting a key. So, we
    -- just decode the key successfully as `()`. We can't use `drupalEndpont`
    -- directly, because it assumes the key is some kind of `EntityId` (which
    -- is normally convenient). This could change in future if the backend
    -- were to queue the request and give it an ID, instead of executing it
    -- immediately.
    endpoint "api/training_session_actions" (succeed ()) decodeTrainingSessionRequest drupalBackend
        |> withValueEncoder encodeTrainingSessionRequest
