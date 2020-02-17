module Backend.Endpoints exposing (NurseParams, PersonParams, PmtctParticipantParams(..), RelationshipParams, SessionParams(..), attendanceEndpoint, childMeasurementListEndpoint, clinicEndpoint, counselingScheduleEndpoint, counselingSessionEndpoint, counselingTopicEndpoint, encodeNurseParams, encodePersonParams, encodePmtctParticipantParams, encodeRelationshipParams, encodeSessionParams, familyPlanningEndpoint, healthCenterEndpoint, heightEndpoint, motherMeasurementListEndpoint, muacEndpoint, nurseEndpoint, nutritionEndpoint, participantConsentEndpoint, participantFormEndpoint, personEndpoint, photoEndpoint, pmtctParticipantEndpoint, relationshipEndpoint, sessionEndpoint, swEndpoint, syncDataEndpoint, weightEndpoint)

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
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.ParticipantConsent.Encoder exposing (encodeParticipantForm)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.Person.Encoder exposing (encodePerson)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.PmtctParticipant.Encoder exposing (encodePmtctParticipant)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Decoder exposing (decodeRelationship)
import Backend.Relationship.Encoder exposing (encodeRelationship)
import Backend.Relationship.Model exposing (Relationship)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Session.Encoder exposing (encodeSession)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.SyncData.Decoder exposing (decodeSyncData)
import Backend.SyncData.Encoder exposing (encodeSyncData)
import Backend.SyncData.Model exposing (SyncData)
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
    endpoint path decodeKey decodeValue fromEntityUuid drupalBackend
        |> withKeyEncoder fromEntityUuid


personEndpoint : ReadWriteEndPoint Error PersonId Person Person PersonParams
personEndpoint =
    swEndpoint "nodes/person" decodePerson
        |> withValueEncoder encodePerson
        |> withParamsEncoder encodePersonParams


type alias PersonParams =
    { nameContains : Maybe String
    }


encodePersonParams : PersonParams -> List ( String, String )
encodePersonParams params =
    List.filterMap identity
        [ Maybe.map (\name -> ( "name_contains", name )) params.nameContains
        ]


relationshipEndpoint : ReadWriteEndPoint Error RelationshipId Relationship Relationship RelationshipParams
relationshipEndpoint =
    swEndpoint "nodes/relationship" decodeRelationship
        |> withValueEncoder encodeRelationship
        |> withParamsEncoder encodeRelationshipParams


type alias RelationshipParams =
    { person : Maybe PersonId
    , relatedTo : Maybe PersonId
    }


encodeRelationshipParams : RelationshipParams -> List ( String, String )
encodeRelationshipParams params =
    List.filterMap identity
        [ Maybe.map (\person -> ( "person", fromEntityUuid person )) params.person
        , Maybe.map (\relatedTo -> ( "related_to", fromEntityUuid relatedTo )) params.relatedTo
        ]


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


motherMeasurementListEndpoint : ReadOnlyEndPoint Error PersonId MotherMeasurementList ()
motherMeasurementListEndpoint =
    swEndpoint "nodes/mother-measurements" decodeMotherMeasurementList


childMeasurementListEndpoint : ReadOnlyEndPoint Error PersonId ChildMeasurementList ()
childMeasurementListEndpoint =
    swEndpoint "nodes/child-measurements" decodeChildMeasurementList


{-| Type-safe params ... how nice!
-}
type SessionParams
    = AllSessions
    | ForClinic ClinicId
    | ForChild PersonId


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


type PmtctParticipantParams
    = ParticipantsForSession SessionId
    | ParticipantsForChild PersonId
    | ParticipantsForAdult PersonId


encodePmtctParticipantParams : PmtctParticipantParams -> List ( String, String )
encodePmtctParticipantParams params =
    case params of
        ParticipantsForSession id ->
            [ ( "session", fromEntityUuid id ) ]

        ParticipantsForChild id ->
            [ ( "person", fromEntityUuid id ) ]

        ParticipantsForAdult id ->
            [ ( "adult", fromEntityUuid id ) ]


pmtctParticipantEndpoint : ReadWriteEndPoint Error PmtctParticipantId PmtctParticipant PmtctParticipant PmtctParticipantParams
pmtctParticipantEndpoint =
    swEndpoint "nodes/pmtct_participant" decodePmtctParticipant
        |> withValueEncoder encodePmtctParticipant
        |> withParamsEncoder encodePmtctParticipantParams
