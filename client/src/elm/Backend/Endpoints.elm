module Backend.Endpoints exposing (NurseParams, PersonParams, PmtctParticipantParams(..), RelationshipParams, SessionParams(..), attendanceEndpoint, breastExamEndpoint, childFbfEndpoint, childMeasurementListEndpoint, clinicEndpoint, corePhysicalExamEndpoint, counselingScheduleEndpoint, counselingSessionEndpoint, counselingTopicEndpoint, dangerSignsEndpoint, encodeIndividualEncounterParams, encodeIndividualEncounterParticipantParams, encodeNurseParams, encodePersonParams, encodePmtctParticipantParams, encodeRelationshipParams, encodeSessionParams, familyPlanningEndpoint, fbfEndpoint, healthCenterEndpoint, heightEndpoint, individualEncounterParticipantEndpoint, lactationEndpoint, lastMenstrualPeriodEndpoint, medicalHistoryEndpoint, medicationEndpoint, motherFbfEndpoint, motherMeasurementListEndpoint, muacEndpoint, nurseEndpoint, nutritionEncounterEndpoint, nutritionEndpoint, nutritionHeightEndpoint, nutritionMeasurementsEndpoint, nutritionMuacEndpoint, nutritionNutritionEndpoint, nutritionPhotoEndpoint, nutritionWeightEndpoint, obstetricHistoryEndpoint, obstetricHistoryStep2Endpoint, obstetricalExamEndpoint, participantConsentEndpoint, participantFormEndpoint, personEndpoint, photoEndpoint, pmtctParticipantEndpoint, prenatalEncounterEndpoint, prenatalFamilyPlanningEndpoint, prenatalMeasurementsEndpoint, prenatalNutritionEndpoint, prenatalPhotoEndpoint, relationshipEndpoint, resourceEndpoint, sessionEndpoint, socialHistoryEndpoint, swEndpoint, syncDataEndpoint, villageEndpoint, vitalsEndpoint, weightEndpoint)

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.Counseling.Encoder exposing (encodeCounselingSchedule, encodeCounselingTopic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Decoder exposing (decodeHealthCenter)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Encoder exposing (encodeIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Decoder exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter)
import Backend.NutritionEncounter.Encoder exposing (encodeNutritionEncounter)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.ParticipantConsent.Encoder exposing (encodeParticipantForm)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.Person.Encoder exposing (encodePerson)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.PmtctParticipant.Encoder exposing (encodePmtctParticipant)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.PrenatalEncounter.Encoder exposing (encodePrenatalEncounter)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.Relationship.Decoder exposing (decodeRelationship)
import Backend.Relationship.Encoder exposing (encodeRelationship)
import Backend.Relationship.Model exposing (Relationship)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Session.Encoder exposing (encodeSession)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.Village.Decoder exposing (decodeVillage)
import Backend.Village.Model exposing (Village)
import DataManager.Decoder exposing (decodeSyncData)
import DataManager.Encoder exposing (encodeSyncData)
import DataManager.Model exposing (SyncData)
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


villageEndpoint : ReadOnlyEndPoint Error VillageId Village ()
villageEndpoint =
    swEndpoint "nodes/village" decodeVillage


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


prenatalPhotoEndpoint : ReadWriteEndPoint Error PrenatalPhotoId PrenatalPhoto PrenatalPhoto ()
prenatalPhotoEndpoint =
    swEndpoint "nodes/prenatal_photo" decodePrenatalPhoto
        |> withValueEncoder (object << encodePrenatalPhoto)


familyPlanningEndpoint : ReadWriteEndPoint Error FamilyPlanningId FamilyPlanning FamilyPlanning ()
familyPlanningEndpoint =
    swEndpoint "nodes/family_planning" decodeFamilyPlanning
        |> withValueEncoder (object << encodeFamilyPlanning)


lactationEndpoint : ReadWriteEndPoint Error LactationId Lactation Lactation ()
lactationEndpoint =
    swEndpoint "nodes/lactation" decodeLactation
        |> withValueEncoder (object << encodeLactation)


childFbfEndpoint : ReadWriteEndPoint Error ChildFbfId Fbf Fbf ()
childFbfEndpoint =
    fbfEndpoint "nodes/child_fbf"


motherFbfEndpoint : ReadWriteEndPoint Error MotherFbfId Fbf Fbf ()
motherFbfEndpoint =
    fbfEndpoint "nodes/mother_fbf"


fbfEndpoint : String -> ReadWriteEndPoint Error (EntityUuid a) Fbf Fbf ()
fbfEndpoint path =
    swEndpoint path decodeFbf
        |> withValueEncoder (object << encodeFbf)


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


prenatalMeasurementsEndpoint : ReadOnlyEndPoint Error PrenatalEncounterId PrenatalMeasurements ()
prenatalMeasurementsEndpoint =
    swEndpoint "nodes/prenatal-measurements" decodePrenatalMeasurements


nutritionMeasurementsEndpoint : ReadOnlyEndPoint Error NutritionEncounterId NutritionMeasurements ()
nutritionMeasurementsEndpoint =
    swEndpoint "nodes/nutrition-measurements" decodeNutritionMeasurements


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


prenatalEncounterEndpoint : ReadWriteEndPoint Error PrenatalEncounterId PrenatalEncounter PrenatalEncounter (Maybe IndividualEncounterParticipantId)
prenatalEncounterEndpoint =
    swEndpoint "nodes/prenatal_encounter" decodePrenatalEncounter
        |> withValueEncoder (object << encodePrenatalEncounter)
        |> withParamsEncoder encodeIndividualEncounterParams


nutritionEncounterEndpoint : ReadWriteEndPoint Error NutritionEncounterId NutritionEncounter NutritionEncounter (Maybe IndividualEncounterParticipantId)
nutritionEncounterEndpoint =
    swEndpoint "nodes/nutrition_encounter" decodeNutritionEncounter
        |> withValueEncoder (object << encodeNutritionEncounter)
        |> withParamsEncoder encodeIndividualEncounterParams


encodeIndividualEncounterParams : Maybe IndividualEncounterParticipantId -> List ( String, String )
encodeIndividualEncounterParams params =
    case params of
        Just id ->
            [ ( "individual_participant", fromEntityUuid id ) ]

        Nothing ->
            []


individualEncounterParticipantEndpoint : ReadWriteEndPoint Error IndividualEncounterParticipantId IndividualEncounterParticipant IndividualEncounterParticipant (Maybe PersonId)
individualEncounterParticipantEndpoint =
    swEndpoint "nodes/individual_participant" decodeIndividualEncounterParticipant
        |> withValueEncoder encodeIndividualEncounterParticipant
        |> withParamsEncoder encodeIndividualEncounterParticipantParams


encodeIndividualEncounterParticipantParams : Maybe PersonId -> List ( String, String )
encodeIndividualEncounterParticipantParams params =
    case params of
        Just id ->
            [ ( "person", fromEntityUuid id ) ]

        Nothing ->
            []


breastExamEndpoint : ReadWriteEndPoint Error BreastExamId BreastExam BreastExam ()
breastExamEndpoint =
    swEndpoint "nodes/breast_exam" decodeBreastExam
        |> withValueEncoder (object << encodeBreastExam)


corePhysicalExamEndpoint : ReadWriteEndPoint Error CorePhysicalExamId CorePhysicalExam CorePhysicalExam ()
corePhysicalExamEndpoint =
    swEndpoint "nodes/core_physical_exam" decodeCorePhysicalExam
        |> withValueEncoder (object << encodeCorePhysicalExam)


dangerSignsEndpoint : ReadWriteEndPoint Error DangerSignsId DangerSigns DangerSigns ()
dangerSignsEndpoint =
    swEndpoint "nodes/danger_signs" decodeDangerSigns
        |> withValueEncoder (object << encodeDangerSigns)


lastMenstrualPeriodEndpoint : ReadWriteEndPoint Error LastMenstrualPeriodId LastMenstrualPeriod LastMenstrualPeriod ()
lastMenstrualPeriodEndpoint =
    swEndpoint "nodes/last_menstrual_period" decodeLastMenstrualPeriod
        |> withValueEncoder (object << encodeLastMenstrualPeriod)


medicalHistoryEndpoint : ReadWriteEndPoint Error MedicalHistoryId MedicalHistory MedicalHistory ()
medicalHistoryEndpoint =
    swEndpoint "nodes/medical_history" decodeMedicalHistory
        |> withValueEncoder (object << encodeMedicalHistory)


medicationEndpoint : ReadWriteEndPoint Error MedicationId Medication Medication ()
medicationEndpoint =
    swEndpoint "nodes/medication" decodeMedication
        |> withValueEncoder (object << encodeMedication)


obstetricalExamEndpoint : ReadWriteEndPoint Error ObstetricalExamId ObstetricalExam ObstetricalExam ()
obstetricalExamEndpoint =
    swEndpoint "nodes/obstetrical_exam" decodeObstetricalExam
        |> withValueEncoder (object << encodeObstetricalExam)


obstetricHistoryEndpoint : ReadWriteEndPoint Error ObstetricHistoryId ObstetricHistory ObstetricHistory ()
obstetricHistoryEndpoint =
    swEndpoint "nodes/obstetric_history" decodeObstetricHistory
        |> withValueEncoder (object << encodeObstetricHistory)


obstetricHistoryStep2Endpoint : ReadWriteEndPoint Error ObstetricHistoryStep2Id ObstetricHistoryStep2 ObstetricHistoryStep2 ()
obstetricHistoryStep2Endpoint =
    swEndpoint "nodes/obstetric_history_step2" decodeObstetricHistoryStep2
        |> withValueEncoder (object << encodeObstetricHistoryStep2)


prenatalFamilyPlanningEndpoint : ReadWriteEndPoint Error PrenatalFamilyPlanningId PrenatalFamilyPlanning PrenatalFamilyPlanning ()
prenatalFamilyPlanningEndpoint =
    swEndpoint "nodes/prenatal_family_planning" decodePrenatalFamilyPlanning
        |> withValueEncoder (object << encodePrenatalFamilyPlanning)


prenatalNutritionEndpoint : ReadWriteEndPoint Error PrenatalNutritionId PrenatalNutrition PrenatalNutrition ()
prenatalNutritionEndpoint =
    swEndpoint "nodes/prenatal_nutrition" decodePrenatalNutrition
        |> withValueEncoder (object << encodePrenatalNutrition)


resourceEndpoint : ReadWriteEndPoint Error ResourceId Resource Resource ()
resourceEndpoint =
    swEndpoint "nodes/resource" decodeResource
        |> withValueEncoder (object << encodeResource)


socialHistoryEndpoint : ReadWriteEndPoint Error SocialHistoryId SocialHistory SocialHistory ()
socialHistoryEndpoint =
    swEndpoint "nodes/social_history" decodeSocialHistory
        |> withValueEncoder (object << encodeSocialHistory)


vitalsEndpoint : ReadWriteEndPoint Error VitalsId Vitals Vitals ()
vitalsEndpoint =
    swEndpoint "nodes/vitals" decodeVitals
        |> withValueEncoder (object << encodeVitals)


nutritionMuacEndpoint : ReadWriteEndPoint Error NutritionMuacId NutritionMuac NutritionMuac ()
nutritionMuacEndpoint =
    swEndpoint "nodes/nutrition_muac" decodeNutritionMuac
        |> withValueEncoder (object << encodeNutritionMuac)


nutritionHeightEndpoint : ReadWriteEndPoint Error NutritionHeightId NutritionHeight NutritionHeight ()
nutritionHeightEndpoint =
    swEndpoint "nodes/nutrition_height" decodeNutritionHeight
        |> withValueEncoder (object << encodeNutritionHeight)


nutritionNutritionEndpoint : ReadWriteEndPoint Error NutritionNutritionId NutritionNutrition NutritionNutrition ()
nutritionNutritionEndpoint =
    swEndpoint "nodes/nutrition_nutrition" decodeNutritionNutrition
        |> withValueEncoder (object << encodeNutritionNutrition)


nutritionPhotoEndpoint : ReadWriteEndPoint Error NutritionPhotoId NutritionPhoto NutritionPhoto ()
nutritionPhotoEndpoint =
    swEndpoint "nodes/nutrition_photo" decodeNutritionPhoto
        |> withValueEncoder (object << encodeNutritionPhoto)


nutritionWeightEndpoint : ReadWriteEndPoint Error NutritionWeightId NutritionWeight NutritionWeight ()
nutritionWeightEndpoint =
    swEndpoint "nodes/nutrition_weight" decodeNutritionWeight
        |> withValueEncoder (object << encodeNutritionWeight)
