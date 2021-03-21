module Backend.Endpoints exposing (..)

import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.Counseling.Encoder exposing (encodeCounselingSchedule, encodeCounselingTopic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Dashboard.Decoder exposing (decodeDashboardStats)
import Backend.Dashboard.Model exposing (DashboardStats)
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
import Http exposing (Error)
import Json.Decode exposing (Decoder, field)
import Json.Encode exposing (Value, object)
import Maybe.Extra
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, drupalBackend, endpoint, fromEntityUuid, toEntityUuid, withKeyEncoder, withParamsEncoder, withValueEncoder)


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
        |> withValueEncoder (\val -> Json.Encode.object (encodePerson val))
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
        |> withValueEncoder (object << encodeRelationship)
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


computedDashboardEndpoint : ReadOnlyEndPoint Error HealthCenterId DashboardStats ()
computedDashboardEndpoint =
    swEndpoint "statistics" decodeDashboardStats


healthCenterEndpoint : ReadOnlyEndPoint Error HealthCenterId HealthCenter ()
healthCenterEndpoint =
    swEndpoint "nodes/health_center" decodeHealthCenter


villageEndpoint : ReadOnlyEndPoint Error VillageId Village ()
villageEndpoint =
    swEndpoint "nodes/village" decodeVillage


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
    swEndpoint "nodes/child_fbf" decodeFbf
        |> withValueEncoder (object << encodeChildFbf)


motherFbfEndpoint : ReadWriteEndPoint Error MotherFbfId Fbf Fbf ()
motherFbfEndpoint =
    swEndpoint "nodes/mother_fbf" decodeFbf
        |> withValueEncoder (object << encodeMotherFbf)


participantConsentEndpoint : ReadWriteEndPoint Error ParticipantConsentId ParticipantConsent ParticipantConsent ()
participantConsentEndpoint =
    swEndpoint "nodes/participant_consent" decodeParticipantConsent
        |> withValueEncoder (object << encodeParticipantConsent)


counselingScheduleEndpoint : ReadWriteEndPoint Error CounselingScheduleId CounselingSchedule CounselingSchedule ()
counselingScheduleEndpoint =
    swEndpoint "nodes/counseling_schedule" decodeCounselingSchedule
        |> withValueEncoder (object << encodeCounselingSchedule)


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


acuteIllnessMeasurementsEndpoint : ReadOnlyEndPoint Error AcuteIllnessEncounterId AcuteIllnessMeasurements ()
acuteIllnessMeasurementsEndpoint =
    swEndpoint "nodes/acute-illness-measurements" decodeAcuteIllnessMeasurements


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
        |> withValueEncoder (object << encodePmtctParticipant)
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


acuteIllnessEncounterEndpoint : ReadWriteEndPoint Error AcuteIllnessEncounterId AcuteIllnessEncounter AcuteIllnessEncounter (Maybe IndividualEncounterParticipantId)
acuteIllnessEncounterEndpoint =
    swEndpoint "nodes/acute_illness_encounter" decodeAcuteIllnessEncounter
        |> withValueEncoder (object << encodeAcuteIllnessEncounter)
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
        |> withValueEncoder (object << encodeIndividualEncounterParticipant)
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


symptomsGeneralEndpoint : ReadWriteEndPoint Error SymptomsGeneralId SymptomsGeneral SymptomsGeneral ()
symptomsGeneralEndpoint =
    swEndpoint "nodes/symptoms_general" decodeSymptomsGeneral
        |> withValueEncoder (object << encodeSymptomsGeneral)


symptomsRespiratoryEndpoint : ReadWriteEndPoint Error SymptomsRespiratoryId SymptomsRespiratory SymptomsRespiratory ()
symptomsRespiratoryEndpoint =
    swEndpoint "nodes/symptoms_respiratory" decodeSymptomsRespiratory
        |> withValueEncoder (object << encodeSymptomsRespiratory)


symptomsGIEndpoint : ReadWriteEndPoint Error SymptomsGIId SymptomsGI SymptomsGI ()
symptomsGIEndpoint =
    swEndpoint "nodes/symptoms_gi" decodeSymptomsGI
        |> withValueEncoder (object << encodeSymptomsGI)


acuteIllnessVitalsEndpoint : ReadWriteEndPoint Error AcuteIllnessVitalsId AcuteIllnessVitals AcuteIllnessVitals ()
acuteIllnessVitalsEndpoint =
    swEndpoint "nodes/acute_illness_vitals" decodeAcuteIllnessVitals
        |> withValueEncoder (object << encodeAcuteIllnessVitals)


acuteFindingsEndpoint : ReadWriteEndPoint Error AcuteFindingsId AcuteFindings AcuteFindings ()
acuteFindingsEndpoint =
    swEndpoint "nodes/acute_findings" decodeAcuteFindings
        |> withValueEncoder (object << encodeAcuteFindings)


malariaTestingEndpoint : ReadWriteEndPoint Error MalariaTestingId MalariaTesting MalariaTesting ()
malariaTestingEndpoint =
    swEndpoint "nodes/malaria_testing" decodeMalariaTesting
        |> withValueEncoder (object << encodeMalariaTesting)


sendToHCEndpoint : ReadWriteEndPoint Error SendToHCId SendToHC SendToHC ()
sendToHCEndpoint =
    swEndpoint "nodes/send_to_hc" decodeSendToHC
        |> withValueEncoder (object << encodeSendToHC)


medicationDistributionEndpoint : ReadWriteEndPoint Error MedicationDistributionId MedicationDistribution MedicationDistribution ()
medicationDistributionEndpoint =
    swEndpoint "nodes/medication_distribution" decodeMedicationDistribution
        |> withValueEncoder (object << encodeMedicationDistribution)


travelHistoryEndpoint : ReadWriteEndPoint Error TravelHistoryId TravelHistory TravelHistory ()
travelHistoryEndpoint =
    swEndpoint "nodes/travel_history" decodeTravelHistory
        |> withValueEncoder (object << encodeTravelHistory)


treatmentReviewEndpoint : ReadWriteEndPoint Error TreatmentReviewId TreatmentReview TreatmentReview ()
treatmentReviewEndpoint =
    swEndpoint "nodes/treatment_history" decodeTreatmentReview
        |> withValueEncoder (object << encodeTreatmentReview)


exposureEndpoint : ReadWriteEndPoint Error ExposureId Exposure Exposure ()
exposureEndpoint =
    swEndpoint "nodes/exposure" decodeExposure
        |> withValueEncoder (object << encodeExposure)


isolationEndpoint : ReadWriteEndPoint Error IsolationId Isolation Isolation ()
isolationEndpoint =
    swEndpoint "nodes/isolation" decodeIsolation
        |> withValueEncoder (object << encodeIsolation)


hcContactEndpoint : ReadWriteEndPoint Error HCContactId HCContact HCContact ()
hcContactEndpoint =
    swEndpoint "nodes/hc_contact" decodeHCContact
        |> withValueEncoder (object << encodeHCContact)


call114Endpoint : ReadWriteEndPoint Error Call114Id Call114 Call114 ()
call114Endpoint =
    swEndpoint "nodes/call_114" decodeCall114
        |> withValueEncoder (object << encodeCall114)


acuteIllnessMuacEndpoint : ReadWriteEndPoint Error AcuteIllnessMuacId AcuteIllnessMuac AcuteIllnessMuac ()
acuteIllnessMuacEndpoint =
    swEndpoint "nodes/acute_illness_muac" decodeAcuteIllnessMuac
        |> withValueEncoder (object << encodeAcuteIllnessMuac)


treatmentOngoingEndpoint : ReadWriteEndPoint Error TreatmentOngoingId TreatmentOngoing TreatmentOngoing ()
treatmentOngoingEndpoint =
    swEndpoint "nodes/treatment_ongoing" decodeTreatmentOngoing
        |> withValueEncoder (object << encodeTreatmentOngoing)


acuteIllnessDangerSignsEndpoint : ReadWriteEndPoint Error AcuteIllnessDangerSignsId AcuteIllnessDangerSigns AcuteIllnessDangerSigns ()
acuteIllnessDangerSignsEndpoint =
    swEndpoint "nodes/acute_illness_danger_signs" decodeAcuteIllnessDangerSigns
        |> withValueEncoder (object << encodeAcuteIllnessDangerSigns)


acuteIllnessNutritionEndpoint : ReadWriteEndPoint Error AcuteIllnessNutritionId AcuteIllnessNutrition AcuteIllnessNutrition ()
acuteIllnessNutritionEndpoint =
    swEndpoint "nodes/acute_illness_nutrition" decodeAcuteIllnessNutrition
        |> withValueEncoder (object << encodeAcuteIllnessNutrition)


healthEducationEndpoint : ReadWriteEndPoint Error HealthEducationId HealthEducation HealthEducation ()
healthEducationEndpoint =
    swEndpoint "nodes/health_education" decodeHealthEducation
        |> withValueEncoder (object << encodeHealthEducation)


nutritionSendToHCEndpoint : ReadWriteEndPoint Error NutritionSendToHCId NutritionSendToHC NutritionSendToHC ()
nutritionSendToHCEndpoint =
    swEndpoint "nodes/nutrition_send_to_hc" decodeNutritionSendToHC
        |> withValueEncoder (object << encodeNutritionSendToHC)


nutritionHealthEducationEndpoint : ReadWriteEndPoint Error NutritionHealthEducationId NutritionHealthEducation NutritionHealthEducation ()
nutritionHealthEducationEndpoint =
    swEndpoint "nodes/nutrition_health_education" decodeNutritionHealthEducation
        |> withValueEncoder (object << encodeNutritionHealthEducation)


nutritionContributingFactorsEndpoint : ReadWriteEndPoint Error NutritionContributingFactorsId NutritionContributingFactors NutritionContributingFactors ()
nutritionContributingFactorsEndpoint =
    swEndpoint "nodes/nutrition_contributing_factors" decodeNutritionContributingFactors
        |> withValueEncoder (object << encodeNutritionContributingFactors)


nutritionFollowUpEndpoint : ReadWriteEndPoint Error NutritionFollowUpId NutritionFollowUp NutritionFollowUp ()
nutritionFollowUpEndpoint =
    swEndpoint "nodes/nutrition_follow_up" decodeNutritionFollowUp
        |> withValueEncoder (object << encodeNutritionFollowUp)


gropupSendToHCEndpoint : ReadWriteEndPoint Error GroupSendToHCId GroupSendToHC GroupSendToHC ()
gropupSendToHCEndpoint =
    swEndpoint "nodes/gropup_send_to_hc" decodeGroupSendToHC
        |> withValueEncoder (object << encodeGroupSendToHC)


gropupHealthEducationEndpoint : ReadWriteEndPoint Error GroupHealthEducationId GroupHealthEducation GroupHealthEducation ()
gropupHealthEducationEndpoint =
    swEndpoint "nodes/gropup_health_education" decodeGroupHealthEducation
        |> withValueEncoder (object << encodeGroupHealthEducation)


contributingFactorsEndpoint : ReadWriteEndPoint Error ContributingFactorsId ContributingFactors ContributingFactors ()
contributingFactorsEndpoint =
    swEndpoint "nodes/contributing_factors" decodeContributingFactors
        |> withValueEncoder (object << encodeContributingFactors)


followUpEndpoint : ReadWriteEndPoint Error FollowUpId FollowUp FollowUp ()
followUpEndpoint =
    swEndpoint "nodes/follow_up" decodeFollowUp
        |> withValueEncoder (object << encodeFollowUp)
