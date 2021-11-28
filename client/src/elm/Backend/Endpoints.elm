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
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)
import Backend.Dashboard.Model exposing (DashboardStatsRaw)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Decoder exposing (decodeHealthCenter)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.HomeVisitEncounter.Decoder exposing (decodeHomeVisitEncounter)
import Backend.HomeVisitEncounter.Encoder exposing (encodeHomeVisitEncounter)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
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
import Backend.WellChildEncounter.Decoder exposing (decodeWellChildEncounter)
import Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
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
    Maybe.Extra.values
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
    Maybe.Extra.values
        [ Maybe.map (\person -> ( "person", fromEntityUuid person )) params.person
        , Maybe.map (\relatedTo -> ( "related_to", fromEntityUuid relatedTo )) params.relatedTo
        ]


computedDashboardEndpoint : ReadOnlyEndPoint Error HealthCenterId DashboardStatsRaw ComputedDashboardParams
computedDashboardEndpoint =
    swEndpoint "statistics" decodeDashboardStatsRaw
        |> withParamsEncoder encodeComputedDashboardParams


type alias ComputedDashboardParams =
    { healthCenter : HealthCenterId
    }


encodeComputedDashboardParams : ComputedDashboardParams -> List ( String, String )
encodeComputedDashboardParams params =
    [ ( "healthCenter", fromEntityUuid params.healthCenter ) ]


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


followUpMeasurementsEndpoint : ReadOnlyEndPoint Error HealthCenterId FollowUpMeasurements ()
followUpMeasurementsEndpoint =
    swEndpoint "nodes/follow-up-measurements" decodeFollowUpMeasurements


homeVisitMeasurementsEndpoint : ReadOnlyEndPoint Error HomeVisitEncounterId HomeVisitMeasurements ()
homeVisitMeasurementsEndpoint =
    swEndpoint "nodes/home-visit-measurements" decodeHomeVisitMeasurements


wellChildMeasurementsEndpoint : ReadOnlyEndPoint Error WellChildEncounterId WellChildMeasurements ()
wellChildMeasurementsEndpoint =
    swEndpoint "nodes/well-child-measurements" decodeWellChildMeasurements


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


pregnancyTestingEndpoint : ReadWriteEndPoint Error PregnancyTestId PregnancyTest PregnancyTest ()
pregnancyTestingEndpoint =
    swEndpoint "nodes/pregnancy_testing" decodePregnancyTesting
        |> withValueEncoder (object << encodePregnancyTesting)


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


homeVisitEncounterEndpoint : ReadWriteEndPoint Error HomeVisitEncounterId HomeVisitEncounter HomeVisitEncounter (Maybe IndividualEncounterParticipantId)
homeVisitEncounterEndpoint =
    swEndpoint "nodes/home_visit_encounter" decodeHomeVisitEncounter
        |> withValueEncoder (object << encodeHomeVisitEncounter)
        |> withParamsEncoder encodeIndividualEncounterParams


wellChildEncounterEndpoint : ReadWriteEndPoint Error WellChildEncounterId WellChildEncounter WellChildEncounter (Maybe IndividualEncounterParticipantId)
wellChildEncounterEndpoint =
    swEndpoint "nodes/well_child_encounter" decodeWellChildEncounter
        |> withValueEncoder (object << encodeWellChildEncounter)
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


birthPlanEndpoint : ReadWriteEndPoint Error BirthPlanId BirthPlan BirthPlan ()
birthPlanEndpoint =
    swEndpoint "nodes/birth_plan" decodeBirthPlan
        |> withValueEncoder (object << encodeBirthPlan)


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


malariaPreventionEndpoint : ReadWriteEndPoint Error MalariaPreventionId MalariaPrevention MalariaPrevention ()
malariaPreventionEndpoint =
    swEndpoint "nodes/resource" decodeMalariaPrevention
        |> withValueEncoder (object << encodeMalariaPrevention)


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


acuteIllnessCoreExamEndpoint : ReadWriteEndPoint Error AcuteIllnessCoreExamId AcuteIllnessCoreExam AcuteIllnessCoreExam ()
acuteIllnessCoreExamEndpoint =
    swEndpoint "nodes/acute_illness_core_exam" decodeAcuteIllnessCoreExam
        |> withValueEncoder (object << encodeAcuteIllnessCoreExam)


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


nutritionCaringEndpoint : ReadWriteEndPoint Error NutritionCaringId NutritionCaring NutritionCaring ()
nutritionCaringEndpoint =
    swEndpoint "nodes/nutrition_caring" decodeNutritionCaring
        |> withValueEncoder (object << encodeNutritionCaring)


nutritionContributingFactorsEndpoint : ReadWriteEndPoint Error NutritionContributingFactorsId NutritionContributingFactors NutritionContributingFactors ()
nutritionContributingFactorsEndpoint =
    swEndpoint "nodes/nutrition_contributing_factors" decodeNutritionContributingFactors
        |> withValueEncoder (object << encodeNutritionContributingFactors)


nutritionFollowUpEndpoint : ReadWriteEndPoint Error NutritionFollowUpId NutritionFollowUp NutritionFollowUp ()
nutritionFollowUpEndpoint =
    swEndpoint "nodes/nutrition_follow_up" decodeNutritionFollowUp
        |> withValueEncoder (object << encodeNutritionFollowUp)


groupSendToHCEndpoint : ReadWriteEndPoint Error GroupSendToHCId GroupSendToHC GroupSendToHC ()
groupSendToHCEndpoint =
    swEndpoint "nodes/group_send_to_hc" decodeGroupSendToHC
        |> withValueEncoder (object << encodeGroupSendToHC)


groupHealthEducationEndpoint : ReadWriteEndPoint Error GroupHealthEducationId GroupHealthEducation GroupHealthEducation ()
groupHealthEducationEndpoint =
    swEndpoint "nodes/group_health_education" decodeGroupHealthEducation
        |> withValueEncoder (object << encodeGroupHealthEducation)


contributingFactorsEndpoint : ReadWriteEndPoint Error ContributingFactorsId ContributingFactors ContributingFactors ()
contributingFactorsEndpoint =
    swEndpoint "nodes/contributing_factors" decodeContributingFactors
        |> withValueEncoder (object << encodeContributingFactors)


followUpEndpoint : ReadWriteEndPoint Error FollowUpId FollowUp FollowUp ()
followUpEndpoint =
    swEndpoint "nodes/follow_up" decodeFollowUp
        |> withValueEncoder (object << encodeFollowUp)


nutritionFeedingEndpoint : ReadWriteEndPoint Error NutritionFeedingId NutritionFeeding NutritionFeeding ()
nutritionFeedingEndpoint =
    swEndpoint "nodes/nutrition_feeding" decodeNutritionFeeding
        |> withValueEncoder (object << encodeNutritionFeeding)


nutritionHygieneEndpoint : ReadWriteEndPoint Error NutritionHygieneId NutritionHygiene NutritionHygiene ()
nutritionHygieneEndpoint =
    swEndpoint "nodes/nutrition_hygiene" decodeNutritionHygiene
        |> withValueEncoder (object << encodeNutritionHygiene)


nutritionFoodSecurityEndpoint : ReadWriteEndPoint Error NutritionFoodSecurityId NutritionFoodSecurity NutritionFoodSecurity ()
nutritionFoodSecurityEndpoint =
    swEndpoint "nodes/nutrition_food_security" decodeNutritionFoodSecurity
        |> withValueEncoder (object << encodeNutritionFoodSecurity)


acuteIllnessFollowUpEndpoint : ReadWriteEndPoint Error AcuteIllnessFollowUpId AcuteIllnessFollowUp AcuteIllnessFollowUp ()
acuteIllnessFollowUpEndpoint =
    swEndpoint "nodes/acute_illness_follow_up" decodeAcuteIllnessFollowUp
        |> withValueEncoder (object << encodeAcuteIllnessFollowUp)


prenatalHealthEducationEndpoint : ReadWriteEndPoint Error PrenatalHealthEducationId PrenatalHealthEducation PrenatalHealthEducation ()
prenatalHealthEducationEndpoint =
    swEndpoint "nodes/prenatal_health_education" decodePrenatalHealthEducation
        |> withValueEncoder (object << encodePrenatalHealthEducation)


prenatalFollowUpEndpoint : ReadWriteEndPoint Error PrenatalFollowUpId PrenatalFollowUp PrenatalFollowUp ()
prenatalFollowUpEndpoint =
    swEndpoint "nodes/prenatal_follow_up" decodePrenatalFollowUp
        |> withValueEncoder (object << encodePrenatalFollowUp)


prenatalSendToHcEndpoint : ReadWriteEndPoint Error PrenatalSendToHcId PrenatalSendToHC PrenatalSendToHC ()
prenatalSendToHcEndpoint =
    swEndpoint "nodes/prenatal_send_to_hc" decodePrenatalSendToHc
        |> withValueEncoder (object << encodePrenatalSendToHC)


appointmentConfirmationEndpoint : ReadWriteEndPoint Error PrenatalAppointmentConfirmationId PrenatalAppointmentConfirmation PrenatalAppointmentConfirmation ()
appointmentConfirmationEndpoint =
    swEndpoint "nodes/appointment_confirmation" decodeAppointmentConfirmation
        |> withValueEncoder (object << encodeAppointmentConfirmation)


wellChildECDEndpoint : ReadWriteEndPoint Error WellChildECDId WellChildECD WellChildECD ()
wellChildECDEndpoint =
    swEndpoint "nodes/well_child_ecd" decodeWellChildECD
        |> withValueEncoder (object << encodeWellChildECD)


wellChildHeightEndpoint : ReadWriteEndPoint Error WellChildHeightId WellChildHeight WellChildHeight ()
wellChildHeightEndpoint =
    swEndpoint "nodes/well_child_height" decodeWellChildHeight
        |> withValueEncoder (object << encodeWellChildHeight)


wellChildMuacEndpoint : ReadWriteEndPoint Error WellChildMuacId WellChildMuac WellChildMuac ()
wellChildMuacEndpoint =
    swEndpoint "nodes/well_child_muac" decodeWellChildMuac
        |> withValueEncoder (object << encodeWellChildMuac)


wellChildNutritionEndpoint : ReadWriteEndPoint Error WellChildNutritionId WellChildNutrition WellChildNutrition ()
wellChildNutritionEndpoint =
    swEndpoint "nodes/well_child_nutrition" decodeWellChildNutrition
        |> withValueEncoder (object << encodeWellChildNutrition)


wellChildPhotoEndpoint : ReadWriteEndPoint Error WellChildPhotoId WellChildPhoto WellChildPhoto ()
wellChildPhotoEndpoint =
    swEndpoint "nodes/well_child_photo" decodeWellChildPhoto
        |> withValueEncoder (object << encodeWellChildPhoto)


wellChildWeightEndpoint : ReadWriteEndPoint Error WellChildWeightId WellChildWeight WellChildWeight ()
wellChildWeightEndpoint =
    swEndpoint "nodes/well_child_weight" decodeWellChildWeight
        |> withValueEncoder (object << encodeWellChildWeight)


wellChildContributingFactorsEndpoint : ReadWriteEndPoint Error WellChildContributingFactorsId WellChildContributingFactors WellChildContributingFactors ()
wellChildContributingFactorsEndpoint =
    swEndpoint "nodes/well_child_contributing_factors" decodeWellChildContributingFactors
        |> withValueEncoder (object << encodeWellChildContributingFactors)


wellChildFollowUpEndpoint : ReadWriteEndPoint Error WellChildFollowUpId WellChildFollowUp WellChildFollowUp ()
wellChildFollowUpEndpoint =
    swEndpoint "nodes/well_child_follow_up" decodeWellChildFollowUp
        |> withValueEncoder (object << encodeWellChildFollowUp)


wellChildSendToHCEndpoint : ReadWriteEndPoint Error WellChildSendToHCId WellChildSendToHC WellChildSendToHC ()
wellChildSendToHCEndpoint =
    swEndpoint "nodes/well_child_send_to_hc" decodeWellChildSendToHC
        |> withValueEncoder (object << encodeWellChildSendToHC)


wellChildHealthEducationEndpoint : ReadWriteEndPoint Error WellChildHealthEducationId WellChildHealthEducation WellChildHealthEducation ()
wellChildHealthEducationEndpoint =
    swEndpoint "nodes/well_child_health_education" decodeWellChildHealthEducation
        |> withValueEncoder (object << encodeWellChildHealthEducation)


wellChildHeadCircumferenceEndpoint : ReadWriteEndPoint Error WellChildHeadCircumferenceId WellChildHeadCircumference WellChildHeadCircumference ()
wellChildHeadCircumferenceEndpoint =
    swEndpoint "nodes/well_child_head_circumference" decodeWellChildHeadCircumference
        |> withValueEncoder (object << encodeWellChildHeadCircumference)


wellChildSymptomsReviewEndpoint : ReadWriteEndPoint Error WellChildSymptomsReviewId WellChildSymptomsReview WellChildSymptomsReview ()
wellChildSymptomsReviewEndpoint =
    swEndpoint "nodes/well_child_symptoms_review" decodeWellChildSymptomsReview
        |> withValueEncoder (object << encodeWellChildSymptomsReview)


wellChildVitalsEndpoint : ReadWriteEndPoint Error WellChildVitalsId WellChildVitals WellChildVitals ()
wellChildVitalsEndpoint =
    swEndpoint "nodes/well_child_vitals" decodeWellChildVitals
        |> withValueEncoder (object << encodeWellChildVitals)


wellChildAlbendazoleEndpoint : ReadWriteEndPoint Error WellChildAlbendazoleId WellChildAlbendazole WellChildAlbendazole ()
wellChildAlbendazoleEndpoint =
    swEndpoint "nodes/well_child_albendazole" decodeWellChildAlbendazole
        |> withValueEncoder (object << encodeWellChildAlbendazole)


wellChildMebendezoleEndpoint : ReadWriteEndPoint Error WellChildMebendezoleId WellChildMebendezole WellChildMebendezole ()
wellChildMebendezoleEndpoint =
    swEndpoint "nodes/well_child_mebendezole" decodeWellChildMebendezole
        |> withValueEncoder (object << encodeWellChildMebendezole)


wellChildVitaminAEndpoint : ReadWriteEndPoint Error WellChildVitaminAId WellChildVitaminA WellChildVitaminA ()
wellChildVitaminAEndpoint =
    swEndpoint "nodes/well_child_vitamin_a" decodeWellChildVitaminA
        |> withValueEncoder (object << encodeWellChildVitaminA)


wellChildPregnancySummaryEndpoint : ReadWriteEndPoint Error WellChildPregnancySummaryId WellChildPregnancySummary WellChildPregnancySummary ()
wellChildPregnancySummaryEndpoint =
    swEndpoint "nodes/well_child_pregnancy_summary" decodeWellChildPregnancySummary
        |> withValueEncoder (object << encodeWellChildPregnancySummary)


wellChildNextVisitEndpoint : ReadWriteEndPoint Error WellChildNextVisitId WellChildNextVisit WellChildNextVisit ()
wellChildNextVisitEndpoint =
    swEndpoint "nodes/well_child_next_visit" decodeWellChildNextVisit
        |> withValueEncoder (object << encodeWellChildNextVisit)


wellChildBCGImmunisationEndpoint : ReadWriteEndPoint Error WellChildBCGImmunisationId WellChildBCGImmunisation WellChildBCGImmunisation ()
wellChildBCGImmunisationEndpoint =
    swEndpoint "nodes/well_child_bcg_immunisation" decodeWellChildBCGImmunisation
        |> withValueEncoder (object << encodeWellChildBCGImmunisation)


wellChildDTPImmunisationEndpoint : ReadWriteEndPoint Error WellChildDTPImmunisationId WellChildDTPImmunisation WellChildDTPImmunisation ()
wellChildDTPImmunisationEndpoint =
    swEndpoint "nodes/well_child_dtp_immunisation" decodeWellChildDTPImmunisation
        |> withValueEncoder (object << encodeWellChildDTPImmunisation)


wellChildHPVImmunisationEndpoint : ReadWriteEndPoint Error WellChildHPVImmunisationId WellChildHPVImmunisation WellChildHPVImmunisation ()
wellChildHPVImmunisationEndpoint =
    swEndpoint "nodes/well_child_hpv_immunisation" decodeWellChildHPVImmunisation
        |> withValueEncoder (object << encodeWellChildHPVImmunisation)


wellChildIPVImmunisationEndpoint : ReadWriteEndPoint Error WellChildIPVImmunisationId WellChildIPVImmunisation WellChildIPVImmunisation ()
wellChildIPVImmunisationEndpoint =
    swEndpoint "nodes/well_child_ipv_immunisation" decodeWellChildIPVImmunisation
        |> withValueEncoder (object << encodeWellChildIPVImmunisation)


wellChildMRImmunisationEndpoint : ReadWriteEndPoint Error WellChildMRImmunisationId WellChildMRImmunisation WellChildMRImmunisation ()
wellChildMRImmunisationEndpoint =
    swEndpoint "nodes/well_child_mr_immunisation" decodeWellChildMRImmunisation
        |> withValueEncoder (object << encodeWellChildMRImmunisation)


wellChildOPVImmunisationEndpoint : ReadWriteEndPoint Error WellChildOPVImmunisationId WellChildOPVImmunisation WellChildOPVImmunisation ()
wellChildOPVImmunisationEndpoint =
    swEndpoint "nodes/well_child_opv_immunisation" decodeWellChildOPVImmunisation
        |> withValueEncoder (object << encodeWellChildOPVImmunisation)


wellChildPCV13ImmunisationEndpoint : ReadWriteEndPoint Error WellChildPCV13ImmunisationId WellChildPCV13Immunisation WellChildPCV13Immunisation ()
wellChildPCV13ImmunisationEndpoint =
    swEndpoint "nodes/well_child_pcv13_immunisation" decodeWellChildPCV13Immunisation
        |> withValueEncoder (object << encodeWellChildPCV13Immunisation)


wellChildRotarixImmunisationEndpoint : ReadWriteEndPoint Error WellChildRotarixImmunisationId WellChildRotarixImmunisation WellChildRotarixImmunisation ()
wellChildRotarixImmunisationEndpoint =
    swEndpoint "nodes/well_child_rotarix_immunisation" decodeWellChildRotarixImmunisation
        |> withValueEncoder (object << encodeWellChildRotarixImmunisation)


covidTestingEndpoint : ReadWriteEndPoint Error CovidTestingId CovidTesting CovidTesting ()
covidTestingEndpoint =
    swEndpoint "nodes/covid_testing" decodeCovidTesting
        |> withValueEncoder (object << encodeCovidTesting)


acuteIllnessContactsTracingEndpoint : ReadWriteEndPoint Error AcuteIllnessContactsTracingId AcuteIllnessContactsTracing AcuteIllnessContactsTracing ()
acuteIllnessContactsTracingEndpoint =
    swEndpoint "nodes/acute_illness_contacts_tracing" decodeAcuteIllnessContactsTracing
        |> withValueEncoder (object << encodeAcuteIllnessContactsTracing)


acuteIllnessTraceContactEndpoint : ReadWriteEndPoint Error AcuteIllnessTraceContactId AcuteIllnessTraceContact AcuteIllnessTraceContact ()
acuteIllnessTraceContactEndpoint =
    swEndpoint "nodes/acute_illness_trace_contact" decodeAcuteIllnessTraceContact
        |> withValueEncoder (object << encodeAcuteIllnessTraceContact)
