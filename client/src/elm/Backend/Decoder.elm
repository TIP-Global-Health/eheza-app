module Backend.Decoder exposing (decodeRevision)

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.Measurement.Decoder exposing (..)
import Backend.Model exposing (..)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.Relationship.Decoder exposing (decodeRelationship)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Village.Decoder exposing (decodeVillage)
import Json.Decode exposing (..)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid)


decodeRevision : Decoder Revision
decodeRevision =
    field "type" string
        |> andThen
            (\s ->
                -- Some of these aren't implemented yet, because they need
                -- to be converted from ID to UUID references first.
                case s of
                    "attendance" ->
                        decodeWithUuid AttendanceRevision decodeAttendance

                    "breast_exam" ->
                        decodeWithUuid BreastExamRevision decodeBreastExam

                    "catchment_area" ->
                        decodeWithUuid CatchmentAreaRevision decodeCatchmentArea

                    "clinic" ->
                        decodeWithUuid ClinicRevision decodeClinic

                    "core_physical_exam" ->
                        decodeWithUuid CorePhysicalExamRevision decodeCorePhysicalExam

                    "counseling_schedule" ->
                        decodeWithUuid CounselingScheduleRevision decodeCounselingSchedule

                    "counseling_session" ->
                        decodeWithUuid CounselingSessionRevision decodeCounselingSession

                    "counseling_topic" ->
                        decodeWithUuid CounselingTopicRevision decodeCounselingTopic

                    "danger_signs" ->
                        decodeWithUuid DangerSignsRevision decodeDangerSigns

                    "family_planning" ->
                        decodeWithUuid FamilyPlanningRevision decodeFamilyPlanning

                    "health_center" ->
                        decodeWithUuid HealthCenterRevision decodeHealthCenter

                    "height" ->
                        decodeWithUuid HeightRevision decodeHeight

                    "last_menstrual_period" ->
                        decodeWithUuid LastMenstrualPeriodRevision decodeLastMenstrualPeriod

                    "medical_history" ->
                        decodeWithUuid MedicalHistoryRevision decodeMedicalHistory

                    "medication" ->
                        decodeWithUuid MedicationRevision decodeMedication

                    "muac" ->
                        decodeWithUuid MuacRevision decodeMuac

                    "nurse" ->
                        decodeWithUuid NurseRevision decodeNurse

                    "nutrition" ->
                        decodeWithUuid ChildNutritionRevision decodeNutrition

                    "obstetric_history" ->
                        decodeWithUuid ObstetricHistoryRevision decodeObstetricHistory

                    "obstetric_history_step2" ->
                        decodeWithUuid ObstetricHistoryStep2Revision decodeObstetricHistoryStep2

                    "obstetrical_exam" ->
                        decodeWithUuid ObstetricalExamRevision decodeObstetricalExam

                    "participant_consent" ->
                        decodeWithUuid ParticipantConsentRevision decodeParticipantConsent

                    "participant_form" ->
                        decodeWithUuid ParticipantFormRevision decodeParticipantForm

                    "person" ->
                        decodeWithUuid PersonRevision decodePerson

                    "photo" ->
                        decodeWithUuid PhotoRevision decodePhoto

                    "village" ->
                        decodeWithUuid VillageRevision decodeVillage

                    "pmtct_participant" ->
                        decodeWithUuid PmtctParticipantRevision decodePmtctParticipant

                    "prenatal_encounter" ->
                        decodeWithUuid PrenatalEncounterRevision decodePrenatalEncounter

                    "prenatal_family_planning" ->
                        decodeWithUuid PrenatalFamilyPlanningRevision decodePrenatalFamilyPlanning

                    "prenatal_nutrition" ->
                        decodeWithUuid PrenatalNutritionRevision decodePrenatalNutrition

                    "individual_participant" ->
                        decodeWithUuid IndividualEncounterParticipantRevision decodeIndividualEncounterParticipant

                    "prenatal_photo" ->
                        decodeWithUuid PrenatalPhotoRevision decodePrenatalPhoto

                    "relationship" ->
                        decodeWithUuid RelationshipRevision decodeRelationship

                    "resource" ->
                        decodeWithUuid ResourceRevision decodeResource

                    "session" ->
                        decodeWithUuid SessionRevision decodeSession

                    "social_history" ->
                        decodeWithUuid SocialHistoryRevision decodeSocialHistory

                    "vitals" ->
                        decodeWithUuid VitalsRevision decodeVitals

                    "weight" ->
                        decodeWithUuid WeightRevision decodeWeight

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized type"
            )


decodeWithUuid : (EntityUuid a -> b -> Revision) -> Decoder b -> Decoder Revision
decodeWithUuid tag =
    map2 tag (field "uuid" decodeEntityUuid)
