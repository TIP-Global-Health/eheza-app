module Backend.Decoder exposing (decodeRevision)

import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessEncounter)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.HomeVisitEncounter.Decoder exposing (decodeHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.Measurement.Decoder exposing (..)
import Backend.Model exposing (..)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.Relationship.Decoder exposing (decodeRelationship)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Village.Decoder exposing (decodeVillage)
import Backend.WellChildEncounter.Decoder exposing (decodeWellChildEncounter)
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
                    "acute_findings" ->
                        decodeWithUuid AcuteFindingsRevision decodeAcuteFindings

                    "acute_illness_contacts_tracing" ->
                        decodeWithUuid AcuteIllnessContactsTracingRevision decodeAcuteIllnessContactsTracing

                    "acute_illness_core_exam" ->
                        decodeWithUuid AcuteIllnessCoreExamRevision decodeAcuteIllnessCoreExam

                    "acute_illness_danger_signs" ->
                        decodeWithUuid AcuteIllnessDangerSignsRevision decodeAcuteIllnessDangerSigns

                    "acute_illness_encounter" ->
                        decodeWithUuid AcuteIllnessEncounterRevision decodeAcuteIllnessEncounter

                    "acute_illness_follow_up" ->
                        decodeWithUuid AcuteIllnessFollowUpRevision decodeAcuteIllnessFollowUp

                    "acute_illness_muac" ->
                        decodeWithUuid AcuteIllnessMuacRevision decodeAcuteIllnessMuac

                    "acute_illness_nutrition" ->
                        decodeWithUuid AcuteIllnessNutritionRevision decodeAcuteIllnessNutrition

                    "acute_illness_trace_contact" ->
                        decodeWithUuid AcuteIllnessTraceContactRevision decodeAcuteIllnessTraceContact

                    "acute_illness_vitals" ->
                        decodeWithUuid AcuteIllnessVitalsRevision decodeAcuteIllnessVitals

                    "appointment_confirmation" ->
                        decodeWithUuid AppointmentConfirmationRevision decodeAppointmentConfirmation

                    "attendance" ->
                        decodeWithUuid AttendanceRevision decodeAttendance

                    "breast_exam" ->
                        decodeWithUuid BreastExamRevision decodeBreastExam

                    "birth_plan" ->
                        decodeWithUuid BirthPlanRevision decodeBirthPlan

                    "call_114" ->
                        decodeWithUuid Call114Revision decodeCall114

                    "catchment_area" ->
                        decodeWithUuid CatchmentAreaRevision decodeCatchmentArea

                    "child_fbf" ->
                        decodeWithUuid ChildFbfRevision decodeFbf

                    "clinic" ->
                        decodeWithUuid ClinicRevision decodeClinic

                    "contributing_factors" ->
                        decodeWithUuid ContributingFactorsRevision decodeContributingFactors

                    "core_physical_exam" ->
                        decodeWithUuid CorePhysicalExamRevision decodeCorePhysicalExam

                    "counseling_schedule" ->
                        decodeWithUuid CounselingScheduleRevision decodeCounselingSchedule

                    "counseling_session" ->
                        decodeWithUuid CounselingSessionRevision decodeCounselingSession

                    "counseling_topic" ->
                        decodeWithUuid CounselingTopicRevision decodeCounselingTopic

                    "covid_testing" ->
                        decodeWithUuid CovidTestingRevision decodeCovidTesting

                    "danger_signs" ->
                        decodeWithUuid DangerSignsRevision decodeDangerSigns

                    "exposure" ->
                        decodeWithUuid ExposureRevision decodeExposure

                    "family_planning" ->
                        decodeWithUuid FamilyPlanningRevision decodeFamilyPlanning

                    "follow_up" ->
                        decodeWithUuid FollowUpRevision decodeFollowUp

                    "group_health_education" ->
                        decodeWithUuid GroupHealthEducationRevision decodeGroupHealthEducation

                    "group_send_to_hc" ->
                        decodeWithUuid GroupSendToHCRevision decodeGroupSendToHC

                    "hc_contact" ->
                        decodeWithUuid HCContactRevision decodeHCContact

                    "health_center" ->
                        decodeWithUuid HealthCenterRevision decodeHealthCenter

                    "health_education" ->
                        decodeWithUuid HealthEducationRevision decodeHealthEducation

                    "height" ->
                        decodeWithUuid HeightRevision decodeHeight

                    "home_visit_encounter" ->
                        decodeWithUuid HomeVisitEncounterRevision decodeHomeVisitEncounter

                    "individual_participant" ->
                        decodeWithUuid IndividualEncounterParticipantRevision decodeIndividualEncounterParticipant

                    "isolation" ->
                        decodeWithUuid IsolationRevision decodeIsolation

                    "lactation" ->
                        decodeWithUuid LactationRevision decodeLactation

                    "last_menstrual_period" ->
                        decodeWithUuid LastMenstrualPeriodRevision decodeLastMenstrualPeriod

                    "malaria_testing" ->
                        decodeWithUuid MalariaTestingRevision decodeMalariaTesting

                    "medical_history" ->
                        decodeWithUuid MedicalHistoryRevision decodeMedicalHistory

                    "medication" ->
                        decodeWithUuid MedicationRevision decodeMedication

                    "medication_distribution" ->
                        decodeWithUuid MedicationDistributionRevision decodeMedicationDistribution

                    "mother_fbf" ->
                        decodeWithUuid MotherFbfRevision decodeFbf

                    "muac" ->
                        decodeWithUuid MuacRevision decodeMuac

                    "nurse" ->
                        decodeWithUuid NurseRevision decodeNurse

                    "nutrition" ->
                        decodeWithUuid ChildNutritionRevision decodeNutrition

                    "nutrition_caring" ->
                        decodeWithUuid NutritionCaringRevision decodeNutritionCaring

                    "nutrition_contributing_factors" ->
                        decodeWithUuid NutritionContributingFactorsRevision decodeNutritionContributingFactors

                    "nutrition_encounter" ->
                        decodeWithUuid NutritionEncounterRevision decodeNutritionEncounter

                    "nutrition_feeding" ->
                        decodeWithUuid NutritionFeedingRevision decodeNutritionFeeding

                    "nutrition_follow_up" ->
                        decodeWithUuid NutritionFollowUpRevision decodeNutritionFollowUp

                    "nutrition_food_security" ->
                        decodeWithUuid NutritionFoodSecurityRevision decodeNutritionFoodSecurity

                    "nutrition_health_education" ->
                        decodeWithUuid NutritionHealthEducationRevision decodeNutritionHealthEducation

                    "nutrition_height" ->
                        decodeWithUuid NutritionHeightRevision decodeNutritionHeight

                    "nutrition_hygiene" ->
                        decodeWithUuid NutritionHygieneRevision decodeNutritionHygiene

                    "nutrition_muac" ->
                        decodeWithUuid NutritionMuacRevision decodeNutritionMuac

                    "nutrition_nutrition" ->
                        decodeWithUuid NutritionNutritionRevision decodeNutritionNutrition

                    "nutrition_photo" ->
                        decodeWithUuid NutritionPhotoRevision decodeNutritionPhoto

                    "nutrition_send_to_hc" ->
                        decodeWithUuid NutritionSendToHCRevision decodeNutritionSendToHC

                    "nutrition_weight" ->
                        decodeWithUuid NutritionWeightRevision decodeNutritionWeight

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

                    "pregnancy_testing" ->
                        decodeWithUuid PregnancyTestingRevision decodePregnancyTesting

                    "pmtct_participant" ->
                        decodeWithUuid PmtctParticipantRevision decodePmtctParticipant

                    "prenatal_encounter" ->
                        decodeWithUuid PrenatalEncounterRevision decodePrenatalEncounter

                    "prenatal_family_planning" ->
                        decodeWithUuid PrenatalFamilyPlanningRevision decodePrenatalFamilyPlanning

                    "prenatal_health_education" ->
                        decodeWithUuid PrenatalHealthEducationRevision decodePrenatalHealthEducation

                    "prenatal_follow_up" ->
                        decodeWithUuid PrenatalFollowUpRevision decodePrenatalFollowUp

                    "prenatal_nutrition" ->
                        decodeWithUuid PrenatalNutritionRevision decodePrenatalNutrition

                    "prenatal_photo" ->
                        decodeWithUuid PrenatalPhotoRevision decodePrenatalPhoto

                    "prenatal_send_to_hc" ->
                        decodeWithUuid PrenatalSendToHCRevision decodePrenatalSendToHc

                    "relationship" ->
                        decodeWithUuid RelationshipRevision decodeRelationship

                    "resource" ->
                        decodeWithUuid ResourceRevision decodeResource

                    "send_to_hc" ->
                        decodeWithUuid SendToHCRevision decodeSendToHC

                    "session" ->
                        decodeWithUuid SessionRevision decodeSession

                    "social_history" ->
                        decodeWithUuid SocialHistoryRevision decodeSocialHistory

                    "statistics" ->
                        decodeWithUuid DashboardStatsRevision decodeDashboardStatsRaw

                    "symptoms_general" ->
                        decodeWithUuid SymptomsGeneralRevision decodeSymptomsGeneral

                    "symptoms_gi" ->
                        decodeWithUuid SymptomsGIRevision decodeSymptomsGI

                    "symptoms_respiratory" ->
                        decodeWithUuid SymptomsRespiratoryRevision decodeSymptomsRespiratory

                    "travel_history" ->
                        decodeWithUuid TravelHistoryRevision decodeTravelHistory

                    "treatment_history" ->
                        decodeWithUuid TreatmentReviewRevision decodeTreatmentReview

                    "treatment_ongoing" ->
                        decodeWithUuid TreatmentOngoingRevision decodeTreatmentOngoing

                    "village" ->
                        decodeWithUuid VillageRevision decodeVillage

                    "vitals" ->
                        decodeWithUuid VitalsRevision decodeVitals

                    "weight" ->
                        decodeWithUuid WeightRevision decodeWeight

                    "well_child_albendazole" ->
                        decodeWithUuid WellChildAlbendazoleRevision decodeWellChildAlbendazole

                    "well_child_bcg_immunisation" ->
                        decodeWithUuid WellChildBCGImmunisationRevision decodeWellChildBCGImmunisation

                    "well_child_contributing_factors" ->
                        decodeWithUuid WellChildContributingFactorsRevision decodeWellChildContributingFactors

                    "well_child_dtp_immunisation" ->
                        decodeWithUuid WellChildDTPImmunisationRevision decodeWellChildDTPImmunisation

                    "well_child_ecd" ->
                        decodeWithUuid WellChildECDRevision decodeWellChildECD

                    "well_child_encounter" ->
                        decodeWithUuid WellChildEncounterRevision decodeWellChildEncounter

                    "well_child_follow_up" ->
                        decodeWithUuid WellChildFollowUpRevision decodeWellChildFollowUp

                    "well_child_head_circumference" ->
                        decodeWithUuid WellChildHeadCircumferenceRevision decodeWellChildHeadCircumference

                    "well_child_health_education" ->
                        decodeWithUuid WellChildHealthEducationRevision decodeWellChildHealthEducation

                    "well_child_height" ->
                        decodeWithUuid WellChildHeightRevision decodeWellChildHeight

                    "well_child_hpv_immunisation" ->
                        decodeWithUuid WellChildHPVImmunisationRevision decodeWellChildHPVImmunisation

                    "well_child_ipv_immunisation" ->
                        decodeWithUuid WellChildIPVImmunisationRevision decodeWellChildIPVImmunisation

                    "well_child_mebendezole" ->
                        decodeWithUuid WellChildMebendezoleRevision decodeWellChildMebendezole

                    "well_child_mr_immunisation" ->
                        decodeWithUuid WellChildMRImmunisationRevision decodeWellChildMRImmunisation

                    "well_child_muac" ->
                        decodeWithUuid WellChildMuacRevision decodeWellChildMuac

                    "well_child_next_visit" ->
                        decodeWithUuid WellChildNextVisitRevision decodeWellChildNextVisit

                    "well_child_nutrition" ->
                        decodeWithUuid WellChildNutritionRevision decodeWellChildNutrition

                    "well_child_opv_immunisation" ->
                        decodeWithUuid WellChildOPVImmunisationRevision decodeWellChildOPVImmunisation

                    "well_child_pcv13_immunisation" ->
                        decodeWithUuid WellChildPCV13ImmunisationRevision decodeWellChildPCV13Immunisation

                    "well_child_photo" ->
                        decodeWithUuid WellChildPhotoRevision decodeWellChildPhoto

                    "well_child_pregnancy_summary" ->
                        decodeWithUuid WellChildPregnancySummaryRevision decodeWellChildPregnancySummary

                    "well_child_rotarix_immunisation" ->
                        decodeWithUuid WellChildRotarixImmunisationRevision decodeWellChildRotarixImmunisation

                    "well_child_send_to_hc" ->
                        decodeWithUuid WellChildSendToHCRevision decodeWellChildSendToHC

                    "well_child_symptoms_review" ->
                        decodeWithUuid WellChildSymptomsReviewRevision decodeWellChildSymptomsReview

                    "well_child_vitals" ->
                        decodeWithUuid WellChildVitalsRevision decodeWellChildVitals

                    "well_child_vitamin_a" ->
                        decodeWithUuid WellChildVitaminARevision decodeWellChildVitaminA

                    "well_child_weight" ->
                        decodeWithUuid WellChildWeightRevision decodeWellChildWeight

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized type"
            )


decodeWithUuid : (EntityUuid a -> b -> Revision) -> Decoder b -> Decoder Revision
decodeWithUuid tag =
    map2 tag (field "uuid" decodeEntityUuid)
