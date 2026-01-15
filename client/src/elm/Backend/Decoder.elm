module Backend.Decoder exposing (decodeRevision)

import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessEncounter)
import Backend.ChildScoreboardEncounter.Decoder exposing (decodeChildScoreboardEncounter)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Counseling.Decoder exposing (decodeCounselingSchedule, decodeCounselingTopic)
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)
import Backend.EducationSession.Decoder exposing (decodeEducationSession)
import Backend.HIVEncounter.Decoder exposing (decodeHIVEncounter)
import Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)
import Backend.HomeVisitEncounter.Decoder exposing (decodeHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.Measurement.Decoder exposing (..)
import Backend.Model exposing (..)
import Backend.NCDEncounter.Decoder exposing (decodeNCDEncounter)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.Relationship.Decoder exposing (decodeRelationship)
import Backend.ResilienceSurvey.Decoder exposing (decodeResilienceSurvey)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.StockUpdate.Decoder exposing (decodeStockUpdate)
import Backend.TuberculosisEncounter.Decoder exposing (decodeTuberculosisEncounter)
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

                    "child_scoreboard_encounter" ->
                        decodeWithUuid ChildScoreboardEncounterRevision decodeChildScoreboardEncounter

                    "child_scoreboard_bcg_iz" ->
                        decodeWithUuid ChildScoreboardBCGImmunisationRevision decodeChildScoreboardBCGImmunisation

                    "child_scoreboard_dtp_iz" ->
                        decodeWithUuid ChildScoreboardDTPImmunisationRevision decodeChildScoreboardDTPImmunisation

                    "child_scoreboard_dtp_sa_iz" ->
                        decodeWithUuid ChildScoreboardDTPStandaloneImmunisationRevision decodeChildScoreboardDTPStandaloneImmunisation

                    "child_scoreboard_ipv_iz" ->
                        decodeWithUuid ChildScoreboardIPVImmunisationRevision decodeChildScoreboardIPVImmunisation

                    "child_scoreboard_mr_iz" ->
                        decodeWithUuid ChildScoreboardMRImmunisationRevision decodeChildScoreboardMRImmunisation

                    "child_scoreboard_ncda" ->
                        decodeWithUuid ChildScoreboardNCDARevision decodeChildScoreboardNCDA

                    "child_scoreboard_opv_iz" ->
                        decodeWithUuid ChildScoreboardOPVImmunisationRevision decodeChildScoreboardOPVImmunisation

                    "child_scoreboard_pcv13_iz" ->
                        decodeWithUuid ChildScoreboardPCV13ImmunisationRevision decodeChildScoreboardPCV13Immunisation

                    "child_scoreboard_rotarix_iz" ->
                        decodeWithUuid ChildScoreboardRotarixImmunisationRevision decodeChildScoreboardRotarixImmunisation

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

                    "education_session" ->
                        decodeWithUuid EducationSessionRevision decodeEducationSession

                    "exposure" ->
                        decodeWithUuid ExposureRevision decodeExposure

                    "family_planning" ->
                        decodeWithUuid FamilyPlanningRevision decodeFamilyPlanning

                    "follow_up" ->
                        decodeWithUuid FollowUpRevision decodeFollowUp

                    "group_health_education" ->
                        decodeWithUuid GroupHealthEducationRevision decodeGroupHealthEducation

                    "group_ncda" ->
                        decodeWithUuid GroupNCDARevision decodeGroupNCDA

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

                    "hiv_diagnostics" ->
                        decodeWithUuid HIVDiagnosticsRevision decodeHIVDiagnostics

                    "hiv_encounter" ->
                        decodeWithUuid HIVEncounterRevision decodeHIVEncounter

                    "hiv_follow_up" ->
                        decodeWithUuid HIVFollowUpRevision decodeHIVFollowUp

                    "hiv_health_education" ->
                        decodeWithUuid HIVHealthEducationRevision decodeHIVHealthEducation

                    "hiv_medication" ->
                        decodeWithUuid HIVMedicationRevision decodeHIVMedication

                    "hiv_referral" ->
                        decodeWithUuid HIVReferralRevision decodeHIVReferral

                    "hiv_symptom_review" ->
                        decodeWithUuid HIVSymptomReviewRevision decodeHIVSymptomReview

                    "hiv_treatment_review" ->
                        decodeWithUuid HIVTreatmentReviewRevision decodeHIVTreatmentReview

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

                    "ncd_co_morbidities" ->
                        decodeWithUuid NCDCoMorbiditiesRevision decodeNCDCoMorbidities

                    "ncd_core_exam" ->
                        decodeWithUuid NCDCoreExamRevision decodeNCDCoreExam

                    "ncd_creatinine_test" ->
                        decodeWithUuid NCDCreatinineTestRevision decodeNCDCreatinineTest

                    "ncd_danger_signs" ->
                        decodeWithUuid NCDDangerSignsRevision decodeNCDDangerSigns

                    "ncd_encounter" ->
                        decodeWithUuid NCDEncounterRevision decodeNCDEncounter

                    "ncd_family_history" ->
                        decodeWithUuid NCDFamilyHistoryRevision decodeNCDFamilyHistory

                    "ncd_family_planning" ->
                        decodeWithUuid NCDFamilyPlanningRevision decodeNCDFamilyPlanning

                    "ncd_hba1c_test" ->
                        decodeWithUuid NCDHbA1cTestRevision decodeNCDHbA1cTest

                    "ncd_health_education" ->
                        decodeWithUuid NCDHealthEducationRevision decodeNCDHealthEducation

                    "ncd_hiv_test" ->
                        decodeWithUuid NCDHIVTestRevision decodeNCDHIVTest

                    "ncd_labs_results" ->
                        decodeWithUuid NCDLabsResultsRevision decodeNCDLabsResults

                    "ncd_lipid_panel_test" ->
                        decodeWithUuid NCDLipidPanelTestRevision decodeNCDLipidPanelTest

                    "ncd_liver_function_test" ->
                        decodeWithUuid NCDLiverFunctionTestRevision decodeNCDLiverFunctionTest

                    "ncd_medication_distribution" ->
                        decodeWithUuid NCDMedicationDistributionRevision decodeNCDMedicationDistribution

                    "ncd_medication_history" ->
                        decodeWithUuid NCDMedicationHistoryRevision decodeNCDMedicationHistory

                    "ncd_outside_care" ->
                        decodeWithUuid NCDOutsideCareRevision decodeNCDOutsideCare

                    "ncd_pregnancy_test" ->
                        decodeWithUuid NCDPregnancyTestRevision decodeNCDPregnancyTest

                    "ncd_random_blood_sugar_test" ->
                        decodeWithUuid NCDRandomBloodSugarTestRevision decodeNCDRandomBloodSugarTest

                    "ncd_referral" ->
                        decodeWithUuid NCDReferralRevision decodeNCDReferral

                    "ncd_social_history" ->
                        decodeWithUuid NCDSocialHistoryRevision decodeNCDSocialHistory

                    "ncd_symptom_review" ->
                        decodeWithUuid NCDSymptomReviewRevision decodeNCDSymptomReview

                    "ncd_urine_dipstick_test" ->
                        decodeWithUuid NCDUrineDipstickTestRevision decodeNCDUrineDipstickTest

                    "ncd_vitals" ->
                        decodeWithUuid NCDVitalsRevision decodeNCDVitals

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

                    "nutrition_ncda" ->
                        decodeWithUuid NutritionNCDARevision decodeNutritionNCDA

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
                        decodeWithUuid PregnancyTestRevision decodePregnancyTest

                    "pmtct_participant" ->
                        decodeWithUuid PmtctParticipantRevision decodePmtctParticipant

                    "prenatal_aspirin" ->
                        decodeWithUuid PrenatalAspirinRevision decodePrenatalAspirin

                    "prenatal_blood_gprs_test" ->
                        decodeWithUuid PrenatalBloodGpRsTestRevision decodePrenatalBloodGpRsTest

                    "prenatal_breastfeeding" ->
                        decodeWithUuid PrenatalBreastfeedingRevision decodePrenatalBreastfeeding

                    "prenatal_calcium" ->
                        decodeWithUuid PrenatalCalciumRevision decodePrenatalCalcium

                    "prenatal_encounter" ->
                        decodeWithUuid PrenatalEncounterRevision decodePrenatalEncounter

                    "prenatal_family_planning" ->
                        decodeWithUuid PrenatalFamilyPlanningRevision decodePrenatalFamilyPlanning

                    "prenatal_fefol" ->
                        decodeWithUuid PrenatalFefolRevision decodePrenatalFefol

                    "prenatal_folate" ->
                        decodeWithUuid PrenatalFolateRevision decodePrenatalFolate

                    "prenatal_follow_up" ->
                        decodeWithUuid PrenatalFollowUpRevision decodePrenatalFollowUp

                    "prenatal_gu_exam" ->
                        decodeWithUuid PrenatalGUExamRevision decodePrenatalGUExam

                    "prenatal_health_education" ->
                        decodeWithUuid PrenatalHealthEducationRevision decodePrenatalHealthEducation

                    "prenatal_hemoglobin_test" ->
                        decodeWithUuid PrenatalHemoglobinTestRevision decodePrenatalHemoglobinTest

                    "prenatal_hepatitis_b_test" ->
                        decodeWithUuid PrenatalHepatitisBTestRevision decodePrenatalHepatitisBTest

                    "prenatal_hiv_test" ->
                        decodeWithUuid PrenatalHIVTestRevision decodePrenatalHIVTest

                    "prenatal_hiv_pcr_test" ->
                        decodeWithUuid PrenatalHIVPCRTestRevision decodePrenatalHIVPCRTest

                    "prenatal_iron" ->
                        decodeWithUuid PrenatalIronRevision decodePrenatalIron

                    "prenatal_labs_results" ->
                        decodeWithUuid PrenatalLabsResultsRevision decodePrenatalLabsResults

                    "prenatal_malaria_test" ->
                        decodeWithUuid PrenatalMalariaTestRevision decodePrenatalMalariaTest

                    "prenatal_mebendazole" ->
                        decodeWithUuid PrenatalMebendazoleRevision decodePrenatalMebendazole

                    "prenatal_mental_health" ->
                        decodeWithUuid PrenatalMentalHealthRevision decodePrenatalMentalHealth

                    "prenatal_medication_distribution" ->
                        decodeWithUuid PrenatalMedicationDistributionRevision decodePrenatalMedicationDistribution

                    "prenatal_mms" ->
                        decodeWithUuid PrenatalMMSRevision decodePrenatalMMS

                    "prenatal_nutrition" ->
                        decodeWithUuid PrenatalNutritionRevision decodePrenatalNutrition

                    "prenatal_outside_care" ->
                        decodeWithUuid PrenatalOutsideCareRevision decodePrenatalOutsideCare

                    "prenatal_partner_hiv_test" ->
                        decodeWithUuid PrenatalPartnerHIVTestRevision decodePrenatalPartnerHIVTest

                    "prenatal_photo" ->
                        decodeWithUuid PrenatalPhotoRevision decodePrenatalPhoto

                    "prenatal_random_blood_sugar_test" ->
                        decodeWithUuid PrenatalRandomBloodSugarTestRevision decodePrenatalRandomBloodSugarTest

                    "prenatal_send_to_hc" ->
                        decodeWithUuid PrenatalSendToHCRevision decodePrenatalSendToHc

                    "prenatal_speciality_care" ->
                        decodeWithUuid PrenatalSpecialityCareRevision decodePrenatalSpecialityCare

                    "prenatal_symptom_review" ->
                        decodeWithUuid PrenatalSymptomReviewRevision decodePrenatalSymptomReview

                    "prenatal_syphilis_test" ->
                        decodeWithUuid PrenatalSyphilisTestRevision decodePrenatalSyphilisTest

                    "prenatal_tetanus_immunisation" ->
                        decodeWithUuid PrenatalTetanusImmunisationRevision decodePrenatalTetanusImmunisation

                    "prenatal_urine_dipstick_test" ->
                        decodeWithUuid PrenatalUrineDipstickTestRevision decodePrenatalUrineDipstickTest

                    "relationship" ->
                        decodeWithUuid RelationshipRevision decodeRelationship

                    "resilience_survey" ->
                        decodeWithUuid ResilienceSurveyRevision decodeResilienceSurvey

                    "resource" ->
                        decodeWithUuid MalariaPreventionRevision decodeMalariaPrevention

                    "send_to_hc" ->
                        decodeWithUuid SendToHCRevision decodeSendToHC

                    "session" ->
                        decodeWithUuid SessionRevision decodeSession

                    "social_history" ->
                        decodeWithUuid SocialHistoryRevision decodeSocialHistory

                    "statistics" ->
                        decodeWithUuid DashboardStatsRevision decodeDashboardStatsRaw

                    "stock_update" ->
                        decodeWithUuid StockUpdateRevision decodeStockUpdate

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

                    "tuberculosis_diagnostics" ->
                        decodeWithUuid TuberculosisDiagnosticsRevision decodeTuberculosisDiagnostics

                    "tuberculosis_dot" ->
                        decodeWithUuid TuberculosisDOTRevision decodeTuberculosisDOT

                    "tuberculosis_encounter" ->
                        decodeWithUuid TuberculosisEncounterRevision decodeTuberculosisEncounter

                    "tuberculosis_follow_up" ->
                        decodeWithUuid TuberculosisFollowUpRevision decodeTuberculosisFollowUp

                    "tuberculosis_health_education" ->
                        decodeWithUuid TuberculosisHealthEducationRevision decodeTuberculosisHealthEducation

                    "tuberculosis_medication" ->
                        decodeWithUuid TuberculosisMedicationRevision decodeTuberculosisMedication

                    "tuberculosis_referral" ->
                        decodeWithUuid TuberculosisReferralRevision decodeTuberculosisReferral

                    "tuberculosis_symptom_review" ->
                        decodeWithUuid TuberculosisSymptomReviewRevision decodeTuberculosisSymptomReview

                    "tuberculosis_treatment_review" ->
                        decodeWithUuid TuberculosisTreatmentReviewRevision decodeTuberculosisTreatmentReview

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

                    "well_child_caring" ->
                        decodeWithUuid WellChildCaringRevision decodeWellChildCaring

                    "well_child_contributing_factors" ->
                        decodeWithUuid WellChildContributingFactorsRevision decodeWellChildContributingFactors

                    "well_child_dtp_immunisation" ->
                        decodeWithUuid WellChildDTPImmunisationRevision decodeWellChildDTPImmunisation

                    "well_child_dtp_sa_immunisation" ->
                        decodeWithUuid WellChildDTPStandaloneImmunisationRevision decodeWellChildDTPStandaloneImmunisation

                    "well_child_ecd" ->
                        decodeWithUuid WellChildECDRevision decodeWellChildECD

                    "well_child_encounter" ->
                        decodeWithUuid WellChildEncounterRevision decodeWellChildEncounter

                    "well_child_feeding" ->
                        decodeWithUuid WellChildFeedingRevision decodeWellChildFeeding

                    "well_child_follow_up" ->
                        decodeWithUuid WellChildFollowUpRevision decodeWellChildFollowUp

                    "well_child_food_security" ->
                        decodeWithUuid WellChildFoodSecurityRevision decodeWellChildFoodSecurity

                    "well_child_head_circumference" ->
                        decodeWithUuid WellChildHeadCircumferenceRevision decodeWellChildHeadCircumference

                    "well_child_health_education" ->
                        decodeWithUuid WellChildHealthEducationRevision decodeWellChildHealthEducation

                    "well_child_height" ->
                        decodeWithUuid WellChildHeightRevision decodeWellChildHeight

                    "well_child_hygiene" ->
                        decodeWithUuid WellChildHygieneRevision decodeWellChildHygiene

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

                    "well_child_ncda" ->
                        decodeWithUuid WellChildNCDARevision decodeWellChildNCDA

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
