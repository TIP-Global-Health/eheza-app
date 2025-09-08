module Backend.Measurement.Decoder exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessDiagnosis)
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Person.Decoder exposing (decodeGender)
import Backend.Person.Utils exposing (genderFromString)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalDiagnosis)
import Backend.StockUpdate.Decoder exposing (decodeStockUpdate)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeFloat, decodeInt, decodeStringWithDefault)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


decodeGroupMeasurement : Decoder value -> Decoder (Measurement SessionId value)
decodeGroupMeasurement =
    decodeMeasurement "session"


decodePrenatalMeasurement : Decoder value -> Decoder (Measurement PrenatalEncounterId value)
decodePrenatalMeasurement =
    decodeMeasurement "prenatal_encounter"


decodeNutritionMeasurement : Decoder value -> Decoder (Measurement NutritionEncounterId value)
decodeNutritionMeasurement =
    decodeMeasurement "nutrition_encounter"


decodeAcuteIllnessMeasurement : Decoder value -> Decoder (Measurement AcuteIllnessEncounterId value)
decodeAcuteIllnessMeasurement =
    decodeMeasurement "acute_illness_encounter"


decodeHomeVisitMeasurement : Decoder value -> Decoder (Measurement HomeVisitEncounterId value)
decodeHomeVisitMeasurement =
    decodeMeasurement "home_visit_encounter"


decodeWellChildMeasurement : Decoder value -> Decoder (Measurement WellChildEncounterId value)
decodeWellChildMeasurement =
    decodeMeasurement "well_child_encounter"


decodeNCDMeasurement : Decoder value -> Decoder (Measurement NCDEncounterId value)
decodeNCDMeasurement =
    decodeMeasurement "ncd_encounter"


decodeChildScoreboardMeasurement : Decoder value -> Decoder (Measurement ChildScoreboardEncounterId value)
decodeChildScoreboardMeasurement =
    decodeMeasurement "child_scoreboard_encounter"


decodeTuberculosisMeasurement : Decoder value -> Decoder (Measurement TuberculosisEncounterId value)
decodeTuberculosisMeasurement =
    decodeMeasurement "tuberculosis_encounter"


decodeHIVMeasurement : Decoder value -> Decoder (Measurement HIVEncounterId value)
decodeHIVMeasurement =
    decodeMeasurement "hiv_encounter"


decodeMeasurement : String -> Decoder value -> Decoder (Measurement (EntityUuid a) value)
decodeMeasurement encounterTag valueDecoder =
    succeed Measurement
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "nurse" (nullable decodeEntityUuid) Nothing
        |> optional "health_center" (nullable decodeEntityUuid) Nothing
        |> required "person" decodeEntityUuid
        |> optional encounterTag (nullable decodeEntityUuid) Nothing
        |> custom valueDecoder


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (\a b -> ( a, b ))
        (field "uuid" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    succeed MotherMeasurementList
        |> optional "attendance" (map Dict.fromList <| list (decodeWithEntityUuid decodeAttendance)) Dict.empty
        |> optional "family_planning" (map Dict.fromList <| list (decodeWithEntityUuid decodeFamilyPlanning)) Dict.empty
        |> optional "participant_consent" (map Dict.fromList <| list (decodeWithEntityUuid decodeParticipantConsent)) Dict.empty
        |> optional "lactation" (map Dict.fromList <| list (decodeWithEntityUuid decodeLactation)) Dict.empty
        |> optional "mother_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    succeed ChildMeasurementList
        |> optional "height" (map Dict.fromList <| list (decodeWithEntityUuid decodeHeight)) Dict.empty
        |> optional "muac" (map Dict.fromList <| list (decodeWithEntityUuid decodeMuac)) Dict.empty
        |> optional "nutrition" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutrition)) Dict.empty
        |> optional "photo" (map Dict.fromList <| list (decodeWithEntityUuid decodePhoto)) Dict.empty
        |> optional "weight" (map Dict.fromList <| list (decodeWithEntityUuid decodeWeight)) Dict.empty
        |> optional "counseling_session" (map Dict.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) Dict.empty
        |> optional "child_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty
        |> optional "contributing_factors" (map Dict.fromList <| list (decodeWithEntityUuid decodeContributingFactors)) Dict.empty
        |> optional "follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeFollowUp)) Dict.empty
        |> optional "group_health_education" (map Dict.fromList <| list (decodeWithEntityUuid decodeGroupHealthEducation)) Dict.empty
        |> optional "group_send_to_hc" (map Dict.fromList <| list (decodeWithEntityUuid decodeGroupSendToHC)) Dict.empty
        |> optional "group_ncda" (map Dict.fromList <| list (decodeWithEntityUuid decodeGroupNCDA)) Dict.empty


decodePrenatalMeasurements : Decoder PrenatalMeasurements
decodePrenatalMeasurements =
    succeed PrenatalMeasurements
        |> optional "breast_exam" (decodeHead decodeBreastExam) Nothing
        |> optional "core_physical_exam" (decodeHead decodeCorePhysicalExam) Nothing
        |> optional "danger_signs" (decodeHead decodeDangerSigns) Nothing
        |> optional "last_menstrual_period" (decodeHead decodeLastMenstrualPeriod) Nothing
        |> optional "medical_history" (decodeHead decodeMedicalHistory) Nothing
        |> optional "medication" (decodeHead decodeMedication) Nothing
        |> optional "obstetrical_exam" (decodeHead decodeObstetricalExam) Nothing
        |> optional "obstetric_history" (decodeHead decodeObstetricHistory) Nothing
        |> optional "obstetric_history_step2" (decodeHead decodeObstetricHistoryStep2) Nothing
        |> optional "prenatal_family_planning" (decodeHead decodePrenatalFamilyPlanning) Nothing
        |> optional "prenatal_nutrition" (decodeHead decodePrenatalNutrition) Nothing
        |> optional "resource" (decodeHead decodeMalariaPrevention) Nothing
        |> optional "social_history" (decodeHead decodeSocialHistory) Nothing
        |> optional "vitals" (decodeHead decodeVitals) Nothing
        |> optional "prenatal_photo" (decodeHead decodePrenatalPhoto) Nothing
        |> optional "birth_plan" (decodeHead decodeBirthPlan) Nothing
        |> optional "pregnancy_testing" (decodeHead decodePregnancyTest) Nothing
        |> optional "prenatal_health_education" (decodeHead decodePrenatalHealthEducation) Nothing
        |> optional "prenatal_follow_up" (decodeHead decodePrenatalFollowUp) Nothing
        |> optional "prenatal_send_to_hc" (decodeHead decodePrenatalSendToHc) Nothing
        |> optional "appointment_confirmation" (decodeHead decodeAppointmentConfirmation) Nothing
        |> optional "prenatal_blood_gprs_test" (decodeHead decodePrenatalBloodGpRsTest) Nothing
        |> optional "prenatal_hemoglobin_test" (decodeHead decodePrenatalHemoglobinTest) Nothing
        |> optional "prenatal_hepatitis_b_test" (decodeHead decodePrenatalHepatitisBTest) Nothing
        |> optional "prenatal_hiv_test" (decodeHead decodePrenatalHIVTest) Nothing
        |> optional "prenatal_malaria_test" (decodeHead decodePrenatalMalariaTest) Nothing
        |> optional "prenatal_random_blood_sugar_test" (decodeHead decodePrenatalRandomBloodSugarTest) Nothing
        |> optional "prenatal_syphilis_test" (decodeHead decodePrenatalSyphilisTest) Nothing
        |> optional "prenatal_urine_dipstick_test" (decodeHead decodePrenatalUrineDipstickTest) Nothing
        |> optional "prenatal_labs_results" (decodeHead decodePrenatalLabsResults) Nothing
        |> optional "prenatal_medication_distribution" (decodeHead decodePrenatalMedicationDistribution) Nothing
        |> optional "prenatal_symptom_review" (decodeHead decodePrenatalSymptomReview) Nothing
        |> optional "prenatal_outside_care" (decodeHead decodePrenatalOutsideCare) Nothing
        |> optional "prenatal_hiv_pcr_test" (decodeHead decodePrenatalHIVPCRTest) Nothing
        |> optional "prenatal_mental_health" (decodeHead decodePrenatalMentalHealth) Nothing
        |> optional "prenatal_tetanus_immunisation" (decodeHead decodePrenatalTetanusImmunisation) Nothing
        |> optional "prenatal_breastfeeding" (decodeHead decodePrenatalBreastfeeding) Nothing
        |> optional "prenatal_gu_exam" (decodeHead decodePrenatalGUExam) Nothing
        |> optional "prenatal_speciality_care" (decodeHead decodePrenatalSpecialityCare) Nothing
        |> optional "prenatal_partner_hiv_test" (decodeHead decodePrenatalPartnerHIVTest) Nothing
        |> optional "prenatal_aspirin" (decodeHead decodePrenatalAspirin) Nothing
        |> optional "prenatal_calcium" (decodeHead decodePrenatalCalcium) Nothing
        |> optional "prenatal_fefol" (decodeHead decodePrenatalFefol) Nothing
        |> optional "prenatal_folate" (decodeHead decodePrenatalFolate) Nothing
        |> optional "prenatal_iron" (decodeHead decodePrenatalIron) Nothing
        |> optional "prenatal_mms" (decodeHead decodePrenatalMMS) Nothing
        |> optional "prenatal_mebendazole" (decodeHead decodePrenatalMebendazole) Nothing


decodeNutritionMeasurements : Decoder NutritionMeasurements
decodeNutritionMeasurements =
    succeed NutritionMeasurements
        |> optional "nutrition_muac" (decodeHead decodeNutritionMuac) Nothing
        |> optional "nutrition_height" (decodeHead decodeNutritionHeight) Nothing
        |> optional "nutrition_nutrition" (decodeHead decodeNutritionNutrition) Nothing
        |> optional "nutrition_photo" (decodeHead decodeNutritionPhoto) Nothing
        |> optional "nutrition_weight" (decodeHead decodeNutritionWeight) Nothing
        |> optional "nutrition_send_to_hc" (decodeHead decodeNutritionSendToHC) Nothing
        |> optional "nutrition_health_education" (decodeHead decodeNutritionHealthEducation) Nothing
        |> optional "nutrition_contributing_factors" (decodeHead decodeNutritionContributingFactors) Nothing
        |> optional "nutrition_follow_up" (decodeHead decodeNutritionFollowUp) Nothing
        |> optional "nutrition_ncda" (decodeHead decodeNutritionNCDA) Nothing


decodeAcuteIllnessMeasurements : Decoder AcuteIllnessMeasurements
decodeAcuteIllnessMeasurements =
    succeed AcuteIllnessMeasurements
        |> optional "symptoms_general" (decodeHead decodeSymptomsGeneral) Nothing
        |> optional "symptoms_respiratory" (decodeHead decodeSymptomsRespiratory) Nothing
        |> optional "symptoms_gi" (decodeHead decodeSymptomsGI) Nothing
        |> optional "acute_illness_vitals" (decodeHead decodeAcuteIllnessVitals) Nothing
        |> optional "acute_findings" (decodeHead decodeAcuteFindings) Nothing
        |> optional "malaria_testing" (decodeHead decodeMalariaTesting) Nothing
        |> optional "travel_history" (decodeHead decodeTravelHistory) Nothing
        |> optional "exposure" (decodeHead decodeExposure) Nothing
        |> optional "isolation" (decodeHead decodeIsolation) Nothing
        |> optional "hc_contact" (decodeHead decodeHCContact) Nothing
        |> optional "call_114" (decodeHead decodeCall114) Nothing
        |> optional "treatment_history" (decodeHead decodeTreatmentReview) Nothing
        |> optional "send_to_hc" (decodeHead decodeSendToHC) Nothing
        |> optional "medication_distribution" (decodeHead decodeMedicationDistribution) Nothing
        |> optional "acute_illness_muac" (decodeHead decodeAcuteIllnessMuac) Nothing
        |> optional "treatment_ongoing" (decodeHead decodeTreatmentOngoing) Nothing
        |> optional "acute_illness_danger_signs" (decodeHead decodeAcuteIllnessDangerSigns) Nothing
        |> optional "acute_illness_nutrition" (decodeHead decodeAcuteIllnessNutrition) Nothing
        |> optional "health_education" (decodeHead decodeHealthEducation) Nothing
        |> optional "acute_illness_follow_up" (decodeHead decodeAcuteIllnessFollowUp) Nothing
        |> optional "acute_illness_core_exam" (decodeHead decodeAcuteIllnessCoreExam) Nothing
        |> optional "covid_testing" (decodeHead decodeCovidTesting) Nothing
        |> optional "acute_illness_contacts_tracing" (decodeHead decodeAcuteIllnessContactsTracing) Nothing


decodeFollowUpMeasurements : Decoder FollowUpMeasurements
decodeFollowUpMeasurements =
    succeed FollowUpMeasurements
        |> optional "follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeFollowUp)) Dict.empty
        |> optional "nutrition_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutritionFollowUp)) Dict.empty
        |> optional "acute_illness_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeAcuteIllnessFollowUp)) Dict.empty
        |> optional "prenatal_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodePrenatalFollowUp)) Dict.empty
        |> optional "well_child_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeWellChildFollowUp)) Dict.empty
        |> optional "tuberculosis_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeTuberculosisFollowUp)) Dict.empty
        |> optional "hiv_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeHIVFollowUp)) Dict.empty
        |> optional "acute_illness_trace_contact" (map Dict.fromList <| list (decodeWithEntityUuid decodeAcuteIllnessTraceContact)) Dict.empty
        |> optional "prenatal_labs_results" (map Dict.fromList <| list (decodeWithEntityUuid decodePrenatalLabsResults)) Dict.empty
        |> optional "ncd_labs_results" (map Dict.fromList <| list (decodeWithEntityUuid decodeNCDLabsResults)) Dict.empty
        |> optional "well_child_next_visit" (map Dict.fromList <| list (decodeWithEntityUuid decodeWellChildNextVisit)) Dict.empty


decodeHomeVisitMeasurements : Decoder HomeVisitMeasurements
decodeHomeVisitMeasurements =
    succeed HomeVisitMeasurements
        |> optional "nutrition_feeding" (decodeHead decodeNutritionFeeding) Nothing
        |> optional "nutrition_hygiene" (decodeHead decodeNutritionHygiene) Nothing
        |> optional "nutrition_food_security" (decodeHead decodeNutritionFoodSecurity) Nothing
        |> optional "nutrition_caring" (decodeHead decodeNutritionCaring) Nothing


decodeWellChildMeasurements : Decoder WellChildMeasurements
decodeWellChildMeasurements =
    succeed WellChildMeasurements
        |> optional "well_child_pregnancy_summary" (decodeHead decodeWellChildPregnancySummary) Nothing
        |> optional "well_child_symptoms_review" (decodeHead decodeWellChildSymptomsReview) Nothing
        |> optional "well_child_vitals" (decodeHead decodeWellChildVitals) Nothing
        |> optional "well_child_height" (decodeHead decodeWellChildHeight) Nothing
        |> optional "well_child_muac" (decodeHead decodeWellChildMuac) Nothing
        |> optional "well_child_nutrition" (decodeHead decodeWellChildNutrition) Nothing
        |> optional "well_child_photo" (decodeHead decodeWellChildPhoto) Nothing
        |> optional "well_child_weight" (decodeHead decodeWellChildWeight) Nothing
        |> optional "well_child_contributing_factors" (decodeHead decodeWellChildContributingFactors) Nothing
        |> optional "well_child_health_education" (decodeHead decodeWellChildHealthEducation) Nothing
        |> optional "well_child_follow_up" (decodeHead decodeWellChildFollowUp) Nothing
        |> optional "well_child_send_to_hc" (decodeHead decodeWellChildSendToHC) Nothing
        |> optional "well_child_head_circumference" (decodeHead decodeWellChildHeadCircumference) Nothing
        |> optional "well_child_ecd" (decodeHead decodeWellChildECD) Nothing
        |> optional "well_child_albendazole" (decodeHead decodeWellChildAlbendazole) Nothing
        |> optional "well_child_mebendezole" (decodeHead decodeWellChildMebendezole) Nothing
        |> optional "well_child_vitamin_a" (decodeHead decodeWellChildVitaminA) Nothing
        |> optional "well_child_next_visit" (decodeHead decodeWellChildNextVisit) Nothing
        |> optional "well_child_bcg_immunisation" (decodeHead decodeWellChildBCGImmunisation) Nothing
        |> optional "well_child_dtp_immunisation" (decodeHead decodeWellChildDTPImmunisation) Nothing
        |> optional "well_child_dtp_sa_immunisation" (decodeHead decodeWellChildDTPStandaloneImmunisation) Nothing
        |> optional "well_child_hpv_immunisation" (decodeHead decodeWellChildHPVImmunisation) Nothing
        |> optional "well_child_ipv_immunisation" (decodeHead decodeWellChildIPVImmunisation) Nothing
        |> optional "well_child_mr_immunisation" (decodeHead decodeWellChildMRImmunisation) Nothing
        |> optional "well_child_opv_immunisation" (decodeHead decodeWellChildOPVImmunisation) Nothing
        |> optional "well_child_pcv13_immunisation" (decodeHead decodeWellChildPCV13Immunisation) Nothing
        |> optional "well_child_rotarix_immunisation" (decodeHead decodeWellChildRotarixImmunisation) Nothing
        |> optional "well_child_ncda" (decodeHead decodeWellChildNCDA) Nothing
        |> optional "well_child_feeding" (decodeHead decodeWellChildFeeding) Nothing
        |> optional "well_child_hygiene" (decodeHead decodeWellChildHygiene) Nothing
        |> optional "well_child_food_security" (decodeHead decodeWellChildFoodSecurity) Nothing
        |> optional "well_child_caring" (decodeHead decodeWellChildCaring) Nothing


decodeNCDMeasurements : Decoder NCDMeasurements
decodeNCDMeasurements =
    succeed NCDMeasurements
        |> optional "ncd_co_morbidities" (decodeHead decodeNCDCoMorbidities) Nothing
        |> optional "ncd_core_exam" (decodeHead decodeNCDCoreExam) Nothing
        |> optional "ncd_creatinine_test" (decodeHead decodeNCDCreatinineTest) Nothing
        |> optional "ncd_danger_signs" (decodeHead decodeNCDDangerSigns) Nothing
        |> optional "ncd_family_history" (decodeHead decodeNCDFamilyHistory) Nothing
        |> optional "ncd_family_planning" (decodeHead decodeNCDFamilyPlanning) Nothing
        |> optional "ncd_hba1c_test" (decodeHead decodeNCDHbA1cTest) Nothing
        |> optional "ncd_health_education" (decodeHead decodeNCDHealthEducation) Nothing
        |> optional "ncd_hiv_test" (decodeHead decodeNCDHIVTest) Nothing
        |> optional "ncd_labs_results" (decodeHead decodeNCDLabsResults) Nothing
        |> optional "ncd_lipid_panel_test" (decodeHead decodeNCDLipidPanelTest) Nothing
        |> optional "ncd_liver_function_test" (decodeHead decodeNCDLiverFunctionTest) Nothing
        |> optional "ncd_medication_distribution" (decodeHead decodeNCDMedicationDistribution) Nothing
        |> optional "ncd_medication_history" (decodeHead decodeNCDMedicationHistory) Nothing
        |> optional "ncd_outside_care" (decodeHead decodeNCDOutsideCare) Nothing
        |> optional "ncd_pregnancy_test" (decodeHead decodeNCDPregnancyTest) Nothing
        |> optional "ncd_random_blood_sugar_test" (decodeHead decodeNCDRandomBloodSugarTest) Nothing
        |> optional "ncd_referral" (decodeHead decodeNCDReferral) Nothing
        |> optional "ncd_social_history" (decodeHead decodeNCDSocialHistory) Nothing
        |> optional "ncd_symptom_review" (decodeHead decodeNCDSymptomReview) Nothing
        |> optional "ncd_urine_dipstick_test" (decodeHead decodeNCDUrineDipstickTest) Nothing
        |> optional "ncd_vitals" (decodeHead decodeNCDVitals) Nothing


decodeChildScoreboardMeasurements : Decoder ChildScoreboardMeasurements
decodeChildScoreboardMeasurements =
    succeed ChildScoreboardMeasurements
        |> optional "child_scoreboard_ncda" (decodeHead decodeChildScoreboardNCDA) Nothing
        |> optional "child_scoreboard_bcg_iz" (decodeHead decodeChildScoreboardBCGImmunisation) Nothing
        |> optional "child_scoreboard_dtp_iz" (decodeHead decodeChildScoreboardDTPImmunisation) Nothing
        |> optional "child_scoreboard_dtp_sa_iz" (decodeHead decodeChildScoreboardDTPStandaloneImmunisation) Nothing
        |> optional "child_scoreboard_ipv_iz" (decodeHead decodeChildScoreboardIPVImmunisation) Nothing
        |> optional "child_scoreboard_mr_iz" (decodeHead decodeChildScoreboardMRImmunisation) Nothing
        |> optional "child_scoreboard_opv_iz" (decodeHead decodeChildScoreboardOPVImmunisation) Nothing
        |> optional "child_scoreboard_pcv13_iz" (decodeHead decodeChildScoreboardPCV13Immunisation) Nothing
        |> optional "child_scoreboard_rotarix_iz" (decodeHead decodeChildScoreboardRotarixImmunisation) Nothing


decodeTuberculosisMeasurements : Decoder TuberculosisMeasurements
decodeTuberculosisMeasurements =
    succeed TuberculosisMeasurements
        |> optional "tuberculosis_diagnostics" (decodeHead decodeTuberculosisDiagnostics) Nothing
        |> optional "tuberculosis_dot" (decodeHead decodeTuberculosisDOT) Nothing
        |> optional "tuberculosis_follow_up" (decodeHead decodeTuberculosisFollowUp) Nothing
        |> optional "tuberculosis_health_education" (decodeHead decodeTuberculosisHealthEducation) Nothing
        |> optional "tuberculosis_medication" (decodeHead decodeTuberculosisMedication) Nothing
        |> optional "tuberculosis_referral" (decodeHead decodeTuberculosisReferral) Nothing
        |> optional "tuberculosis_symptom_review" (decodeHead decodeTuberculosisSymptomReview) Nothing
        |> optional "tuberculosis_treatment_review" (decodeHead decodeTuberculosisTreatmentReview) Nothing


decodeHIVMeasurements : Decoder HIVMeasurements
decodeHIVMeasurements =
    succeed HIVMeasurements
        |> optional "hiv_diagnostics" (decodeHead decodeHIVDiagnostics) Nothing
        |> optional "hiv_follow_up" (decodeHead decodeHIVFollowUp) Nothing
        |> optional "hiv_health_education" (decodeHead decodeHIVHealthEducation) Nothing
        |> optional "hiv_medication" (decodeHead decodeHIVMedication) Nothing
        |> optional "hiv_referral" (decodeHead decodeHIVReferral) Nothing
        |> optional "hiv_symptom_review" (decodeHead decodeHIVSymptomReview) Nothing
        |> optional "hiv_treatment_review" (decodeHead decodeHIVTreatmentReview) Nothing


decodeStockManagementMeasurements : Decoder StockManagementMeasurements
decodeStockManagementMeasurements =
    succeed StockManagementMeasurements
        |> optional "child_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty
        |> optional "mother_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty
        |> optional "stock_update" (map Dict.fromList <| list (decodeWithEntityUuid decodeStockUpdate)) Dict.empty


decodeHead : Decoder a -> Decoder (Maybe ( EntityUuid b, a ))
decodeHead =
    map List.head << list << decodeWithEntityUuid


decodePregnancyTest : Decoder PregnancyTest
decodePregnancyTest =
    decodePregnancyTestResult
        |> field "urine_pregnancy_test"
        |> decodePrenatalMeasurement


decodePregnancyTestResult : Decoder PregnancyTestResult
decodePregnancyTestResult =
    string
        |> andThen
            (\result ->
                pregnancyTestResultFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized PregnancyTestResult" |> fail)
            )


decodePrenatalHealthEducation : Decoder PrenatalHealthEducation
decodePrenatalHealthEducation =
    decodePrenatalMeasurement decodePrenatalHealthEducationValue


decodePrenatalHealthEducationValue : Decoder PrenatalHealthEducationValue
decodePrenatalHealthEducationValue =
    succeed PrenatalHealthEducationValue
        |> required "prenatal_health_education" (decodeEverySet decodePrenatalHealthEducationSign)
        |> optional "health_education_signs_ph2" (nullable (decodeEverySet decodePrenatalHealthEducationSign)) Nothing


decodePrenatalHealthEducationSign : Decoder PrenatalHealthEducationSign
decodePrenatalHealthEducationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "expectations" ->
                        succeed EducationExpectations

                    "visits-review" ->
                        succeed EducationVisitsReview

                    "warning-signs" ->
                        succeed EducationWarningSigns

                    "hemorrhaging" ->
                        succeed EducationHemorrhaging

                    "family-planning" ->
                        succeed EducationFamilyPlanning

                    "breastfeeding" ->
                        succeed EducationBreastfeeding

                    "immunization" ->
                        succeed EducationImmunization

                    "hygiene" ->
                        succeed EducationHygiene

                    "positive-hiv" ->
                        succeed EducationPositiveHIV

                    "safer-sex-hiv" ->
                        succeed EducationSaferSexHIV

                    "partner-testing" ->
                        succeed EducationPartnerTesting

                    "nausea-vomiting" ->
                        succeed EducationNauseaVomiting

                    "leg-cramps" ->
                        succeed EducationLegCramps

                    "low-back-pain" ->
                        succeed EducationLowBackPain

                    "constipation" ->
                        succeed EducationConstipation

                    "heartburn" ->
                        succeed EducationHeartburn

                    "varicose-veins" ->
                        succeed EducationVaricoseVeins

                    "leg-pain-redness" ->
                        succeed EducationLegPainRedness

                    "pelvic-pain" ->
                        succeed EducationPelvicPain

                    "safer-sex" ->
                        succeed EducationSaferSex

                    "hiv-detectable-viral-load" ->
                        succeed EducationHIVDetectableViralLoad

                    "mental-health" ->
                        succeed EducationMentalHealth

                    "diabetes" ->
                        succeed EducationDiabetes

                    "early-mastitis-engorgment" ->
                        succeed EducationEarlyMastitisOrEngorgment

                    "mastitis" ->
                        succeed EducationMastitis

                    "grief" ->
                        succeed EducationGrief

                    "hiv-partner-presence" ->
                        succeed EducationHIVPartnerPresence

                    "none" ->
                        succeed NoPrenatalHealthEducationSigns

                    _ ->
                        sign ++ " is not a recognized PrenatalHealthEducationSign" |> fail
            )


decodePrenatalFollowUp : Decoder PrenatalFollowUp
decodePrenatalFollowUp =
    decodePrenatalMeasurement decodePrenatalFollowUpValue


decodePrenatalFollowUpValue : Decoder PrenatalFollowUpValue
decodePrenatalFollowUpValue =
    succeed PrenatalFollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> optional "date_concluded" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> required "prenatal_assesment" decodePrenatalAssesment


decodePrenatalAssesment : Decoder PrenatalAssesment
decodePrenatalAssesment =
    string
        |> andThen
            (\assesment ->
                case assesment of
                    "normal" ->
                        succeed AssesmentNormalPregnancy

                    "high-risk" ->
                        succeed AssesmentHighRiskPregnancy

                    _ ->
                        assesment ++ " is not a recognized PrenatalAssesment" |> fail
            )


decodePrenatalSendToHc : Decoder PrenatalSendToHC
decodePrenatalSendToHc =
    decodePrenatalMeasurement decodePrenatalReferralValue


decodeAppointmentConfirmation : Decoder PrenatalAppointmentConfirmation
decodeAppointmentConfirmation =
    succeed PrenatalAppointmentConfirmationValue
        |> required "appointment_confirmation" Gizra.NominalDate.decodeYYYYMMDD
        |> decodePrenatalMeasurement


decodePrenatalBloodGpRsTest : Decoder PrenatalBloodGpRsTest
decodePrenatalBloodGpRsTest =
    decodePrenatalMeasurement decodeBloodGpRsTestValue


decodeBloodGpRsTestValue : Decoder (BloodGpRsTestValue (EntityUuid a))
decodeBloodGpRsTestValue =
    succeed BloodGpRsTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "blood_group" (nullable decodeBloodGroup) Nothing
        |> optional "rhesus" (nullable decodeRhesus) Nothing
        |> optional "originating_encounter" (nullable decodeEntityUuid) Nothing


decodeBloodGroup : Decoder BloodGroup
decodeBloodGroup =
    string
        |> andThen
            (\s ->
                bloodGroupFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized BloodGroup")
            )


decodeRhesus : Decoder Rhesus
decodeRhesus =
    string
        |> andThen
            (\s ->
                rhesusFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized Rhesus")
            )


decodePrenatalHemoglobinTest : Decoder PrenatalHemoglobinTest
decodePrenatalHemoglobinTest =
    decodePrenatalMeasurement decodeHemoglobinTestValue


decodeHemoglobinTestValue : Decoder HemoglobinTestValue
decodeHemoglobinTestValue =
    succeed HemoglobinTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "hemoglobin_count" (nullable decodeFloat) Nothing


decodePrenatalHepatitisBTest : Decoder PrenatalHepatitisBTest
decodePrenatalHepatitisBTest =
    decodePrenatalMeasurement decodeHepatitisBTestValue


decodeHepatitisBTestValue : Decoder (HepatitisBTestValue (EntityUuid a))
decodeHepatitisBTestValue =
    succeed HepatitisBTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "test_result" (nullable decodeTestResult) Nothing
        |> optional "originating_encounter" (nullable decodeEntityUuid) Nothing


decodePrenatalHIVTest : Decoder PrenatalHIVTest
decodePrenatalHIVTest =
    decodePrenatalMeasurement decodeHIVTestValue


decodeHIVTestValue : Decoder HIVTestValue
decodeHIVTestValue =
    succeed HIVTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultRDT)
        |> optional "test_result" (nullable decodeTestResult) Nothing
        |> optional "hiv_signs" (nullable (decodeEverySet decodePrenatalHIVSign)) Nothing


decodePrenatalHIVPCRTest : Decoder PrenatalHIVPCRTest
decodePrenatalHIVPCRTest =
    decodePrenatalMeasurement decodeHIVPCRTestValue


decodeHIVPCRTestValue : Decoder HIVPCRTestValue
decodeHIVPCRTestValue =
    succeed HIVPCRTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "hiv_viral_load_status" (nullable decodeViralLoadStatus) Nothing
        |> optional "hiv_viral_load" (nullable decodeFloat) Nothing


decodePrenatalPartnerHIVTest : Decoder PrenatalPartnerHIVTest
decodePrenatalPartnerHIVTest =
    decodePrenatalMeasurement decodePartnerHIVTestValue


decodePartnerHIVTestValue : Decoder PartnerHIVTestValue
decodePartnerHIVTestValue =
    succeed PartnerHIVTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultRDT)
        |> optional "test_result" (nullable decodeTestResult) Nothing
        |> optional "hiv_signs" (nullable (decodeEverySet decodePrenatalHIVSign)) Nothing


decodeViralLoadStatus : Decoder ViralLoadStatus
decodeViralLoadStatus =
    string
        |> andThen
            (\value ->
                case value of
                    "detectable" ->
                        succeed ViralLoadDetectable

                    "undetectable" ->
                        succeed ViralLoadUndetectable

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized ViralLoadStatus"
            )


decodePrenatalHIVSign : Decoder PrenatalHIVSign
decodePrenatalHIVSign =
    string
        |> andThen
            (\value ->
                prenatalHIVSignFromString value
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| value ++ " is not a recognized PrenatalHIVSign")
            )


decodePrenatalMalariaTest : Decoder PrenatalMalariaTest
decodePrenatalMalariaTest =
    decodePrenatalMeasurement decodeMalariaTestValue


decodeMalariaTestValue : Decoder MalariaTestValue
decodeMalariaTestValue =
    succeed MalariaTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultRDT)
        |> optional "test_result" (nullable decodeTestResult) Nothing
        |> optional "blood_smear_result" decodeBloodSmearResult BloodSmearNotTaken


decodeBloodSmearResult : Decoder BloodSmearResult
decodeBloodSmearResult =
    string
        |> andThen
            (\value ->
                bloodSmearResultFromString value
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| value ++ " is not a recognized BloodSmearResult")
            )


decodePrenatalRandomBloodSugarTest : Decoder PrenatalRandomBloodSugarTest
decodePrenatalRandomBloodSugarTest =
    decodePrenatalMeasurement decodeRandomBloodSugarTestValue


decodeRandomBloodSugarTestValue : Decoder (RandomBloodSugarTestValue (EntityUuid a))
decodeRandomBloodSugarTestValue =
    succeed RandomBloodSugarTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites" (nullable (decodeEverySet decodeTestPrerequisite)) Nothing
        |> optional "sugar_count" (nullable decodeFloat) Nothing
        |> optional "originating_encounter" (nullable decodeEntityUuid) Nothing


decodeTestPrerequisite : Decoder TestPrerequisite
decodeTestPrerequisite =
    string
        |> andThen
            (\value ->
                case value of
                    "fasting-12h" ->
                        succeed PrerequisiteFastFor12h

                    "immediate-result" ->
                        succeed PrerequisiteImmediateResult

                    "none" ->
                        succeed NoTestPrerequisites

                    _ ->
                        fail <| value ++ " is not a recognized TestPrerequisite"
            )


decodePrenatalSyphilisTest : Decoder PrenatalSyphilisTest
decodePrenatalSyphilisTest =
    decodePrenatalMeasurement decodeSyphilisTestValue


decodeSyphilisTestValue : Decoder (SyphilisTestValue (EntityUuid a))
decodeSyphilisTestValue =
    succeed SyphilisTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "test_result" (nullable decodeTestResult) Nothing
        |> optional "illness_symptoms" (nullable (decodeEverySet decodeIllnessSymptom)) Nothing
        |> optional "originating_encounter" (nullable decodeEntityUuid) Nothing


decodeIllnessSymptom : Decoder IllnessSymptom
decodeIllnessSymptom =
    string
        |> andThen
            (\value ->
                illnessSymptomFromString value
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| value ++ " is not a recognized IllnessSymptom")
            )


decodePrenatalUrineDipstickTest : Decoder PrenatalUrineDipstickTest
decodePrenatalUrineDipstickTest =
    decodePrenatalMeasurement decodeUrineDipstickTestValue


decodeUrineDipstickTestValue : Decoder UrineDipstickTestValue
decodeUrineDipstickTestValue =
    succeed UrineDipstickTestValue
        |> optional "test_variant" (nullable decodeTestVariant) Nothing
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_prerequisites"
            (nullable
                (decodeWithFallback prerequisitesDefaultNonRDT (decodeEverySet decodeTestPrerequisite))
            )
            (Just prerequisitesDefaultNonRDT)
        |> optional "protein" (nullable decodeProteinValue) Nothing
        |> optional "ph" (nullable decodePHValue) Nothing
        |> optional "glucose" (nullable decodeGlucoseValue) Nothing
        |> optional "leukocytes" (nullable decodeLeukocytesValue) Nothing
        |> optional "nitrite" (nullable decodeNitriteValue) Nothing
        |> optional "urobilinogen" (nullable decodeUrobilinogenValue) Nothing
        |> optional "haemoglobin" (nullable decodeHaemoglobinValue) Nothing
        |> optional "ketone" (nullable decodeKetoneValue) Nothing
        |> optional "bilirubin" (nullable decodeBilirubinValue) Nothing


decodeTestVariant : Decoder TestVariant
decodeTestVariant =
    string
        |> andThen
            (\value ->
                case value of
                    "short" ->
                        succeed VariantShortTest

                    "long" ->
                        succeed VariantLongTest

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized TestVariant"
            )


decodeProteinValue : Decoder ProteinValue
decodeProteinValue =
    string
        |> andThen
            (\s ->
                proteinValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized ProteinValue")
            )


decodePHValue : Decoder PHValue
decodePHValue =
    string
        |> andThen
            (\s ->
                phValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized PHValue")
            )


decodeGlucoseValue : Decoder GlucoseValue
decodeGlucoseValue =
    string
        |> andThen
            (\s ->
                glucoseValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized GlucoseValue")
            )


decodeLeukocytesValue : Decoder LeukocytesValue
decodeLeukocytesValue =
    string
        |> andThen
            (\s ->
                leukocytesValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized LeukocytesValue")
            )


decodeNitriteValue : Decoder NitriteValue
decodeNitriteValue =
    string
        |> andThen
            (\s ->
                nitriteValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NitriteValue")
            )


decodeUrobilinogenValue : Decoder UrobilinogenValue
decodeUrobilinogenValue =
    string
        |> andThen
            (\s ->
                urobilinogenValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized UrobilinogenValue")
            )


decodeHaemoglobinValue : Decoder HaemoglobinValue
decodeHaemoglobinValue =
    string
        |> andThen
            (\s ->
                haemoglobinValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized HaemoglobinValue")
            )


decodeKetoneValue : Decoder KetoneValue
decodeKetoneValue =
    string
        |> andThen
            (\s ->
                ketoneValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized KetoneValue")
            )


decodeBilirubinValue : Decoder BilirubinValue
decodeBilirubinValue =
    string
        |> andThen
            (\s ->
                bilirubinValueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized BilirubinValue")
            )


decodeTestExecutionNote : Decoder TestExecutionNote
decodeTestExecutionNote =
    string
        |> andThen
            (\note ->
                case note of
                    "run-today" ->
                        succeed TestNoteRunToday

                    "run-previously" ->
                        succeed TestNoteRunPreviously

                    "lack-of-reagents" ->
                        succeed TestNoteLackOfReagents

                    "lack-of-other-supplies" ->
                        succeed TestNoteLackOfOtherSupplies

                    "no-equipment" ->
                        succeed TestNoteNoEquipment

                    "broken-equipment" ->
                        succeed TestNoteBrokenEquipment

                    "not-indicated" ->
                        succeed TestNoteNotIndicated

                    "known-as-positive" ->
                        succeed TestNoteKnownAsPositive

                    "to-be-done-at-hospital" ->
                        succeed TestNoteToBeDoneAtHospital

                    "run-confirmed-by-lab-tech" ->
                        succeed TestNoteRunConfirmedByLabTech

                    "not-present" ->
                        succeed TestNoteNotPresent

                    _ ->
                        fail <|
                            note
                                ++ " is not a recognized TestExecutionNote"
            )


decodeTestResult : Decoder TestResult
decodeTestResult =
    string
        |> andThen
            (\s ->
                testResultFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized TestResult")
            )


prerequisitesDefaultRDT : EverySet TestPrerequisite
prerequisitesDefaultRDT =
    EverySet.singleton PrerequisiteImmediateResult


prerequisitesDefaultNonRDT : EverySet TestPrerequisite
prerequisitesDefaultNonRDT =
    EverySet.singleton NoTestPrerequisites


decodePrenatalLabsResults : Decoder PrenatalLabsResults
decodePrenatalLabsResults =
    decodePrenatalMeasurement decodeLabsResultsValue


decodeLabsResultsValue : Decoder LabsResultsValue
decodeLabsResultsValue =
    succeed LabsResultsValue
        |> required "performed_tests" (decodeEverySet decodeLaboratoryTest)
        |> required "completed_tests" (decodeEverySet decodeLaboratoryTest)
        |> required "date_concluded" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "patient_notified" bool False
        |> optional "review_state" (nullable decodeLabsResultsReviewState) Nothing
        |> optional "tests_with_follow_up" (nullable (decodeEverySet decodeLaboratoryTest)) Nothing


decodeLaboratoryTest : Decoder LaboratoryTest
decodeLaboratoryTest =
    string
        |> andThen
            (\s ->
                laboratoryTestFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized LaboratoryTest")
            )


decodeLabsResultsReviewState : Decoder LabsResultsReviewState
decodeLabsResultsReviewState =
    string
        |> andThen
            (\s ->
                reviewStateFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized LaboratoryTest")
            )


decodePrenatalMedicationDistribution : Decoder PrenatalMedicationDistribution
decodePrenatalMedicationDistribution =
    decodePrenatalMeasurement decodePrenatalMedicationDistributionValue


decodePrenatalMedicationDistributionValue : Decoder PrenatalMedicationDistributionValue
decodePrenatalMedicationDistributionValue =
    succeed PrenatalMedicationDistributionValue
        |> required "prescribed_medication" (decodeEverySet decodeMedicationDistributionSign)
        |> required "non_administration_reason" (decodeEverySet decodeMedicationNonAdministrationSign)
        |> optional "recommended_treatment" (nullable (decodeEverySet decodeRecommendedTreatmentSign)) Nothing
        |> optional "avoiding_guidance_reason" (nullable (decodeEverySet decodeAvoidingGuidanceReason)) Nothing
        |> optional "reinforce_treatment_signs" (nullable (decodeEverySet decodeReinforceTreatmentSign)) Nothing


decodeRecommendedTreatmentSign : Decoder RecommendedTreatmentSign
decodeRecommendedTreatmentSign =
    string
        |> andThen
            (\s ->
                recommendedTreatmentSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized RecommendedTreatmentSign")
            )


decodeAvoidingGuidanceReason : Decoder AvoidingGuidanceReason
decodeAvoidingGuidanceReason =
    string
        |> andThen
            (\s ->
                avoidingGuidanceReasonFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized AvoidingGuidanceReason")
            )


decodeReinforceTreatmentSign : Decoder ReinforceTreatmentSign
decodeReinforceTreatmentSign =
    string
        |> andThen
            (\s ->
                reinforceTreatmentSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized ReinforceTreatmentSign")
            )


decodePrenatalMentalHealth : Decoder PrenatalMentalHealth
decodePrenatalMentalHealth =
    decodePrenatalMeasurement decodePrenatalMentalHealthValue


decodePrenatalMentalHealthValue : Decoder PrenatalMentalHealthValue
decodePrenatalMentalHealthValue =
    succeed PrenatalMentalHealthValue
        |> required "mental_health_signs" (list decodePrenatalMentalHealthQuestionTuple |> map Dict.fromList)
        |> required "specialist_at_hc" bool


decodePrenatalMentalHealthQuestionTuple : Decoder ( PrenatalMentalHealthQuestion, PrenatalMentalHealthQuestionOption )
decodePrenatalMentalHealthQuestionTuple =
    string
        |> andThen
            (\s ->
                let
                    parts =
                        String.split "-" s

                    failure =
                        fail <|
                            s
                                ++ " is not a recognized PrenatalMentalHealthQuestionTuple"
                in
                case parts of
                    [ question, answer ] ->
                        Maybe.map2
                            (\decodedQuestion decodedAnswer ->
                                succeed ( decodedQuestion, decodedAnswer )
                            )
                            (prenatalMentalHealthQuestionFromString question)
                            (prenatalMentalHealthQuestionOptionFromString answer)
                            |> Maybe.withDefault failure

                    _ ->
                        failure
            )


decodePrenatalMentalHealthQuestion : Decoder PrenatalMentalHealthQuestion
decodePrenatalMentalHealthQuestion =
    string
        |> andThen
            (\s ->
                prenatalMentalHealthQuestionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized PrenatalMentalHealthQuestion"
                        )
            )


decodePrenatalMentalHealthQuestionOption : Decoder PrenatalMentalHealthQuestionOption
decodePrenatalMentalHealthQuestionOption =
    string
        |> andThen
            (\s ->
                prenatalMentalHealthQuestionOptionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized PrenatalMentalHealthQuestionOption"
                        )
            )


decodePrenatalTetanusImmunisation : Decoder PrenatalTetanusImmunisation
decodePrenatalTetanusImmunisation =
    decodePrenatalMeasurement decodeVaccinationValue


decodePrenatalBreastfeeding : Decoder PrenatalBreastfeeding
decodePrenatalBreastfeeding =
    decodePrenatalMeasurement decodeBreastfeedingValue


decodeBreastfeedingValue : Decoder BreastfeedingValue
decodeBreastfeedingValue =
    field "breastfeeding_signs" (decodeEverySet decodeBreastfeedingSign)


decodeBreastfeedingSign : Decoder BreastfeedingSign
decodeBreastfeedingSign =
    string
        |> andThen
            (\s ->
                breastfeedingSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized BreastfeedingSign"
                        )
            )


decodePrenatalGUExam : Decoder PrenatalGUExam
decodePrenatalGUExam =
    decodePrenatalMeasurement decodeGUExamValue


decodeGUExamValue : Decoder GUExamValue
decodeGUExamValue =
    succeed GUExamValue
        |> required "vaginal_exam_signs" (decodeEverySet decodeVaginalExamSign)
        |> required "gu_exam_signs" (decodeEverySet decodeGUExamSign)
        |> optional "postpartum_healing_problem" (nullable (decodeEverySet decodePostpartumHealingProblem)) Nothing


decodeVaginalExamSign : Decoder VaginalExamSign
decodeVaginalExamSign =
    string
        |> andThen
            (\s ->
                vaginalExamSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized VaginalExamSign"
                        )
            )


decodeGUExamSign : Decoder GUExamSign
decodeGUExamSign =
    string
        |> andThen
            (\s ->
                guExamSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized GUExamSign"
                        )
            )


decodePostpartumHealingProblem : Decoder PostpartumHealingProblem
decodePostpartumHealingProblem =
    string
        |> andThen
            (\s ->
                postpartumHealingProblemFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized PostpartumHealingProblem"
                        )
            )


decodePrenatalSpecialityCare : Decoder PrenatalSpecialityCare
decodePrenatalSpecialityCare =
    decodePrenatalMeasurement decodeSpecialityCareValue


decodeSpecialityCareValue : Decoder SpecialityCareValue
decodeSpecialityCareValue =
    field "speciality_care_signs" (decodeEverySet decodeSpecialityCareSign)


decodeSpecialityCareSign : Decoder SpecialityCareSign
decodeSpecialityCareSign =
    string
        |> andThen
            (\value ->
                case value of
                    "arv" ->
                        succeed EnrolledToARVProgram

                    "ncd" ->
                        succeed EnrolledToNCDProgram

                    "none" ->
                        succeed NoSpecialityCareSigns

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized SpecialityCareSign"
            )


decodeHeight : Decoder Height
decodeHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeGroupMeasurement


decodeMuac : Decoder Muac
decodeMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeGroupMeasurement


decodeNutrition : Decoder ChildNutrition
decodeNutrition =
    decodeGroupMeasurement decodeNutritionValue


decodeNutritionValue : Decoder NutritionValue
decodeNutritionValue =
    succeed NutritionValue
        |> required "nutrition_signs" (decodeEverySet decodeChildNutritionSign)
        |> custom (decodeWithFallback (EverySet.fromList [ NoNutritionAssessment ]) decodeNutritionAssessment)


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" (decodeStringWithDefault "")
        |> map ImageUrl
        |> decodeGroupMeasurement


decodeWeight : Decoder Weight
decodeWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeGroupMeasurement


decodePrenatalPhoto : Decoder PrenatalPhoto
decodePrenatalPhoto =
    field "photo" (decodeStringWithDefault "")
        |> map ImageUrl
        |> decodePrenatalMeasurement


decodeNutritionHeight : Decoder NutritionHeight
decodeNutritionHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeNutritionMeasurement


decodeNutritionMuac : Decoder NutritionMuac
decodeNutritionMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeNutritionMeasurement


decodeNutritionNutrition : Decoder NutritionNutrition
decodeNutritionNutrition =
    decodeNutritionMeasurement decodeNutritionValue


decodeNutritionPhoto : Decoder NutritionPhoto
decodeNutritionPhoto =
    field "photo" (decodeStringWithDefault "")
        |> map ImageUrl
        |> decodeNutritionMeasurement


decodeNutritionWeight : Decoder NutritionWeight
decodeNutritionWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeNutritionMeasurement


decodeWellChildHeight : Decoder WellChildHeight
decodeWellChildHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeWellChildMeasurement


decodeWellChildMuac : Decoder WellChildMuac
decodeWellChildMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeWellChildMeasurement


decodeWellChildNutrition : Decoder WellChildNutrition
decodeWellChildNutrition =
    decodeWellChildMeasurement decodeNutritionValue


decodeWellChildPhoto : Decoder WellChildPhoto
decodeWellChildPhoto =
    field "photo" (decodeStringWithDefault "")
        |> map ImageUrl
        |> decodeWellChildMeasurement


decodeWellChildWeight : Decoder WellChildWeight
decodeWellChildWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeWellChildMeasurement


decodeFamilyPlanning : Decoder FamilyPlanning
decodeFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeGroupMeasurement


decodeLactation : Decoder Lactation
decodeLactation =
    decodeEverySet decodeLactationSign
        |> field "lactation_signs"
        |> decodeGroupMeasurement


decodeFbf : Decoder Fbf
decodeFbf =
    decodeGroupMeasurement decodeFbfValue


decodeAttendance : Decoder Attendance
decodeAttendance =
    field "attended" bool
        |> decodeGroupMeasurement


decodeParticipantConsent : Decoder ParticipantConsent
decodeParticipantConsent =
    decodeGroupMeasurement decodeParticipantConsentValue


decodeParticipantConsentValue : Decoder ParticipantConsentValue
decodeParticipantConsentValue =
    succeed ParticipantConsentValue
        |> required "language" decodeLanguage
        |> required "participant_form" decodeEntityUuid


decodeCounselingSession : Decoder CounselingSession
decodeCounselingSession =
    decodeGroupMeasurement <|
        map2 (\a b -> ( a, b ))
            (field "timing" decodeCounselingTiming)
            (field "topics" (decodeEverySet decodeEntityUuid))


decodeChildNutritionSign : Decoder ChildNutritionSign
decodeChildNutritionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    -- We're keeping this one for back-compat, as
                    -- this value still may exists in the browser
                    -- local storage. Should be removed a little bit
                    -- later.
                    "abdominal-disortion" ->
                        succeed AbdominalDistension

                    -- We briefly used this typo as well, so also
                    -- keeping for back-compat.
                    "abdominal-distention" ->
                        succeed AbdominalDistension

                    "abdominal-distension" ->
                        succeed AbdominalDistension

                    "apathy" ->
                        succeed Apathy

                    "brittle-hair" ->
                        succeed BrittleHair

                    "dry-skin" ->
                        succeed DrySkin

                    "edema" ->
                        succeed Edema

                    "none" ->
                        succeed NormalChildNutrition

                    "poor-appetite" ->
                        succeed PoorAppetite

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ChildNutritionSign"
            )


decodeFamilyPlanningSign : Decoder FamilyPlanningSign
decodeFamilyPlanningSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "auto-observation" ->
                        succeed AutoObservation

                    "condoms" ->
                        succeed Condoms

                    "cycle-counting" ->
                        succeed CycleCounting

                    "hysterectomy" ->
                        succeed Hysterectomy

                    "implant" ->
                        succeed Implants

                    "injection" ->
                        succeed Injectables

                    "iud" ->
                        succeed IUD

                    "lactation-amenorrhea" ->
                        succeed LactationAmenorrhea

                    "none" ->
                        succeed NoFamilyPlanning

                    "spermicide" ->
                        succeed Spermicide

                    "tubal-ligatures" ->
                        succeed TubalLigatures

                    "vasectomy" ->
                        succeed Vasectomy

                    "pill" ->
                        succeed OralContraceptives

                    "necklace" ->
                        succeed CycleBeads

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FamilyPlanningSign"
            )


decodeLactationSign : Decoder LactationSign
decodeLactationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "breastfeeding" ->
                        succeed Breastfeeding

                    "none" ->
                        succeed NoLactationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized LactationSign"
            )


decodeFbfValue : Decoder FbfValue
decodeFbfValue =
    succeed FbfValue
        |> required "distributed_amount" decodeFloat
        |> required "distribution_notice" decodeDistributionNotice


decodeDistributionNotice : Decoder DistributionNotice
decodeDistributionNotice =
    string
        |> andThen
            (\notice ->
                case notice of
                    "complete" ->
                        succeed DistributedFully

                    "lack-of-stock" ->
                        succeed DistributedPartiallyLackOfStock

                    "other" ->
                        succeed DistributedPartiallyOther

                    _ ->
                        fail <|
                            notice
                                ++ " is not a recognized DistributionNotice"
            )


decodeBreastExam : Decoder BreastExam
decodeBreastExam =
    succeed BreastExamValue
        |> required "breast" (decodeEverySet decodeBreastExamSign)
        |> optional "discharge_type" (nullable decodeDischargeType) Nothing
        |> required "breast_self_exam" bool
        |> decodePrenatalMeasurement


decodeBreastExamSign : Decoder BreastExamSign
decodeBreastExamSign =
    string
        |> andThen
            (\s ->
                case s of
                    "mass" ->
                        succeed Mass

                    "discharge" ->
                        succeed Discharge

                    "infection" ->
                        succeed Infection

                    "warmth" ->
                        succeed Warmth

                    "normal" ->
                        succeed NormalBreast

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized BreastExamSign"
            )


decodeDischargeType : Decoder DischargeType
decodeDischargeType =
    string
        |> andThen
            (\s ->
                case s of
                    "milky" ->
                        succeed DischargeMilky

                    "clear" ->
                        succeed DischargeClear

                    "brown-bloody" ->
                        succeed DischargeBrownOrBloody

                    "yellow" ->
                        succeed DischargeYellow

                    "green" ->
                        succeed DischargeGreen

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized DischargeType"
            )


decodeHairHeadCPESign : Decoder HairHeadCPESign
decodeHairHeadCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "brittle-hair" ->
                        succeed BrittleHairCPE

                    "normal" ->
                        succeed NormalHairHead

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized hair/head sign"
            )


decodeEyesCPESign : Decoder EyesCPESign
decodeEyesCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pale-conjuctiva" ->
                        succeed PaleConjuctiva

                    "normal" ->
                        succeed NormalEyes

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized EyesCPESign"
            )


decodeHeartCPESign : Decoder HeartCPESign
decodeHeartCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "irregular-rhythm" ->
                        succeed IrregularRhythm

                    "normal-rate-and-rhythm" ->
                        succeed NormalRateAndRhythm

                    "sinus-tachycardia" ->
                        succeed SinusTachycardia

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized HeartCPESign"
            )


decodeNeckCPESign : Decoder NeckCPESign
decodeNeckCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "enlarged-thyroid" ->
                        succeed EnlargedThyroid

                    "enlarged-lymph-nodes" ->
                        succeed EnlargedLymphNodes

                    "normal" ->
                        succeed NormalNeck

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized NeckCPESign"
            )


decodeLungsCPESign : Decoder LungsCPESign
decodeLungsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "wheezes" ->
                        succeed Wheezes

                    "crackles" ->
                        succeed Crackles

                    "normal" ->
                        succeed NormalLungs

                    _ ->
                        fail <| s ++ " is not a recognized LungsCPESign"
            )


decodeAbdomenCPESign : Decoder AbdomenCPESign
decodeAbdomenCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "hepatomegaly" ->
                        succeed Hepatomegaly

                    "splenomegaly" ->
                        succeed Splenomegaly

                    "tender-to-palpitation-right-upper" ->
                        succeed TPRightUpper

                    "tender-to-palpitation-left-upper" ->
                        succeed TPLeftUpper

                    "tender-to-palpitation-right-lower" ->
                        succeed TPRightLower

                    "tender-to-palpitation-left-lower" ->
                        succeed TPLeftLower

                    "hernia" ->
                        succeed Hernia

                    "normal" ->
                        succeed NormalAbdomen

                    _ ->
                        fail <| s ++ " is not a recognized AbdomenCPESign"
            )


decodeHandsCPESign : Decoder HandsCPESign
decodeHandsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pallor" ->
                        succeed PallorHands

                    "edema" ->
                        succeed EdemaHands

                    "normal" ->
                        succeed NormalHands

                    _ ->
                        fail <| s ++ " is not a recognized HandsCPESign"
            )


decodeLegsCPESign : Decoder LegsCPESign
decodeLegsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pallor" ->
                        succeed PallorLegs

                    "edema" ->
                        succeed EdemaLegs

                    "normal" ->
                        succeed NormalLegs

                    _ ->
                        fail <| s ++ " is not a recognized LegsCPESign"
            )


decodeCorePhysicalExam : Decoder CorePhysicalExam
decodeCorePhysicalExam =
    decodePrenatalMeasurement decodeCorePhysicalExamValue


decodeCorePhysicalExamValue : Decoder CorePhysicalExamValue
decodeCorePhysicalExamValue =
    succeed CorePhysicalExamValue
        |> required "head_hair" (decodeEverySet decodeHairHeadCPESign)
        |> required "eyes" (decodeEverySet decodeEyesCPESign)
        |> required "heart" (decodeEverySet decodeHeartCPESign)
        |> required "heart_murmur" bool
        |> required "neck" (decodeEverySet decodeNeckCPESign)
        |> required "lungs" (decodeEverySet decodeLungsCPESign)
        |> required "abdomen" (decodeEverySet decodeAbdomenCPESign)
        |> required "hands" (decodeEverySet decodeHandsCPESign)
        |> required "legs" (decodeEverySet decodeLegsCPESign)


decodeDangerSign : Decoder DangerSign
decodeDangerSign =
    string
        |> andThen
            (\s ->
                case s of
                    "vaginal-bleeding" ->
                        succeed VaginalBleeding

                    "sever-headaches-with-blurred-vision" ->
                        succeed HeadacheBlurredVision

                    "convulsions" ->
                        succeed Convulsions

                    "abdominal-pain" ->
                        succeed AbdominalPain

                    "difficulty-breathing" ->
                        succeed DifficultyBreathing

                    "fever" ->
                        succeed Fever

                    "extreme-weakness" ->
                        succeed ExtremeWeakness

                    "imminent-delivery" ->
                        succeed ImminentDelivery

                    "labor" ->
                        succeed Labor

                    "looks-very-ill" ->
                        succeed LooksVeryIll

                    "severe-vomiting" ->
                        succeed SevereVomiting

                    "unconscious" ->
                        succeed Unconscious

                    "gush-leaking-vaginal-fluid" ->
                        succeed GushLeakingVaginalFluid

                    "premature-onset-contractions" ->
                        succeed PrematureOnsetContractions

                    "none" ->
                        succeed NoDangerSign

                    _ ->
                        fail <| s ++ " is not a recognized DangerSign"
            )


decodeDangerSigns : Decoder DangerSigns
decodeDangerSigns =
    succeed DangerSignsValue
        |> required "danger_signs" (decodeEverySet decodeDangerSign)
        |> optional "postpartum_mother" (decodeEverySet decodePostpartumMotherDangerSign) (EverySet.fromList [ NoPostpartumMotherDangerSigns ])
        |> optional "postpartum_child" (decodeEverySet decodePostpartumChildDangerSign) (EverySet.fromList [ NoPostpartumChildDangerSigns ])
        |> decodePrenatalMeasurement


decodePostpartumMotherDangerSign : Decoder PostpartumMotherDangerSign
decodePostpartumMotherDangerSign =
    string
        |> andThen
            (\s ->
                postpartumMotherDangerSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (succeed NoPostpartumMotherDangerSigns)
            )


decodePostpartumChildDangerSign : Decoder PostpartumChildDangerSign
decodePostpartumChildDangerSign =
    string
        |> andThen
            (\s ->
                postpartumChildDangerSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (succeed NoPostpartumChildDangerSigns)
            )


decodeLastMenstrualPeriod : Decoder LastMenstrualPeriod
decodeLastMenstrualPeriod =
    succeed LastMenstrualPeriodValue
        |> required "last_menstrual_period" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "weight" (nullable (map WeightInKg decodeFloat)) Nothing
        |> required "confident" bool
        |> optional "not_confident_reason" (nullable decodeLmpDateNotConfidentReason) Nothing
        |> optional "late_first_visit_reason" (nullable decodeLateFirstANCVisitReason) Nothing
        |> optional "confirmation" (decodeWithFallback False bool) False
        |> decodePrenatalMeasurement


decodeLmpDateNotConfidentReason : Decoder LmpDateNotConfidentReason
decodeLmpDateNotConfidentReason =
    string
        |> andThen
            (\reason ->
                lmpDateNotConfidentReasonFromString reason
                    |> Maybe.map succeed
                    |> Maybe.withDefault (reason ++ " is not a recognized LmpDateNotConfidentReason" |> fail)
            )


decodeLateFirstANCVisitReason : Decoder LateFirstANCVisitReason
decodeLateFirstANCVisitReason =
    string
        |> andThen
            (\reason ->
                lateFirstANCVisitReasonFromString reason
                    |> Maybe.map succeed
                    |> Maybe.withDefault (reason ++ " is not a recognized LateFirstANCVisitReason" |> fail)
            )


decodeMedicalHistory : Decoder MedicalHistory
decodeMedicalHistory =
    decodePrenatalMeasurement decodeMedicalHistoryValue


decodeMedicalHistoryValue : Decoder MedicalHistoryValue
decodeMedicalHistoryValue =
    succeed MedicalHistoryValue
        |> required "medical_history" (decodeEverySet decodeMedicalHistorySign)
        |> optional "physical_condition_history"
            (decodeEverySet decodeMedicalHistoryPhysicalCondition)
            (EverySet.singleton MigrateMedicalHistoryPhysicalCondition)
        |> optional "infectious_disease_history"
            (decodeEverySet decodeMedicalHistoryInfectiousDisease)
            (EverySet.singleton NoMedicalHistoryInfectiousDisease)
        |> optional "mental_health_issues"
            (decodeEverySet decodeMedicalHistoryMentalHealthIssue)
            (EverySet.singleton NoMedicalHistoryMentalHealthIssue)
        |> optional "preeclampsia_in_family" decodeOccursInFamilySign NotKnownIfOccurs


decodeMedicalHistorySign : Decoder MedicalHistorySign
decodeMedicalHistorySign =
    string
        |> andThen
            (\s ->
                medicalHistorySignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized MedicalHistorySign" |> fail)
            )


decodeMedicalHistoryPhysicalCondition : Decoder MedicalHistoryPhysicalCondition
decodeMedicalHistoryPhysicalCondition =
    string
        |> andThen
            (\s ->
                medicalHistoryPhysicalConditionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized MedicalHistoryPhysicalCondition" |> fail)
            )


decodeMedicalHistoryInfectiousDisease : Decoder MedicalHistoryInfectiousDisease
decodeMedicalHistoryInfectiousDisease =
    string
        |> andThen
            (\s ->
                medicalHistoryInfectiousDiseaseFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized MedicalHistoryInfectiousDisease" |> fail)
            )


decodeMedicalHistoryMentalHealthIssue : Decoder MedicalHistoryMentalHealthIssue
decodeMedicalHistoryMentalHealthIssue =
    string
        |> andThen
            (\s ->
                medicalHistoryMentalHealthIssueFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized MedicalHistoryMentalHealthIssue" |> fail)
            )


decodeOccursInFamilySign : Decoder OccursInFamilySign
decodeOccursInFamilySign =
    string
        |> andThen
            (\s ->
                occursInFamilySignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized OccursInFamilySign" |> fail)
            )


decodeMedicationSign : Decoder MedicationSign
decodeMedicationSign =
    string
        |> andThen
            (\s ->
                case s of
                    "iron-and-folic-acid-supplement" ->
                        succeed IronAndFolicAcidSupplement

                    "deworming-pill" ->
                        succeed DewormingPill

                    "mebendezole" ->
                        succeed Mebendazole

                    "folic-acid" ->
                        succeed PostpartumFolicAcid

                    "vitamin-a" ->
                        succeed PostpartumVitaminA

                    "none" ->
                        succeed NoMedication

                    _ ->
                        fail <| s ++ " is not a recognized MedicationSign"
            )


decodeMedicationTreatmentSign : Decoder MedicationTreatmentSign
decodeMedicationTreatmentSign =
    string
        |> andThen
            (\s ->
                case s of
                    "still-taking" ->
                        succeed MedicationTreatmentStillTaking

                    "missed-doses" ->
                        succeed MedicationTreatmentMissedDoses

                    "adverse-events" ->
                        succeed MedicationTreatmentAdverseEvents

                    "adverse-events-hospitalization" ->
                        succeed MedicationTreatmentAdverseEventsHospitalization

                    "none" ->
                        succeed NoMedicationTreatment

                    _ ->
                        fail <| s ++ " is not a recognized MedicationTreatmentSign"
            )


decodeHIVTreatmentSign : Decoder HIVTreatmentSign
decodeHIVTreatmentSign =
    string
        |> andThen
            (\s ->
                case s of
                    "still-taking" ->
                        succeed HIVTreatmentStillTaking

                    "missed-doses" ->
                        succeed HIVTreatmentMissedDoses

                    "adverse-events" ->
                        succeed HIVTreatmentAdverseEvents

                    "adverse-events-hospitalization" ->
                        succeed HIVTreatmentAdverseEventsHospitalization

                    "medicine-pmtct" ->
                        succeed HIVTreatmentMedicineByPMTCT

                    "no-medicine-not-seen" ->
                        succeed HIVTreatmentNoMedicineNotSeenAtPMTCT

                    "no-medicine-out-of-stock" ->
                        succeed HIVTreatmentNoMedicineOutOfStock

                    "no-medicine-patient-refused" ->
                        succeed HIVTreatmentNoMedicinePatientRefused

                    "no-medicine-other" ->
                        succeed HIVTreatmentNoMedicineOther

                    "none" ->
                        succeed NoHIVTreatment

                    _ ->
                        fail <| s ++ " is not a recognized HIVTreatmentSign"
            )


decodeMedication : Decoder Medication
decodeMedication =
    decodePrenatalMeasurement decodeMedicationValue


decodeMedicationValue : Decoder MedicationValue
decodeMedicationValue =
    succeed MedicationValue
        |> optional "medication" (nullable (decodeEverySet decodeMedicationSign)) Nothing
        |> optional "hiv_treatment" (nullable (decodeEverySet decodeHIVTreatmentSign)) Nothing
        |> optional "hypertension_treatment" (nullable (decodeEverySet decodeMedicationTreatmentSign)) Nothing
        |> optional "malaria_treatment" (nullable (decodeEverySet decodeMedicationTreatmentSign)) Nothing
        |> optional "anemia_treatment" (nullable (decodeEverySet decodeMedicationTreatmentSign)) Nothing
        |> optional "syphilis_treatment" (nullable (decodeEverySet decodeMedicationTreatmentSign)) Nothing


decodeFetalPresentation : Decoder FetalPresentation
decodeFetalPresentation =
    string
        |> andThen
            (\s ->
                case s of
                    "transverse" ->
                        succeed Transverse

                    "cephalic" ->
                        succeed Cephalic

                    "breech" ->
                        succeed FetalBreech

                    "twins" ->
                        succeed Twins

                    "unclear-imprecise" ->
                        succeed UnclearImprecise

                    "unknown" ->
                        succeed Unknown

                    _ ->
                        fail <| s ++ " is not a recognized FetalPresentation"
            )


decodeObstetricalExam : Decoder ObstetricalExam
decodeObstetricalExam =
    succeed ObstetricalExamValue
        |> optional "fundal_palpable" bool True
        |> optional "fundal_height" (nullable (map HeightInCm decodeFloat)) Nothing
        |> required "fetal_presentation" decodeFetalPresentation
        |> required "fetal_movement" bool
        |> required "fetal_heart_rate" decodeInt
        |> required "c_section_scar" decodeCSectionScar
        |> decodePrenatalMeasurement


decodeObstetricHistory : Decoder ObstetricHistory
decodeObstetricHistory =
    succeed ObstetricHistoryValue
        |> required "currently_pregnant" bool
        |> required "term_pregnancy" decodeInt
        |> required "preterm_pregnancy" decodeInt
        |> required "stillbirths_at_term" decodeInt
        |> required "stillbirths_preterm" decodeInt
        |> required "abortions" decodeInt
        |> required "live_children" decodeInt
        |> decodePrenatalMeasurement


decodePrenatalFamilyPlanning : Decoder PrenatalFamilyPlanning
decodePrenatalFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodePrenatalMeasurement


decodePrenatalNutrition : Decoder PrenatalNutrition
decodePrenatalNutrition =
    succeed PrenatalNutritionValue
        |> required "height" (map HeightInCm decodeFloat)
        |> required "weight" (map WeightInKg decodeFloat)
        |> required "muac" (map MuacInCm decodeFloat)
        |> decodePrenatalMeasurement


decodeMalariaPrevention : Decoder MalariaPrevention
decodeMalariaPrevention =
    decodePrenatalMeasurement decodeMalariaPreventionValue


decodeMalariaPreventionValue : Decoder MalariaPreventionValue
decodeMalariaPreventionValue =
    succeed MalariaPreventionValue
        |> required "resources" (decodeEverySet decodeMalariaPreventionSign)
        |> optional "phase_recorded" decodePhaseRecorded PhaseInitial


decodeMalariaPreventionSign : Decoder MalariaPreventionSign
decodeMalariaPreventionSign =
    string
        |> andThen
            (\s ->
                case s of
                    "mosquito-net" ->
                        succeed MosquitoNet

                    "none" ->
                        succeed NoMalariaPreventionSigns

                    _ ->
                        fail <| s ++ " is not a recognized MalariaPreventionSign"
            )


decodePhaseRecorded : Decoder PhaseRecorded
decodePhaseRecorded =
    string
        |> andThen
            (\s ->
                case s of
                    "initial" ->
                        succeed PhaseInitial

                    "recurrent" ->
                        succeed PhaseRecurrent

                    _ ->
                        fail <| s ++ " is not a recognized PhaseRecorded"
            )


decodeSocialHistorySign : Decoder SocialHistorySign
decodeSocialHistorySign =
    string
        |> andThen
            (\s ->
                case s of
                    "accompanied-by-partner" ->
                        succeed AccompaniedByPartner

                    "partner-hiv-counseling" ->
                        succeed PartnerHivCounseling

                    "none" ->
                        succeed NoSocialHistorySign

                    _ ->
                        fail <| s ++ " is not a recognized SocialHistorySign"
            )


decodeSocialHistory : Decoder SocialHistory
decodeSocialHistory =
    decodeEverySet decodeSocialHistorySign
        |> field "social_history"
        |> decodePrenatalMeasurement


decodeVitals : Decoder Vitals
decodeVitals =
    decodePrenatalMeasurement decodeVitalsValue


decodeVitalsValue : Decoder VitalsValue
decodeVitalsValue =
    succeed VitalsValue
        |> optional "sys" (nullable decodeFloat) Nothing
        |> optional "dia" (nullable decodeFloat) Nothing
        |> optional "heart_rate" (nullable decodeInt) Nothing
        |> required "respiratory_rate" decodeInt
        |> required "body_temperature" decodeFloat
        |> optional "sys_repeated" (nullable decodeFloat) Nothing
        |> optional "dia_repeated" (nullable decodeFloat) Nothing


decodeCSectionReason : Decoder CSectionReason
decodeCSectionReason =
    string
        |> andThen
            (\s ->
                case s of
                    "breech" ->
                        succeed Breech

                    "emergency" ->
                        succeed Emergency

                    "failure-to-progress" ->
                        succeed FailureToProgress

                    "none" ->
                        succeed None

                    "other" ->
                        succeed Other

                    "previous-c-section" ->
                        succeed PreviousCSection

                    _ ->
                        fail <| s ++ " is not a recognized CSectionReason"
            )


decodeCSectionScar : Decoder CSectionScar
decodeCSectionScar =
    string
        |> andThen
            (\s ->
                case s of
                    "vertical" ->
                        succeed Vertical

                    "horizontal" ->
                        succeed Horizontal

                    "none" ->
                        succeed NoScar

                    _ ->
                        fail <| s ++ " is not a recognized CSectionScar"
            )


decodePreviousDeliveryPeriod : Decoder PreviousDeliveryPeriod
decodePreviousDeliveryPeriod =
    string
        |> andThen
            (\s ->
                case s of
                    "less-than-18-month" ->
                        succeed LessThan18Month

                    "more-than-5-years" ->
                        succeed MoreThan5Years

                    "more-than-10-years" ->
                        succeed MoreThan10Years

                    "neither" ->
                        succeed Neither

                    _ ->
                        fail <| s ++ " is not a recognized PreviousDeliveryPeriod"
            )


decodePreviousDeliverySign : Decoder PreviousDeliverySign
decodePreviousDeliverySign =
    string
        |> andThen
            (\s ->
                case s of
                    "c-section-in-past" ->
                        succeed CSectionInPast

                    "c-section-in-previous-delivery" ->
                        succeed CSectionInPreviousDelivery

                    "stillborn-previous-delivery" ->
                        succeed StillbornPreviousDelivery

                    "baby-died-on-day-of-birth-previous-delivery" ->
                        succeed BabyDiedOnDayOfBirthPreviousDelivery

                    "partial-placenta-previous-delivery" ->
                        succeed PartialPlacentaPreviousDelivery

                    "severe-hemorrhaging-previous-delivery" ->
                        succeed SevereHemorrhagingPreviousDelivery

                    "convulsions-previous-delivery" ->
                        succeed ConvulsionsPreviousDelivery

                    "convulsions-and-unconscious-previous-delivery" ->
                        succeed ConvulsionsAndUnconsciousPreviousDelivery

                    "none" ->
                        succeed NoPreviousDeliverySign

                    _ ->
                        fail <| s ++ " is not a recognized PreviousDeliverySign"
            )


decodeObstetricHistorySign : Decoder ObstetricHistorySign
decodeObstetricHistorySign =
    string
        |> andThen
            (\s ->
                case s of
                    "successive-abortions" ->
                        succeed SuccessiveAbortions

                    "successive-premature-deliveries" ->
                        succeed SuccessivePrematureDeliveries

                    "preeclampsia-previous-pregnancy" ->
                        succeed PreeclampsiaPreviousPregnancy

                    "gestational-diabetes-previous-pregnancy" ->
                        succeed GestationalDiabetesPreviousPregnancy

                    "incomplete-cervix-previous-pregnancy" ->
                        succeed IncompleteCervixPreviousPregnancy

                    "rh-negative" ->
                        succeed RhNegative

                    "none" ->
                        succeed NoObstetricHistorySign

                    _ ->
                        fail <| s ++ " is not a recognized ObstetricHistorySign"
            )


decodeObstetricHistoryStep2Sign : Decoder ObstetricHistoryStep2Sign
decodeObstetricHistoryStep2Sign =
    string
        |> andThen
            (\sign ->
                obstetricHistoryStep2SignFromString sign
                    |> Maybe.map succeed
                    |> Maybe.withDefault (sign ++ " is not a recognized ObstetricHistoryStep2Sign" |> fail)
            )


decodeObstetricHistoryStep2 : Decoder ObstetricHistoryStep2
decodeObstetricHistoryStep2 =
    succeed ObstetricHistoryStep2Value
        |> optional "c_sections" decodeInt -1
        |> optional "c_section_reason" (nullable (decodeEverySet decodeCSectionReason)) Nothing
        |> required "previous_delivery" (decodeEverySet decodePreviousDeliverySign)
        |> required "previous_delivery_period" (decodeEverySet decodePreviousDeliveryPeriod)
        |> required "obstetric_history" (decodeEverySet decodeObstetricHistorySign)
        |> optional "obstetric_history_step2"
            (decodeEverySet decodeObstetricHistoryStep2Sign)
            (EverySet.singleton MigrateObstetricHistoryStep2Sign)
        |> decodePrenatalMeasurement


decodeBirthPlan : Decoder BirthPlan
decodeBirthPlan =
    decodePrenatalMeasurement decodeBirthPlanValue


decodeBirthPlanValue : Decoder BirthPlanValue
decodeBirthPlanValue =
    succeed BirthPlanValue
        |> required "birth_plan_signs" (decodeEverySet decodeBirthPlanSign)
        |> required "family_planning_signs" (decodeEverySet decodeFamilyPlanningSign)


decodeBirthPlanSign : Decoder BirthPlanSign
decodeBirthPlanSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "have-insurance" ->
                        succeed Insurance

                    "bought-clothes-for-child" ->
                        succeed BoughtClothes

                    "caregiver-to-accompany-you" ->
                        succeed CaregiverAccompany

                    "saved-money-for-use" ->
                        succeed SavedMoney

                    "planned-for-transportation" ->
                        succeed Transportation

                    "none" ->
                        succeed NoBirthPlan

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized BirthPlanSign"
            )


decodeContributingFactors : Decoder ContributingFactors
decodeContributingFactors =
    decodeGroupMeasurement decodeContributingFactorsValue


decodeNutritionContributingFactors : Decoder NutritionContributingFactors
decodeNutritionContributingFactors =
    decodeNutritionMeasurement decodeContributingFactorsValue


decodeWellChildContributingFactors : Decoder WellChildContributingFactors
decodeWellChildContributingFactors =
    decodeWellChildMeasurement decodeContributingFactorsValue


decodeContributingFactorsValue : Decoder (EverySet ContributingFactorsSign)
decodeContributingFactorsValue =
    decodeEverySet decodeContributingFactorsSign
        |> field "contributing_factors_signs"


decodeFollowUp : Decoder FollowUp
decodeFollowUp =
    decodeGroupMeasurement decodeNutritionFollowUpValue


decodeNutritionFollowUp : Decoder NutritionFollowUp
decodeNutritionFollowUp =
    decodeNutritionMeasurement decodeNutritionFollowUpValue


decodeWellChildFollowUp : Decoder WellChildFollowUp
decodeWellChildFollowUp =
    decodeWellChildMeasurement decodeNutritionFollowUpValue


decodeNutritionFollowUpValue : Decoder NutritionFollowUpValue
decodeNutritionFollowUpValue =
    succeed NutritionFollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> optional "date_concluded" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> custom decodeNutritionAssessment


decodeAcuteIllnessFollowUp : Decoder AcuteIllnessFollowUp
decodeAcuteIllnessFollowUp =
    decodeAcuteIllnessMeasurement decodeAcuteIllnessFollowUpValue


decodeAcuteIllnessFollowUpValue : Decoder AcuteIllnessFollowUpValue
decodeAcuteIllnessFollowUpValue =
    succeed AcuteIllnessFollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> optional "date_concluded" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "acute_illness_diagnosis" (nullable decodeAcuteIllnessDiagnosis) Nothing


decodeNutritionFeeding : Decoder NutritionFeeding
decodeNutritionFeeding =
    decodeHomeVisitMeasurement decodeNutritionFeedingValue


decodeNutritionFeedingValue : Decoder NutritionFeedingValue
decodeNutritionFeedingValue =
    succeed NutritionFeedingValue
        |> required "nutrition_feeding_signs" (decodeEverySet decodeNutritionFeedingSign)
        |> required "supplement_type" decodeNutritionSupplementType
        |> required "sachets_per_day" decodeFloat


decodeNutritionSupplementType : Decoder NutritionSupplementType
decodeNutritionSupplementType =
    string
        |> andThen
            (\type_ ->
                case type_ of
                    "fortified-porridge" ->
                        succeed FortifiedPorridge

                    "rutf" ->
                        succeed Rutf

                    "ongera" ->
                        succeed Ongera

                    "therapeutic-milk" ->
                        succeed TherapeuticMilk

                    "none" ->
                        succeed NoNutritionSupplementType

                    _ ->
                        fail <|
                            type_
                                ++ " is not a recognized NutritionSupplementType"
            )


decodeNutritionFeedingSign : Decoder NutritionFeedingSign
decodeNutritionFeedingSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "receive-supplement" ->
                        succeed ReceiveSupplement

                    "ration-present-at-home" ->
                        succeed RationPresentAtHome

                    "enough-till-next-session" ->
                        succeed EnoughTillNextSession

                    "supplement-shared" ->
                        succeed SupplementShared

                    "encouraged-to-eat" ->
                        succeed EncouragedToEat

                    "refusing-to-eat" ->
                        succeed RefusingToEat

                    "breastfeeding" ->
                        succeed FeedingSignBreastfeeding

                    "clean-water-available" ->
                        succeed CleanWaterAvailable

                    "eaten-with-water" ->
                        succeed EatenWithWater

                    "none" ->
                        succeed NoNutritionFeedingSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionFeedingSign"
            )


decodeNutritionHygiene : Decoder NutritionHygiene
decodeNutritionHygiene =
    decodeHomeVisitMeasurement decodeNutritionHygieneValue


decodeNutritionHygieneValue : Decoder NutritionHygieneValue
decodeNutritionHygieneValue =
    succeed NutritionHygieneValue
        |> required "nutrition_hygiene_signs" (decodeEverySet decodeNutritionHygieneSign)
        |> required "main_water_source" decodeMainWaterSource
        |> required "water_preparation_option" decodeWaterPreparationOption


decodeNutritionHygieneSign : Decoder NutritionHygieneSign
decodeNutritionHygieneSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "soap-in-the-house" ->
                        succeed SoapInTheHouse

                    "wash-hands-before-feeding" ->
                        succeed WashHandsBeforeFeeding

                    "food-covered" ->
                        succeed FoodCovered

                    "none" ->
                        succeed NoNutritionHygieneSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionHygieneSign"
            )


decodeMainWaterSource : Decoder MainWaterSource
decodeMainWaterSource =
    string
        |> andThen
            (\sign ->
                case sign of
                    "piped-water-to-home" ->
                        succeed PipedWaterToHome

                    "public-water-tap" ->
                        succeed PublicWaterTap

                    "rain-water-collection-system" ->
                        succeed RainWaterCollectionSystem

                    "natural-source-flowing-water" ->
                        succeed NaturalSourceFlowingWater

                    "natural-source-standing-water" ->
                        succeed NaturalSourceStandingWater

                    "bottled-water" ->
                        succeed BottledWater

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized MainWaterSource"
            )


decodeWaterPreparationOption : Decoder WaterPreparationOption
decodeWaterPreparationOption =
    string
        |> andThen
            (\sign ->
                case sign of
                    "boiled" ->
                        succeed Boiled

                    "purification-solution" ->
                        succeed PurificationSolution

                    "filtered" ->
                        succeed Filtered

                    "bottled" ->
                        succeed Bottled

                    "none" ->
                        succeed NoWaterPreparationOption

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized WaterPreparationOption"
            )


decodeNutritionFoodSecurity : Decoder NutritionFoodSecurity
decodeNutritionFoodSecurity =
    decodeHomeVisitMeasurement decodeNutritionFoodSecurityValue


decodeNutritionFoodSecurityValue : Decoder NutritionFoodSecurityValue
decodeNutritionFoodSecurityValue =
    succeed NutritionFoodSecurityValue
        |> required "food_security_signs" (decodeEverySet decodeNutritionFoodSecuritySign)
        |> required "main_income_source" decodeMainIncomeSource


decodeNutritionFoodSecuritySign : Decoder NutritionFoodSecuritySign
decodeNutritionFoodSecuritySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "household-got-food" ->
                        succeed HouseholdGotFood

                    "none" ->
                        succeed NoNutritionFoodSecuritySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionFoodSecuritySign"
            )


decodeMainIncomeSource : Decoder MainIncomeSource
decodeMainIncomeSource =
    string
        |> andThen
            (\sign ->
                case sign of
                    "home-based-agriculture" ->
                        succeed HomeBasedAgriculture

                    "commercial-agriculture" ->
                        succeed CommercialAgriculture

                    "public-employee" ->
                        succeed PublicEmployee

                    "private-business-employee" ->
                        succeed PrivateBusinessEmpployee

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized MainIncomeSource"
            )


decodeNutritionCaring : Decoder NutritionCaring
decodeNutritionCaring =
    decodeHomeVisitMeasurement decodeNutritionCaringValue


decodeNutritionCaringValue : Decoder NutritionCaringValue
decodeNutritionCaringValue =
    succeed NutritionCaringValue
        |> required "nutrition_caring_signs" (decodeEverySet decodeNutritionCaringSign)
        |> required "child_caring_options" decodeNutritionCaringOption


decodeNutritionCaringSign : Decoder NutritionCaringSign
decodeNutritionCaringSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "parent-alive-and-healthy" ->
                        succeed ParentsAliveHealthy

                    "child-clean" ->
                        succeed ChildClean

                    "none" ->
                        succeed NoCaringSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionCaringSign"
            )


decodeNutritionCaringOption : Decoder CaringOption
decodeNutritionCaringOption =
    string
        |> andThen
            (\option ->
                case option of
                    "parent" ->
                        succeed CaredByParent

                    "grandparent" ->
                        succeed CaredByGrandparent

                    "sibling" ->
                        succeed CaredBySibling

                    "neighbor" ->
                        succeed CaredByNeighbor

                    "house-helper" ->
                        succeed CaredByHouseHelper

                    "daycare" ->
                        succeed CaredByDaycare

                    _ ->
                        fail <|
                            option
                                ++ " is not a recognized CaringOption"
            )


decodeSymptomsGeneral : Decoder SymptomsGeneral
decodeSymptomsGeneral =
    succeed SymptomsGeneralValue
        |> required "fever_period" decodeInt
        |> required "chills_period" decodeInt
        |> required "night_sweats_period" decodeInt
        |> required "body_aches_period" decodeInt
        |> required "headache_period" decodeInt
        |> required "lethargy_period" decodeInt
        |> required "poor_suck_period" decodeInt
        |> required "unable_to_drink_period" decodeInt
        |> required "unable_to_eat_period" decodeInt
        |> required "increased_thirst_period" decodeInt
        |> required "dry_mouth_period" decodeInt
        |> required "severe_weakness_period" decodeInt
        |> required "yellow_eyes_period" decodeInt
        |> required "coke_colored_urine_period" decodeInt
        |> required "convulsions_period" decodeInt
        |> required "spontaneos_bleeding_period" decodeInt
        |> map symptomsGeneralToDict
        |> decodeAcuteIllnessMeasurement


symptomsGeneralToDict : SymptomsGeneralValue -> Dict SymptomsGeneralSign Int
symptomsGeneralToDict value =
    [ ( SymptomGeneralFever, value.fever )
    , ( Chills, value.chills )
    , ( NightSweats, value.nightSweats )
    , ( BodyAches, value.bodyAches )
    , ( Headache, value.headache )
    , ( Lethargy, value.lethargy )
    , ( PoorSuck, value.poorSuck )
    , ( UnableToDrink, value.unableToDrink )
    , ( UnableToEat, value.unableToEat )
    , ( IncreasedThirst, value.increasedThirst )
    , ( DryMouth, value.dryMouth )
    , ( SevereWeakness, value.severeWeakness )
    , ( YellowEyes, value.yellowEyes )
    , ( CokeColoredUrine, value.cokeColoredUrine )
    , ( SymptomsGeneralConvulsions, value.convulsions )
    , ( SpontaneousBleeding, value.spontaneousBleeding )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsGeneral, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsRespiratory : Decoder SymptomsRespiratory
decodeSymptomsRespiratory =
    decodeAcuteIllnessMeasurement <|
        map7 symptomsRespiratoryToDict
            (field "cough_period" decodeInt)
            (field "shortness_of_breath_period" decodeInt)
            (field "nasal_congestion_period" decodeInt)
            (field "blood_in_sputum_period" decodeInt)
            (field "sore_throat_period" decodeInt)
            (field "loss_of_smell_period" decodeInt)
            (field "stabbing_chest_pain_period" decodeInt)


symptomsRespiratoryToDict : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Dict SymptomsRespiratorySign Int
symptomsRespiratoryToDict cough shortnessOfBreath nasalCongestion bloodInSputum soreThroat lossOfSmell stabbingChestPain =
    [ ( Cough, cough )
    , ( ShortnessOfBreath, shortnessOfBreath )
    , ( NasalCongestion, nasalCongestion )
    , ( BloodInSputum, bloodInSputum )
    , ( SoreThroat, soreThroat )
    , ( LossOfSmell, lossOfSmell )
    , ( StabbingChestPain, stabbingChestPain )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsRespiratory, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsGI : Decoder SymptomsGI
decodeSymptomsGI =
    succeed SymptomsGIValue
        |> custom decodeSymptomsGIDict
        |> required "symptoms_gi_derived_signs" (decodeEverySet decodeSymptomsGIDerivedSign)
        |> decodeAcuteIllnessMeasurement


decodeSymptomsGIDict : Decoder (Dict SymptomsGISign Int)
decodeSymptomsGIDict =
    map5 symptomsGIToDict
        (field "bloody_diarrhea_period" decodeInt)
        (field "non_bloody_diarrhea_period" decodeInt)
        (field "nausea_period" decodeInt)
        (field "vomiting_period" decodeInt)
        (field "abdominal_pain_period" decodeInt)


symptomsGIToDict : Int -> Int -> Int -> Int -> Int -> Dict SymptomsGISign Int
symptomsGIToDict bloodyDiarrhea nonBloodyDiarrhea nausea vomiting abdominalPain =
    [ ( BloodyDiarrhea, bloodyDiarrhea )
    , ( NonBloodyDiarrhea, nonBloodyDiarrhea )
    , ( Nausea, nausea )
    , ( Vomiting, vomiting )
    , ( SymptomGIAbdominalPain, abdominalPain )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsGI, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsGIDerivedSign : Decoder SymptomsGIDerivedSign
decodeSymptomsGIDerivedSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "intractable-vomiting" ->
                        succeed IntractableVomiting

                    "none" ->
                        succeed NoSymptomsGIDerived

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized SymptomsGIDerivedSign"
            )


decodeAcuteIllnessVitals : Decoder AcuteIllnessVitals
decodeAcuteIllnessVitals =
    decodeAcuteIllnessMeasurement decodeVitalsValue


decodeWellChildVitals : Decoder WellChildVitals
decodeWellChildVitals =
    decodeWellChildMeasurement decodeVitalsValue


decodeAcuteFindings : Decoder AcuteFindings
decodeAcuteFindings =
    succeed AcuteFindingsValue
        |> required "findings_signs_general" (decodeEverySet decodeAcuteFindingsGeneralSign)
        |> required "findings_signs_respiratory" (decodeEverySet decodeAcuteFindingsRespiratorySign)
        |> decodeAcuteIllnessMeasurement


decodeAcuteFindingsGeneralSign : Decoder AcuteFindingsGeneralSign
decodeAcuteFindingsGeneralSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "lethargic-or-unconscious" ->
                        succeed LethargicOrUnconscious

                    "poor-suck" ->
                        succeed AcuteFindingsPoorSuck

                    "sunken-eyes" ->
                        succeed SunkenEyes

                    "poor-skin-turgor" ->
                        succeed PoorSkinTurgor

                    "jaundice" ->
                        succeed Jaundice

                    "none" ->
                        succeed NoAcuteFindingsGeneralSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteFindingsGeneralSign"
            )


decodeAcuteFindingsRespiratorySign : Decoder AcuteFindingsRespiratorySign
decodeAcuteFindingsRespiratorySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "stridor" ->
                        succeed Stridor

                    "nasal-flaring" ->
                        succeed NasalFlaring

                    "severe-wheezing" ->
                        succeed SevereWheezing

                    "sub-costal-retractions" ->
                        succeed SubCostalRetractions

                    "none" ->
                        succeed NoAcuteFindingsRespiratorySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteFindingsRespiratorySign"
            )


decodeMalariaTesting : Decoder MalariaTesting
decodeMalariaTesting =
    decodeRapidTestResult
        |> field "malaria_rapid_test"
        |> decodeAcuteIllnessMeasurement


decodeCovidTesting : Decoder CovidTesting
decodeCovidTesting =
    decodeAcuteIllnessMeasurement decodeCovidTestingValue


decodeCovidTestingValue : Decoder CovidTestingValue
decodeCovidTestingValue =
    succeed CovidTestingValue
        |> required "rapid_test_result" decodeRapidTestResult
        |> optional "administration_note" (maybe decodeAdministrationNote) Nothing


decodeRapidTestResult : Decoder RapidTestResult
decodeRapidTestResult =
    string
        |> andThen
            (\result ->
                malariaRapidTestResultFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized RapidTestResult" |> fail)
            )


malariaRapidTestResultFromString : String -> Maybe RapidTestResult
malariaRapidTestResultFromString result =
    case result of
        "positive" ->
            Just RapidTestPositive

        "positive-and-pregnant" ->
            Just RapidTestPositiveAndPregnant

        "negative" ->
            Just RapidTestNegative

        "indeterminate" ->
            Just RapidTestIndeterminate

        "unable-to-run" ->
            Just RapidTestUnableToRun

        "unable-to-run-and-pregnant" ->
            Just RapidTestUnableToRunAndPregnant

        _ ->
            Nothing


decodeSendToHC : Decoder SendToHC
decodeSendToHC =
    decodeAcuteIllnessMeasurement decodeSendToHCValue


decodeNutritionSendToHC : Decoder NutritionSendToHC
decodeNutritionSendToHC =
    decodeNutritionMeasurement decodeSendToHCValue


decodeWellChildSendToHC : Decoder WellChildSendToHC
decodeWellChildSendToHC =
    decodeWellChildMeasurement decodeSendToHCValue


decodeGroupSendToHC : Decoder GroupSendToHC
decodeGroupSendToHC =
    decodeGroupMeasurement decodeSendToHCValue


decodeSendToHCValue : Decoder SendToHCValue
decodeSendToHCValue =
    succeed SendToHCValue
        |> required "send_to_hc" (decodeEverySet decodeSendToHCSign)
        |> optional "reason_not_sent_to_hc" decodeReasonForNonReferral NoReasonForNonReferral


decodeSendToHCSign : Decoder SendToHCSign
decodeSendToHCSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "referral-form" ->
                        succeed HandReferrerForm

                    "refer-to-hc" ->
                        succeed ReferToHealthCenter

                    "accompany-to-hc" ->
                        succeed PrenatalAccompanyToHC

                    "enroll-to-nutrition-program" ->
                        succeed EnrollToNutritionProgram

                    "refer-to-nutrition-program" ->
                        succeed ReferToNutritionProgram

                    "none" ->
                        succeed NoSendToHCSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized SendToHCSign"
            )


decodeReasonForNonReferral : Decoder ReasonForNonReferral
decodeReasonForNonReferral =
    string
        |> andThen
            (\event ->
                reasonForNonReferralFromString event
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            event
                                ++ "is not a recognized ReasonForNonReferral"
                        )
            )


decodePrenatalReferralValue : Decoder PrenatalReferralValue
decodePrenatalReferralValue =
    succeed PrenatalReferralValue
        |> optional "send_to_hc" (nullable (decodeEverySet decodeSendToHCSign)) Nothing
        |> optional "reason_not_sent_to_hc" (nullable decodeReasonForNonReferral) Nothing
        |> optional "referrals" (nullable (decodeEverySet decodeReferToFacilitySign)) Nothing
        |> optional "reasons_for_non_referrals" (nullable (decodeEverySet decodeNonReferralSign)) Nothing


decodeReferToFacilitySign : Decoder ReferToFacilitySign
decodeReferToFacilitySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "hospital" ->
                        succeed ReferToHospital

                    "hospital-referral-form" ->
                        succeed ReferralFormHospital

                    "mhs" ->
                        succeed ReferToMentalHealthSpecialist

                    "mhs-referral-form" ->
                        succeed ReferralFormMentalHealthSpecialist

                    "mhs-accompany" ->
                        succeed AccompanyToMentalHealthSpecialist

                    "arv" ->
                        succeed ReferToARVProgram

                    "arv-referral-form" ->
                        succeed ReferralFormARVProgram

                    "arv-accompany" ->
                        succeed AccompanyToARVProgram

                    "ncd" ->
                        succeed ReferToNCDProgram

                    "ncd-referral-form" ->
                        succeed ReferralFormNCDProgram

                    "ncd-accompany" ->
                        succeed AccompanyToNCDProgram

                    "anc" ->
                        succeed ReferToANCServices

                    "anc-referral-form" ->
                        succeed ReferralFormANCServices

                    "anc-accompany" ->
                        succeed AccompanyToANCServices

                    "us" ->
                        succeed ReferToUltrasound

                    "us-referral-form" ->
                        succeed ReferralFormUltrasound

                    "none" ->
                        succeed NoReferToFacilitySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ReferToFacilitySign"
            )


decodeNonReferralSign : Decoder NonReferralSign
decodeNonReferralSign =
    string
        |> andThen
            (\sign ->
                if sign == "none" then
                    succeed NoNonReferralSigns

                else
                    let
                        parts =
                            String.split "-" sign

                        failure =
                            fail <| sign ++ " is not a recognized NonReferralSign"
                    in
                    List.head parts
                        |> Maybe.map
                            (\prefix ->
                                let
                                    reasonForNonReferral =
                                        List.tail parts
                                            |> Maybe.map (List.intersperse "-" >> String.concat)
                                            |> Maybe.andThen reasonForNonReferralFromString
                                in
                                case prefix of
                                    "hospital" ->
                                        Maybe.map (NonReferralReasonHospital >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "mhs" ->
                                        Maybe.map (NonReferralReasonMentalHealthSpecialist >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "arv" ->
                                        Maybe.map (NonReferralReasonARVProgram >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "ncd" ->
                                        Maybe.map (NonReferralReasonNCDProgram >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "anc" ->
                                        Maybe.map (NonReferralReasonANCServices >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "us" ->
                                        Maybe.map (NonReferralReasonUltrasound >> succeed) reasonForNonReferral
                                            |> Maybe.withDefault failure

                                    "none" ->
                                        succeed NoNonReferralSigns

                                    _ ->
                                        failure
                            )
                        |> Maybe.withDefault failure
            )


decodeReferralFacility : Decoder ReferralFacility
decodeReferralFacility =
    string
        |> andThen
            (\facility ->
                case facility of
                    "hc" ->
                        succeed FacilityHealthCenter

                    "hospital" ->
                        succeed FacilityHospital

                    "mhs" ->
                        succeed FacilityMentalHealthSpecialist

                    "arv" ->
                        succeed FacilityARVProgram

                    "ncd" ->
                        succeed FacilityNCDProgram

                    "anc" ->
                        succeed FacilityANCServices

                    "us" ->
                        succeed FacilityUltrasound

                    _ ->
                        fail <|
                            facility
                                ++ " is not a recognized ReferralFacility"
            )


decodeContributingFactorsSign : Decoder ContributingFactorsSign
decodeContributingFactorsSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "lack-of-breast-milk" ->
                        succeed FactorLackOfBreastMilk

                    "maternal-mastitis" ->
                        succeed FactorMaternalMastitis

                    "poor-suck" ->
                        succeed FactorPoorSuck

                    "diarrhea-or-vomiting" ->
                        succeed FactorDiarrheaOrVomiting

                    "none" ->
                        succeed NoContributingFactorsSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ContributingFactorsSign"
            )


decodeFollowUpOption : Decoder FollowUpOption
decodeFollowUpOption =
    string
        |> andThen
            (\sign ->
                case sign of
                    "1-d" ->
                        succeed OneDay

                    "3-d" ->
                        succeed ThreeDays

                    "1-w" ->
                        succeed OneWeek

                    "2-w" ->
                        succeed TwoWeeks

                    "1-m" ->
                        succeed OneMonth

                    "2-m" ->
                        succeed TwoMonths

                    "3-m" ->
                        succeed ThreeMonths

                    "none" ->
                        succeed FollowUpNotNeeded

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FollowUpOption"
            )


decodeMedicationDistribution : Decoder MedicationDistribution
decodeMedicationDistribution =
    decodeAcuteIllnessMeasurement decodeMedicationDistributionValue


decodeMedicationDistributionValue : Decoder MedicationDistributionValue
decodeMedicationDistributionValue =
    succeed MedicationDistributionValue
        |> required "prescribed_medication" (decodeEverySet decodeMedicationDistributionSign)
        |> required "non_administration_reason" (decodeEverySet decodeMedicationNonAdministrationSign)


decodeMedicationDistributionSign : Decoder MedicationDistributionSign
decodeMedicationDistributionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "amoxicillin" ->
                        succeed Amoxicillin

                    "aspirin" ->
                        succeed Aspirin

                    "coartem" ->
                        succeed Coartem

                    "ors" ->
                        succeed ORS

                    "zinc" ->
                        succeed Zinc

                    "lemon-juice-or-honey" ->
                        succeed LemonJuiceOrHoney

                    "albendazole" ->
                        succeed Albendazole

                    "mebendezole" ->
                        succeed Mebendezole

                    "vitamina" ->
                        succeed VitaminA

                    "paracetamol" ->
                        succeed Paracetamol

                    "tenofovir" ->
                        succeed Tenofovir

                    "lamivudine" ->
                        succeed Lamivudine

                    "dolutegravir" ->
                        succeed Dolutegravir

                    "tdf3tc" ->
                        succeed TDF3TC

                    "iron" ->
                        succeed Iron

                    "folicacid" ->
                        succeed FolicAcid

                    "ceftriaxone" ->
                        succeed Ceftriaxone

                    "azithromycin" ->
                        succeed Azithromycin

                    "metronidazole" ->
                        succeed Metronidazole

                    "calcium" ->
                        succeed Calcium

                    "mms" ->
                        succeed MMS

                    "fefol" ->
                        succeed Fefol

                    "none" ->
                        succeed NoMedicationDistributionSigns

                    "none-initial" ->
                        succeed NoMedicationDistributionSignsInitialPhase

                    "none-recurrent" ->
                        succeed NoMedicationDistributionSignsRecurrentPhase

                    _ ->
                        fail <| sign ++ " is not a recognized MedicationDistributionSign"
            )


decodeMedicationNonAdministrationSign : Decoder MedicationNonAdministrationSign
decodeMedicationNonAdministrationSign =
    string
        |> andThen
            (\sign ->
                if sign == "none" then
                    succeed NoMedicationNonAdministrationSigns

                else
                    let
                        parts =
                            String.split "-" sign

                        failure =
                            fail <| sign ++ " is not a recognized MedicationNonAdministrationSign"
                    in
                    List.head parts
                        |> Maybe.map
                            (\prefix ->
                                let
                                    administrationNote =
                                        List.tail parts
                                            |> Maybe.map (List.intersperse "-" >> String.concat)
                                            |> Maybe.andThen administrationNoteFromString
                                in
                                case prefix of
                                    "amoxicillin" ->
                                        administrationNote
                                            |> Maybe.map (MedicationAmoxicillin >> succeed)
                                            |> Maybe.withDefault failure

                                    "aspirin" ->
                                        administrationNote
                                            |> Maybe.map (MedicationAspirin >> succeed)
                                            |> Maybe.withDefault failure

                                    "coartem" ->
                                        administrationNote
                                            |> Maybe.map (MedicationCoartem >> succeed)
                                            |> Maybe.withDefault failure

                                    "ors" ->
                                        administrationNote
                                            |> Maybe.map (MedicationORS >> succeed)
                                            |> Maybe.withDefault failure

                                    "zinc" ->
                                        administrationNote
                                            |> Maybe.map (MedicationZinc >> succeed)
                                            |> Maybe.withDefault failure

                                    "paracetamol" ->
                                        administrationNote
                                            |> Maybe.map (MedicationParacetamol >> succeed)
                                            |> Maybe.withDefault failure

                                    "mebendezole" ->
                                        administrationNote
                                            |> Maybe.map (MedicationMebendezole >> succeed)
                                            |> Maybe.withDefault failure

                                    "tenofovir" ->
                                        administrationNote
                                            |> Maybe.map (MedicationTenofovir >> succeed)
                                            |> Maybe.withDefault failure

                                    "lamivudine" ->
                                        administrationNote
                                            |> Maybe.map (MedicationLamivudine >> succeed)
                                            |> Maybe.withDefault failure

                                    "dolutegravir" ->
                                        administrationNote
                                            |> Maybe.map (MedicationDolutegravir >> succeed)
                                            |> Maybe.withDefault failure

                                    "tdf3tc" ->
                                        administrationNote
                                            |> Maybe.map (MedicationTDF3TC >> succeed)
                                            |> Maybe.withDefault failure

                                    "iron" ->
                                        administrationNote
                                            |> Maybe.map (MedicationIron >> succeed)
                                            |> Maybe.withDefault failure

                                    "folicacid" ->
                                        administrationNote
                                            |> Maybe.map (MedicationFolicAcid >> succeed)
                                            |> Maybe.withDefault failure

                                    "ceftriaxone" ->
                                        administrationNote
                                            |> Maybe.map (MedicationCeftriaxone >> succeed)
                                            |> Maybe.withDefault failure

                                    "azithromycin" ->
                                        administrationNote
                                            |> Maybe.map (MedicationAzithromycin >> succeed)
                                            |> Maybe.withDefault failure

                                    "metronidazole" ->
                                        administrationNote
                                            |> Maybe.map (MedicationMetronidazole >> succeed)
                                            |> Maybe.withDefault failure

                                    "vitamina" ->
                                        administrationNote
                                            |> Maybe.map (MedicationVitaminA >> succeed)
                                            |> Maybe.withDefault failure

                                    _ ->
                                        failure
                            )
                        |> Maybe.withDefault failure
            )


decodeTravelHistory : Decoder TravelHistory
decodeTravelHistory =
    decodeEverySet decodeTravelHistorySign
        |> field "travel_history"
        |> decodeAcuteIllnessMeasurement


decodeTravelHistorySign : Decoder TravelHistorySign
decodeTravelHistorySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "covid19-country" ->
                        succeed COVID19Country

                    "none" ->
                        succeed NoTravelHistorySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TravelHistorySign"
            )


decodeTreatmentReview : Decoder TreatmentReview
decodeTreatmentReview =
    decodeEverySet decodeTreatmentReviewSign
        |> field "treatment_history"
        |> decodeAcuteIllnessMeasurement


decodeTreatmentReviewSign : Decoder TreatmentReviewSign
decodeTreatmentReviewSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "fever-past-six-hours" ->
                        succeed FeverPast6Hours

                    "fever-past-six-hours-helped" ->
                        succeed FeverPast6HoursHelped

                    "malaria-today" ->
                        succeed MalariaToday

                    "malaria-today-helped" ->
                        succeed MalariaTodayHelped

                    "malaria-past-month" ->
                        succeed MalariaWithinPastMonth

                    "malaria-past-month-helped" ->
                        succeed MalariaWithinPastMonthHelped

                    "none" ->
                        succeed NoTreatmentReviewSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TreatmentReviewSign"
            )


decodeExposure : Decoder Exposure
decodeExposure =
    decodeEverySet decodeExposureSign
        |> field "exposure"
        |> decodeAcuteIllnessMeasurement


decodeExposureSign : Decoder ExposureSign
decodeExposureSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "covid19-symptoms" ->
                        succeed COVID19Symptoms

                    "none" ->
                        succeed NoExposureSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ExposureSign"
            )


decodeIsolation : Decoder Isolation
decodeIsolation =
    succeed IsolationValue
        |> required "isolation" (decodeEverySet decodeIsolationSign)
        |> required "reason_for_not_isolating" (decodeEverySet decodeReasonForNotIsolating)
        |> decodeAcuteIllnessMeasurement


decodeIsolationSign : Decoder IsolationSign
decodeIsolationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "patient-isolated" ->
                        succeed PatientIsolated

                    "sign-on-door" ->
                        succeed SignOnDoor

                    "health-education" ->
                        succeed HealthEducation

                    "none" ->
                        succeed NoIsolationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized IsolationSign"
            )


decodeReasonForNotIsolating : Decoder ReasonForNotIsolating
decodeReasonForNotIsolating =
    string
        |> andThen
            (\sign ->
                case sign of
                    "no-space" ->
                        succeed NoSpace

                    "too-ill" ->
                        succeed TooIll

                    "can-not-separate" ->
                        succeed CanNotSeparateFromFamily

                    "other" ->
                        succeed OtherReason

                    "n-a" ->
                        succeed IsolationReasonNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized IsolationSign"
            )


decodeHCContact : Decoder HCContact
decodeHCContact =
    succeed HCContactValue
        |> required "hc_contact" (decodeEverySet decodeHCContactSign)
        |> required "hc_recommendation" (decodeEverySet decodeHCRecommendation)
        |> required "hc_response_time" (decodeEverySet decodeResponsePeriod)
        |> required "ambulance_arrival_time" (decodeEverySet decodeResponsePeriod)
        |> decodeAcuteIllnessMeasurement


decodeHCContactSign : Decoder HCContactSign
decodeHCContactSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "contact-hc" ->
                        succeed ContactedHealthCenter

                    "none" ->
                        succeed NoHCContactSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HCContactSign"
            )


decodeHCRecommendation : Decoder HCRecommendation
decodeHCRecommendation =
    string
        |> andThen
            (\sign ->
                case sign of
                    "send-ambulance" ->
                        succeed SendAmbulance

                    "home-isolation" ->
                        succeed HomeIsolation

                    "come-to-hc" ->
                        succeed ComeToHealthCenter

                    "chw-monitoring" ->
                        succeed ChwMonitoring

                    "n-a" ->
                        succeed HCRecommendationNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HCRecommendation"
            )


decodeResponsePeriod : Decoder ResponsePeriod
decodeResponsePeriod =
    string
        |> andThen
            (\sign ->
                case sign of
                    "less-than-30m" ->
                        succeed LessThan30Min

                    "30m-1h" ->
                        succeed Between30min1Hour

                    "1h-2h" ->
                        succeed Between1Hour2Hour

                    "2h-1d" ->
                        succeed Between2Hour1Day

                    "n-a" ->
                        succeed ResponsePeriodNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ResponsePeriod"
            )


decodeCall114 : Decoder Call114
decodeCall114 =
    succeed Call114Value
        |> required "114_contact" (decodeEverySet decodeCall114Sign)
        |> required "114_recommendation" (decodeEverySet (decodeWithFallback NoneOtherRecommendation114 decodeRecommendation114))
        |> required "site_recommendation" (decodeEverySet (decodeWithFallback RecommendationSiteNotApplicable decodeRecommendationSite))
        |> decodeAcuteIllnessMeasurement


decodeCall114Sign : Decoder Call114Sign
decodeCall114Sign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "call-114" ->
                        succeed Call114

                    "contact-site" ->
                        succeed ContactSite

                    "none" ->
                        succeed NoCall114Signs

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized Call114Sign"
            )


decodeRecommendation114 : Decoder Recommendation114
decodeRecommendation114 =
    string
        |> andThen
            (\sign ->
                case sign of
                    "send-to-hc" ->
                        succeed SendToHealthCenter

                    "send-to-rrtc" ->
                        succeed SendToRRTCenter

                    "send-to-hospital" ->
                        succeed SendToHospital

                    "other" ->
                        succeed OtherRecommendation114

                    "none-no-answer" ->
                        succeed NoneNoAnswer

                    "none-busy-signal" ->
                        succeed NoneBusySignal

                    "none-other" ->
                        succeed NoneOtherRecommendation114

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized Recommendation114"
            )


decodeRecommendationSite : Decoder RecommendationSite
decodeRecommendationSite =
    string
        |> andThen
            (\sign ->
                case sign of
                    "team-to-village" ->
                        succeed TeamComeToVillage

                    "send-with-form" ->
                        succeed SendToSiteWithForm

                    "other" ->
                        succeed OtherRecommendationSite

                    "none-sent-with-form" ->
                        succeed NoneSentWithForm

                    "none-patient-refused" ->
                        succeed NonePatientRefused

                    "none-other" ->
                        succeed NoneOtherRecommendationSite

                    "n-a" ->
                        succeed RecommendationSiteNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized RecommendationSite"
            )


decodeAcuteIllnessMuac : Decoder AcuteIllnessMuac
decodeAcuteIllnessMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeAcuteIllnessMeasurement


decodeTreatmentOngoing : Decoder TreatmentOngoing
decodeTreatmentOngoing =
    decodeAcuteIllnessMeasurement decodeTreatmentOngoingValue


decodeTreatmentOngoingValue : Decoder TreatmentOngoingValue
decodeTreatmentOngoingValue =
    succeed TreatmentOngoingValue
        |> required "treatment_ongoing" (decodeEverySet decodeTreatmentOngoingSign)
        |> required "reason_for_not_taking" decodeReasonForNotTaking
        |> required "missed_doses" decodeInt
        |> required "adverse_events" (decodeEverySet decodeAdverseEvent)


decodeTreatmentOngoingSign : Decoder TreatmentOngoingSign
decodeTreatmentOngoingSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "taken-as-prescribed" ->
                        succeed TakenAsPrescribed

                    "missed-doses" ->
                        succeed MissedDoses

                    "feel-better" ->
                        succeed FeelingBetter

                    "side-effects" ->
                        succeed SideEffects

                    "none" ->
                        succeed NoTreatmentOngoingSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TreatmentOngoingSign"
            )


decodeReasonForNotTaking : Decoder ReasonForNotTaking
decodeReasonForNotTaking =
    string
        |> andThen
            (\reason ->
                case reason of
                    "adverse-event" ->
                        succeed NotTakingAdverseEvent

                    "no-money" ->
                        succeed NotTakingNoMoney

                    "memory-problems" ->
                        succeed NotTakingMemoryProblems

                    "treatment-not-started" ->
                        succeed NotTakingTreatmentNotStarted

                    "other" ->
                        succeed NotTakingOther

                    "none" ->
                        succeed NoReasonForNotTakingSign

                    _ ->
                        fail <|
                            reason
                                ++ " is not a recognized ReasonForNotTaking"
            )


decodeAdverseEvent : Decoder AdverseEvent
decodeAdverseEvent =
    string
        |> andThen
            (\event ->
                case event of
                    "rash-itching" ->
                        succeed AdverseEventRashOrItching

                    "fever" ->
                        succeed AdverseEventFever

                    "diarrhea" ->
                        succeed AdverseEventDiarrhea

                    "vomiting" ->
                        succeed AdverseEventVomiting

                    "fatigue" ->
                        succeed AdverseEventFatigue

                    "other" ->
                        succeed AdverseEventOther

                    "none" ->
                        succeed NoAdverseEvent

                    _ ->
                        fail <|
                            event
                                ++ " is not a recognized AdverseEvent"
            )


decodeAcuteIllnessCoreExam : Decoder AcuteIllnessCoreExam
decodeAcuteIllnessCoreExam =
    decodeAcuteIllnessMeasurement decodeAcuteIllnessCoreExamValue


decodeAcuteIllnessCoreExamValue : Decoder AcuteIllnessCoreExamValue
decodeAcuteIllnessCoreExamValue =
    succeed AcuteIllnessCoreExamValue
        |> required "heart" (decodeEverySet decodeHeartCPESign)
        |> required "lungs" (decodeEverySet decodeLungsCPESign)


decodeAcuteIllnessDangerSigns : Decoder AcuteIllnessDangerSigns
decodeAcuteIllnessDangerSigns =
    decodeEverySet decodeAcuteIllnessDangerSign
        |> field "acute_illness_danger_signs"
        |> decodeAcuteIllnessMeasurement


decodeAcuteIllnessDangerSign : Decoder AcuteIllnessDangerSign
decodeAcuteIllnessDangerSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "condition-not-improving" ->
                        succeed DangerSignConditionNotImproving

                    "unable-drink-suck" ->
                        succeed DangerSignUnableDrinkSuck

                    "vomiting" ->
                        succeed DangerSignVomiting

                    "convulsions" ->
                        succeed DangerSignConvulsions

                    "lethargy-unconsciousness" ->
                        succeed DangerSignLethargyUnconsciousness

                    "respiratory-distress" ->
                        succeed DangerSignRespiratoryDistress

                    "spontaneous-bleeding" ->
                        succeed DangerSignSpontaneousBleeding

                    "bloody-diarrhea" ->
                        succeed DangerSignBloodyDiarrhea

                    "new-skip-rash" ->
                        succeed DangerSignNewSkinRash

                    "none" ->
                        succeed NoAcuteIllnessDangerSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteIllnessDangerSign"
            )


decodeAcuteIllnessNutrition : Decoder AcuteIllnessNutrition
decodeAcuteIllnessNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeAcuteIllnessMeasurement


decodeAcuteIllnessContactsTracing : Decoder AcuteIllnessContactsTracing
decodeAcuteIllnessContactsTracing =
    decodeWithFallback [] (list decodeContactTraceItemFromString)
        |> field "contacts_trace_data"
        |> decodeAcuteIllnessMeasurement


decodeContactTraceItemFromString : Decoder ContactTraceItem
decodeContactTraceItemFromString =
    string
        |> andThen
            (\data ->
                let
                    parts =
                        String.split "[&]" data
                in
                case parts of
                    [ id, firstName, secondName, contactGender, phoneNumber, contactDate ] ->
                        let
                            date =
                                Date.fromIsoString contactDate
                                    |> Result.toMaybe

                            gender =
                                genderFromString contactGender
                        in
                        Maybe.map2
                            (\date_ gender_ ->
                                succeed <|
                                    ContactTraceItem
                                        (toEntityUuid id)
                                        firstName
                                        secondName
                                        gender_
                                        phoneNumber
                                        date_
                                        -- Resolution date is set to the date on which
                                        -- Covid  isolation is completed.
                                        (Date.add Days (covidIsolationPeriod + 1) date_)
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                            )
                            date
                            gender
                            |> Maybe.withDefault
                                (fail <|
                                    contactDate
                                        ++ " is not a valid date format at ContactTraceItem"
                                )

                    _ ->
                        fail <|
                            data
                                ++ " is not a recognized ContactTraceItem"
            )


decodeAcuteIllnessTraceContact : Decoder AcuteIllnessTraceContact
decodeAcuteIllnessTraceContact =
    decodeAcuteIllnessMeasurement decodeContactTraceItem


decodeContactTraceItem : Decoder ContactTraceItem
decodeContactTraceItem =
    succeed ContactTraceItem
        |> required "referred_person" decodeEntityUuid
        |> required "first_name" string
        |> required "second_name" string
        |> required "gender" decodeGender
        |> required "phone_number" string
        |> required "contact_date" Gizra.NominalDate.decodeYYYYMMDD
        |> required "date_concluded" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "last_follow_up_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "symptoms_general" (nullable <| decodeEverySet decodeSymptomsGeneralSign) Nothing
        |> optional "symptoms_respiratory" (nullable <| decodeEverySet decodeSymptomsRespiratorySign) Nothing
        |> optional "symptoms_gi" (nullable <| decodeEverySet decodeSymptomsGISign) Nothing
        |> optional "trace_outcome" (nullable decodeTraceOutcome) Nothing


decodeSymptomsGeneralSign : Decoder SymptomsGeneralSign
decodeSymptomsGeneralSign =
    string
        |> andThen
            (\sign ->
                symptomsGeneralSignFromString sign
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| sign ++ " is not a recognized SymptomsGeneralSign")
            )


decodeSymptomsRespiratorySign : Decoder SymptomsRespiratorySign
decodeSymptomsRespiratorySign =
    string
        |> andThen
            (\sign ->
                symptomsRespiratorySignFromString sign
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| sign ++ " is not a recognized SymptomsRespiratorySign")
            )


decodeSymptomsGISign : Decoder SymptomsGISign
decodeSymptomsGISign =
    string
        |> andThen
            (\sign ->
                symptomsGISignFromString sign
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| sign ++ " is not a recognized SymptomsGISign")
            )


decodeTraceOutcome : Decoder TraceOutcome
decodeTraceOutcome =
    string
        |> andThen
            (\outcome ->
                case outcome of
                    "no-answer" ->
                        succeed OutcomeNoAnswer

                    "wrong-contact-info" ->
                        succeed OutcomeWrongContactInfo

                    "declined-follow-up" ->
                        succeed OutcomeDeclinedFollowUp

                    "no-symptoms" ->
                        succeed OutcomeNoSymptoms

                    "referred-to-hc" ->
                        succeed OutcomeReferredToHC

                    _ ->
                        fail <|
                            outcome
                                ++ " is not a recognized TraceOutcome"
            )


decodeHealthEducation : Decoder HealthEducation
decodeHealthEducation =
    decodeAcuteIllnessMeasurement decodeHealthEducationValue


decodeNutritionHealthEducation : Decoder NutritionHealthEducation
decodeNutritionHealthEducation =
    decodeNutritionMeasurement decodeHealthEducationValue


decodeWellChildHealthEducation : Decoder WellChildHealthEducation
decodeWellChildHealthEducation =
    decodeWellChildMeasurement decodeHealthEducationValue


decodeGroupHealthEducation : Decoder GroupHealthEducation
decodeGroupHealthEducation =
    decodeGroupMeasurement decodeHealthEducationValue


decodeHealthEducationValue : Decoder HealthEducationValue
decodeHealthEducationValue =
    succeed HealthEducationValue
        |> required "health_education_signs" (decodeEverySet decodeHealthEducationSign)
        |> optional "reason_not_given_education" decodeReasonForNotProvidingHealthEducation NoReasonForNotProvidingHealthEducation


decodeHealthEducationSign : Decoder HealthEducationSign
decodeHealthEducationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "education-for-diagnosis" ->
                        succeed MalariaPrevention

                    "none" ->
                        succeed NoHealthEducationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HealthEducationSign"
            )


decodeReasonForNotProvidingHealthEducation : Decoder ReasonForNotProvidingHealthEducation
decodeReasonForNotProvidingHealthEducation =
    string
        |> andThen
            (\reason ->
                case reason of
                    "needs-emergency-referral" ->
                        succeed PatientNeedsEmergencyReferral

                    "received-emergency-case" ->
                        succeed ReceivedEmergencyCase

                    "lack-of-appropriate-education-guide" ->
                        succeed LackOfAppropriateEducationUserGuide

                    "patient-refused" ->
                        succeed PatientRefused

                    "patient-too-ill" ->
                        succeed PatientTooIll

                    "none" ->
                        succeed NoReasonForNotProvidingHealthEducation

                    _ ->
                        fail <|
                            reason
                                ++ " is not a recognized ReasonForNotProvidingHealthEducation"
            )


decodeNutritionAssessment : Decoder (EverySet NutritionAssessment)
decodeNutritionAssessment =
    map2 postProcessNutritionAssessment
        (field "nutrition_assesment" (decodeEverySet decodeNutritionAssessmentFromString))
        (field "nutrition_signs" (decodeEverySet decodeChildNutritionSign))


decodeNutritionAssessmentFromString : Decoder NutritionAssessment
decodeNutritionAssessmentFromString =
    string
        |> andThen
            (\s ->
                nutritionAssessmentFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized NutritionAssessment" |> fail)
            )


postProcessNutritionAssessment : EverySet NutritionAssessment -> EverySet ChildNutritionSign -> EverySet NutritionAssessment
postProcessNutritionAssessment assesmentFromString nutritionSign =
    assesmentFromString
        |> EverySet.toList
        |> List.head
        |> Maybe.map
            (\assesment ->
                case assesment of
                    AssesmentMalnutritionSigns _ ->
                        EverySet.toList nutritionSign
                            |> AssesmentMalnutritionSigns
                            |> EverySet.singleton

                    _ ->
                        assesmentFromString
            )
        |> Maybe.withDefault assesmentFromString


decodeWellChildSymptomsReview : Decoder WellChildSymptomsReview
decodeWellChildSymptomsReview =
    decodeEverySet decodeWellChildSymptom
        |> field "well_child_symptoms"
        |> decodeWellChildMeasurement


decodeWellChildSymptom : Decoder WellChildSymptom
decodeWellChildSymptom =
    string
        |> andThen
            (\sign ->
                case sign of
                    "breathing-problems" ->
                        succeed SymptomBreathingProblems

                    "convulsions" ->
                        succeed SymptomConvulsions

                    "lethargy-or-unresponsiveness" ->
                        succeed SymptomLethargyOrUnresponsiveness

                    "diarrhea" ->
                        succeed SymptomDiarrhea

                    "vomiting" ->
                        succeed SymptomVomiting

                    "umbilical-cord-redness" ->
                        succeed SymptomUmbilicalCordRedness

                    "stiff-neck-or-bulging-fontanelle" ->
                        succeed SymptomStiffNeckOrBulgingFontanelle

                    "severe-edema" ->
                        succeed SymptomSevereEdema

                    "palmoplantar-pallor" ->
                        succeed SymptomPalmoplantarPallor

                    "history-of-fever" ->
                        succeed SymptomHistoryOfFever

                    "baby-tires-quickly-when-feeding" ->
                        succeed SymptomBabyTiresQuicklyWhenFeeding

                    "coughing-or-tearing-while-feeding" ->
                        succeed SymptomCoughingOrTearingWhileFeeding

                    "rigid-muscles-or-jaw-clenching" ->
                        succeed SymptomRigidMusclesOrJawClenchingPreventingFeeding

                    "excessive-sweating-when-feeding" ->
                        succeed ExcessiveSweatingWhenFeeding

                    "none" ->
                        succeed NoWellChildSymptoms

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized WellChildSymptom"
            )


decodeWellChildECD : Decoder WellChildECD
decodeWellChildECD =
    decodeEverySet decodeECDSign
        |> field "ecd_signs"
        |> decodeWellChildMeasurement


decodeECDSign : Decoder ECDSign
decodeECDSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "follow-mothers-eyes" ->
                        succeed FollowMothersEyes

                    "move-arms-and-legs" ->
                        succeed MoveArmsAndLegs

                    "raise-hands-up" ->
                        succeed RaiseHandsUp

                    "smile" ->
                        succeed Smile

                    "roll-sideways" ->
                        succeed RollSideways

                    "bring-hands-to-mouth" ->
                        succeed BringHandsToMouth

                    "hold-head-without-support" ->
                        succeed HoldHeadWithoutSupport

                    "hold-and-shake-toys" ->
                        succeed HoldAndShakeToys

                    "react-to-sudden-sounds" ->
                        succeed ReactToSuddenSounds

                    "use-consonant-sounds" ->
                        succeed UseConsonantSounds

                    "respond-to-sound-with-sound" ->
                        succeed RespondToSoundWithSound

                    "turn-head-when-called" ->
                        succeed TurnHeadWhenCalled

                    "sit-without-support" ->
                        succeed SitWithoutSupport

                    "smile-back" ->
                        succeed SmileBack

                    "roll-tummy-to-back" ->
                        succeed RollTummyToBack

                    "reach-for-toys" ->
                        succeed ReachForToys

                    "use-simple-gestures" ->
                        succeed UseSimpleGestures

                    "stand-on-their-own" ->
                        succeed StandOnTheirOwn

                    "copy-during-play" ->
                        succeed CopyDuringPlay

                    "say-mama-dada" ->
                        succeed SayMamaDada

                    "can-hold-small-objects" ->
                        succeed CanHoldSmallObjects

                    "looks-when-pointed-at" ->
                        succeed LooksWhenPointedAt

                    "use-single-words" ->
                        succeed UseSingleWords

                    "walk-without-help" ->
                        succeed WalkWithoutHelp

                    "play-pretend" ->
                        succeed PlayPretend

                    "point-to-things-of-interest" ->
                        succeed PointToThingsOfInterest

                    "use-short-phrases" ->
                        succeed UseShortPhrases

                    "interested-in-other-children" ->
                        succeed InterestedInOtherChildren

                    "follow-simple-instructions" ->
                        succeed FollowSimpleInstructions

                    "kick-ball" ->
                        succeed KickBall

                    "point-at-named-objects" ->
                        succeed PointAtNamedObjects

                    "dress-themselves" ->
                        succeed DressThemselves

                    "wash-hands-go-to-toiled" ->
                        succeed WashHandsGoToToiled

                    "knows-colors-and-numbers" ->
                        succeed KnowsColorsAndNumbers

                    "use-medium-phrases" ->
                        succeed UseMediumPhrases

                    "play-make-believe" ->
                        succeed PlayMakeBelieve

                    "follow-three-step-instructions" ->
                        succeed FollowThreeStepInstructions

                    "stand-on-one-foot-five-seconds" ->
                        succeed StandOnOneFootFiveSeconds

                    "use-long-phrases" ->
                        succeed UseLongPhrases

                    "share-with-other-children" ->
                        succeed ShareWithOtherChildren

                    "count-to-ten" ->
                        succeed CountToTen

                    "none" ->
                        succeed NoECDSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ECDSign"
            )


decodeWellChildHeadCircumference : Decoder WellChildHeadCircumference
decodeWellChildHeadCircumference =
    decodeWellChildMeasurement decodeHeadCircumferenceValue


decodeHeadCircumferenceValue : Decoder HeadCircumferenceValue
decodeHeadCircumferenceValue =
    succeed HeadCircumferenceValue
        |> required "head_circumference" (map HeadCircumferenceInCm decodeFloat)
        |> required "measurement_notes" (decodeEverySet decodeMeasurementNote)


decodeMeasurementNote : Decoder MeasurementNote
decodeMeasurementNote =
    string
        |> andThen
            (\sign ->
                case sign of
                    "not-taken" ->
                        succeed NoteNotTaken

                    "none" ->
                        succeed NoMeasurementNotes

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized MeasurementNote"
            )


decodeVaccineDose : Decoder VaccineDose
decodeVaccineDose =
    string
        |> andThen
            (\dose ->
                vaccineDoseFromString dose
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| dose ++ " is not a recognized VaccineDose")
            )


decodeWellChildAlbendazole : Decoder WellChildAlbendazole
decodeWellChildAlbendazole =
    decodeWellChildMeasurement decodeAdministrationNoteField


decodeWellChildMebendezole : Decoder WellChildMebendezole
decodeWellChildMebendezole =
    decodeWellChildMeasurement decodeAdministrationNoteField


decodeWellChildVitaminA : Decoder WellChildVitaminA
decodeWellChildVitaminA =
    decodeWellChildMeasurement decodeAdministrationNoteField


decodeAdministrationNoteField : Decoder AdministrationNote
decodeAdministrationNoteField =
    decodeAdministrationNote
        |> field "administration_note"


decodeAdministrationNote : Decoder AdministrationNote
decodeAdministrationNote =
    string
        |> andThen
            (\note ->
                administrationNoteFromString note
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| note ++ " is not a recognized AdministrationNote")
            )


decodeWellChildPregnancySummary : Decoder WellChildPregnancySummary
decodeWellChildPregnancySummary =
    decodeWellChildMeasurement decodePregnancySummaryValue


decodePregnancySummaryValue : Decoder PregnancySummaryValue
decodePregnancySummaryValue =
    succeed PregnancySummaryValue
        |> required "expected_date_concluded" Gizra.NominalDate.decodeYYYYMMDD
        |> required "delivery_complications" (decodeEverySet decodeDeliveryComplication)
        |> required "pregnancy_summary_signs"
            (decodeWithFallback
                (EverySet.singleton NoPregnancySummarySigns)
                (decodeEverySet decodePregnancySummarySign)
            )
        |> optional "apgar_one_min" (nullable decodeFloat) Nothing
        |> optional "apgar_five_min" (nullable decodeFloat) Nothing
        |> optional "weight" (nullable (map WeightInGrm decodeFloat)) Nothing
        |> optional "height" (nullable (map HeightInCm decodeFloat)) Nothing
        |> required "birth_defects"
            (decodeWithFallback
                (EverySet.singleton NoBirthDefects)
                (decodeEverySet decodeBirthDefect)
            )


decodeDeliveryComplication : Decoder DeliveryComplication
decodeDeliveryComplication =
    string
        |> andThen
            (\complication ->
                case complication of
                    "gestational-diabetes" ->
                        succeed ComplicationGestationalDiabetes

                    "emergency-c-section" ->
                        succeed ComplicationEmergencyCSection

                    "preclampsia" ->
                        succeed ComplicationPreclampsia

                    "maternal-hemmorhage" ->
                        succeed ComplicationMaternalHemmorhage

                    "hiv" ->
                        succeed ComplicationHiv

                    "maternal-death" ->
                        succeed ComplicationMaternalDeath

                    "other" ->
                        succeed ComplicationOther

                    "none" ->
                        succeed NoDeliveryComplications

                    _ ->
                        fail <| complication ++ " is not a recognized DeliveryComplication"
            )


decodePregnancySummarySign : Decoder PregnancySummarySign
decodePregnancySummarySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "apgar-scores" ->
                        succeed ApgarScores

                    "birth-length" ->
                        succeed BirthLength

                    "none" ->
                        succeed NoPregnancySummarySigns

                    _ ->
                        fail <| sign ++ " is not a recognized PregnancySummarySign"
            )


decodeBirthDefect : Decoder BirthDefect
decodeBirthDefect =
    string
        |> andThen
            (\defect ->
                case defect of
                    "birth-injury" ->
                        succeed DefectBirthInjury

                    "cleft-lip-with-cleft-palate" ->
                        succeed DefectCleftLipWithCleftPalate

                    "cleft-palate" ->
                        succeed DefectCleftPalate

                    "club-foot" ->
                        succeed DefectClubFoot

                    "macrocephaly" ->
                        succeed DefectMacrocephaly

                    "gastroschisis" ->
                        succeed DefectGastroschisis

                    "hearing-loss" ->
                        succeed DefectHearingLoss

                    "undescended-testes" ->
                        succeed DefectUndescendedTestes

                    "hypospadias" ->
                        succeed DefectHypospadias

                    "inguinal-hernia" ->
                        succeed DefectInguinalHernia

                    "microcephaly" ->
                        succeed DefectMicrocephaly

                    "neural-tubes" ->
                        succeed DefectNeuralTubes

                    "down-syndrome" ->
                        succeed DefectDownSyndrome

                    "congenital-heart" ->
                        succeed DefectCongenitalHeart

                    "ventrical-septal" ->
                        succeed DefectVentricalSeptal

                    "pulmonary-valve-atresia-and-stenosis" ->
                        succeed DefectPulmonaryValveAtresiaAndStenosis

                    "none" ->
                        succeed NoBirthDefects

                    _ ->
                        fail <| defect ++ " is not a recognized BirthDefect"
            )


decodeWellChildNextVisit : Decoder WellChildNextVisit
decodeWellChildNextVisit =
    decodeWellChildMeasurement decodeNextVisitValue


decodeNextVisitValue : Decoder NextVisitValue
decodeNextVisitValue =
    succeed NextVisitValue
        |> required "immunisation_date" (nullable Gizra.NominalDate.decodeYYYYMMDD)
        |> optional "asap_immunisation_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> required "pediatric_visit_date" (nullable Gizra.NominalDate.decodeYYYYMMDD)
        |> optional "date_concluded" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing


decodeWellChildBCGImmunisation : Decoder WellChildBCGImmunisation
decodeWellChildBCGImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildDTPImmunisation : Decoder WellChildDTPImmunisation
decodeWellChildDTPImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildDTPStandaloneImmunisation : Decoder WellChildDTPStandaloneImmunisation
decodeWellChildDTPStandaloneImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildHPVImmunisation : Decoder WellChildHPVImmunisation
decodeWellChildHPVImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildIPVImmunisation : Decoder WellChildIPVImmunisation
decodeWellChildIPVImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildMRImmunisation : Decoder WellChildMRImmunisation
decodeWellChildMRImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildOPVImmunisation : Decoder WellChildOPVImmunisation
decodeWellChildOPVImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildPCV13Immunisation : Decoder WellChildPCV13Immunisation
decodeWellChildPCV13Immunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeWellChildRotarixImmunisation : Decoder WellChildRotarixImmunisation
decodeWellChildRotarixImmunisation =
    decodeWellChildMeasurement decodeVaccinationValue


decodeVaccinationValue : Decoder VaccinationValue
decodeVaccinationValue =
    succeed VaccinationValue
        |> required "administered_doses" (decodeEverySet decodeVaccineDose)
        |> required "administration_dates" (decodeEverySet Gizra.NominalDate.decodeYYYYMMDD)
        |> required "administration_note" decodeAdministrationNote


decodePrenatalSymptomReview : Decoder PrenatalSymptomReview
decodePrenatalSymptomReview =
    decodePrenatalMeasurement decodePrenatalSymptomReviewValue


decodePrenatalSymptomReviewValue : Decoder PrenatalSymptomReviewValue
decodePrenatalSymptomReviewValue =
    succeed PrenatalSymptomReviewValue
        |> required "prenatal_symptoms" (decodeEverySet decodePrenatalSymptom)
        |> required "prenatal_symptom_questions" (decodeEverySet decodePrenatalSymptomQuestion)
        |> optional "flank_pain_sign" (nullable decodePrenatalFlankPainSign) Nothing


decodePrenatalSymptom : Decoder PrenatalSymptom
decodePrenatalSymptom =
    string
        |> andThen
            (\s ->
                prenatalSymptomFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized PrenatalSymptom")
            )


decodePrenatalSymptomQuestion : Decoder PrenatalSymptomQuestion
decodePrenatalSymptomQuestion =
    string
        |> andThen
            (\s ->
                prenatalSymptomQuestionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized PrenatalSymptomQuestion")
            )


decodePrenatalFlankPainSign : Decoder PrenatalFlankPainSign
decodePrenatalFlankPainSign =
    string
        |> andThen
            (\s ->
                prenatalFlankPainSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized PrenatalFlankPainSign")
            )


decodePrenatalOutsideCare : Decoder PrenatalOutsideCare
decodePrenatalOutsideCare =
    decodePrenatalMeasurement (decodeOutsideCareValue "prenatal_diagnoses" decodePrenatalDiagnosis)


decodeOutsideCareValue : String -> Decoder diagnosis -> Decoder (OutsideCareValue diagnosis)
decodeOutsideCareValue fieldName diagnosisDecoder =
    succeed OutsideCareValue
        |> required "outside_care_signs" (decodeEverySet decodeOutsideCareSign)
        |> optional fieldName (nullable (decodeEverySet diagnosisDecoder)) Nothing
        |> optional "outside_care_medications" (nullable (decodeEverySet decodeOutsideCareMedication)) Nothing


decodeOutsideCareSign : Decoder OutsideCareSign
decodeOutsideCareSign =
    string
        |> andThen
            (\s ->
                outsideCareSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized OutsideCareSign")
            )


decodeOutsideCareMedication : Decoder OutsideCareMedication
decodeOutsideCareMedication =
    string
        |> andThen
            (\s ->
                outsideCareMedicationFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized OutsideCareMedication")
            )


decodePrenatalAspirin : Decoder PrenatalAspirin
decodePrenatalAspirin =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalCalcium : Decoder PrenatalCalcium
decodePrenatalCalcium =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalFefol : Decoder PrenatalFefol
decodePrenatalFefol =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalFolate : Decoder PrenatalFolate
decodePrenatalFolate =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalIron : Decoder PrenatalIron
decodePrenatalIron =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalMMS : Decoder PrenatalMMS
decodePrenatalMMS =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodePrenatalMebendazole : Decoder PrenatalMebendazole
decodePrenatalMebendazole =
    decodePrenatalMeasurement decodeAdministrationNoteField


decodeNCDCoMorbidities : Decoder NCDCoMorbidities
decodeNCDCoMorbidities =
    decodeNCDMeasurement decodeNCDCoMorbiditiesValue


decodeNCDCoMorbiditiesValue : Decoder NCDCoMorbiditiesValue
decodeNCDCoMorbiditiesValue =
    field "comorbidities" (decodeEverySet decodeMedicalCondition)


decodeMedicalCondition : Decoder MedicalCondition
decodeMedicalCondition =
    string
        |> andThen
            (\s ->
                medicalConditionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized MedicalCondition")
            )


decodeNCDCoreExam : Decoder NCDCoreExam
decodeNCDCoreExam =
    decodeNCDMeasurement decodeCorePhysicalExamValue


decodeNCDCreatinineTest : Decoder NCDCreatinineTest
decodeNCDCreatinineTest =
    decodeNCDMeasurement decodeCreatinineTestValue


decodeCreatinineTestValue : Decoder CreatinineTestValue
decodeCreatinineTestValue =
    succeed CreatinineTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "creatinine_result" (nullable decodeFloat) Nothing
        |> optional "bun_result" (nullable decodeFloat) Nothing


decodeNCDDangerSigns : Decoder NCDDangerSigns
decodeNCDDangerSigns =
    decodeNCDMeasurement decodeNCDDangerSignsValue


decodeNCDDangerSignsValue : Decoder NCDDangerSignsValue
decodeNCDDangerSignsValue =
    field "ncd_danger_signs" (decodeEverySet decodeNCDDangerSign)


decodeNCDDangerSign : Decoder NCDDangerSign
decodeNCDDangerSign =
    string
        |> andThen
            (\s ->
                ncdDangerSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDDangerSign")
            )


decodeNCDFamilyHistory : Decoder NCDFamilyHistory
decodeNCDFamilyHistory =
    decodeNCDMeasurement decodeNCDFamilyHistoryValue


decodeNCDFamilyHistoryValue : Decoder NCDFamilyHistoryValue
decodeNCDFamilyHistoryValue =
    succeed NCDFamilyHistoryValue
        |> required "ncd_family_history_signs" (decodeEverySet decodeNCDFamilyHistorySign)
        |> optional "hypertension_predecessors" (nullable (decodeEverySet decodePredecessor)) Nothing
        |> optional "heart_problem_predecessors" (nullable (decodeEverySet decodePredecessor)) Nothing
        |> optional "diabetes_predecessors" (nullable (decodeEverySet decodePredecessor)) Nothing


decodeNCDFamilyHistorySign : Decoder NCDFamilyHistorySign
decodeNCDFamilyHistorySign =
    string
        |> andThen
            (\s ->
                ncdFamilyHistorySignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDFamilyHistorySign")
            )


decodePredecessor : Decoder Predecessor
decodePredecessor =
    string
        |> andThen
            (\s ->
                predecessorFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized Predecessor")
            )


decodeNCDFamilyPlanning : Decoder NCDFamilyPlanning
decodeNCDFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeNCDMeasurement


decodeNCDHealthEducation : Decoder NCDHealthEducation
decodeNCDHealthEducation =
    decodeNCDMeasurement decodeNCDHealthEducationValue


decodeNCDHealthEducationValue : Decoder NCDHealthEducationValue
decodeNCDHealthEducationValue =
    field "ncd_health_education_signs" (decodeEverySet decodeNCDHealthEducationSign)


decodeNCDHealthEducationSign : Decoder NCDHealthEducationSign
decodeNCDHealthEducationSign =
    string
        |> andThen
            (\s ->
                case s of
                    "hypertension" ->
                        succeed EducationHypertension

                    "none" ->
                        succeed NoNCDHealthEducationSigns

                    _ ->
                        fail <| s ++ " is not a recognized NCDHealthEducationSign"
            )


decodeNCDHIVTest : Decoder NCDHIVTest
decodeNCDHIVTest =
    decodeNCDMeasurement decodeHIVTestValue


decodeNCDLabsResults : Decoder NCDLabsResults
decodeNCDLabsResults =
    decodeNCDMeasurement decodeLabsResultsValue


decodeNCDLiverFunctionTest : Decoder NCDLiverFunctionTest
decodeNCDLiverFunctionTest =
    decodeNCDMeasurement decodeLiverFunctionTestValue


decodeLiverFunctionTestValue : Decoder LiverFunctionTestValue
decodeLiverFunctionTestValue =
    succeed LiverFunctionTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "alt_result" (nullable decodeFloat) Nothing
        |> optional "ast_result" (nullable decodeFloat) Nothing


decodeNCDMedicationDistribution : Decoder NCDMedicationDistribution
decodeNCDMedicationDistribution =
    decodeNCDMeasurement decodeNCDMedicationDistributionValue


decodeNCDMedicationDistributionValue : Decoder NCDMedicationDistributionValue
decodeNCDMedicationDistributionValue =
    succeed NCDMedicationDistributionValue
        |> required "recommended_treatment" (decodeEverySet decodeRecommendedTreatmentSign)
        |> required "ncd_guidance" (decodeEverySet decodeNCDGuidanceSign)


decodeNCDGuidanceSign : Decoder NCDGuidanceSign
decodeNCDGuidanceSign =
    string
        |> andThen
            (\s ->
                case s of
                    "return-1m" ->
                        succeed ReturnInOneMonth

                    "none" ->
                        succeed NoNCDGuidanceSigns

                    _ ->
                        fail <| s ++ " is not a recognized NCDGuidanceSign"
            )


decodeNCDMedicationHistory : Decoder NCDMedicationHistory
decodeNCDMedicationHistory =
    decodeNCDMeasurement decodeNCDMedicationHistoryValue


decodeNCDMedicationHistoryValue : Decoder NCDMedicationHistoryValue
decodeNCDMedicationHistoryValue =
    succeed NCDMedicationHistoryValue
        |> required "causing_hypertension" (decodeEverySet decodeMedicationCausingHypertension)
        |> required "treating_hypertension" (decodeEverySet decodeMedicationTreatingHypertension)
        |> required "treating_diabetes" (decodeEverySet decodeMedicationTreatingDiabetes)


decodeMedicationCausingHypertension : Decoder MedicationCausingHypertension
decodeMedicationCausingHypertension =
    string
        |> andThen
            (\s ->
                medicationCausingHypertensionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized MedicationCausingHypertension")
            )


decodeMedicationTreatingHypertension : Decoder MedicationTreatingHypertension
decodeMedicationTreatingHypertension =
    string
        |> andThen
            (\s ->
                medicationTreatingHypertensionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized MedicationTreatingHypertension")
            )


decodeMedicationTreatingDiabetes : Decoder MedicationTreatingDiabetes
decodeMedicationTreatingDiabetes =
    string
        |> andThen
            (\s ->
                medicationTreatingDiabetesFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized MedicationTreatingDiabetes")
            )


decodeNCDOutsideCare : Decoder NCDOutsideCare
decodeNCDOutsideCare =
    decodeNCDMeasurement (decodeOutsideCareValue "medical_conditions" decodeMedicalCondition)


decodeNCDPregnancyTest : Decoder NCDPregnancyTest
decodeNCDPregnancyTest =
    decodeNCDMeasurement decodePregnancyTestValue


decodePregnancyTestValue : Decoder PregnancyTestValue
decodePregnancyTestValue =
    succeed PregnancyTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_result" (nullable decodeTestResult) Nothing


decodeNCDRandomBloodSugarTest : Decoder NCDRandomBloodSugarTest
decodeNCDRandomBloodSugarTest =
    decodeNCDMeasurement decodeRandomBloodSugarTestValue


decodeNCDReferral : Decoder NCDReferral
decodeNCDReferral =
    decodeNCDMeasurement decodeReferralValue


decodeReferralValue : Decoder ReferralValue
decodeReferralValue =
    succeed ReferralValue
        |> required "referrals" (decodeEverySet decodeReferToFacilitySign)
        |> optional "reasons_for_non_referrals" (nullable (decodeEverySet decodeNonReferralSign)) Nothing


decodeNCDSocialHistory : Decoder NCDSocialHistory
decodeNCDSocialHistory =
    decodeNCDMeasurement decodeNCDSocialHistoryValue


decodeNCDSocialHistoryValue : Decoder NCDSocialHistoryValue
decodeNCDSocialHistoryValue =
    succeed NCDSocialHistoryValue
        |> required "ncd_social_history_signs" (decodeEverySet decodeNCDSocialHistorySign)
        |> required "food_group" decodeFoodGroup
        |> optional "beverages_per_week" (nullable decodeInt) Nothing
        |> optional "cigarettes_per_week" (nullable decodeInt) Nothing


decodeNCDSocialHistorySign : Decoder NCDSocialHistorySign
decodeNCDSocialHistorySign =
    string
        |> andThen
            (\s ->
                ncdSocialHistorySignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDSocialHistorySign")
            )


decodeFoodGroup : Decoder FoodGroup
decodeFoodGroup =
    string
        |> andThen
            (\s ->
                foodGroupFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized FoodGroup")
            )


decodeNCDSymptomReview : Decoder NCDSymptomReview
decodeNCDSymptomReview =
    decodeNCDMeasurement decodeNCDSymptomReviewValue


decodeNCDSymptomReviewValue : Decoder NCDSymptomReviewValue
decodeNCDSymptomReviewValue =
    succeed NCDSymptomReviewValue
        |> required "ncd_group1_symptoms" (decodeEverySet decodeNCDGroup1Symptom)
        |> required "ncd_group2_symptoms" (decodeEverySet decodeNCDGroup2Symptom)
        |> required "ncd_pain_symptoms" (decodeEverySet decodeNCDPainSymptom)


decodeNCDGroup1Symptom : Decoder NCDGroup1Symptom
decodeNCDGroup1Symptom =
    string
        |> andThen
            (\s ->
                ncdGroup1SymptomFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDGroup1Symptom")
            )


decodeNCDGroup2Symptom : Decoder NCDGroup2Symptom
decodeNCDGroup2Symptom =
    string
        |> andThen
            (\s ->
                ncdGroup2SymptomFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDGroup2Symptom")
            )


decodeNCDPainSymptom : Decoder NCDPainSymptom
decodeNCDPainSymptom =
    string
        |> andThen
            (\s ->
                ncdPainSymptomFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDPainSymptom")
            )


decodeNCDUrineDipstickTest : Decoder NCDUrineDipstickTest
decodeNCDUrineDipstickTest =
    decodeNCDMeasurement decodeUrineDipstickTestValue


decodeNCDVitals : Decoder NCDVitals
decodeNCDVitals =
    decodeNCDMeasurement decodeVitalsValue


decodeNCDAValue : Decoder NCDAValue
decodeNCDAValue =
    succeed NCDAValue
        |> required "ncda_signs" (decodeEverySet decodeNCDASign)
        |> optional "birth_weight" (nullable (map WeightInGrm decodeFloat)) Nothing
        |> required "anc_visits_dates" (decodeEverySet Gizra.NominalDate.decodeYYYYMMDD)
        |> optional "receive_option" (nullable decodeReceiveOption) Nothing
        |> optional "stunting_level" (nullable decodeStuntingLevel) Nothing
        |> optional "weight" (nullable (map WeightInKg decodeFloat)) Nothing
        |> optional "muac" (nullable (map MuacInCm decodeFloat)) Nothing


decodeNCDASign : Decoder NCDASign
decodeNCDASign =
    string
        |> andThen
            (\s ->
                ncdaSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized NCDASign")
            )


decodeReceiveOption : Decoder ReceiveOption
decodeReceiveOption =
    string
        |> andThen
            (\s ->
                receiveOptionFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized ReceiveOption")
            )


decodeStuntingLevel : Decoder StuntingLevel
decodeStuntingLevel =
    string
        |> andThen
            (\s ->
                stuntingLevelFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized StuntingLevel")
            )


decodeGroupNCDA : Decoder GroupNCDA
decodeGroupNCDA =
    decodeGroupMeasurement decodeNCDAValue


decodeNutritionNCDA : Decoder NutritionNCDA
decodeNutritionNCDA =
    decodeNutritionMeasurement decodeNCDAValue


decodeWellChildNCDA : Decoder WellChildNCDA
decodeWellChildNCDA =
    decodeWellChildMeasurement decodeNCDAValue


decodeChildScoreboardNCDA : Decoder ChildScoreboardNCDA
decodeChildScoreboardNCDA =
    decodeChildScoreboardMeasurement decodeNCDAValue


decodeNCDLipidPanelTest : Decoder NCDLipidPanelTest
decodeNCDLipidPanelTest =
    decodeNCDMeasurement decodeLipidPanelTestValue


decodeLipidPanelTestValue : Decoder LipidPanelTestValue
decodeLipidPanelTestValue =
    succeed LipidPanelTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "unit_of_measurement" (nullable decodeUnitOfMeasurement) Nothing
        |> optional "total_cholesterol" (nullable decodeFloat) Nothing
        |> optional "ldl_cholesterol" (nullable decodeFloat) Nothing
        |> optional "hdl_cholesterol" (nullable decodeFloat) Nothing
        |> optional "triglycerides" (nullable decodeFloat) Nothing


decodeUnitOfMeasurement : Decoder UnitOfMeasurement
decodeUnitOfMeasurement =
    string
        |> andThen
            (\s ->
                unitOfMeasurementFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is not a recognized UnitOfMeasurement")
            )


decodeNCDHbA1cTest : Decoder NCDHbA1cTest
decodeNCDHbA1cTest =
    decodeNCDMeasurement decodeHbA1cTestValue


decodeHbA1cTestValue : Decoder HbA1cTestValue
decodeHbA1cTestValue =
    succeed HbA1cTestValue
        |> required "test_execution_note" decodeTestExecutionNote
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "hba1c_result" (nullable decodeFloat) Nothing


decodePregnancyByNewborn : Decoder (Maybe ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
decodePregnancyByNewborn =
    oneOf
        [ at [ "individual_participant" ] (decodeHead decodeIndividualEncounterParticipant)

        -- Seems there're no individual participants for the pregnancy, so
        -- we can determine that pregnancy was not tracked on E-Heza.
        , succeed Nothing
        ]


decodeChildScoreboardBCGImmunisation : Decoder ChildScoreboardBCGImmunisation
decodeChildScoreboardBCGImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardDTPImmunisation : Decoder ChildScoreboardDTPImmunisation
decodeChildScoreboardDTPImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardDTPStandaloneImmunisation : Decoder ChildScoreboardDTPStandaloneImmunisation
decodeChildScoreboardDTPStandaloneImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardIPVImmunisation : Decoder ChildScoreboardIPVImmunisation
decodeChildScoreboardIPVImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardMRImmunisation : Decoder ChildScoreboardMRImmunisation
decodeChildScoreboardMRImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardOPVImmunisation : Decoder ChildScoreboardOPVImmunisation
decodeChildScoreboardOPVImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardPCV13Immunisation : Decoder ChildScoreboardPCV13Immunisation
decodeChildScoreboardPCV13Immunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeChildScoreboardRotarixImmunisation : Decoder ChildScoreboardRotarixImmunisation
decodeChildScoreboardRotarixImmunisation =
    decodeChildScoreboardMeasurement decodeVaccinationValue


decodeWellChildFeeding : Decoder WellChildFeeding
decodeWellChildFeeding =
    decodeWellChildMeasurement decodeNutritionFeedingValue


decodeWellChildHygiene : Decoder WellChildHygiene
decodeWellChildHygiene =
    decodeWellChildMeasurement decodeNutritionHygieneValue


decodeWellChildFoodSecurity : Decoder WellChildFoodSecurity
decodeWellChildFoodSecurity =
    decodeWellChildMeasurement decodeNutritionFoodSecurityValue


decodeWellChildCaring : Decoder WellChildCaring
decodeWellChildCaring =
    decodeWellChildMeasurement decodeNutritionCaringValue


decodeTuberculosisDiagnostics : Decoder TuberculosisDiagnostics
decodeTuberculosisDiagnostics =
    decodeTuberculosisMeasurement decodeTuberculosisDiagnosticsValue


decodeTuberculosisDiagnosticsValue : Decoder TuberculosisDiagnosticsValue
decodeTuberculosisDiagnosticsValue =
    field "tuberculosis_diagnosis" decodeTuberculosisDiagnosis


decodeTuberculosisDiagnosis : Decoder TuberculosisDiagnosis
decodeTuberculosisDiagnosis =
    string
        |> andThen
            (\result ->
                tuberculosisDiagnosisFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized TuberculosisDiagnosis" |> fail)
            )


decodeTuberculosisDOT : Decoder TuberculosisDOT
decodeTuberculosisDOT =
    decodeTuberculosisMeasurement decodeTuberculosisDOTValue


decodeTuberculosisDOTValue : Decoder TuberculosisDOTValue
decodeTuberculosisDOTValue =
    succeed TuberculosisDOTValue
        |> required "dot_signs" decodeTuberculosisDOTSign
        |> required "dot_meds_distribution_sign" decodeTuberculosisDOTSign


decodeTuberculosisDOTSign : Decoder TuberculosisDOTSign
decodeTuberculosisDOTSign =
    string
        |> andThen
            (\result ->
                tuberculosisDOTSignFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized TuberculosisDOTSign" |> fail)
            )


decodeTuberculosisFollowUp : Decoder TuberculosisFollowUp
decodeTuberculosisFollowUp =
    decodeTuberculosisMeasurement decodeFollowUpValue


decodeFollowUpValue : Decoder FollowUpValue
decodeFollowUpValue =
    succeed FollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> optional "date_concluded" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing


decodeTuberculosisHealthEducation : Decoder TuberculosisHealthEducation
decodeTuberculosisHealthEducation =
    decodeTuberculosisMeasurement decodeTuberculosisHealthEducationValue


decodeTuberculosisHealthEducationValue : Decoder TuberculosisHealthEducationValue
decodeTuberculosisHealthEducationValue =
    decodeEverySet decodeTuberculosisHealthEducationSign
        |> field "tb_health_education_signs"


decodeTuberculosisHealthEducationSign : Decoder TuberculosisHealthEducationSign
decodeTuberculosisHealthEducationSign =
    string
        |> andThen
            (\result ->
                tuberculosisHealthEducationSignFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized TuberculosisHealthEducationSign" |> fail)
            )


decodeTuberculosisMedication : Decoder TuberculosisMedication
decodeTuberculosisMedication =
    decodeTuberculosisMeasurement decodeTuberculosisMedicationValue


decodeTuberculosisMedicationValue : Decoder TuberculosisMedicationValue
decodeTuberculosisMedicationValue =
    decodeEverySet decodeTuberculosisPrescribedMedication
        |> field "prescribed_tb_medications"


decodeTuberculosisPrescribedMedication : Decoder TuberculosisPrescribedMedication
decodeTuberculosisPrescribedMedication =
    string
        |> andThen
            (\result ->
                tuberculosisPrescribedMedicationFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized TuberculosisPrescribedMedication" |> fail)
            )


decodeTuberculosisReferral : Decoder TuberculosisReferral
decodeTuberculosisReferral =
    decodeTuberculosisMeasurement decodeSendToHCValue


decodeTuberculosisSymptomReview : Decoder TuberculosisSymptomReview
decodeTuberculosisSymptomReview =
    decodeTuberculosisMeasurement decodeTuberculosisSymptomReviewValue


decodeTuberculosisSymptomReviewValue : Decoder TuberculosisSymptomReviewValue
decodeTuberculosisSymptomReviewValue =
    decodeEverySet decodeTuberculosisSymptom
        |> field "tuberculosis_symptoms"


decodeTuberculosisSymptom : Decoder TuberculosisSymptom
decodeTuberculosisSymptom =
    string
        |> andThen
            (\result ->
                tuberculosisSymptomFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized TuberculosisSymptom" |> fail)
            )


decodeTuberculosisTreatmentReview : Decoder TuberculosisTreatmentReview
decodeTuberculosisTreatmentReview =
    decodeTuberculosisMeasurement decodeTreatmentOngoingValue


decodeHIVDiagnostics : Decoder HIVDiagnostics
decodeHIVDiagnostics =
    decodeHIVMeasurement decodeHIVDiagnosticsValue


decodeHIVDiagnosticsValue : Decoder HIVDiagnosticsValue
decodeHIVDiagnosticsValue =
    succeed HIVDiagnosticsValue
        |> required "hiv_diagnosis_signs" (decodeEverySet decodeHIVDiagnosisSign)
        |> optional "positive_result_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "test_result" (nullable decodeTestResult) Nothing


decodeHIVDiagnosisSign : Decoder HIVDiagnosisSign
decodeHIVDiagnosisSign =
    string
        |> andThen
            (\result ->
                hivDiagnosisSignFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized HIVDiagnosisSign" |> fail)
            )


decodeHIVFollowUp : Decoder HIVFollowUp
decodeHIVFollowUp =
    decodeHIVMeasurement decodeFollowUpValue


decodeHIVHealthEducation : Decoder HIVHealthEducation
decodeHIVHealthEducation =
    decodeHIVMeasurement decodeHIVHealthEducationValue


decodeHIVHealthEducationValue : Decoder HIVHealthEducationValue
decodeHIVHealthEducationValue =
    decodeEverySet decodeHIVHealthEducationSign
        |> field "hiv_health_education_signs"


decodeHIVHealthEducationSign : Decoder HIVHealthEducationSign
decodeHIVHealthEducationSign =
    string
        |> andThen
            (\result ->
                hivHealthEducationSignFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized HIVHealthEducationSign" |> fail)
            )


decodeHIVMedication : Decoder HIVMedication
decodeHIVMedication =
    decodeHIVMeasurement decodeHIVMedicationValue


decodeHIVMedicationValue : Decoder HIVMedicationValue
decodeHIVMedicationValue =
    decodeEverySet decodeHIVPrescribedMedication
        |> field "prescribed_hiv_medications"


decodeHIVPrescribedMedication : Decoder HIVPrescribedMedication
decodeHIVPrescribedMedication =
    string
        |> andThen
            (\result ->
                hivPrescribedMedicationFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized HIVPrescribedMedication" |> fail)
            )


decodeHIVReferral : Decoder HIVReferral
decodeHIVReferral =
    decodeHIVMeasurement decodeSendToHCValue


decodeHIVSymptomReview : Decoder HIVSymptomReview
decodeHIVSymptomReview =
    decodeHIVMeasurement decodeHIVSymptomReviewValue


decodeHIVSymptomReviewValue : Decoder HIVSymptomReviewValue
decodeHIVSymptomReviewValue =
    decodeEverySet decodeHIVSymptom
        |> field "hiv_symptoms"


decodeHIVSymptom : Decoder HIVSymptom
decodeHIVSymptom =
    string
        |> andThen
            (\result ->
                hivSymptomFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized HIVSymptom" |> fail)
            )


decodeHIVTreatmentReview : Decoder HIVTreatmentReview
decodeHIVTreatmentReview =
    decodeHIVMeasurement decodeTreatmentOngoingValue
