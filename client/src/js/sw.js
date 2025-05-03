/*
 * This contains some common code for our service worker script. So,
 * it is included by sw-precache first, in a global context.
 */
'use strict';

// Various constants that get used in multiple places.
var syncTag = 'sync';

var configCache = 'config';
var configUrlRegex = /\/sw\/config/;

var credentialsUrlRegex = /\/sw\/config\/device$/;
var credentialsUrl = '/sw/config/device';

var deviceUuidUrl = '/sw/config/device-uuid';

var photosDownloadCache = "photos";
var photosUploadCache = "photos-upload";
var screenshotsUploadCache = "screenshots-upload";

var photosDownloadUrlRegex = /\/system\/files\//;
var photosUploadUrlRegex = /\/cache-upload\/images/;
var screenshotsUploadUrlRegex = /\/cache-upload\/screenshots/;

/**
 * The DB version on the backend.
 *
 * We use this constant to verify that we work with most updated
 * DB at nodes.js.
 *
 * @type {number}
 */
var dbVerno = 30;

// All those entities are the entities we're going to get from the backend.
// They should also be mapped in SyncManager.Model.BackendGeneralEntity (for
// General entities), or SyncManager.Model.BackendAuthorityEntity (for Authority
// entities).
var tableForType = {
    acute_findings: 'shards',
    acute_illness_core_exam: 'shards',
    acute_illness_contacts_tracing: 'shards',
    acute_illness_danger_signs: 'shards',
    acute_illness_encounter: 'shards',
    acute_illness_follow_up: 'shards',
    acute_illness_muac: 'shards',
    acute_illness_nutrition: 'shards',
    acute_illness_trace_contact: 'shards',
    acute_illness_vitals: 'shards',
    appointment_confirmation: 'shards',
    attendance: 'shards',
    breast_exam: 'shards',
    birth_plan: 'shards',
    call_114: 'shards',
    catchment_area: 'nodes',
    child_fbf: 'shards',
    child_scoreboard_encounter: 'shards',
    child_scoreboard_bcg_iz: 'shards',
    child_scoreboard_dtp_iz: 'shards',
    child_scoreboard_dtp_sa_iz: 'shards',
    child_scoreboard_ipv_iz: 'shards',
    child_scoreboard_mr_iz: 'shards',
    child_scoreboard_ncda: 'shards',
    child_scoreboard_opv_iz: 'shards',
    child_scoreboard_pcv13_iz: 'shards',
    child_scoreboard_rotarix_iz: 'shards',
    clinic: 'shards',
    contributing_factors: 'shards',
    counseling_schedule: 'nodes',
    counseling_session: 'shards',
    counseling_topic: 'nodes',
    core_physical_exam: 'shards',
    covid_testing: 'shards',
    danger_signs: 'shards',
    education_session: 'shards',
    exposure: 'shards',
    family_planning: 'shards',
    follow_up: 'shards',
    group_health_education: 'shards',
    group_ncda: 'shards',
    group_send_to_hc: 'shards',
    hc_contact: 'shards',
    health_center: 'nodes',
    health_education: 'shards',
    height: 'shards',
    hiv_diagnostics: 'shards',
    hiv_dot: 'shards',
    hiv_encounter: 'shards',
    hiv_follow_up: 'shards',
    hiv_health_education: 'shards',
    hiv_medication: 'shards',
    hiv_referral: 'shards',
    hiv_symptom_review: 'shards',
    hiv_treatment_review: 'shards',
    home_visit_encounter: 'shards',
    individual_participant: 'shards',
    isolation: 'shards',
    lactation: 'shards',
    last_menstrual_period: 'shards',
    malaria_testing: 'shards',
    medical_history: 'shards',
    medication: 'shards',
    medication_distribution: 'shards',
    mother_fbf: 'shards',
    muac: 'shards',
    ncd_co_morbidities: 'shards',
    ncd_core_exam: 'shards',
    ncd_creatinine_test: 'shards',
    ncd_danger_signs: 'shards',
    ncd_encounter: 'shards',
    ncd_family_history: 'shards',
    ncd_family_planning: 'shards',
    ncd_hba1c_test: 'shards',
    ncd_health_education: 'shards',
    ncd_hiv_test: 'shards',
    ncd_labs_results: 'shards',
    ncd_lipid_panel_test: 'shards',
    ncd_liver_function_test : 'shards',
    ncd_medication_distribution: 'shards',
    ncd_medication_history: 'shards',
    ncd_outside_care: 'shards',
    ncd_pregnancy_test: 'shards',
    ncd_random_blood_sugar_test: 'shards',
    ncd_referral: 'shards',
    ncd_social_history: 'shards',
    ncd_symptom_review: 'shards',
    ncd_urine_dipstick_test: 'shards',
    ncd_vitals: 'shards',
    nurse: 'nodes',
    nutrition: 'shards',
    nutrition_caring: 'shards',
    nutrition_contributing_factors: 'shards',
    nutrition_feeding: 'shards',
    nutrition_follow_up: 'shards',
    nutrition_food_security: 'shards',
    nutrition_encounter: 'shards',
    nutrition_health_education: 'shards',
    nutrition_height: 'shards',
    nutrition_hygiene: 'shards',
    nutrition_muac: 'shards',
    nutrition_ncda: 'shards',
    nutrition_nutrition: 'shards',
    nutrition_photo: 'shards',
    nutrition_send_to_hc: 'shards',
    nutrition_weight: 'shards',
    obstetric_history: 'shards',
    obstetric_history_step2: 'shards',
    obstetrical_exam: 'shards',
    participant_consent: 'shards',
    participant_form: 'nodes',
    person: 'shards',
    photo: 'shards',
    pregnancy_testing: 'shards',
    pmtct_participant: 'shards',
    prenatal_blood_gprs_test: 'shards',
    prenatal_breastfeeding: 'shards',
    prenatal_encounter: 'shards',
    prenatal_family_planning: 'shards',
    prenatal_follow_up: 'shards',
    prenatal_gu_exam: 'shards',
    prenatal_health_education: 'shards',
    prenatal_hemoglobin_test: 'shards',
    prenatal_hepatitis_b_test: 'shards',
    prenatal_hiv_test: 'shards',
    prenatal_hiv_pcr_test: 'shards',
    prenatal_labs_results: 'shards',
    prenatal_malaria_test: 'shards',
    prenatal_medication_distribution: 'shards',
    prenatal_mental_health: 'shards',
    prenatal_nutrition: 'shards',
    prenatal_outside_care: 'shards',
    prenatal_partner_hiv_test: 'shards',
    prenatal_photo: 'shards',
    prenatal_random_blood_sugar_test: 'shards',
    prenatal_send_to_hc: 'shards',
    prenatal_speciality_care: 'shards',
    prenatal_symptom_review: 'shards',
    prenatal_syphilis_test: 'shards',
    prenatal_tetanus_immunisation: 'shards',
    prenatal_urine_dipstick_test: 'shards',
    relationship: 'shards',
    resilience_survey: 'nodes',
    resource: 'shards',
    send_to_hc: 'shards',
    session: 'shards',
    social_history: 'shards',
    stock_update: 'shards',
    syncmetadata: 'syncMetadata',
    symptoms_general: 'shards',
    symptoms_gi: 'shards',
    symptoms_respiratory: 'shards',
    travel_history: 'shards',
    treatment_history: 'shards',
    treatment_ongoing: 'shards',
    tuberculosis_diagnostics: 'shards',
    tuberculosis_dot: 'shards',
    tuberculosis_encounter: 'shards',
    tuberculosis_follow_up: 'shards',
    tuberculosis_health_education: 'shards',
    tuberculosis_medication: 'shards',
    tuberculosis_referral: 'shards',
    tuberculosis_symptom_review: 'shards',
    tuberculosis_treatment_review: 'shards',
    village: 'nodes',
    vitals: 'shards',
    weight: 'shards',
    well_child_albendazole: 'shards',
    well_child_bcg_immunisation: 'shards',
    well_child_caring: 'shards',
    well_child_contributing_factors: 'shards',
    well_child_dtp_immunisation: 'shards',
    well_child_dtp_sa_immunisation: 'shards',
    well_child_ecd: 'shards',
    well_child_encounter: 'shards',
    well_child_feeding: 'shards',
    well_child_follow_up: 'shards',
    well_child_food_security: 'shards',
    well_child_head_circumference: 'shards',
    well_child_health_education: 'shards',
    well_child_hygiene: 'shards',
    well_child_height: 'shards',
    well_child_hpv_immunisation: 'shards',
    well_child_ipv_immunisation: 'shards',
    well_child_mebendezole: 'shards',
    well_child_mr_immunisation: 'shards',
    well_child_muac: 'shards',
    well_child_ncda: 'shards',
    well_child_next_visit: 'shards',
    well_child_nutrition: 'shards',
    well_child_opv_immunisation: 'shards',
    well_child_pcv13_immunisation: 'shards',
    well_child_photo: 'shards',
    well_child_pregnancy_summary: 'shards',
    well_child_rotarix_immunisation: 'shards',
    well_child_send_to_hc: 'shards',
    well_child_symptoms_review: 'shards',
    well_child_vitamin_a: 'shards',
    well_child_vitals: 'shards',
    well_child_weight: 'shards'
};

function sendRevisions (revisions) {
    var message = {
        tag: 'NewRevisions',
        data: revisions
    };

    return self.clients.matchAll().then(function (clients) {

        clients.forEach(function (client) {
            client.postMessage(message);
        });

        return Promise.resolve();
    });
}

function gatherWords (text) {
  // Split on spaces, and remove blanks from result.
  return (text || '').split(/\s+/).flatMap(function (word) {
    if (word) {
      return [word.toLowerCase()];
    } else {
      return [];
    }
  });
}

// This is meant for the end of a promise chain.
function sendErrorResponses (err) {
    if (err instanceof Response) {
        return Promise.resolve(err);
    } else {
        return Promise.reject(err);
    }
}

function databaseError (err) {
    var response = new Response(JSON.stringify(err), {
        status: 500,
        statusText: 'Database Error'
    });

    return Promise.reject(response);
}
