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

var photosDownloadUrlRegex = /\/system\/files\//;
var photosUploadUrlRegex = /\/cache-upload\/images/;

/**
 * The DB version on the backend.
 *
 * We use this constant to verify that we work with most updated
 * DB at nodes.js.
 *
 * @type {number}
 */
var dbVerno = 16;

// All those entities are the entities we're going to get from the backend.
// They should also be mapped in SyncManager.Model.BackendGeneralEntity (for
// General entities), or SyncManager.Model.BackendAuthorityEntity (for Authority
// entities).
var tableForType = {
    acute_findings: 'shards',
    acute_illness_danger_signs: 'shards',
    acute_illness_encounter: 'shards',
    acute_illness_follow_up: 'shards',
    acute_illness_muac: 'shards',
    acute_illness_nutrition: 'shards',
    acute_illness_vitals: 'shards',
    attendance: 'shards',
    breast_exam: 'shards',
    birth_plan: 'shards',
    call_114: 'shards',
    catchment_area: 'nodes',
    child_fbf: 'shards',
    clinic: 'shards',
    contributing_factors: 'shards',
    counseling_schedule: 'nodes',
    counseling_session: 'shards',
    counseling_topic: 'nodes',
    core_physical_exam: 'shards',
    danger_signs: 'shards',
    exposure: 'shards',
    family_planning: 'shards',
    follow_up: 'shards',
    group_health_education: 'shards',
    group_send_to_hc: 'shards',
    hc_contact: 'shards',
    health_center: 'nodes',
    health_education: 'shards',
    height: 'shards',
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
    prenatal_health_education: 'shards',
    prenatal_photo: 'shards',
    pmtct_participant: 'shards',
    prenatal_family_planning: 'shards',
    prenatal_nutrition: 'shards',
    prenatal_encounter: 'shards',
    prenatal_follow_up: 'shards',
    prenatal_send_to_hc: 'shards',
    prenatal_appointment_confirmation: 'shards',
    relationship: 'shards',
    resource: 'shards',
    send_to_hc: 'shards',
    session: 'shards',
    social_history: 'shards',
    syncmetadata: 'syncMetadata',
    symptoms_general: 'shards',
    symptoms_gi: 'shards',
    symptoms_respiratory: 'shards',
    travel_history: 'shards',
    treatment_history: 'shards',
    treatment_ongoing: 'shards',
    village: 'nodes',
    vitals: 'shards',
    weight: 'shards'
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
