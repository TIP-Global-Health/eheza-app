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


// A UUID which represents the "shard" which is our general data that
// all devices get. (That is, unsharded data).
var nodesUuid = '78cf21d1-b3f4-496a-b312-d8ae73041f09';

// All those entities are the entities we're going to get from the backend.
// They should also be mapped in SyncManager.Model.BackendGeneralEntity (for
// General entities), or SyncManager.Model.BackendAuthorityEntity (for Authority
// entities).
var tableForType = {
    acute_findings: 'shards',
    acute_illness_encounter: 'shards',
    acute_illness_vitals: 'shards',
    attendance: 'shards',
    breast_exam: 'shards',
    catchment_area: 'nodes',
    child_fbf: 'shards',
    clinic: 'shards',
    counseling_schedule: 'nodes',
    counseling_session: 'shards',
    counseling_topic: 'nodes',
    core_physical_exam: 'shards',
    danger_signs: 'shards',
    exposure: 'shards',
    family_planning: 'shards',
    hc_contact: 'shards',
    health_center: 'nodes',
    height: 'shards',
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
    nutrition_encounter: 'shards',
    nutrition_height: 'shards',
    nutrition_muac: 'shards',
    nutrition_nutrition: 'shards',
    nutrition_photo: 'shards',
    nutrition_weight: 'shards',
    obstetric_history: 'shards',
    obstetric_history_step2: 'shards',
    obstetrical_exam: 'shards',
    participant_consent: 'shards',
    participant_form: 'nodes',
    person: 'shards',
    photo: 'shards',
    prenatal_photo: 'shards',
    pmtct_participant: 'shards',
    prenatal_family_planning: 'shards',
    prenatal_nutrition: 'shards',
    prenatal_encounter: 'shards',
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
    village: 'nodes',
    vitals: 'shards',
    weight: 'shards'
};

/**
 * @DEPRECATED
 * Kept here for now, to avoid deleting from other places, which should happen
 * in a follow up.
 */
function sendSyncData () {}
