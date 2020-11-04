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
var backendUploadUrlRegex = /\/backend-upload\/images/;


// A UUID which represents the "shard" which is our general data that
// all devices get. (That is, unsharded data).
var nodesUuid = '78cf21d1-b3f4-496a-b312-d8ae73041f09';

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
    call_114: 'shards',
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

var dbSync = new Dexie('sync');

// Note that this code only configures ... the actual database upgrade will
// only take place once the db is opened, which only happens once we're
// the active service worker.
dbSync.version(1).stores({
    // It's not entirely clear whether it will be more convenient to split
    // up the content-types into their own stores, or keep them together.
    // Intuitively, it seems as though it will be more convenient to keep
    // them together, but we can revisit that if necessary. IndexedDB is
    // fundamentally a NoSQL-type database, so each item need not have
    // the same shape. And, there are no SQL-type joins, so using many
    // stores is inconvenient.
    //
    // (It turns out that you can only use one IndexedDB index at a time.
    // This will make for a lot of indexes -- it may be nicer to split up
    // the types into different tables. However, that will make it harder
    // to calculate the maximum `vid`. So, we'll see).
    //
    // What we're specifying here is a comma-separate list of the fields to
    // index. The first field is the primary key, and the `&` indicates
    // that it should be unique.
    nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+mother]',

    // We'll write local changes here and eventually upload them.
    nodeChanges: '++localId',

    // Metadata that tracks information about the sync process. The uuid is the
    // UUID of the shard we are syncing. So, for things we sync by health
    // center, it's the UUID of the health center. For things in the nodes
    // table, which every device gets, we use a static UUID here (`nodesUuid`).
    syncMetadata: '&uuid',

    // This is like the `nodes` table, but for the things that we don't
    // download onto all devices -- that is, for the things for which we are
    // "sharding" the database by health center.
    //
    // The `uuid` is the UUID of the node. The `shard` is the UUID of the
    // health center which is the reason we're downloading this node to this
    // device. We need a compound key with shard and vid, because IndexedDb
    // is a bit weird about using indexes -- you can only use one at a time.
    shards: '&uuid,type,vid,status,child,mother,[shard+vid]',

    // Write local changes here and eventually upload.
    shardChanges: '++localId,shard'
});

dbSync.version(2).stores({
    nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to]',
    shards: '&uuid,type,vid,status,person,[shard+vid]',
}).upgrade(function (tx) {
    // On upgrading to version 2, clear nodes and shards.
    return tx.nodes.clear().then(function () {
        return tx.shards.clear();
    }).then(function () {
        // And reset sync metadata.
        return tx.syncMetadata.toCollection().modify(function (data) {
            delete data.download;
            delete data.upload;

            data.attempt = {
                tag: 'NotAsked',
                timestamp: Date.now()
            };
        });
    });
});

dbSync.version(3).stores({
    nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
});

dbSync.version(4).stores({
    nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
    return tx.nodes.where({
        type: 'person'
    }).modify(function (person) {
        person.name_search = gatherWords(person.label);
    });
});

dbSync.version(5).stores({
    nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
    return tx.nodes.where({
        type: 'clinic'
    }).delete();
});

dbSync.version(6).stores({
    nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
    return tx.nodes.where({
        type: 'participant_form'
    }).delete();
});

dbSync.version(7).stores({
    nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter',
}).upgrade(function (tx) {
    return tx.nodes.where({
        type: 'session'
    }).delete();
});

dbSync.version(8).upgrade(function (tx) {
    return tx.nodes.where({
        type: 'clinic'
    }).delete().then(function () {
        return tx.nodes.where({
            type: 'nurse'
        }).delete();
    });
});

dbSync.version(9).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter',
});

dbSync.version(10).stores({
    nodes: '&uuid,type,vid,status,[type+pin_code]',
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(11).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(12).stores({
    statistics: '&uuid',
});

dbSync.version(13).upgrade(function (tx) {
    return tx.nodes.toCollection().modify(function (node) {
        node.deleted = false;
    })
    .then(tx.shards.toCollection().modify(function (shard) {
        shard.deleted = false;
    });
});

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

// Hooks that index persons for searching name.
dbSync.shards.hook("creating", function (primKey, obj, trans) {
    if (obj.type === 'person') {
        if (typeof obj.label == 'string') {
            obj.name_search = gatherWords(obj.label);
        }
    }
});

dbSync.shards.hook("updating", function (mods, primKey, obj, trans) {
    if (obj.type === 'person') {
        if (mods.hasOwnProperty("label")) {
            if (typeof mods.label == 'string') {
                return {
                    name_search: gatherWords(mods.label)
                };
            } else {
                return {
                    name_search: []
                };
            }
        } else {
            return {
                name_search: gatherWords(obj.label)
            };
        }
    }
});

// For when any sync metadata changes, send it all to the app
function sendSyncData () {
    return dbSync.syncMetadata.toArray().then (function (syncData) {
        var message = {
            tag: 'SyncData',
            data: syncData
        };

        return self.clients.matchAll().then(function (clients) {
            clients.forEach(function (client) {
                client.postMessage(message);
            });

            return Promise.resolve();
        });
    });
}

// When we download revisions, send them to the app.
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
