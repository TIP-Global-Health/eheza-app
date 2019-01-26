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

var nodesUuid = "78cf21d1-b3f4-496a-b312-d8ae73041f09";

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
    // What we're specifying here is a comma-separate list of the fields to
    // index. The first field is the primary key, and the `&` indicates
    // that it should be unique.
    nodes: '&uuid,type,vid',

    // Metadata that tracks information about the sync process. The uuid is
    // the UUID of the health center (for the things that we sync by health
    // center). For the general nodes store, we use a static UUID.
    syncMetadata: '&uuid'
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
