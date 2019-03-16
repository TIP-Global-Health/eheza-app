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

var photosDownloadUrlRegex = /\/sites\/default\/files\//;
var photosUploadUrlRegex = /\/cache-upload\/images/;
var backendUploadUrlRegex = /\/backend-upload\/images/;


// A UUID which represents the "shard" which is our general data that
// all devices get. (That is, unsharded data).
var nodesUuid = '78cf21d1-b3f4-496a-b312-d8ae73041f09';

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
