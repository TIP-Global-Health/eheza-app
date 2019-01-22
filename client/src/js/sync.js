/*
 * This is a service worker script which sw-precache will import,
 * to handle sending reports to Rollbar while offline.
 *
 * This gets 'included' in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

(function () {
    var db = new Dexie('sync');

    // Note that this code only configures ... the actual database upgrade will
    // only take place once the db is opened, which only happens once we're
    // the active service worker.
    db.version(1).stores({
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

    self.addEventListener('sync', function(event) {
        if (event.tag === syncTag) {
            return event.waitUntil(trySyncing());
        }
    });

    // This is for cases where we want to manually try a sync right away.
    self.addEventListener('message', function(event) {
        if (event.data === syncTag) {
            return event.waitUntil(trySyncing());
        }
    });

    function recordStatus(status) {
        return db.syncMetadata.get(nodesUuid).catch(function () {
            return Promise.resolve(null);
        }).then(function (meta) {
            if (!meta) {
                meta = {
                    uuid: nodesUuid,
                    last_timestamp: null,
                    remaining: null,
                };
            }

            meta.status = status;

            return db.syncMetadata.put(meta);
        });
    }

    function getLastVid() {
        return db.nodes.orderBy('vid').last().then(function (last) {
            return last ? last.vid : 0;
        });
    }

    function getCredentials() {
        return caches.open(configCache).then(function (cache) {
            return cache.match(credentialsUrl);
        }).then(function (response) {
            return response.json();
        });
    }

    function trySyncing() {
        return getCredentials().then(function (credentials) {
            return db.open().then(function () {
                return getLastVid().then(function (baseRevision) {
                    var token = credentials.access_token;
                    var backendUrl = credentials.backend_url;
                    var dbVersion = db.verno;

                    var url = backendUrl + '/api/v1.0/sync?base_revision=' + baseRevision + '&access_token=' + token + '&db_version=' + dbVersion;

                    return recordStatus({
                        type: 'Loading',
                        revision: baseRevision
                    }).then(function () {
                        return fetchFromBackend(url);
                    });
                });
            });
        });
    }

    function fetchFromBackend (url) {
        return fetch(url).catch(function (err) {
            return recordStatus({
                type: 'NetworkError',
                message: err.message
            }).then(function () {
                // Schedule an attempt to try this again.
                registration.sync.register(syncTag);

                // We reject, so that the sync mechanism will retry
                // after a backoff.
                return Promise.reject(err);
            });
        }).then(function (response) {
            return handleResponse(response);
        });
    }

    function handleResponse (response) {
        if (!response.ok) {
            return recordStatus({
                type: 'BadResponse',
                status: response.status,
                statusText: response.statusText
            }).then(function () {
                // We don't want to retry these automatically, so
                // we'll actually indicate success here. The client
                // app will display the failure and allow for the
                // user to take some action.
                return Promise.resolve();
            });
        } else {
            return response.json().then (function (json) {
                var remaining = parseInt(json.data.revision_count) - json.data.batch.length;

                return db.transaction('rw', db.nodes, db.syncMetadata, function () {
                    var promises = json.data.batch.map(function (item) {
                        item.vid = parseInt(item.vid);
                        item.id = parseInt(item.id);
                        item.timestamp = parseInt(item.timestamp);

                        return db.nodes.put(item);
                    });


                    var metadata = {
                        uuid: nodesUuid,
                        last_timestamp: parseInt(json.data.last_timestamp),
                        remaining: remaining,
                        status: {
                            type: 'Success'
                        }
                    };

                    promises.push(db.syncMetadata.put(metadata));

                    return Promise.all(promises);
                }).then(function () {
                    // If our transaction commits, we'll see if there
                    // were any remaining revisions ... if so, we'll
                    // ask for another sync.
                    if (remaining > 0) {
                        registration.sync.register(syncTag);
                    }

                    // And indicate success!
                    return Promise.resolve();
                });
            });
        }
    }

})();
