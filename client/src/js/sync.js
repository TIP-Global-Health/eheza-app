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
        nodes: '&uuid,type,vid'
    });

    self.addEventListener('sync', function(event) {
        if (event.tag === syncTag) {
            return event.waitUntil(trySyncing());
        }
    });

    // This is for cases where we want to manually try a sync right away.
    self.addEventListener('message', function(event) {
        if (event.data === syncTag) {
            trySyncing();
        }
    });

    function trySyncing() {
        return caches.open(configCache).then(function (cache) {
            return cache.match(credentialsUrl);
        }).then(function (response) {
            return response.json();
        }).then(function (credentials) {
            return db.open().then(function () {
                return db.nodes.orderBy('vid').last().then(function (last) {
                    return last ? last.vid : 0;
                }).catch(function () {
                    return 0;
                }).then(function (baseRevision) {
                    var token = credentials.access_token;
                    var backendUrl = credentials.backend_url;
                    var dbVersion = db.verno;

                    var url = backendUrl + '/api/v1.0/sync?base_revision=' + baseRevision + '&access_token=' + token + '&db_version=' + dbVersion;

                    return fetch(url).then(function (response) {
                        return response.json();
                    }).then (function (json) {
                        return db.transaction('rw', db.nodes, function () {
                            var promises = json.data.batch.map(function (item) {
                                item.vid = parseInt(item.vid);
                                item.id = parseInt(item.id);

                                return db.nodes.put(item);
                            });

                            return Promise.all(promises);
                        });
                    }).catch(function (err) {
                        // Should inspect error.
                        return Promise.reject(err);
                    });
                });
            });
        });
    }

})();
