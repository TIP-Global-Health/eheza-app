/*
 * This is a service worker script which sw-precache will import.
 *
 * This gets 'included' in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

(function () {
    // Here is a list of possible values for the 'tag` field for our status,
    // along with other fields to look for.
    //
    // The only thing we will automatically retry via background sync is
    // a NetworkError, since that's the only thing that can reasonably
    // benefit from an automatic retry. However, the background sync
    // will only retry so many times.
    //
    // NotAsked
    // Loading {revision, timestamp}
    // DatabaseError {message, timestamp}
    // NetworkError {message, timestamp, willRetry}
    // NoCredentials {timestamp}
    // BadResponse {timestamp, status, statusText}
    // BadJson {timestamp}
    // Success {timestamp}

    // This is for cases where we've scheduled a background sync.
    self.addEventListener('sync', function(event) {
        if (event.tag === syncTag) {
            var action = dbSync.open().catch(databaseError).then(function () {
                return trySyncing();
            }).then(function (meta) {
                // We suceeded!
                meta.status = {
                    tag: 'Success',
                    timestamp: Date.now()
                };

                meta.uuid = nodesUuid;

                if (meta.remaining > 0) {
                    // Keep going if there are more.
                    registration.sync.register(syncTag);
                }

                return dbSync.syncMetadata.put(meta).then(sendSyncData);
            }, function (err) {
                // We failed!
                if (err.tag === 'NetworkError') {
                    err.willRetry = !event.lastChance;
                }

                return recordStatus(err).then(function () {
                    if (err.tag === 'NetworkError') {
                        return Promise.reject();
                    } else {
                        // It's only the NetworkErrors that can be usefully
                        // retried automatically. So, for other errors, we tell
                        // background sync that we don't need to try again.
                        return Promise.resolve();
                    }
                });
            });

            return event.waitUntil(action);
         }
    });

    // This is for cases where we want to manually try a sync right away.
    self.addEventListener('message', function(event) {
        if (event.data === syncTag) {
            var action = dbSync.open().catch(databaseError).then(function () {
                return manualSync();
            });

            return event.waitUntil(action);
        }
    });

    function manualSync() {
        return trySyncing().then(function (meta) {
            // We suceeded!
            meta.status = {
                tag: 'Success',
                timestamp: Date.now()
            };

            meta.uuid = nodesUuid;

            return dbSync.syncMetadata.put(meta).then(sendSyncData).then(function () {
                if (meta.remaining > 0) {
                    // Schedule another if the backend says there are more.
                    return manualSync();
                } else {
                    return Promise.resolve();
                }
            });
        }, function (err) {
            // We failed!
            if (err.tag === 'NetworkError') {
                err.willRetry = true;
                registration.sync.register(syncTag);
            }

            return recordStatus(err);
        });
    }

    function databaseError(err) {
        return Promise.reject({
            tag: 'DatabaseError',
            message: JSON.stringify(err),
            timestamp: Date.now()
        });
    }

    function recordStatus(status) {
        return dbSync.syncMetadata.get(nodesUuid).then(function (meta) {
            if (!meta) {
                meta = {
                    uuid: nodesUuid
                };
            }

            // For some reason, Dexie seems to add a _promise field to the
            // status. So, remove it in a copy.
            var withoutPromise = Object.assign({}, status);
            delete withoutPromise._promise;

            meta.status = withoutPromise;

            return dbSync.syncMetadata.put(meta).then(sendSyncData);
        });
    }

    function getLastVid() {
        return dbSync.nodes.orderBy('vid').last().then(function (last) {
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

    // Resolves with metadata, or rejects with a status message.
    function trySyncing () {
        return getCredentials().catch(function (err) {
            return Promise.reject({
                tag: 'NoCredentials',
                timestamp: Date.now()
            });
        }).then(function (credentials) {
            return getLastVid().catch(databaseError).then(function (baseRevision) {
                var token = credentials.access_token;
                var backendUrl = credentials.backend_url;
                var dbVersion = dbSync.verno;

                var url = backendUrl + '/api/v1.0/sync?base_revision=' + baseRevision + '&access_token=' + token + '&db_version=' + dbVersion;

                return recordStatus({
                    tag: 'Loading',
                    revision: baseRevision,
                    timestamp: Date.now()
                }).catch(databaseError).then(function () {
                    return fetchFromBackend(url, credentials);
                });
            });
        });
    }

    function fetchFromBackend (url, credentials) {
        return fetch(url).catch(function (err) {
            return Promise.reject({
                tag: 'NetworkError',
                message: err.message,
                timestamp: Date.now()
            });
        }).then(function (response) {
            return handleResponse(response, credentials);
        });
    }

    function handleResponse (response, credentials) {
        if (!response.ok) {
            if (response.status === 401) {
                return tryRefreshToken(credentials);
            } else {
                return Promise.reject({
                    tag: 'BadResponse',
                    status: response.status,
                    statusText: response.statusText,
                    timestamp: Date.now()
                });
            }
        } else {
            return response.json().catch(function (err) {
               return Promise.reject({
                    tag: 'BadJson',
                    timestamp: Date.now()
                });
            }).then (function (json) {
                var remaining = parseInt(json.data.revision_count) - json.data.batch.length;

                return dbSync.transaction('rw', dbSync.nodes, function () {
                    var promises = json.data.batch.map(function (item) {
                        formatNode(item);

                        return dbSync.nodes.put(item);
                    });

                    return Promise.all(promises);
                }).catch(databaseError).then(function () {
                    return Promise.resolve({
                        last_timestamp: parseInt(json.data.last_timestamp),
                        last_contact: Date.now(),
                        remaining: remaining
                    });
                });
            });
        }
    }

    function formatNode (node) {
        node.vid = parseInt(node.vid);
        node.id = parseInt(node.id);
        node.timestamp = parseInt(node.timestamp);
        node.status = parseInt(node.status);
    }

    function tryRefreshToken(credentials) {
        var refreshUrl = credentials.backend_url + '/api/refresh-token/' + credentials.refresh_token;

        return fetch(refreshUrl).catch (function (err) {
            return Promise.reject({
                tag: 'NetworkError',
                message: err.message,
                timestamp: Date.now()
            });
        }).then(function (response) {
            if (!response.ok) {
                return Promise.reject({
                    tag: 'BadResponse',
                    status: response.status,
                    statusText: response.statusText,
                    timestamp: Date.now()
                });
            } else {
                return response.json().catch(function (err) {
                    return Promise.reject({
                        tag: 'BadJson',
                        timestamp: Date.now()
                    });
                }).then (function (json) {
                    return storeCredentials(credentials, json).catch(databaseError).then(function () {
                        return trySyncing();
                    });
                });
            }
        });
    }

    function storeCredentials (credentials, json) {
        var body = JSON.stringify({
            backend_url: credentials.backend_url,
            access_token: json.access_token,
            refresh_token: json.refresh_token
        });

        return caches.open(configCache).then(function (cache) {
            var cachedResponse = new Response (body, {
                status: 200,
                statusText: 'OK',
                headers: {
                    'Content-Length': body.length,
                    'Content-Type': 'application/json'
                }
            });

            var cachedRequest = new Request (credentialsUrl, {
                method: 'GET'
            });

            return cache.put(cachedRequest, cachedResponse);
        });
    }

})();
