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
    // NotAsked {timestamp}
    // Loading {revision, timestamp}
    // Uploading {timestamp}
    // DatabaseError {message, timestamp}
    // NetworkError {message, timestamp}
    // NoCredentials {timestamp}
    // BadResponse {timestamp, status, statusText}
    // BadJson {timestamp}
    // Success {timestamp}

    // Tag values
    var NotAsked = 'NotAsked';
    var Loading = 'Loading';
    var Uploading = 'Uploading';
    var DatabaseError = 'DatabaseError';
    var NetworkError = 'NetworkError';
    var NoCredentials = 'NoCredentials';
    var BadResponse = 'BadResponse';
    var BadJson = 'BadJson';
    var Success = 'Success';

    // Transaction constants
    var rw = 'rw';

    // Listen for background sync requests. We can get here in one of several
    // ways:
    //
    // - In app.js, we trigger this on startup, and poll every 5 minutes. We
    //   also can trigger this manually from the Elm app.
    //
    // - If we get a NetworkError while we are executing one of these, we'll
    //   reject, and the browser will retry eventually.
    //
    // The browser will wait to trigger this event until it thinks we're online.
    // So, we can issue as many of these as we like while offline -- we'll just
    // receive one event here, once we're online again.
    self.addEventListener('sync', function (event) {
        if (event.tag === syncTag) {
            var action = syncAllShards().catch(function (attempt) {
                // Decide whether to indicate to background sync that we want
                // an automatic retry.
                if (attempt.tag === NetworkError) {
                    // A `NetworkError` could benefit from automatic retry, so
                    // we reject.
                    return Promise.reject();
                } else {
                    // Other error will not benefit from automatic retry, so we
                    // resolve. We'll still try again in the polling interval.
                    return Promise.resolve();
                }
            });

            return event.waitUntil(action);
        }
    });

    // Checks our `syncMetadata` table for shards that we ought to sync.  Also
    // creates the metadata for our `nodesUuid` shard if it doesn't exist yet.
    function shardsToSync () {
        return dbSync.transaction(rw, dbSync.syncMetadata, function () {
            return dbSync.syncMetadata.get(nodesUuid).then(function (item) {
                // If we don't have metadata for nodesUuid yet, create it.
                if (item) {
                    return Promise.resolve(item.uuid);
                } else {
                    return dbSync.syncMetadata.add({
                        uuid: nodesUuid,
                        attempt: {
                            tag: NotAsked,
                            timestamp: Date.now()
                        }
                    });
                }
            }).then(function () {
                // Then, get all the metadata entries that are not currently in
                // a `Loading` state. (So, `Loading` is a kind of a lock, but
                // we break it after 10 minutes).
               var tenMinutesAgo = Date.now() - (10 * 60 * 1000);

                return dbSync.syncMetadata.filter(function (item) {
                    return ((item.attempt.tag !== Loading) && (item.attempt.tag !== Uploading)) || (item.attempt.timestamp < tenMinutesAgo);
                }).toArray();
            })
        }).catch(formatDatabaseError).catch(function (attempt) {
            // If we get an error at this level, record it against the
            // nodesUUID, so we'll see it, and then re-throw, so we
            // won't continue.
            return recordAttempt(nodesUuid, attempt).then(function () {
                return Promise.reject(attempt);
            });
        });
    }

    // This kicks off the sync process for all shards. So, resolves if all
    // succeed, and rejects if any reject. (However, each shard will record
    // its own success or failure).
    function syncAllShards () {
        return getCredentials().then(function (credentials) {
            return dbSync.open().then(function () {
                return shardsToSync().then(function (shards) {
                    var actions = shards.map(function (shard) {
                        // Probably makes sense to upload and then download ...
                        // that way, we'll get the server's interpretation of
                        // our changes immediately.
                        return uploadSingleShard(shard, credentials).then(function () {
                            return downloadSingleShard(shard, credentials);
                        });
                    });

                    return Promise.all(actions).catch(function (err) {
                        if ((err.tag === BadResponse) && (err.status === 401)) {
                            return tryRefreshToken(credentials).catch(function () {
                                // If we couldn't get a new access token,
                                // then reject with our original error.
                                return Promise.reject(err);
                            }).then(function () {
                                // If we could, then try again.
                                return syncAllShards();
                            });
                        } else {
                            return Promise.reject(err);
                        }
                    });
                });
            });
        });
    }

    function getSyncUrl (shard, credentials) {
        var token = credentials.access_token;
        var backendUrl = credentials.backend_url;
        var dbVersion = dbSync.verno;

        var shardUrlPart = shard.uuid === nodesUuid ? '' : '/' + shard.uuid;

        var url = [
            backendUrl, '/api/v1.0/sync', shardUrlPart,
            '?access_token=', token,
            '&db_version=', dbVersion
        ].join('');

        return url;
    }

    // Resolves with metadata, or rejects with an attempt result. In either
    // case, the result has been recorded once this resolves or rejects.
    //
    // The parameter is our shard metadata.
    function uploadSingleShard (shard, credentials) {
        var url = getSyncUrl(shard, credentials);

        return recordAttempt(shard.uuid, {
            tag: Uploading,
            timestamp: Date.now()
        }).then(function () {
            return sendToBackend(shard.uuid, url, credentials).catch(function (err) {
                return recordAttempt(shard.uuid, err).then(function () {
                    return Promise.reject(err);
                });
            }).then(function (status) {
                return recordAttempt(shard.uuid, {
                    tag: Success,
                    timestamp: Date.now()
                }).then(function () {
                    if (status.remaining > 0) {
                        // Keep going if there are more.
                        return uploadSingleShard(shard, credentials);
                    } else {
                        return Promise.resolve(status);
                    }
                });
            });
        });
    }

    // Resolves with metadata, or rejects with an attempt result. In either
    // case, the result has been recorded once this resolves or rejects.
    //
    // The parameter is our shard metadata.
    function downloadSingleShard (shard, credentials) {
        return getLastVid(shard.uuid).then(function (baseRevision) {
            var url = getSyncUrl(shard, credentials) + '&base_revision=' + baseRevision; 

            return recordAttempt(shard.uuid, {
                tag: Loading,
                revision: baseRevision,
                timestamp: Date.now()
            }).then(function () {
                return fetchFromBackend(shard.uuid, url, credentials).catch(function (err) {
                    return recordAttempt(shard.uuid, err).then(function () {
                        return Promise.reject(err);
                    });
                }).then(function (status) {
                    return recordAttempt(shard.uuid, {
                        tag: Success,
                        timestampe: Date.now()
                    }).then(function () {
                        return dbSync.syncMetadata.update(shard.uuid, {
                            download: status
                        }).then(sendSyncData).then(function () {
                            if (status.remaining > 0) {
                                // Keep going if there are more.
                                return downloadSingleShard(shard, credentials);
                            } else {
                                return Promise.resolve(status);
                            }
                        });
                    });
                });
            });
        });
    }

    function formatDatabaseError (err) {
        return Promise.reject({
            tag: DatabaseError,
            message: err.message,
            timestamp: Date.now()
        });
    }

    function recordAttempt (shardUuid, attempt) {
        return dbSync.syncMetadata.update(shardUuid, {
            attempt: attempt
        }).catch(formatDatabaseError).then(sendSyncData);
    }

    function getLastVid (shardUuid) {
        // We can only use one index at a time, so we use a compound index.
        // The result will be sorted in the way we want.
        var collection = dbSync.shards.where('[shard+vid]').between(
            [shardUuid, Dexie.minKey],
            [shardUuid, Dexie.maxKey]
        );

        // Or, this simpler version if we're looking for the general shard.
        if (shardUuid === nodesUuid) {
            collection = dbSync.nodes.orderBy('vid');
        }

        return collection.last().then(function (last) {
            return last ? last.vid : 0;
        }).catch(formatDatabaseError);
    }

    function getCredentials () {
        return caches.open(configCache).then(function (cache) {
            return cache.match(credentialsUrl);
        }).then(function (response) {
            return response.json();
        }).catch(function (err) {
            return Promise.reject({
                tag: NoCredentials,
                timestamp: Date.now()
            });
        });
    }

    var batchSize = 50;

    function sendToBackend (shardUuid, url) {
        if (shardUuid === nodesUuid) {
            var table = dbSync.nodeChanges;
            var countQuery = table;
            var query = table.limit(batchSize);
        } else {
            var criteria = {
                shard: shardUuid
            };

            table = dbSync.shardChanges;
            countQuery = table.where(criteria);
            query = table.where(criteria).limit(batchSize);
        }

        return countQuery.count().catch(formatDatabaseError).then(function (remaining) {
            if (remaining === 0) {
                var status = {
                    first_timestamp: null,
                    remaining: remaining
                };

                return dbSync.syncMetadata.update(shardUuid, {
                    upload: status
                }).catch(formatDatabaseError).then(sendSyncData).then(function () {
                    return Promise.resolve(status);
                });
            } else {
                return query.toArray().catch(formatDatabaseError).then(function (changes) {
                    var status = {
                        remaining: remaining,
                        first_timestamp: changes[0].timestamp
                    };

                    return dbSync.syncMetadata.update(shardUuid, {
                        upload: status
                    }).catch(formatDatabaseError).then(sendSyncData).then(function () {
                        return fetch(url, {
                            method: 'POST',
                            body: JSON.stringify(changes)
                        }).catch(function (err) {
                            return Promise.reject({
                                tag: NetworkError,
                                message: err.message,
                                timestamp: Date.now()
                            });
                        }).then(function (response) {
                            if (response.ok) {
                                var ids = changes.map(function (change) {
                                    return change.localId;
                                });

                                return table.bulkDelete(ids).catch(formatDatabaseError).then(function () {
                                    return Promise.resolve(status);
                                });
                            } else {
                                return Promise.reject({
                                    tag: BadResponse,
                                    status: response.status,
                                    statusText: response.statusText,
                                    timestamp: Date.now()
                                });
                            }
                        });
                    });
                });
            }
        });
    }

    function fetchFromBackend (shardUuid, url) {
        return fetch(url).catch(function (err) {
            return Promise.reject({
                tag: NetworkError,
                message: err.message,
                timestamp: Date.now()
            });
        }).then(function (response) {
            if (response.ok) {
                return response.json().catch(function (err) {
                    return Promise.reject({
                        tag: BadJson,
                        timestamp: Date.now()
                    });
                }).then (function (json) {
                    var remaining = parseInt(json.data.revision_count) - json.data.batch.length;

                    var table = shardUuid === nodesUuid ? dbSync.nodes : dbSync.shards;

                    return dbSync.transaction(rw, dbSync.nodes, dbSync.shards, function () {
                        var promises = json.data.batch.map(function (item) {
                            formatNode(item, shardUuid);

                            return table.put(item);
                        });

                        return Promise.all(promises);
                    }).catch(formatDatabaseError).then(function () {
                        // If we've successfully saved the batch, then also send it
                        // to the Elm app.
                        return sendRevisions(json.data.batch).then(function () {
                            return Promise.resolve({
                                last_timestamp: parseInt(json.data.last_timestamp),
                                last_contact: Date.now(),
                                remaining: remaining
                            });
                        });
                    });
                });
            } else {
                return Promise.reject({
                    tag: BadResponse,
                    status: response.status,
                    statusText: response.statusText,
                    timestamp: Date.now()
                });
            }
        });
    }

    function formatNode (node, shardUuid) {
        if (shardUuid !== nodesUuid) {
            node.shard = shardUuid;
        }

        node.vid = parseInt(node.vid);
        node.id = parseInt(node.id);
        node.timestamp = parseInt(node.timestamp);
        node.status = parseInt(node.status);
    }

    function tryRefreshToken (credentials) {
        var refreshUrl = credentials.backend_url + '/api/refresh-token/' + credentials.refresh_token;

        return fetch(refreshUrl).catch (function (err) {
            return Promise.reject({
                tag: NetworkError,
                message: err.message,
                timestamp: Date.now()
            });
        }).then(function (response) {
            if (response.ok) {
                return response.json().catch(function (err) {
                    return Promise.reject({
                        tag: BadJson,
                        timestamp: Date.now()
                    });
                }).then (function (json) {
                    return storeCredentials(credentials, json);
                });
            } else {
                return Promise.reject({
                    tag: BadResponse,
                    status: response.status,
                    statusText: response.statusText,
                    timestamp: Date.now()
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
