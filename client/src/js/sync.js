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
    // ImageNotFound {timestamp, url}

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
    var ImageNotFound = 'ImageNotFound';

    // Transaction constants
    var rw = 'rw';

    var batchSize = 50;
    var dataTimeout = 30000; // 30 seconds
    var imageTimeout = 120000; // 2 minutes

    var integerFields = [
        'abortions',
        'c_sections',
        'fetal_heart_rate',
        'heart_rate',
        'household_size',
        'id',
        'live_children',
        'number_of_children',
        'preterm_pregnancy',
        'respiratory_rate',
        'status',
        'stillbirths_at_term',
        'stillbirths_preterm',
        'term_pregnancy',
        'timestamp',
        'vid'
    ];

    var floatFields = [
        'body_temperature',
        'dia',
        'fundal_height',
        'height',
        'muac',
        'sys',
        'weight'
    ];

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
                // Always indicate to the browser's sync mechanism that we
                // succeeded. Otherwise, it seems that retries are disabled
                // for a while, which isn't really suitable. And we'll retry
                // in a few minutes anyway, or manually, so we don't really
                // need the automatic retries.
                return Promise.resolve();
            });

            return event.waitUntil(action);
        }
    });

    // This is a variant of the above which listens for a message instead.
    // It's triggered via a message rather than the background sync mechanism,
    // so it by-passes the browser's notion of whether we're online or not.
    self.addEventListener('message', function(event) {
        if (event.data === syncTag) {
            var action = syncAllShards();

            // Consider how to handle errors?
            return event.waitUntil(action);
        }
    });

    // Returns node (`nodesUuid`) shard which we ought to sync.  Will
    // creates the metadata for our it, if it doesn't exist yet.
    function nodeShardToSync () {
        return dbSync.transaction(rw, dbSync.syncMetadata, function () {
            return dbSync.syncMetadata.get(nodesUuid)
            .then(function (item) {
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
            })
            .then(function () {
                return dbSync.syncMetadata.get(nodesUuid);
            })
        })
        .catch(formatDatabaseError)
        .catch(function (attempt) {
            // If we get an error at this level, record it against the
            // nodesUUID, so we'll see it, and then re-throw, so we
            // won't continue.
            return recordAttempt(nodesUuid, attempt).then(function () {
                return Promise.reject(attempt);
            });
        });
    }

    // Checks our `syncMetadata` table for shards that we ought to sync, other
    // than the node shard.
    function otherShardsToSync () {
        return dbSync.transaction(rw, dbSync.syncMetadata, function () {
            // We return all the shards which uuid is different from that
            // of node shard. We used to "lock" shards for whom
            // a sync was already in progress. However, it turns out that
            // this is not necessary. Because we're initiating this through
            // the backround sync mechanism, the browser won't handle two
            // of these events at once anyway.
            return dbSync.syncMetadata.filter(function (value) {
                return value.uuid !== nodesUuid;
            }).toArray();
        })
        .catch(formatDatabaseError)
        .catch(function (attempt) {
            // If we get an error at this level, record it against the
            // nodesUUID, so we'll see it, and then re-throw, so we
            // won't continue.
            return recordAttempt(nodesUuid, attempt)
            .then(function () {
                return Promise.reject(attempt);
            });
        });
    }

    // This kicks off the sync process for all shards. So, resolves if all
    // succeed, and rejects if any reject. (However, each shard will record
    // its own success or failure).
    function syncAllShards () {
        return getCredentials().then(function (credentials) {
            return dbSync.open()
            .then(function () {
                return nodeShardToSync()
                .then(function (shard) {
                    return processSingleShard (shard, credentials).catch(function (err) {
                        // When authentication token is invalid, besides standard 401 response,
                        // we may get 403 from file-upload resource.
                        // Therefore, we'll try to refresh token on 403 respnse as well.
                        if ((err.tag === BadResponse) && (err.status === 401 || err.status === 403)) {
                            return tryRefreshToken(credentials).catch(function () {
                                // If we couldn't get a new access token,
                                // then reject with our original error.
                                return Promise.reject(err);
                            }).then(function () {
                                // If we could, then try again.
                                // We fetch new credentials, since they got refreshed.
                                return getCredentials().then(function (credentials) {
                                    return processSingleShard (shard, credentials);
                                });
                            });
                        } else {
                            return Promise.reject(err);
                        }
                    })
                    .then(function () {
                        // Need to fetch new credentials, since they may have got refreshed.
                        return getCredentials().then(function (credentials) {
                            return otherShardsToSync().then(function (shards) {
                                var actions = shards.map(function (shard) {
                                    return processSingleShard(shard, credentials);
                                });

                                return Promise.all(actions);
                            });
                        });
                    });
                });
            });
        });
    }

    // Uploads and then Downloads data for single shard.
    function processSingleShard (shard, credentials) {
        // Probably makes sense to upload and then download ...
        // that way, we'll get the server's interpretation of
        // our changes immediately.
        return uploadSingleShard(shard, credentials).then(function () {
            return downloadSingleShard(shard, credentials);
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

    function getUploadUrl (credentials) {
        var token = credentials.access_token;
        var backendUrl = credentials.backend_url;

        var url = [
            backendUrl,
            '/api/file-upload?access_token=',
            token
        ].join('');

        return url;
    }

    // Resolves with metadata, or rejects with an attempt result. In either
    // case, the result has been recorded once this resolves or rejects.
    //
    // The parameter is our shard metadata.
    function uploadSingleShard (shard, credentials) {
        var url = getSyncUrl(shard, credentials);
        var uploadUrl = getUploadUrl(credentials);

        return recordAttempt(shard.uuid, {
            tag: Uploading,
            timestamp: Date.now()
        }).then(function () {
            return sendToBackend(shard.uuid, url, uploadUrl).catch(function (err) {
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
                return fetchFromBackend(shard.uuid, url).catch(function (err) {
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
        // Dexie seems to add a `_promise` field that we need to remove.
        var withoutPromise = Object.assign({}, attempt);
        delete withoutPromise._promise;

        return dbSync.syncMetadata.update(shardUuid, {
            attempt: withoutPromise
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

    function sendToBackend (shardUuid, url, uploadUrl) {
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
                        return uploadImages(table, changes, uploadUrl).then(function () {
                            return fetchWithTimeout(url, {
                                method: 'POST',
                                body: JSON.stringify({
                                    changes: changes
                                })
                            }, dataTimeout).catch(function (err) {
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
                });
            }
        });
    }

    // Cycle through the array of changes. Check for image fields that point to
    // things in our upload cache. If we find them, upload the file and replace
    // with the fileId that Drupal assigns. (We save it back to the table, so
    // we won't try to upload it again).
    //
    // Note that we don't delete the cached photo here, because we'll still
    // want to look at it in the cache. It will get deleted when the result of
    // the upload gets downloaded ... that is, when we get the image as Drupal
    // has processed it.
    function uploadImages (table, changes, uploadUrl) {
        return changes.reduce(function (previous, change) {
            return previous.then(function () {
                return uploadImageField(table, change, uploadUrl, 'avatar').then(function () {
                    return uploadImageField(table, change, uploadUrl, 'photo');
                });
            });
        }, Promise.resolve());
    }

    function uploadImageField (table, change, uploadUrl, field) {
        if (change.data.hasOwnProperty(field)) {
            if (parseInt(change.data[field])) {
                // It's already a file ID, so we needn't do anything.
                return Promise.resolve();
            } else if (photosUploadUrlRegex.test(change.data[field])) {
                // It's in our upload cache, so we have to upload it, and
                // replace it here with the Drupal file ID we get back.
                return caches.open(photosUploadCache).then(function (cache) {
                    return cache.match(change.data[field]).then(function (cachedResponse) {
                        if (cachedResponse) {
                            return cachedResponse.blob().then(function (blob) {
                                var formData = new FormData();
                                var imageName = 'image-' + change.uuid.substring(0, 7) + '.jpg';

                                formData.set('file', blob, imageName);

                                var request = new Request(uploadUrl, {
                                    method: 'POST',
                                    body: formData,
                                    // This prevents attaching cookies to request, to prevent
                                    // sending authenitaction cookie, as our desired
                                    // authentication method is token.
                                    credentials: 'omit'
                                });

                                return fetchWithTimeout(request, {}, imageTimeout).catch(function (err) {
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
                                            // We successfully uploaded and got
                                            // an ID back, so record that here.
                                            change.data[field] = parseInt(json.data[0].id);

                                            return table.put(change);
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
                        } else {
                            return Promise.reject({
                                tag: ImageNotFound,
                                url: change.data[field],
                                timestamp: Date.now()
                            });
                        }
                    });
                });
            } else if (photosDownloadUrlRegex.test(change.data[field])) {
                // It's a URL that we have cached, so just remove it ...  don't
                // send it to the backend, since it's not a file ID
                delete change.data[field];
                return Promise.resolve();
            }
        }

        return Promise.resolve();
    }

    function fetchFromBackend (shardUuid, url) {
        return fetchWithTimeout(url, {}, dataTimeout).catch(function (err) {
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
                    var remaining = parseInt(json.data.revision_count);

                    var table = shardUuid === nodesUuid ? dbSync.nodes : dbSync.shards;


                    // We keep a list of those nodes successfully saved.
                    var saved = [];

                    // If the node references an image (or other file, for that
                    // matter), we'd like to decide immediately whether to
                    // cache it. Downloading to the cache will be async, and
                    // our db tranasactions can't handle that ... IndexedDB
                    // transactions can handle async db actions, but not other
                    // async actions (no "long" transactions). So, we'll need
                    // to handle this one-by-one. However, we'll wait to the
                    // end to send the successfully saved nodes to Elm.
                    //
                    // Alternatively, we could wait and cache images
                    // separately. However, indicating progress would then
                    // become more complex -- we'd need to indicate separately
                    // our progress in downloading images.
                    //
                    // So, that's why we don't use `Promise.all` here ... it
                    // would execute in parrallel rather than sequentially.
                    return json.data.batch.reduce(function (previous, item) {
                        return previous.then(function () {
                            return formatNode(table, item, shardUuid).then(function (formatted) {
                                return table.put(formatted).then(function () {
                                    saved.push(formatted);
                                    return Promise.resolve();
                                });
                            });
                        });
                    }, Promise.resolve()).catch(function (err) {
                        // If we reject at some stage, but we've saved some
                        // things, then we actually will say that we were
                        // successful here. That allows us to record our
                        // partial progress. Now, we'll note that we have some
                        // remaining things to get, so we'll try again. But,
                        // then, the thing we failed on will be first. So, if
                        // we fail agaim, we won't have saved anything, and
                        // we'll return the error then.  That seems like a
                        // reasonable sequence of events.
                        if (saved.length > 0) {
                            return Promise.resolve();
                        } else {
                            return Promise.reject(err);
                        }
                    }).then(function () {
                        return sendRevisions(saved).then(function () {
                            return Promise.resolve({
                                last_timestamp: parseInt(json.data.last_timestamp),
                                last_contact: Date.now(),
                                remaining: remaining - saved.length
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

    function formatNode (table, node, shardUuid) {
        if (shardUuid !== nodesUuid) {
            node.shard = shardUuid;
        }

        integerFields.forEach(function (field) {
            if (node.hasOwnProperty(field)) {
                if (typeof node[field] == 'string') {
                    node[field] = parseInt(node[field]);
                }
            }
        });

        floatFields.forEach(function (field) {
            if (node.hasOwnProperty(field)) {
                if (typeof node[field] == 'string') {
                    node[field] = parseFloat(node[field]);
                }
            }
        });

        return checkImageField(table, node, 'avatar').then(function (checked) {
            return checkImageField(table, node, 'photo');
        });
    }

    function checkImageField (table, node, field) {
        if (node.hasOwnProperty(field)) {
            // Then, we need to see whether it has changed.
            return table.get(node.uuid).then(function (existing) {
                if (existing) {
                    // There is an existing node, so check for a change.
                    if (node[field]) {
                        // If we now have an avatar, we'll always check to
                        // see that we have cached it.
                        return cachePhotoUrl(node[field]).then(function () {
                            // Then, we check whether to delete the old one.
                            if (existing[field] && node[field] !== existing[field]) {
                                return deleteCachedPhotoUrl(existing[field]).then(function () {
                                    return Promise.resolve(node);
                                });
                            } else {
                                return Promise.resolve(node);
                            }
                        });
                    } else {
                        // If we don't now have an avatar, the only question
                        // is whether to delete the old one.
                        if (existing[field]) {
                            return deleteCachedPhotoUrl(existing[field]).then(function () {
                                return Promise.resolve(node);
                            });
                        } else {
                            return Promise.resolve(node);
                        }
                    }
                } else {
                    // There is no existing node, so just fetch the avatar if
                    // specified.
                    if (node[field]) {
                        return cachePhotoUrl(node[field]).then(function () {
                            return Promise.resolve(node);
                        });
                    } else {
                        return Promise.resolve(node);
                    }
                }
            });
        } else {
            return Promise.resolve(node);
        }
    }

    // Caches provided URL, if not cached already.
    function cachePhotoUrl (url) {
        return caches.open(photosDownloadCache).then(function (cache) {
            return cache.match(url).then(function (cached) {
                if (cached) {
                    // We've already got it ...
                    return Promise.resolve();
                } else {
                    // We need to add our access token.
                    return getCredentials().then(function (credentials) {
                        var fetchUrl = new URL(url);
                        fetchUrl.searchParams.set('access_token', credentials.access_token);

                        var req = new Request(fetchUrl, {
                            mode: 'cors'
                        });

                        return fetchWithTimeout(req, {}, imageTimeout).then(function(response) {
                            if (!response.ok) {
                                // Even though we failed to fetch the image,
                                // we return a success, so we can proceed with sync process.
                                Promise.resolve();
                            }

                            return cache.put(url, response);
                        });
                    });
                }
            });
        });
    }

    function deleteCachedPhotoUrl (url) {
        return caches.open(photosDownloadCache).then(function (cache) {
            return cache.delete(url).then(function () {
                return caches.open(photosUploadCache).then(function (uploadCache) {
                    return uploadCache.delete(url);
                });
            });
        });
    }

    function tryRefreshToken (credentials) {
        var refreshUrl = credentials.backend_url + '/api/refresh-token/' + credentials.refresh_token;

        return fetchWithTimeout(refreshUrl, {}, dataTimeout).catch (function (err) {
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

    function fetchWithTimeout (url, options, timeout) {
        var fetcher = fetch(url, options);

        if (timeout) {
            var canceler = new Promise(function (resolve, reject) {
                setTimeout(function () {
                    var error = new Error('Request timed out');
                    reject(error);
                }, timeout);
            });

            return Promise.race([fetcher, canceler]);
        } else {
            return fetcher;
        }
    }

})();
