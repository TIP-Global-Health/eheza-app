/*
 * This is a service worker script which sw-precache will import.
 *
 * This gets 'included' in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

// This defines endpoints that the Elm app can call via HTTP requests
// in order to get data from IndexedDB. The goal is to make this look,
// to Elm, very similar to getting data from a Drupal backend. We'll
// start by implementing just the things we need -- over time, it may
// become more comprehensive.

(function () {
    // This defines our URL scheme. A URL will look like
    //
    // /sw/nodes/health_center
    //
    // ... where '/sw/nodes/' is a prefix, and then the next part is
    // the 'type' of the bundle we're looking for. Then, if we're addressing
    // a specific one, then we'll have its UUID at the end. So, you might have:
    //
    // /sw/nodes/health_center/78cf21d1-b3f4-496a-b312-d8ae73041f09
    var nodesUrlRegex = /\/sw\/nodes\/([^/]+)\/?(.*)/;

    self.addEventListener('fetch', function (event) {
        var url = new URL(event.request.url);
        var matches = nodesUrlRegex.exec(url.pathname);

        if (matches) {
            var type = matches[1];
            var uuid = matches[2]; // May be null

            if (event.request.method === 'GET') {
                if (uuid) {
                    return event.respondWith(view(url, type, uuid));
                } else {
                    return event.respondWith(index(url, type));
                }
            }

            if (event.request.method === 'DELETE') {
                if (uuid) {
                    return event.respondWith(deleteNode(url, type, uuid));
                }
            }

            if (event.request.method === 'PUT') {
                if (uuid) {
                    return event.respondWith(putNode(event.request, type, uuid));
                }
            }
        }
    });

    function deleteNode (url, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            if (type === 'syncmetadata') {
                // For the syncmetadata type, we actually delete
                return dbSync.syncMetadata.delete(uuid).catch(databaseError).then(function () {
                    var response = new Response(null, {
                        status: 204,
                        statusText: 'Deleted'
                    });

                    return Promise.resolve(response);
                });
            } else {
                // Otherwise, we set the status to unpublished
                return dbSync.nodes.update(uuid, {status: 0}).catch(databaseError).then(function (updated) {
                    var response = new Response(null, {
                        status: 204,
                        statusText: 'Deleted'
                    });

                    return Promise.resolve(response);
                });
            }
        }).catch(sendErrorResponses);
    }

    function putNode (request, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            var table = dbSync.nodes;

            // For the syncmetadata type, we use the syncMetadata table instead
            if (type === 'syncmetadata') {
                table = dbSync.syncMetadata;
            }

            return request.json().catch(jsonError).then(function (json) {
                json.uuid = uuid;
                json.type = type;

                return table.put(json).catch(databaseError).then(function () {
                    var response = new Response(JSON.stringify(json), {
                        status: 200,
                        statusText: 'OK',
                        headers: {
                            'Content-Type': 'application/json'
                        }
                    });

                    return Promise.resolve(response);
                });
            });
        }).catch(sendErrorResponses);
    }

    function view (url, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            var table = dbSync.nodes;

            // For the syncmetadata type, we use the syncMetadata table instead
            if (type === 'syncmetadata') {
                table = dbSync.syncMetadata;
            }

            return table.get(uuid).catch(databaseError).then(function (node) {
                // We could also check that the type is the expected type.
                if (node) {
                    var body = JSON.stringify({
                        data: [node]
                    });

                    var response = new Response(body, {
                        status: 200,
                        statusText: 'OK',
                        headers: {
                            'Content-Type': 'application/json'
                        }
                    });

                    return Promise.resolve(response);
                } else {
                    response = new Response('', {
                        status: 404,
                        statusText: 'Not found'
                    });

                    return Promise.reject(response);
                }
            });
        }).catch(sendErrorResponses);
    }

    function index (url, type) {
        var params = url.searchParams;

        var offset = parseInt(params.get('offset') || '0');
        var range = parseInt(params.get('range') || '0');

        return dbSync.open().catch(databaseError).then(function () {
            var query = dbSync.nodes.where('type').equals(type);
            var countQuery = query.clone();

            // If type is syncmetadata, we use a different table
            if (type === 'syncmetadata') {
                query = dbSync.syncMetadata;
                countQuery = query;
            }

            return countQuery.count().catch(databaseError).then(function (count) {
                if (offset > 0) {
                    query.offset(offset);
                }

                if (range > 0) {
                    query.limit(range);
                }

                return query.toArray().catch(databaseError).then(function (nodes) {
                    var body = JSON.stringify({
                        offset: offset,
                        count: count,
                        data: nodes
                    });

                    var response = new Response(body, {
                        status: 200,
                        statusText: 'OK',
                        headers: {
                            'Content-Type': 'application/json'
                        }
                    });

                    return Promise.resolve(response);
                });
            });
        }).catch(sendErrorResponses);
    }

    // This is meant for the end of a promise chain. If we've rejected with a
    // `Response` object, then we resolve instead, so that we'll send the
    // response. (Otherwise, we'll send a network error).
    function sendErrorResponses (err) {
        if (err instanceof Response) {
            return Promise.resolve(err);
        } else {
            return Promise.reject(err);
        }
    }

    function databaseError(err) {
        var response = new Response(JSON.stringify(err), {
            status: 500,
            statusText: 'Database Error'
        });

        return Promise.reject(response);
    }

    function jsonError (err) {
        var response = new Response(JSON.stringify(err), {
            status: 400,
            statusText: 'Bad JSON'
        });

        return Promise.reject(response);
    }

})();
