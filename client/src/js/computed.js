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
    // /sw/computed/
    //
    // ... where '/sw/computed/' is a prefix, and then the next part is
    // the 'type' of the bundle we're looking for.

    var nodesUrlRegex = /\/sw\/computed\/([^/]+)/;

    self.addEventListener('fetch', function (event) {
        var url = new URL(event.request.url);
        var matches = nodesUrlRegex.exec(url.pathname);

        if (matches) {
            var type = matches[1];

            if (event.request.method === 'GET') {
                return event.respondWith(index(url, type));
            }

            // If we get here, respond with a 404
            var response = new Response('', {
                status: 404,
                statusText: 'Not Found'
            });

            return event.respondWith(response);

        }
    });

    function index (url, type) {
        var params = url.searchParams;

        return dbSync.open().catch(databaseError).then(function () {
            var criteria = {type: type};

            var query = dbSync.nodes.where(criteria);
            var getNodes = query.toArray();

            return getNodes.catch(databaseError).then(function (nodes) {

                var body = JSON.stringify({
                    offset: 0,
                    count: nodes.length,
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

        }).catch(sendErrorResponses);
    }


    // @todo: Remove duplication

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

    function databaseError (err) {
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
