/*
 * This is a service worker script which sw-precache will import,
 * to deal with the manipulation of configuration we wish to save
 * locally.
 *
 * This gets 'included' in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 *
 * The code below is actually fairly generic. It intercepts requests
 * to any URL under /sw/config.
 *
 * - For PUT, it caches the JSON body under the provided URL
 *
 * - For GET, it gets the cached JSON if there has been a PUT.
 *
 * - For DELETE, it deletes any cached JSON for the URL.
 *
 * So, this is basically a nice alternative to local storage or indexd DB
 * for various singleton resources we want to save. (If they are not singletons,
 * then we'd want to use IndexedDB).
 */
'use strict';

(function () {

    self.addEventListener('fetch', function (event) {
        if (configUrlRegex.test(event.request.url)) {
            if (event.request.method === 'GET') {
                var response = caches.open(configCache).then(function (cache) {
                    return cache.match(event.request.url).then(function(response) {
                        if (response) {
                            return response;
                        } else {
                            return new Response ('Not found', {
                                status: 404,
                                statusText: 'Not Found'
                            });
                        }
                    });
                });

                event.respondWith(response);
            }

            if (event.request.method === 'PUT') {
                var response = caches.open(configCache).then(function (cache) {
                    return event.request.text().then(function (body) {
                        var cachedResponse = new Response (body, {
                            status: 200,
                            statusText: 'OK',
                            headers: {
                                'Content-Type': 'application/json'
                            }
                        });

                        var cachedRequest = new Request (event.request.url, {
                            method: 'GET'
                        });

                        return cache.put(cachedRequest, cachedResponse).then(function () {
                            return new Response ('', {
                                status: 201,
                                statusText: 'Created',
                                headers: {
                                    Location: event.request.url
                                }
                            });
                        });
                    }).catch(function (e) {
                        return new Response (e.toString(), {
                            status: 500,
                            statusText: 'Cache upload error'
                        });
                    });
                });

                event.respondWith(response);
            }

            if (event.request.method === 'DELETE') {
                var response = caches.open(configCache).then(function (cache) {
                    var cachedRequest = new Request (event.request.url, {
                        method: 'GET'
                    });

                    return cache.delete(cachedRequest).then(function (deleted) {
                        return new Response ('', {
                            status: 204,
                            statusText: 'Deleted'
                        });
                    });
                });

                event.respondWith(response);
            }
        }
    });
})();
