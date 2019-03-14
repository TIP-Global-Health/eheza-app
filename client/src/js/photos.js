/*
 * This is a service worker script which sw-precache will import,
 * to deal with caching of photos.
 *
 * This gets "included" in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

// TODO: Check if `await` is supported on the browsers we'll support ... could
// simplify some of the nested promises below in that case, I believe.

(function () {
    var cacheName = "photos";

    // So, we're matching all Drupally-provided files here. In theory, this is
    // a bit over-broad, since we could limit it to our actual backend. But, we
    // don't actually know that here, and it probably won't cause any trouble --
    // if it's not in the cache, we'll just try to fetch it.
    var matchUrl = /\/sites\/default\/files\//;

    var uploadCache = "photos-upload";
    var uploadUrl = /\/cache-upload\/images/;
    var backendUploadUrl = /\/backend-upload\/images/;

    self.addEventListener('fetch', function (event) {
        // Handle avatars and photos we've cached from the backend.
        if ((event.request.method === 'GET') && matchUrl.test(event.request.url)) {
            var response = caches.open(cacheName).then(function (cache) {
                return cache.match(event.request.url).then(function(response) {
                    if (response) {
                        return response;
                    } else {
                        throw Error('Image was not cached.');
                    }
                });
            }).catch(function(e) {
                // As a fallback, we will try to get the file from the backend.
                // This will only work if we're online, of course. We don't
                // cache files fetched in this way, because we don't
                // necessarily want to cache all images -- we'll choose which
                // ones to cache.
                return fetch(event.request);
            });

            event.respondWith(response);
        }

        // Handle GET for images which we've uploaded to the cache, but which
        // have not yet reached the backend.
        if ((event.request.method === 'GET') && uploadUrl.test(event.request.url)) {
            var response = caches.open(uploadCache).then(function (cache) {
                return cache.match(event.request.url).then(function(response) {
                    if (response) {
                        return response;
                    } else {
                        return new Response ('Uploaded image was not found', {
                            status: 404,
                            statusText: "Not Found"
                        });
                    }
                });
            });

            event.respondWith(response);
        }

        // This represents a request to POST the cached image to the backend,
        // and return whatever the backend would usually return. You could
        // imagine doing this in Elm via ports instead, but this is actually
        // more convenient ... we can use ordinary Elm code (without ports) to
        // generate the requests that will end up here, and handle the
        // responses. Conceptually, this is just an odd kind of HTTP request,
        // and Elm knows how to handle those well, so we can just do that.
        //
        // The body of the POST should be JSON in the following form:
        //
        // { backendUrl : String
        // , accessToken : String
        // , cachedUrl : String
        // }
        //
        // Given that, we'll upload to the backend as Dropzone would have, and
        // then pass through whatever result the backend provides.
        if ((event.request.method === 'POST') && backendUploadUrl.test(event.request.url)) {
            var response = caches.open(uploadCache).then(function (cache) {
                return event.request.json().then (function (json) {
                    return cache.match(json.cachedUrl).then(function(cachedResponse) {
                        if (cachedResponse) {
                            return cachedResponse.blob().then(function (blob) {
                                var url = json.backendUrl + "/api/file-upload?access_token=" + json.accessToken;

                                // TODO: We don't actually remember the "filename", and in principle it's not
                                // important ... it's just the file's name as it was on the original client, which
                                // doesn't really matter. Though, it does form the base for the filename on the
                                // server, so it might be nice to say something more interesting here. But this
                                // will be fine.
                                var formData = new FormData();
                                formData.set("file", blob, "image-file");

                                var request = new Request (url, {
                                    method: "POST",
                                    body: formData
                                });

                                return fetch(request);
                            });
                        } else {
                            return new Response ('Cached image was not found', {
                                status: 404,
                                statusText: "Not Found"
                            });
                        }
                    });
                });
            });

            event.respondWith(response);
        }

        // Handle the POST requests from Dropzone, uploading the image to our cache
        if ((event.request.method === 'POST') && uploadUrl.test(event.request.url)) {
            var response = caches.open(uploadCache).then (function (cache) {
                return cache.keys().then(function (keys) {
                    // We'll generate a unique URL here, to simulate what happens
                    // on a POST
                    var index = 1;
                    while (true) {
                        var url = (new URL("cache-upload/images/" + index, location.href)).toString();
                        var used = keys.some(function (key) {
                            return key.url == url;
                        });
                        if (!used) return url;
                        index = index + 1;
                    }
                }).then (function (url) {
                    return event.request.formData().then(function (formData) {
                        // The body of our eventual response ... extract the image from the
                        // request.
                        var body = formData.get("file");

                        // So, this is the response we'll eventually send, when the actual
                        // file is requested ...
                        var eventualResponse = new Response (body, {
                            status: 200,
                            statusText: "OK",
                            headers: {
                                'Content-Length': body.size,
                                'Content-Type': body.type
                            }
                        });

                        // We want to extract the file that got sent, and store it in a way
                        // that a request will hand it back.
                        var eventualRequest = new Request (url, {
                            method: "GET"
                        });

                        return cache.put(eventualRequest, eventualResponse).then(function () {
                            var responseText = JSON.stringify({
                                url: url
                            });

                            return new Response (responseText, {
                                status: 201,
                                statusText: "Created",
                                headers: {
                                    Location: url
                                }
                            });
                        });
                    });
                });
            }).catch(function (e) {
                return new Response (e.toString(), {
                    status: 500,
                    statusText: "Cache upload error",
                });
            });

            event.respondWith(response);
        }
    });
})();
