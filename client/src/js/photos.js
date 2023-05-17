/*
 * This is a service worker script which sw-precache will import,
 * to deal with caching of photos.
 *
 * This gets "included" in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';


/**
 *  Handle photos we've cached from the backend.
 *
 *  This file is responsible for fetching cached photos, and also for caching
 *  them once they are uploaded via Dropzone.
 */
(() => {

    self.addEventListener('fetch', function ( event) {

        // Photos download.
        if (event.request.method === 'GET' && photosDownloadUrlRegex.test(event.request.url)) {

            event.respondWith(async function() {
                const cache = await caches.open(photosDownloadCache);
                const cachedResponse = await cache.match(event.request);
                if (cachedResponse) {
                    // Photo is in the cache.
                    return cachedResponse;
                }

                let url = new URL(event.request.url);
                let params = new URLSearchParams(url.search.slice(1));

                if (!params.has('access_token')) {
                  // We can't try fetching photo from server, because we were
                  // not provided with access token. This happens when APP tries
                  // to view photo before it got downloaded. Respond with a 404.
                  return new Response('', {
                      status: 404,
                      statusText: 'Not Found'
                  });
                }

                try {
                    // Try to get photo from server.
                    var response = await fetch(event.request);
                }
                catch (e) {
                    // Network error.
                    return response;
                }

                if (!response.ok) {
                    // If an image style of Drupal is missing from the
                    // file system, but it still exists on the DB
                    // then Drupal sends a corrupted page. If we try
                    // to return the response, it causes Elm to ignore
                    // it (probably a bug in elm/http package), and
                    // `BackendDeferredPhotoFetchHandle` is never called.
                    // So instead, in case of an error, we build our
                    // own response.
                    // If status is 0, we change it to 503, since 0
                    // is illegal.
                    var response = new Response(null,  {"status" : response.status || 503});
                    return response;
                }

                // We got the image, so cache it but without
                // the `access_token` param.
                params.delete('access_token');

                url.search = params.toString();
                cache.put(url, response.clone());
                return response;
            }());
        }

        // Photos upload.
        var uploadUrlMatch = photosUploadUrlRegex.test(event.request.url);
        // Handle GET for images which we've uploaded to the cache, but which
        // have not yet reached the backend.
        if ((event.request.method === 'GET') && uploadUrlMatch) {
            var response = caches.open(photosUploadCache).then(function (cache) {
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
        // Handle the POST requests from Dropzone, uploading the image to our cache.
        if ((event.request.method === 'POST') && uploadUrlMatch) {
            var response = caches.open(photosUploadCache).then (function (cache) {
                  var url = (new URL("cache-upload/images/" + Date.now(), location.href)).toString();
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
            }).catch(function (e) {
                return new Response (e.toString(), {
                    status: 500,
                    statusText: "Cache upload error - photos",
                });
            });

            event.respondWith(response);
        }

        // Screenshots upload.
        var screenshotsUrlMatch = screenshotsUploadUrlRegex.test(event.request.url);
        // Handle GET for images which we've uploaded to the cache, but which
        // have not yet reached the backend.
        if ((event.request.method === 'GET') && screenshotsUrlMatch) {
            var response = caches.open(screenshotsUploadCache).then(function (cache) {
                return cache.match(event.request.url).then(function(response) {
                    if (response) {
                        return response;
                    } else {
                        return new Response ('Uploaded screenshot was not found', {
                            status: 404,
                            statusText: "Not Found"
                        });
                    }
                });
            });

            event.respondWith(response);
        }
        // Handle the POST requests from Dropzone, uploading the image to our cache.
        if ((event.request.method === 'POST') && screenshotsUrlMatch) {
            var response = caches.open(screenshotsUploadCache).then (function (cache) {
                  var url = (new URL("cache-upload/screenshots/" + Date.now(), location.href)).toString();
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
            }).catch(function (e) {
                return new Response (e.toString(), {
                    status: 500,
                    statusText: "Cache upload error - screenshots",
                });
            });

            event.respondWith(response);
        }
    });

})();
