/*
 * This is a service worker script which sw-precache will import,
 * to deal with caching of photos.
 *
 * This gets "included" in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

(function () {
    self.addEventListener('fetch', function ( event) {

        // Handle avatars and photos we've cached from the backend.
        if (event.request.method === 'GET' && photosDownloadUrlRegex.test(event.request.url)) {

            event.respondWith(
                caches.open(photosDownloadCache).then(function(cache) {
                    return cache.match(event.request).then(function (response) {
                        return fetch(event.request).then(function(response) {
                            if (response.ok) {
                                // We got the image, so cache it but without
                                // the `access_token` param.
                                let url = new URL(event.request.url);
                                let params = new URLSearchParams(url.search.slice(1));

                                params.delete('access_token');

                                url.search = params.toString();
                                cache.put(url, response.clone());
                            }
                            else {
                                // If an image style of Drupal is missing from the
                                // file system, but it still exists on the DB
                                // then Drupal sends a corrupted page. If we try
                                // to return the response, it causes Elm to ignore
                                // it (probably a bug in elm/http package), and
                                // `BackendDeferredPhotoFetchHandle` is never called.
                                // So instead, in case of an error, we build our
                                // own response.
                                response = new Response(null,  {"status" : response.status});
                            }

                            return response;
                        });
                    });
                })
            );

            // event.respondWith(
            //     (async () => {
            //         const cache = await caches.open(cacheName);
            //         const cachedFiles = await cache.match(event.request);
            //         if (!!cachedFiles) {
            //            return cachedFiles;
            //         }
            //         else {
            //           return fetch(event.request);
            //         }
            //     })()
            // );


            // var response = caches.open(photosDownloadCache).then(function (cache) {
            //     return cache.match(event.request.url).then(function(response) {
            //         if (response) {
            //             console.log(response);
            //             return response;
            //         } else {
            //             return fetch(event.request);
            //             throw Error('Image was not cached.');
            //         }
            //     });
            // }).catch(function(e) {
            //     // As a fallback, we will try to get the file from the backend.
            //     // This will only work if we're online, of course. We don't
            //     // cache files fetched in this way, because we don't
            //     // necessarily want to cache all images -- we'll choose which
            //     // ones to cache.
            //     console.log('trying to fetch?');
            //     return fetch(event.request);
            // });
            //
            // event.respondWith(response);
        }


        // Handle GET for images which we've uploaded to the cache, but which
        // have not yet reached the backend.
        if ((event.request.method === 'GET') && photosUploadUrlRegex.test(event.request.url)) {
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

        // Handle the POST requests from Dropzone, uploading the image to our cache
        if ((event.request.method === 'POST') && photosUploadUrlRegex.test(event.request.url)) {
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
                    statusText: "Cache upload error",
                });
            });

            event.respondWith(response);
        }
    });
})();
