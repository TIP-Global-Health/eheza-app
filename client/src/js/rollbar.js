/*
 * This is a service worker script which sw-precache will import,
 * to handle sending reports to Rollbar while offline.
 *
 * This gets "included" in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

(function () {

    var trySendingTag = 'try-sending-rollbar';

    var rollbarUrlRegex = /^https:\/\/api.rollbar.com\/api\/1\/item\/$/;

    var db = new Dexie('rollbar-db');

    // This actually just specifies a possible upgrade which will only occur
    // once the db opens. Dexie auto-opens the db when it is first used. So, we
    // don't need to worry about `install` or `activate` here, so long as we
    // only access the db inside the listeners. At that point, it will be
    // sensible to upgrade the database.
    db.version(1).stores({
        posts: '++id'
    });

    self.addEventListener('fetch', function (event) {
        if (rollbarUrlRegex.test(event.request.url)) {
            if (event.request.method === 'POST') {
                // First we try to send it normally.
                var response = fetch(event.request.clone()).catch(function(error) {
                    // If that doesn't work, we save it.
                    return event.request.json().then (function (json) {
                        return db.posts.add({
                            method: event.request.method,
                            url: event.request.url,
                            accessToken: event.request.headers.get('X-Rollbar-Access-Token'),
                            body: json
                        }).then(function (id) {
                            // And, we schedule a future attempt to send this
                            // when we're online.
                            return registration.sync.register(trySendingTag).then(function (sync) {
                                return new Response('', {
                                    status: 202,
                                    statusText: 'Accepted by Service Worker'
                                });
                            });
                        });
                    });
                });

                event.respondWith(response);
            }
        }
    });

    self.addEventListener('sync', function(event) {
        if (event.tag === trySendingTag) {
            return event.waitUntil(sendPosts());
        }
    });

    // Drain as many posts as we can ... resolves if we get them all,
    // and rejects if there are some left.
    function sendPosts () {
        return db.posts.toCollection().first().then(function (post) {
            if (post) {
                // We've got one, so try to send it
                var req = new Request (post.url, {
                    method: post.method,
                    body: JSON.stringify(post.body),
                    headers: {
                        'X-Rollbar-Access-Token': post.accessToken
                    }
                });

                return fetch(req).then(function () {
                    // If it succeeds, we'll delete the post from our storage.
                    // If it fails, the rejection will fall through, which is
                    // exactly what we want anyway.
                    return db.posts.delete(post.id).then(function () {
                        // If we successfully delete, then we call ourselves
                        // recursively! We'll try to get the first post again.
                        // Eventually, there won't be one.
                        return sendPosts();
                    });
                });
            } else {
                // There were no posts to send, so just resolve.
                return Promise.resolve();
            }
        });
    };

})();
