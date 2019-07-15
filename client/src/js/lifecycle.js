/*
 * This is a service worker script which sw-precache will import,
 * to allow us to tell an installed service worker to skip waiting.
 *
 * This gets "included" in service-worker.js via an `importScripts`.
 */
'use strict';

self.addEventListener('message', function(event) {
  if (event.data === 'SkipWaiting') {
    self.skipWaiting();
  }
});
