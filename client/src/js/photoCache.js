/*
 * Cache-key normalization for the "photos" Cache Storage.
 *
 * Both the service worker (on read) and the upcoming bulk-photo fetcher
 * (on cache.put) must compute the same key for the same URL. Drift
 * between the two would silently produce cache misses, so this is the
 * single source of truth.
 *
 * Rule: strip the access_token query param; preserve everything else
 * (notably the itok image-style signature).
 *
 * Loaded in two contexts:
 *   - Service worker, via the importScripts list in client/gulpfile.js
 *     (before photos.js).
 *   - Main thread, via a <script> tag in client/src/index.html
 *     (before app.js / bulkPhotos.js).
 */
'use strict';

self.photoCache = self.photoCache || {};

self.photoCache.cacheName = 'photos';

self.photoCache.stripAccessToken = function (rawUrl) {
  var url = new URL(rawUrl, self.location.origin);
  url.searchParams.delete('access_token');
  return url.toString();
};
