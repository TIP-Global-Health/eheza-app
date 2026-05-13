/*
 * Main-thread bulk photo fetcher.
 *
 * Posts a batch of styled-photo URLs to /api/bulk-photos, parses the
 * custom binary container response, populates the "photos" Cache Storage
 * with each ok blob, and returns per-URL outcomes for the caller (Elm
 * SyncManager) to reconcile against the deferredPhotos IndexedDB store.
 *
 * Response container layout:
 *   [8 bytes LE uint64: manifest JSON length M]
 *   [M bytes: UTF-8 manifest JSON]
 *   [remainder: concatenated photo bytes in manifest order]
 *
 * handleBulkPhotoFetch returns either:
 *   { results: [{url, ok, terminal}, ...] } on success, or
 *   { batchError: <http status or 0 for network/parse failure> } on
 *   whole-batch failure.
 *
 * NOTE: depends on self.photoCache.stripAccessToken — photoCache.js must
 * load before this file (handled by index.html script order).
 */
'use strict';

self.bulkPhotos = self.bulkPhotos || {};

self.bulkPhotos.handleBulkPhotoFetch = async function (params) {
  var urls = params && params.urls;
  var accessToken = params && params.accessToken;
  var backendUrl = (params && params.backendUrl) || '';
  if (!Array.isArray(urls)) {
    return { batchError: 0 };
  }

  var response;
  try {
    response = await fetch(backendUrl + '/api/bulk-photos?access_token=' + encodeURIComponent(accessToken || ''), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ photos: urls }),
    });
  } catch (e) {
    return { batchError: 0 };
  }

  if (!response.ok) {
    return { batchError: response.status };
  }

  var buf = await response.arrayBuffer();
  if (buf.byteLength < 8) {
    return { batchError: 0 };
  }
  var dv = new DataView(buf);
  var manifestLen = Number(dv.getBigUint64(0, true));
  if (8 + manifestLen > buf.byteLength) {
    return { batchError: 0 };
  }

  var manifest;
  try {
    manifest = JSON.parse(new TextDecoder().decode(new Uint8Array(buf, 8, manifestLen)));
  } catch (e) {
    return { batchError: 0 };
  }
  if (!manifest || !Array.isArray(manifest.items)) {
    return { batchError: 0 };
  }

  var binStart = 8 + manifestLen;
  var cache = await caches.open(self.photoCache.cacheName);
  var results = [];

  for (var i = 0; i < manifest.items.length; i++) {
    var item = manifest.items[i];
    if (item.status === 'ok') {
      var offset = Number(item.offset);
      var length = Number(item.length);
      // Validate bounds before slicing. A malformed manifest from a
      // buggy server (NaN, negative, or out-of-range offsets) would
      // otherwise throw RangeError out of new Uint8Array and abort the
      // batch unhandled. Treat any bad item as transient so attempts++
      // bumps the row and we retry on the next cycle.
      if (
        !Number.isFinite(offset) || !Number.isFinite(length)
        || offset < 0 || length < 0
        || binStart + offset + length > buf.byteLength
      ) {
        results.push({ url: item.url, ok: false, terminal: false });
        continue;
      }
      var blob = new Blob(
        [new Uint8Array(buf, binStart + offset, length)],
        { type: item.mime || 'image/jpeg' }
      );
      var cacheKey = self.photoCache.stripAccessToken(item.url);
      try {
        await cache.put(new Request(cacheKey), new Response(blob));
        results.push({ url: item.url, ok: true, terminal: false });
      } catch (e) {
        // Cache.put failed (quota, etc.) — transient.
        results.push({ url: item.url, ok: false, terminal: false });
      }
    } else {
      var terminal = item.status === 'missing' || item.status === 'forbidden';
      results.push({ url: item.url, ok: false, terminal: terminal });
    }
  }

  return { results: results };
};
