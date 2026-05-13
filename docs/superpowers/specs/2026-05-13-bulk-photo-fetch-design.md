# Bulk photo fetch for deferred-photo download — design

**Date:** 2026-05-13
**Branch:** `bulk-photo-fetch` (off `develop`)

## Background

E-Heza's sync engine downloads metadata for a health center in batches of
up to 500 entities per call to `GET /api/sync/<authority_uuid>`. Photo
URLs ride along inside measurement payloads (`Photo`, `NutritionPhoto`,
`PrenatalPhoto`, `WellChildPhoto`, `FamilyNutritionPhoto`, `Person.avatarUrl`,
`StockUpdate.signature`). When metadata for an authority lands in
IndexedDB, the engine extracts photo URLs into a separate `deferredPhotos`
store and fetches them later, **one at a time**, in idle cycles. The
service worker (`client/src/js/photos.js`) caches each successful response
in the `"photos"` Cache Storage so subsequent `<img src="…">` requests are
served locally.

For a fully-loaded health center we can see ~300,000 metadata nodes of
which 5–10% carry photos — up to ~20,000 entries in `deferredPhotos`.
Serial single-photo fetches dominate initial-sync wall time and are
fragile in the field where network drops are common.

## Problem

The one-at-a-time deferred-photo loop:

1. Issues 20,000 HTTP requests per HC. Per-request setup (TLS resume,
   headers, server-side auth handshake) is fixed cost and dominates
   per-photo time when the styled photos themselves are small.
2. Is brittle on flaky connections — every photo is its own opportunity
   for a transient failure.
3. Hides progress: the user sees "X remaining" decrementing one at a time
   with no batching.

Estimated wall time today, at ~50KB/photo and 100ms per-request overhead
+ transfer: **~75 minutes** for 20,000 photos.

## Requirement

A bulk fetch path that downloads many photos per HTTP request, populates
the existing `"photos"` Cache so the rest of the app (and the service
worker) are oblivious to the change, and degrades gracefully back to the
existing single-photo path when the bulk endpoint is absent or failing.

Target: at least an order of magnitude reduction in HTTP request count
(~20,000 → ~200) and a meaningful drop in wall time on both wifi and
field-cellular profiles.

## Non-goals

- **Reducing total bytes transferred.** Lowering image-style resolution
  is a separate optimization.
- **Pre-packaged static archives per HC.** Generating cron-built ZIPs
  for whole-HC initial sync was considered and rejected as
  over-engineered for the win it adds on top of the bulk endpoint.
- **Concurrency for the bulk fetcher.** v1 runs one batch in-flight at
  a time; multi-in-flight is a future optimization once we measure the
  baseline.
- **Changes to the upload-photo path.** Capture/upload mechanics stay
  as-is.
- **Changes to the metadata sync.** Photo URLs continue to ride inside
  measurement payloads; only the consumption side of `deferredPhotos`
  changes.

## Approach

Add a server endpoint that accepts a list of photo URLs and returns a
single binary response containing all the photos plus a small manifest.
The client unpacks the response in JS, populates the `"photos"` Cache
directly via `cache.put(...)` using the same cache-key normalization the
service worker uses today, and reconciles per-photo outcomes back into
the `deferredPhotos` IndexedDB store. The existing single-photo path
remains the fallback.

### Why a custom binary container (not multipart / zip / base64)

- **Custom JSON-header + concatenated binary:** chosen. Server-side
  generation is ~30 lines of PHP (`fpassthru` in a loop with explicit
  flushes); client-side parsing uses only built-in `DataView`,
  `TextDecoder`, `Blob`, and `Cache API` — no new JS dependencies.
- **multipart/mixed:** standard, but boundary handling in JS is fiddly
  and adds parser code without a corresponding benefit.
- **ZIP archive:** universally understood but requires a client-side
  unzip library; doesn't compress JPEGs further so adds no real value.
- **Base64 inside JSON:** simplest to parse but inflates payload by 33%.
  At 20,000 photos × 50KB = 1GB, the extra ~330MB rules it out.

### Why no per-photo authorization tightening

Today's single-photo path relies on the URL itself being unforgeable
(Drupal's `itok` image-style signature). The bulk endpoint matches that
posture: validate `itok`, serve the file, no additional HC-ownership
lookup. This keeps the endpoint as a thin wrapper over the same access
control already in production rather than introducing a new authorization
boundary.

## Architecture and data flow

```
SyncManager (Elm)
   │  (port) BulkPhotoFetchRequest { urls, accessToken }
   ▼
JS bridge (client/src/js/bulkPhotos.js)
   │  POST /api/sync/photos-bulk
   ▼
hedley_restful endpoint (new REST plugin)
   │  itok-validate, fpassthru each file, return container
   ▼
JS bridge
   │  parse manifest, slice ArrayBuffer, cache.put per photo
   ▼
Cache Storage "photos"
   │  (port) BulkPhotoFetchResult [{url, ok, terminal}, …]
   ▼
SyncManager: delete drained rows from deferredPhotos; attempts++ on transient failure
```

All binary parsing happens in JS, not in Elm. Elm orchestrates and
tracks per-photo outcomes against `deferredPhotos`. This matches the
existing division of labor in `client/src/js/app.js`, which already
owns IndexedDB (Dexie) and Cache Storage access.

## Server endpoint contract

**Route:** `POST /api/sync/photos-bulk?access_token=<TOKEN>`

Registered as a new REST plugin under
`server/hedley/modules/custom/hedley_restful/plugins/restful/`,
authenticated via the same device access-token mechanism as `/api/sync`.

### Request body

```json
{ "photos": [
    "https://…/sites/default/files/styles/person-photo/public/2025-11/a.jpg?itok=AAA",
    "https://…/sites/default/files/styles/patient-photo/public/2025-11/b.jpg?itok=BBB"
] }
```

URL strings are passed through verbatim from what the client stored in
`deferredPhotos.photo`. The server does not look up `fid` or owning
nodes — `itok` is the authorization.

### Batch cap

The server rejects requests with more than `MAX_BATCH` URLs (set to 200)
with `400 Bad Request`. Default client batch size is 100.

### Response

`Content-Type: application/octet-stream` with the following layout:

```
[ 8 bytes: little-endian uint64 — manifest JSON length M ]
[ M bytes: manifest JSON (UTF-8) ]
[ remaining bytes: photo binaries, concatenated in manifest order ]
```

Manifest JSON shape:

```json
{
  "items": [
    { "url": "<echoed original URL>", "status": "ok",        "offset": 0,      "length": 12345, "mime": "image/jpeg" },
    { "url": "<echoed original URL>", "status": "missing"   },
    { "url": "<echoed original URL>", "status": "forbidden" },
    { "url": "<echoed original URL>", "status": "error"     }
  ]
}
```

- `offset` is relative to the start of the binary section, not the whole
  response.
- `url` is echoed verbatim from the request; the client uses it as the
  lookup key against `deferredPhotos`.
- `status` separates retryable (`error`) from terminal (`missing`,
  `forbidden`) outcomes so the client can decide whether to retry.

### Whole-request errors

`5xx`, request timeout, or malformed-body responses are treated as a
transient batch failure. No rows in `deferredPhotos` are mutated; the
same batch is retried on the next sync cycle.

### Server-side memory note

PHP must stream binaries with `fpassthru` + explicit `flush()` per file
rather than building a single string with `file_get_contents` + `echo`,
to keep peak memory under control at batch=200.

## Client orchestration

### Elm side (`client/src/elm/SyncManager/`)

- No new `SyncStatus` variant. The bulk fetch runs in the existing
  deferred-photo phase (`SyncIdle` + `downloadPhotosStatus /=
  DownloadPhotosIdle`).
- New `Msg` constructors:
  - `FetchFromIndexDbDeferredPhotoBatch` — replaces the single-row
    fetch when the endpoint is known available.
  - `BackendDeferredPhotoBatchHandle (WebData (List PhotoBatchResult))`
    — receives the JS-side outcomes.
- New IndexedDB query: `IndexDbQueryDeferredPhotoBatch Int` — request up
  to N rows.
- Per-photo result type:
  ```elm
  type alias PhotoBatchResult =
      { url : String, ok : Bool, terminal : Bool }
  ```
  `terminal = True` for `missing`/`forbidden` so the row is deleted
  immediately rather than burning the 3-attempt retry budget.

### JS side (`client/src/js/bulkPhotos.js`, new module)

```js
async function handleBulkPhotoFetch({ urls, accessToken }) {
  const res = await fetch(`/api/sync/photos-bulk?access_token=${accessToken}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ photos: urls }),
  });
  if (!res.ok) { return { batchError: res.status }; }

  const buf = await res.arrayBuffer();
  const dv = new DataView(buf);
  const manifestLen = Number(dv.getBigUint64(0, true));
  const manifest = JSON.parse(
    new TextDecoder().decode(new Uint8Array(buf, 8, manifestLen))
  );
  const binStart = 8 + manifestLen;

  const cache = await caches.open('photos');
  const results = [];
  for (const item of manifest.items) {
    if (item.status === 'ok') {
      const blob = new Blob(
        [new Uint8Array(buf, binStart + item.offset, item.length)],
        { type: item.mime }
      );
      const cacheKey = stripAccessToken(item.url);
      await cache.put(new Request(cacheKey), new Response(blob));
      results.push({ url: item.url, ok: true,  terminal: false });
    } else {
      const terminal = item.status === 'missing' || item.status === 'forbidden';
      results.push({ url: item.url, ok: false, terminal });
    }
  }
  return { results };
}
```

### Reconciliation against `deferredPhotos`

- `ok` → delete row (existing `IndexDbQueryRemoveDeferredPhoto` flow).
- `terminal` → delete row immediately.
- transient (`ok: false`, `terminal: false`) → `attempts++` on that row.
- whole-batch error → no row mutations; same batch retried next cycle.

### Fallback ladder

1. Try bulk endpoint with batch size 100.
2. On 3 consecutive batch failures, halve to 50 and retry once.
3. On further failure, fall back to single-photo fetch (the existing
   code path) for the rest of the current sync cycle.
4. If the bulk endpoint returns `404` at any point — endpoint not
   deployed on this server — remember per-session and stop attempting
   it; use single-photo fetch exclusively.

## Cache integration with the service worker

The service worker today caches `/system/files/` responses using the
URL with `access_token` stripped (and `itok` retained — `itok` is signed
and stable per derivative). The bulk handler must use the **same**
cache-key normalization so subsequent `<img src="…">` requests hit the
cache instead of triggering a fresh fetch.

The normalization function lives in a shared module
(`client/src/js/photoCache.js`) imported by both `photos.js` (service
worker) and `bulkPhotos.js` (main thread). Single source of truth — any
drift between the two would silently produce cache misses.

```js
// photoCache.js
export function stripAccessToken(url) {
  const u = new URL(url, location.origin);
  u.searchParams.delete('access_token');
  return u.toString();
}
```

URL canonicalization is handled by `new URL(...).toString()`, applied
consistently to both the `Request` used in `cache.put` and the `url`
echoed back to Elm for the `deferredPhotos` delete. Trailing slashes,
percent-encoding, and query-param order are normalized identically on
both sides of the cache, so lookups match.

`cache.put` matches by URL + method; since the response we synthesize
from the binary slice carries no `Vary` header, no additional matching
parameters are in play.

## Sizing and performance estimate

| Metric | Today (serial) | With bulk endpoint |
|---|---|---|
| Photos per HC (worst case) | 20,000 | 20,000 |
| HTTP requests | 20,000 | ~200 (batch=100) |
| Per-request overhead (~100ms TLS + headers + auth) | ~33 min | ~20 s |
| Server bootstrap cost (~50ms/req) | ~17 min | ~10 s |
| Pure transfer at 5 Mbps (~1 GB total) | ~27 min | ~27 min |
| **Total wall time (rough)** | **~75 min** | **~28 min** |

Numbers are estimates and will be re-measured after implementation. The
shape is clear: today the engine is request-overhead-bound; with the
bulk endpoint it becomes transfer-bound. Further wins require reducing
bytes on the wire (smaller image styles), out of scope here.

### Memory and concurrency

- Per-batch response holds 100 × ~50KB = ~5MB in a single JS
  ArrayBuffer briefly before `cache.put` persists each slice. Trivial
  on tablet hardware.
- One batch in flight at a time in v1. Multi-in-flight is deferred —
  metadata sync may already be running, and we'd rather not contend.

## Backwards compatibility and rollout

- **Old client, new server:** old client continues using single-photo
  fetch; the new endpoint is unused.
- **New client, old server:** new client probes the endpoint, receives
  `404`, sets a session-scoped flag, and uses single-photo fetch for
  the rest of the session.
- **Branch:** `bulk-photo-fetch` off `develop`.
- **No new feature flag** in v1. Endpoint presence is the gate.
  An admin-level kill-switch (`hedley_admin_feature_bulk_photo_fetch_enabled`)
  can be added if we want a server-side escape hatch; deferred as a
  follow-up.

## Things to verify during implementation

1. Confirm `itok` is enforced by Drupal for `/sites/default/files/styles/…`
   even when an `access_token` is also present — the bulk endpoint's
   security model relies on it.
2. Confirm `fpassthru` + `flush()` keeps PHP peak memory bounded at
   batch=200 under realistic file sizes.
3. Confirm the chosen JS APIs (`DataView.getBigUint64`, `TextDecoder`,
   `Blob`, `caches.open`) are available in the minimum browser version
   E-Heza supports on field devices.
4. Confirm the cache-key normalization in `photoCache.js` matches the
   exact rule applied by `photos.js` today — read the SW code as the
   source of truth, factor it out, and use the shared function from
   both call sites.

## Testing

- **Unit:** binary-container parser given a synthetic response;
  cache-key normalization given canonicalization edge cases (trailing
  slash, encoded `&`, missing `access_token`).
- **Drupal SimpleTest:** POST with N URLs returns expected layout;
  invalid `itok` returns `error` per item; over-cap returns `400`.
- **E2E (Playwright):** initial sync with a fixture HC carrying photos
  populates the `"photos"` Cache; subsequent page navigation renders
  the photos without further network calls. Fallback path exercised by
  pointing the client at a server that returns `404` on the new route.

## Future work

- Multi-batch concurrency once the single-batch baseline is measured.
- Adaptive batch sizing based on response time / failure rate.
- Pre-packaged static archives per HC for fresh-device provisioning, if
  initial-sync time remains a pain point after this lands.
- Reduce image-style resolution for sync-only derivatives (smaller
  bytes on the wire).
