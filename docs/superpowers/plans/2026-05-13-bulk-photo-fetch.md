# Bulk photo fetch — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the one-at-a-time deferred-photo download loop with a bulk fetch that drains the `deferredPhotos` IndexedDB store ~100 photos per HTTP request, populating the existing `"photos"` Cache Storage so the service worker and rendering layer stay untouched. Preserve the single-photo path as a fallback for older servers.

**Architecture:** New Drupal REST endpoint `POST /api/bulk-photos` accepts a list of styled-photo URLs and returns a custom binary container (8-byte manifest length + JSON manifest + concatenated photo bytes). A new JS module parses the container and writes each photo into the `"photos"` Cache via `cache.put`, using the same `access_token`-stripping rule the service worker uses. Elm's `SyncManager` orchestrates: queries N rows from `deferredPhotos`, fires the bulk fetch port, reconciles per-photo outcomes back into IndexedDB, falls back to the existing single-photo path on repeated failure or 404.

**Tech Stack:** Drupal 7 / PHP (RESTful module + RestfulBase plugin), Elm 0.19.1 (`SyncManager`), browser JS (Dexie for IndexedDB, Cache API), Playwright for E2E.

**Spec:** `docs/superpowers/specs/2026-05-13-bulk-photo-fetch-design.md`

**Conventions enforced throughout:**
- Per global preference: **always ask the user before running `git commit` or `git push`** — every commit step asks first.
- Per global preference: commit messages end with `[ci skip]` after `Co-Authored-By` **unless** the user explicitly asks to run CI.
- Per CLAUDE.md: PHP/`*.test` changes must pass `ddev phpcs` (zero errors **and** zero warnings) before commit.
- Per CLAUDE.md: Elm files committed without `Debug.log`; alphabetical ordering preserved in union types, case branches, encoders/decoders.
- Per `MEMORY.md`: before `npx elm-review`, `rm -rf client/elm-stuff`.
- Existing single-photo fetch path (`SyncManager/Update.elm:1632-1810` and JS at `app.js:1015-1061`) must remain intact and reachable as the fallback.

---

## File Structure

**Create (committed):**
- `server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc` — plugin metadata (label, resource name, class, auth).
- `server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php` — endpoint class extending `\RestfulBase`. Handles POST, validates `itok`, streams binary container.
- `server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test` — Drupal SimpleTest covering happy path, missing file, invalid itok, batch-cap rejection.
- `client/src/js/photoCache.js` — single source of truth for the `access_token`-stripping cache-key rule. Imported by both `sw.js`/`photos.js` and `bulkPhotos.js`.
- `client/src/js/bulkPhotos.js` — main-thread bulk fetch handler. Parses the binary container, populates the `photos` Cache, returns per-URL outcomes.
- `client/e2e/sync-bulk-photos.spec.ts` — Playwright E2E: initial sync over fixture HC drains `deferredPhotos` and populates `"photos"` Cache.

**Modify (committed):**
- `client/src/js/sw.js` — replace inline cache-key rule with `photoCache.stripAccessToken` (import via `importScripts`).
- `client/src/js/photos.js` — replace inline `params.delete('access_token')` block with shared helper.
- `client/src/js/app.js` — register port handler that calls `bulkPhotos.handleBulkPhotoFetch`; add `IndexDbQueryDeferredPhotoBatch` case in the `askFromIndexDb` switch; add `deferredPhotos` batch read using Dexie's `.where('attempts').belowOrEqual(2).limit(N)`.
- `client/src/elm/SyncManager/Model.elm` — new types (`IndexDbQueryDeferredPhotoBatch`, `IndexDbQueryDeferredPhotoBatchResult`, `PhotoBatchResult`); two new `Msg` constructors (`BackendDeferredPhotoBatchFetch`, `BackendDeferredPhotoBatchFetchHandle`); session flag `bulkPhotosEndpointAvailable : Maybe Bool` on `Model`.
- `client/src/elm/SyncManager/Update.elm` — new branch in deferred-photo phase that fires the batch port when `bulkPhotosEndpointAvailable /= Just False`; fallback-ladder logic; handler that reconciles per-photo outcomes back into `deferredPhotos`.
- `client/src/elm/SyncManager/Encoder.elm` — encoder for `IndexDbQueryDeferredPhotoBatch Int` query.
- `client/src/elm/SyncManager/Decoder.elm` — decoder for `IndexDbQueryDeferredPhotoBatchResult` (list of result records) and for `PhotoBatchResult` from the JS port response.

**Branch:** `bulk-photo-fetch` off `develop` (already created during brainstorming; spec already committed).

---

## Phase 1 — Server endpoint (Drupal/PHP)

## Task 1: Register the plugin and stub the endpoint

**Files:**
- Create: `server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc`
- Create: `server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php`

- [ ] **Step 1: Create the plugin metadata file**

Save to `server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc`:

```php
<?php

/**
 * @file
 * Restful plugin: bulk photo fetch.
 */

$plugin = array(
  'label' => t('Bulk Photo Fetch'),
  'resource' => 'bulk-photos',
  'name' => 'bulk-photos',
  'description' => t('Return many styled photos in a single binary response.'),
  'class' => 'HedleyRestfulBulkPhotos',
  'authentication_types' => ['token'],
);
```

- [ ] **Step 2: Create the stub class**

Save to `server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php`:

```php
<?php

/**
 * @file
 * Contains \HedleyRestfulBulkPhotos.
 */

/**
 * Class HedleyRestfulBulkPhotos.
 *
 * Bulk photo fetch endpoint. Accepts a JSON list of styled-photo URLs and
 * returns a custom binary container so the client can populate its photo
 * cache in one HTTP request instead of one per photo.
 */
class HedleyRestfulBulkPhotos extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Hard cap on URLs accepted per request.
   */
  const MAX_BATCH = 200;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'bulkFetch',
      ],
    ];
  }

  /**
   * Implements \RestfulInterface::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    return [];
  }

  /**
   * Stub: returns 501 until implemented.
   */
  public function bulkFetch() {
    throw new \RestfulServerConfigurationException('Not implemented yet.');
  }

}
```

- [ ] **Step 3: Clear caches so Drupal picks up the new plugin**

Run: `ddev drush cc all`
Expected: cache-clear completes without errors.

- [ ] **Step 4: Smoke-test the route is registered**

Run:
```bash
ddev drush ev '$h = restful_get_restful_handler("bulk-photos"); var_dump($h !== NULL);'
```
Expected: `bool(true)`.

- [ ] **Step 5: Run phpcs on new files**

Run from the project root (these wrap the project's lint scripts):
```bash
REVIEW_STANDARD=Drupal ci-scripts/test_coder.sh server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php
REVIEW_STANDARD=DrupalPractice ci-scripts/test_coder.sh server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php
```
Expected: zero errors, zero warnings on both passes. Fix any reported issues before continuing.

- [ ] **Step 6: Ask user to commit**

Ask: "Server stub + plugin registration ready. OK to commit as `Add bulk-photos REST endpoint stub`?"

On approval, run:
```bash
git add server/hedley/modules/custom/hedley_restful/plugins/restful/node/bulk-photos.inc \
        server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php
git commit -m "$(cat <<'EOF'
Add bulk-photos REST endpoint stub

Registers POST /api/bulk-photos via ctools plugin; returns 501 until
implemented in next commits. Stubbed first so subsequent simpletests have
a route to hit.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 2: Write SimpleTest for happy path — single valid URL

**Files:**
- Create: `server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test`
- Modify: `server/hedley/modules/custom/hedley_restful/hedley_restful.info` (add `files[] = tests/HedleyRestfulBulkPhotosTest.test`)

- [ ] **Step 1: Find an existing test file as a template**

Run: `find server/hedley -name "*.test" -path "*hedley_*" | head -5`
Identify a file using `DrupalWebTestCase` with an authenticated device-token request. Read the first ~80 lines to learn the test bootstrap pattern (`setUp()` with `parent::setUp(['hedley_restful', ...])`, helper to create a person + node + file + image style).

- [ ] **Step 2: Write the failing test for happy path**

Save to `server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test`:

```php
<?php

/**
 * @file
 * Test the bulk-photos REST endpoint.
 */

/**
 * Class HedleyRestfulBulkPhotosTest.
 */
class HedleyRestfulBulkPhotosTest extends HedleyWebTestBase {

  /**
   * {@inheritdoc}
   */
  public static function getInfo() {
    return [
      'name' => 'Bulk photo fetch endpoint',
      'description' => 'Tests POST /api/bulk-photos container format and per-item statuses.',
      'group' => 'Hedley restful',
    ];
  }

  /**
   * Test happy path: one valid styled-photo URL returns one photo blob.
   */
  public function testSinglePhotoHappyPath() {
    $device = $this->createDeviceWithToken();
    $file = $this->createTestImageFile();
    $styled_url = image_style_url('person-photo', $file->uri);

    $response = $this->postBulkPhotos($device->token, [$styled_url]);

    $this->assertEqual(200, $response['status'], 'Endpoint returns 200.');
    $parsed = $this->parseContainer($response['body']);
    $this->assertEqual(1, count($parsed['manifest']['items']));
    $this->assertEqual('ok', $parsed['manifest']['items'][0]['status']);
    $this->assertEqual($styled_url, $parsed['manifest']['items'][0]['url']);
    $this->assertEqual('image/jpeg', $parsed['manifest']['items'][0]['mime']);
    $item = $parsed['manifest']['items'][0];
    $body = substr($parsed['binary'], $item['offset'], $item['length']);
    $this->assertEqual(filesize(drupal_realpath($file->uri)), strlen($body), 'Returned bytes match file size.');
  }

  /**
   * POST helper. Returns ['status' => int, 'body' => string].
   */
  protected function postBulkPhotos($token, array $urls) {
    $url = url('api/bulk-photos', ['absolute' => TRUE, 'query' => ['access_token' => $token]]);
    $ch = curl_init($url);
    curl_setopt_array($ch, [
      CURLOPT_POST => TRUE,
      CURLOPT_POSTFIELDS => json_encode(['photos' => $urls]),
      CURLOPT_HTTPHEADER => ['Content-Type: application/json'],
      CURLOPT_RETURNTRANSFER => TRUE,
    ]);
    $body = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);
    return ['status' => $status, 'body' => $body];
  }

  /**
   * Parse the 8-byte-length + JSON-manifest + binary container.
   */
  protected function parseContainer($body) {
    $manifest_len = unpack('P', substr($body, 0, 8))[1];
    $manifest = json_decode(substr($body, 8, $manifest_len), TRUE);
    $binary = substr($body, 8 + $manifest_len);
    return ['manifest' => $manifest, 'binary' => $binary];
  }

}
```

NOTE: `createDeviceWithToken()`, `createTestImageFile()`, and `HedleyWebTestBase` are the helpers/base class used by other hedley_restful tests. **In Step 1 you confirmed which file in `server/hedley/...` provides these.** If a helper with a different name exists, adapt the test to that helper — do not invent new helpers. If no such base class exists, extend `DrupalWebTestCase` directly and inline the device/file/image-style setup; mirror the closest existing hedley_restful test verbatim.

- [ ] **Step 3: Register the test file in module info**

Modify `server/hedley/modules/custom/hedley_restful/hedley_restful.info`. Read the file first; append:

```
files[] = tests/HedleyRestfulBulkPhotosTest.test
```

If a `files[]` line for tests already exists, add this line beneath it in alphabetical order.

- [ ] **Step 4: Run the test, verify it fails**

Run: `ddev simpletest --class=HedleyRestfulBulkPhotosTest` (per CLAUDE.md's command). If the project uses a different invocation, adapt by reading `ci-scripts/`.

Expected: FAIL on `testSinglePhotoHappyPath` because `bulkFetch()` throws `RestfulServerConfigurationException` → HTTP 501. The failure message should mention the 501 status, not a setUp failure. If setUp fails, fix the helper invocations in Step 2 before continuing.

- [ ] **Step 5: phpcs on the new test file**

Run the same phpcs commands as Task 1 Step 5 on the test file.
Expected: zero errors, zero warnings.

- [ ] **Step 6: Ask user to commit**

Ask: "Failing simpletest for the bulk-photos happy path is in place. OK to commit as `Add failing test for bulk-photos happy path`?"

On approval:
```bash
git add server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test \
        server/hedley/modules/custom/hedley_restful/hedley_restful.info
git commit -m "$(cat <<'EOF'
Add failing test for bulk-photos happy path

Single valid styled-photo URL should return container with one 'ok' item.
Test currently fails — endpoint is stub. Next commit implements the route.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 3: Implement happy path — itok validation, file read, container assembly

**Files:**
- Modify: `server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php`

- [ ] **Step 1: Replace the stub `bulkFetch()` with the implementation**

Open the class file. Replace the `bulkFetch()` method (and add private helpers below it):

```php
  /**
   * Bulk fetch handler.
   *
   * Returns Content-Type: application/octet-stream with the layout:
   *   [8 bytes LE uint64: manifest JSON length M]
   *   [M bytes: manifest JSON]
   *   [remainder: concatenated photo binaries in manifest order]
   *
   * Manifest items: { url, status: ok|missing|forbidden|error, offset?, length?, mime? }.
   */
  public function bulkFetch() {
    $request = $this->getRequest();
    if (empty($request['photos']) || !is_array($request['photos'])) {
      throw new \RestfulBadRequestException('Missing or invalid "photos" array.');
    }
    if (count($request['photos']) > self::MAX_BATCH) {
      throw new \RestfulBadRequestException(format_string('Exceeded MAX_BATCH (@n).', ['@n' => self::MAX_BATCH]));
    }

    // First pass: classify each URL and stat its file size.
    $items = [];
    $offset = 0;
    foreach ($request['photos'] as $url) {
      $resolved = $this->resolveStyledUrlToPath($url);
      if ($resolved === FALSE) {
        $items[] = ['url' => $url, 'status' => 'error'];
        continue;
      }
      if (!file_exists($resolved['path'])) {
        $items[] = ['url' => $url, 'status' => 'missing'];
        continue;
      }
      $size = filesize($resolved['path']);
      if ($size === FALSE) {
        $items[] = ['url' => $url, 'status' => 'error'];
        continue;
      }
      $items[] = [
        'url' => $url,
        'status' => 'ok',
        'offset' => $offset,
        'length' => $size,
        'mime' => $resolved['mime'],
        '_path' => $resolved['path'],
      ];
      $offset += $size;
    }

    // Strip private _path before serialization.
    $manifest_items = array_map(function ($item) {
      unset($item['_path']);
      return $item;
    }, $items);
    $manifest = json_encode(['items' => $manifest_items]);
    $manifest_len = strlen($manifest);

    // Stream the response. Bypass RESTful's JSON-formatter envelope by
    // emitting headers + body directly and exiting.
    drupal_add_http_header('Content-Type', 'application/octet-stream');
    drupal_add_http_header('Content-Length', (string) (8 + $manifest_len + $offset));
    drupal_send_headers();

    echo pack('P', $manifest_len);
    echo $manifest;
    foreach ($items as $item) {
      if ($item['status'] !== 'ok') {
        continue;
      }
      $fp = fopen($item['_path'], 'rb');
      if ($fp === FALSE) {
        // Should not happen since we statted it above, but defensive.
        continue;
      }
      fpassthru($fp);
      fclose($fp);
      flush();
    }
    drupal_exit();
  }

  /**
   * Resolve a styled-photo URL to its local path, validating the itok signature.
   *
   * Returns ['path' => string, 'mime' => string] on success, FALSE on:
   *   - URL doesn't match the styles path pattern
   *   - itok is missing or invalid for this derivative
   *   - underlying source file URI cannot be resolved
   *
   * Does not check whether the file exists on disk — that's a separate
   * 'missing' status the caller distinguishes.
   */
  protected function resolveStyledUrlToPath($url) {
    $parsed = parse_url($url);
    if (empty($parsed['path'])) {
      return FALSE;
    }
    // Expect: /sites/<site>/files/styles/<style>/public/<rest>
    if (!preg_match('#/files/styles/([^/]+)/public/(.+)$#', $parsed['path'], $m)) {
      return FALSE;
    }
    $style_name = $m[1];
    $source_relative = $m[2];

    $style = image_style_load($style_name);
    if (!$style) {
      return FALSE;
    }

    parse_str($parsed['query'] ?? '', $query);
    $itok = $query['itok'] ?? '';
    $source_uri = 'public://' . $source_relative;
    $expected = image_style_path_token($style_name, $source_uri);
    if (!$itok || !hash_equals($expected, $itok)) {
      return FALSE;
    }

    $derivative_uri = image_style_path($style_name, $source_uri);
    $derivative_path = drupal_realpath($derivative_uri);
    if ($derivative_path === FALSE) {
      // Derivative not generated; trigger generation now (matches behavior
      // of the file_get_contents path the SW takes today).
      if (!image_style_create_derivative($style, $source_uri, $derivative_uri)) {
        return FALSE;
      }
      $derivative_path = drupal_realpath($derivative_uri);
      if ($derivative_path === FALSE) {
        return FALSE;
      }
    }

    return [
      'path' => $derivative_path,
      'mime' => file_get_mimetype($derivative_uri) ?: 'image/jpeg',
    ];
  }
```

- [ ] **Step 2: Run the happy-path test, verify it passes**

Run: `ddev simpletest --class=HedleyRestfulBulkPhotosTest --methods=testSinglePhotoHappyPath` (adapt to actual project invocation if needed).

Expected: PASS.

If FAIL: the test parses the binary; debug by adding a temporary dump of the first 8 bytes hex-encoded inside the test to see the manifest length byte ordering. The container uses little-endian (`pack('P', …)` and `unpack('P', …)`); both ends must agree.

- [ ] **Step 3: Run phpcs**

Run the same phpcs commands as Task 1 Step 5 on the modified class file.
Expected: zero errors, zero warnings.

- [ ] **Step 4: Ask user to commit**

Ask: "Happy path implementation passes the simpletest and phpcs is clean. OK to commit as `Implement bulk-photos endpoint happy path`?"

On approval:
```bash
git add server/hedley/modules/custom/hedley_restful/plugins/restful/node/HedleyRestfulBulkPhotos.class.php
git commit -m "$(cat <<'EOF'
Implement bulk-photos endpoint happy path

Validates each URL's itok against image_style_path_token, streams matched
files via fpassthru with explicit flush per file to keep PHP memory bounded.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 4: SimpleTest for error paths — invalid itok, missing file, batch cap, malformed body

**Files:**
- Modify: `server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test`

- [ ] **Step 1: Add four failing tests to the existing class**

Append these methods to `HedleyRestfulBulkPhotosTest` (before the closing brace):

```php
  /**
   * Invalid itok → per-item status 'error', still HTTP 200.
   */
  public function testInvalidItokReturnsError() {
    $device = $this->createDeviceWithToken();
    $file = $this->createTestImageFile();
    $styled_url = image_style_url('person-photo', $file->uri);
    // Corrupt the itok query param.
    $bad_url = preg_replace('/itok=[^&]+/', 'itok=BADTOKEN', $styled_url);

    $response = $this->postBulkPhotos($device->token, [$bad_url]);
    $this->assertEqual(200, $response['status']);
    $parsed = $this->parseContainer($response['body']);
    $this->assertEqual('error', $parsed['manifest']['items'][0]['status']);
    $this->assertEqual($bad_url, $parsed['manifest']['items'][0]['url']);
  }

  /**
   * URL with valid itok but underlying derivative file deleted on disk →
   * status 'missing'.
   */
  public function testMissingDerivativeReturnsMissing() {
    $device = $this->createDeviceWithToken();
    $file = $this->createTestImageFile();
    $styled_url = image_style_url('person-photo', $file->uri);
    // Pre-generate the derivative, then delete it.
    $source_uri = $file->uri;
    image_style_create_derivative(image_style_load('person-photo'), $source_uri,
      image_style_path('person-photo', $source_uri));
    @unlink(drupal_realpath(image_style_path('person-photo', $source_uri)));
    // Now delete the source too so on-demand generation also fails.
    @unlink(drupal_realpath($source_uri));

    $response = $this->postBulkPhotos($device->token, [$styled_url]);
    $this->assertEqual(200, $response['status']);
    $parsed = $this->parseContainer($response['body']);
    $this->assertEqual('missing', $parsed['manifest']['items'][0]['status']);
  }

  /**
   * Mixed batch with one ok, one error, one missing → all three statuses
   * present; only the ok item contributes bytes; offsets correct.
   */
  public function testMixedBatchOrderingAndOffsets() {
    $device = $this->createDeviceWithToken();
    $file_a = $this->createTestImageFile();
    $file_b = $this->createTestImageFile();
    $url_a_ok = image_style_url('person-photo', $file_a->uri);
    $url_b_bad_itok = preg_replace('/itok=[^&]+/', 'itok=BADTOKEN',
      image_style_url('person-photo', $file_b->uri));
    // Generate the missing case: nuke a real one.
    $file_c = $this->createTestImageFile();
    $url_c = image_style_url('person-photo', $file_c->uri);
    @unlink(drupal_realpath($file_c->uri));

    $response = $this->postBulkPhotos($device->token, [$url_a_ok, $url_b_bad_itok, $url_c]);
    $parsed = $this->parseContainer($response['body']);
    $items = $parsed['manifest']['items'];
    $this->assertEqual(3, count($items));
    $this->assertEqual('ok', $items[0]['status']);
    $this->assertEqual('error', $items[1]['status']);
    $this->assertEqual('missing', $items[2]['status']);
    // Ok item's offset is 0 and length matches.
    $this->assertEqual(0, $items[0]['offset']);
    $bytes = substr($parsed['binary'], $items[0]['offset'], $items[0]['length']);
    $this->assertEqual($items[0]['length'], strlen($bytes));
  }

  /**
   * Request body with > MAX_BATCH URLs → HTTP 400.
   */
  public function testOverBatchCapReturns400() {
    $device = $this->createDeviceWithToken();
    $urls = array_fill(0, HedleyRestfulBulkPhotos::MAX_BATCH + 1, 'https://example.test/anything');
    $response = $this->postBulkPhotos($device->token, $urls);
    $this->assertEqual(400, $response['status']);
  }
```

- [ ] **Step 2: Run all five tests; the four new ones should pass already**

Run: `ddev simpletest --class=HedleyRestfulBulkPhotosTest`
Expected: all five PASS.

Why already? Because Task 3 implemented per-item statuses and the batch cap. This task is the verification layer — we're proving the implementation covered the spec, not adding new behavior. **If a test FAILS, treat it as a defect in Task 3's implementation, not in the test, and fix `HedleyRestfulBulkPhotos.class.php` before continuing.**

- [ ] **Step 3: Run phpcs on the updated test file**

Run the same phpcs commands as Task 2 Step 5 on the test file.
Expected: zero errors, zero warnings.

- [ ] **Step 4: Ask user to commit**

Ask: "Error-path simpletests pass and phpcs is clean. OK to commit as `Cover error paths in bulk-photos simpletest`?"

On approval:
```bash
git add server/hedley/modules/custom/hedley_restful/tests/HedleyRestfulBulkPhotosTest.test
git commit -m "$(cat <<'EOF'
Cover error paths in bulk-photos simpletest

Tests invalid itok (status=error), missing derivative+source (status=missing),
mixed batches (offsets and per-item statuses), and over-cap rejection (400).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Phase 2 — Shared cache-key helper (client JS refactor)

## Task 5: Extract `stripAccessToken` into a shared module

**Files:**
- Create: `client/src/js/photoCache.js`
- Modify: `client/src/js/sw.js`
- Modify: `client/src/js/photos.js`

The current cache-key normalization is inlined in `photos.js:72-75`:
```js
params.delete('access_token');
url.search = params.toString();
cache.put(url, response.clone());
```
The bulk fetcher needs **identical** logic; any drift causes silent cache misses. Extract first.

- [ ] **Step 1: Create the shared module**

Save to `client/src/js/photoCache.js`:

```js
'use strict';

/**
 * Cache-key normalization for the "photos" Cache Storage.
 *
 * Both the service worker (on read) and the bulk-photo fetcher (on
 * cache.put) must compute the same key for the same URL. The rule:
 * strip the access_token query param; preserve everything else
 * (notably the itok image-style signature).
 */
self.photoCache = self.photoCache || {};

self.photoCache.stripAccessToken = function (rawUrl) {
  var url = new URL(rawUrl, self.location.origin);
  url.searchParams.delete('access_token');
  return url.toString();
};

self.photoCache.cacheName = 'photos';
```

The `self.photoCache.*` assignment style is required because this script is `importScripts`ed into the service worker context AND into the main thread; CommonJS/ESM `export` is unavailable in the SW import path.

- [ ] **Step 2: Update `sw.js` to load the helper before `photos.js`**

Read `client/src/js/sw.js` and find the `importScripts` line that loads `photos.js`. Add `photoCache.js` immediately before it.

If `sw.js` uses sw-precache injection, find the manual `importScripts` block and add the new line there. If the build pipeline (gulp) determines the import list, edit the gulpfile entry instead — locate via `grep -rn "photos.js\|photoCache" client/gulp* client/gulpfile* 2>/dev/null`.

- [ ] **Step 3: Replace the inline strip-access-token block in `photos.js`**

In `client/src/js/photos.js`, locate lines 70-75:

```js
                // We got the image, so cache it but without
                // the `access_token` param.
                params.delete('access_token');

                url.search = params.toString();
                cache.put(url, response.clone());
```

Replace with:

```js
                // Cache under the normalized key so bulk-fetch writes and
                // SW reads agree on the lookup URL.
                cache.put(self.photoCache.stripAccessToken(event.request.url), response.clone());
```

Also remove the now-unused `let url = new URL(...)` and `let params = new URLSearchParams(...)` lines higher up if they're only used for the strip step. (Re-check: lines 33-34 also use `params.has('access_token')` — keep those.)

- [ ] **Step 4: Verify the service worker still functions**

Run: `ddev gulp` (watch mode) — wait for compile, then load the app in a browser, perform a sync that downloads at least one photo, and verify the photo renders. (`MEMORY.md` notes the user already runs `ddev gulp`; if a watcher is live, just save the files and reload the app.)

Check DevTools → Application → Cache Storage → `photos` cache. Confirm the key URL has no `access_token` param and the body is non-empty.

- [ ] **Step 5: Ask user to commit**

Ask: "Cache-key helper extracted and SW still serves photos correctly. OK to commit as `Extract stripAccessToken into shared photoCache module`?"

On approval:
```bash
git add client/src/js/photoCache.js client/src/js/sw.js client/src/js/photos.js
git commit -m "$(cat <<'EOF'
Extract stripAccessToken into shared photoCache module

Cache-key normalization for the "photos" Cache Storage is now a single
helper used by both the service worker (on read) and the upcoming bulk
fetcher (on cache.put). Behavior unchanged.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Phase 3 — JS bulk fetch handler

## Task 6: Create `bulkPhotos.js` with `handleBulkPhotoFetch`

**Files:**
- Create: `client/src/js/bulkPhotos.js`

- [ ] **Step 1: Write the module**

Save to `client/src/js/bulkPhotos.js`:

```js
'use strict';

/**
 * Main-thread bulk photo fetcher. Posts a batch of styled-photo URLs to
 * /api/bulk-photos, parses the binary container response, populates
 * the "photos" Cache Storage with each ok blob, and returns per-URL
 * outcomes for the caller (Elm SyncManager) to reconcile against the
 * deferredPhotos IndexedDB store.
 *
 * Response container layout:
 *   [8 bytes LE uint64: manifest JSON length M]
 *   [M bytes: UTF-8 manifest JSON]
 *   [remainder: concatenated photo bytes in manifest order]
 *
 * Returns: { results: [{url, ok, terminal}, ...] } on success,
 *          { batchError: <http status> } on whole-batch failure.
 *
 * NOTE: depends on photoCache.stripAccessToken being on `self`. Load
 * photoCache.js first in the main thread's script-tag order.
 */
self.bulkPhotos = self.bulkPhotos || {};

self.bulkPhotos.handleBulkPhotoFetch = async function (params) {
  const { urls, accessToken } = params;

  let response;
  try {
    response = await fetch('/api/bulk-photos?access_token=' + encodeURIComponent(accessToken), {
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

  const buf = await response.arrayBuffer();
  if (buf.byteLength < 8) {
    return { batchError: 0 };
  }
  const dv = new DataView(buf);
  const manifestLen = Number(dv.getBigUint64(0, true));
  if (8 + manifestLen > buf.byteLength) {
    return { batchError: 0 };
  }
  let manifest;
  try {
    manifest = JSON.parse(new TextDecoder().decode(new Uint8Array(buf, 8, manifestLen)));
  } catch (e) {
    return { batchError: 0 };
  }
  if (!manifest || !Array.isArray(manifest.items)) {
    return { batchError: 0 };
  }

  const binStart = 8 + manifestLen;
  const cache = await caches.open(self.photoCache.cacheName);
  const results = [];
  for (const item of manifest.items) {
    if (item.status === 'ok') {
      const blob = new Blob(
        [new Uint8Array(buf, binStart + item.offset, item.length)],
        { type: item.mime || 'image/jpeg' }
      );
      const cacheKey = self.photoCache.stripAccessToken(item.url);
      try {
        await cache.put(new Request(cacheKey), new Response(blob));
        results.push({ url: item.url, ok: true, terminal: false });
      } catch (e) {
        // Cache.put failed (quota, etc.) — treat as transient.
        results.push({ url: item.url, ok: false, terminal: false });
      }
    } else {
      const terminal = item.status === 'missing' || item.status === 'forbidden';
      results.push({ url: item.url, ok: false, terminal });
    }
  }
  return { results };
};
```

- [ ] **Step 2: Verify the file loads without syntax errors**

Run from `client/`: `node --check src/js/bulkPhotos.js`
Expected: no output (means parse succeeded).

If `node` isn't available locally, rely on the gulp build to surface parse errors when bundling.

- [ ] **Step 3: Ask user to commit**

Ask: "JS bulk fetch handler written and parses cleanly. Not yet wired up — that's the next task. OK to commit as `Add bulkPhotos.js for batched cache population`?"

On approval:
```bash
git add client/src/js/bulkPhotos.js
git commit -m "$(cat <<'EOF'
Add bulkPhotos.js for batched cache population

Module-level handler that POSTs a batch of styled-photo URLs, parses the
custom binary container, and writes each ok blob into the "photos" Cache
via cache.put. Wiring into app.js + Elm SyncManager comes next.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 7: Wire the bulk-fetch port and the batch IndexedDB query into `app.js`

**Files:**
- Modify: `client/src/js/app.js`

The existing `app.js` (per prior exploration) registers ports for IndexedDB queries (`askFromIndexDb` listener around line 1015) and saves (`saveToIndexDb` around line 637). The bulk-fetch port is a separate outgoing port from Elm. We also need to add an `IndexDbQueryDeferredPhotoBatch` case to the `askFromIndexDb` switch.

- [ ] **Step 1: Add bulkPhotos.js to the main-thread script load order**

`bulkPhotos.js` depends on `photoCache.js` being loaded first. Find the gulp pipeline or `index.html` that determines main-thread JS load order (`grep -rn "photoCache\|app.js" client/gulp* client/src/index.* 2>/dev/null`) and ensure load order: `photoCache.js → bulkPhotos.js → app.js`.

If gulp bundles all `client/src/js/*.js` alphabetically, the names already sort correctly (`a` < `b` < `p` doesn't help — but `app.js` is loaded as the entry, `bulkPhotos.js` and `photoCache.js` are imported into a single bundle by gulp; check the gulpfile).

If unsure, use explicit `<script src=…>` tags in the HTML index in the order above.

- [ ] **Step 2: Register the `bulkPhotoFetch` port subscription**

Add this block in `app.js` next to the other `app.ports.<name>.subscribe(...)` calls (find via `grep -n "ports\." client/src/js/app.js | head -20`):

```js
app.ports.bulkPhotoFetch.subscribe(async function (params) {
  // params: { urls: string[], accessToken: string }
  const outcome = await self.bulkPhotos.handleBulkPhotoFetch(params);
  app.ports.bulkPhotoFetchHandle.send(outcome);
});
```

The matching outgoing port `bulkPhotoFetchHandle` is added on the Elm side in Phase 4.

- [ ] **Step 3: Add `IndexDbQueryDeferredPhotoBatch` to the `askFromIndexDb` switch**

Find the switch dispatching on `queryType` (`grep -n "IndexDbQueryDeferredPhoto" client/src/js/app.js`). The existing case for `IndexDbQueryDeferredPhoto` returns 1 row. Add a sibling case immediately after it:

```js
case 'IndexDbQueryDeferredPhotoBatch': {
  // queryParam: { batchSize: int }
  const batchSize = Math.max(1, Math.min(parseInt(query.queryParam.batchSize, 10) || 100, 200));
  const rows = await dbSync.deferredPhotos
    .where('attempts').belowOrEqual(2)
    .limit(batchSize)
    .toArray();
  const remaining = await dbSync.deferredPhotos
    .where('attempts').belowOrEqual(2)
    .count();
  // Mirror the existing single-row response shape; just an array instead.
  app.ports.askFromIndexDbHandle.send({
    queryType: 'IndexDbQueryDeferredPhotoBatch',
    data: rows.map(r => ({ uuid: r.uuid, photo: r.photo, attempts: r.attempts || 0, remaining })),
  });
  break;
}
```

The exact field names (`dbSync`, `app.ports.askFromIndexDbHandle`, the `data` envelope) must match what the existing `IndexDbQueryDeferredPhoto` branch uses — copy the surrounding code verbatim and adapt only the query and the response shape.

- [ ] **Step 4: Build and smoke-test**

If a `ddev gulp` watcher is running, save triggers rebuild. Otherwise: `ddev gulp` once.

Expected: build succeeds. No app behavior change yet — Elm doesn't fire the new port until Phase 4.

- [ ] **Step 5: Ask user to commit**

Ask: "Bulk fetch port subscription + batched deferred-photo query are wired in app.js. OK to commit as `Wire bulkPhotoFetch port and DeferredPhotoBatch IndexedDB query`?"

On approval:
```bash
git add client/src/js/app.js
git commit -m "$(cat <<'EOF'
Wire bulkPhotoFetch port and DeferredPhotoBatch IndexedDB query

JS side now answers two new Elm requests: an outgoing bulkPhotoFetch port
that drives bulkPhotos.handleBulkPhotoFetch, and an
IndexDbQueryDeferredPhotoBatch case returning up to N pending rows.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Phase 4 — Elm SyncManager: types, ports, orchestration

## Task 8: Add new types and `IndexDbQueryDeferredPhotoBatch` query

**Files:**
- Modify: `client/src/elm/SyncManager/Model.elm`
- Modify: `client/src/elm/SyncManager/Encoder.elm`
- Modify: `client/src/elm/SyncManager/Decoder.elm`

- [ ] **Step 1: Add the result-record types and the new query/result variants**

In `client/src/elm/SyncManager/Model.elm`:

A) After the existing `IndexDbQueryDeferredPhotoResultRecord` (around line 690), add:

```elm
{-| Result of an `IndexDbQueryDeferredPhotoBatch` query: up to N rows
plus the total remaining count.
-}
type alias IndexDbQueryDeferredPhotoBatchResultRecord =
    { rows : List IndexDbQueryDeferredPhotoResultRecord
    , remaining : Int
    }


{-| Per-URL outcome from a bulk photo fetch round trip.
-}
type alias PhotoBatchResult =
    { url : String
    , ok : Bool
    , terminal : Bool
    }
```

B) In the `IndexDbQueryType` union (around line 574) add `IndexDbQueryDeferredPhotoBatch Int` (the Int is the requested batch size) **in alphabetical position** among the existing variants. Mirror the alphabetical convention used by neighboring constructors (CLAUDE.md §"Alphabetical Ordering in Elm"). The new variant slots between `IndexDbQueryDeferredPhoto` and `IndexDbQueryRemoveDeferredPhoto`.

C) In the `IndexDbQueryTypeResult` union (around line 598) add `IndexDbQueryDeferredPhotoBatchResult IndexDbQueryDeferredPhotoBatchResultRecord` in alphabetical position (between `IndexDbQueryDeferredPhotoResult` and the next constructor).

D) Update the module exposing list at line 1 to add the two new type aliases (`IndexDbQueryDeferredPhotoBatchResultRecord`, `PhotoBatchResult`) in alphabetical position.

- [ ] **Step 2: Encode the new query in `Encoder.elm`**

In `client/src/elm/SyncManager/Encoder.elm`, find the function that encodes `IndexDbQueryType` to a `{ queryType, queryParam }` payload (referenced from `Update.elm:1837` for the existing `IndexDbQueryDeferredPhoto` branch). Add a branch (alphabetical placement):

```elm
        IndexDbQueryDeferredPhotoBatch batchSize ->
            { queryType = "IndexDbQueryDeferredPhotoBatch"
            , queryParam = Json.Encode.object [ ( "batchSize", Json.Encode.int batchSize ) ]
            }
```

The exact shape (`queryParam` as `Value` vs the encoded form) must match the surrounding branches.

- [ ] **Step 3: Decode the new result in `Decoder.elm`**

In `client/src/elm/SyncManager/Decoder.elm`, find the decoder that produces `IndexDbQueryTypeResult` (referenced from `Update.elm:1937` for `IndexDbQueryDeferredPhotoResult`). Add a branch (alphabetical placement) that decodes `{ queryType: "IndexDbQueryDeferredPhotoBatch", data: [ ... ] }`:

```elm
        "IndexDbQueryDeferredPhotoBatch" ->
            Json.Decode.field "data" (Json.Decode.list decodeDeferredPhotoRow)
                |> Json.Decode.map
                    (\rows ->
                        IndexDbQueryDeferredPhotoBatchResult
                            { rows = rows
                            , remaining =
                                List.head rows
                                    |> Maybe.map .remaining
                                    |> Maybe.withDefault 0
                            }
                    )
```

Where `decodeDeferredPhotoRow` is the existing decoder for one `IndexDbQueryDeferredPhotoResultRecord` — reuse the existing function (find it via `grep -n "uuid.*photo.*attempts.*remaining" client/src/elm/SyncManager/Decoder.elm`).

Also add a decoder for the bulk fetch port's response (`PhotoBatchResult` list or `{batchError}`):

```elm
decodeBulkPhotoFetchHandle : Json.Decode.Decoder (Result Int (List PhotoBatchResult))
decodeBulkPhotoFetchHandle =
    Json.Decode.oneOf
        [ Json.Decode.field "batchError" Json.Decode.int
            |> Json.Decode.map Err
        , Json.Decode.field "results" (Json.Decode.list decodePhotoBatchResult)
            |> Json.Decode.map Ok
        ]


decodePhotoBatchResult : Json.Decode.Decoder PhotoBatchResult
decodePhotoBatchResult =
    Json.Decode.map3 PhotoBatchResult
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "ok" Json.Decode.bool)
        (Json.Decode.field "terminal" Json.Decode.bool)
```

Expose `decodeBulkPhotoFetchHandle` if the module uses an explicit exposing list.

- [ ] **Step 4: Run `elm-format --validate`**

Run from `client/`: `elm-format --validate src/elm/SyncManager/Model.elm src/elm/SyncManager/Encoder.elm src/elm/SyncManager/Decoder.elm`
Expected: silent (no errors).

- [ ] **Step 5: Run the Elm compiler**

If `ddev elm:watch` is running (per `MEMORY.md`), look at its output. Otherwise:
Run from `client/`: `elm make --output=/dev/null src/elm/Main.elm`
Expected: compilation succeeds with no errors. Warnings about unused new constructors are expected — they get used in later tasks.

- [ ] **Step 6: Ask user to commit**

Ask: "New types and codecs added; compiler is clean. OK to commit as `Add IndexDbQueryDeferredPhotoBatch + PhotoBatchResult types`?"

On approval:
```bash
git add client/src/elm/SyncManager/Model.elm \
        client/src/elm/SyncManager/Encoder.elm \
        client/src/elm/SyncManager/Decoder.elm
git commit -m "$(cat <<'EOF'
Add IndexDbQueryDeferredPhotoBatch + PhotoBatchResult types

Introduces the batched-query variant, its result record, and per-URL
PhotoBatchResult plus codecs. Nothing references them yet — wired up next.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 9: Add `bulkPhotoFetch` ports and `Msg` constructors

**Files:**
- Modify: `client/src/elm/SyncManager/Model.elm` (Msg additions, Model field additions)
- Find and modify the ports module — locate via `grep -rn "^port " client/src/elm/SyncManager/ client/src/elm/Ports/ client/src/elm/ 2>/dev/null | head -20`

- [ ] **Step 1: Locate the ports module**

Run: `grep -rn "^port " client/src/elm/SyncManager/ client/src/elm/Ports/ client/src/elm/ 2>/dev/null | head -20`

Identify the module that declares `askFromIndexDb` and `saveToIndexDb`. Add new ports there.

- [ ] **Step 2: Add the outgoing and incoming bulk-fetch ports**

In the ports module, add (alphabetical placement among existing ports):

```elm
{-| Outgoing: ask JS to POST a batch of photo URLs and populate Cache.
-}
port bulkPhotoFetch : { urls : List String, accessToken : String } -> Cmd msg


{-| Incoming: per-URL outcomes or whole-batch error from JS.
-}
port bulkPhotoFetchHandle : (Json.Decode.Value -> msg) -> Sub msg
```

If the existing ports use a specific JSON shape (e.g., wrapped in `Json.Encode.Value`), match that pattern instead. Read 2-3 nearby port declarations and adapt.

- [ ] **Step 3: Add the two new `Msg` constructors**

In `client/src/elm/SyncManager/Model.elm`, in the `Msg` union (around line 741), add (in alphabetical position — between `BackendDeferredPhotoFetchHandle` and the next constructor that sorts higher):

```elm
    | BackendDeferredPhotoBatchFetch (List IndexDbQueryDeferredPhotoResultRecord)
    | BackendDeferredPhotoBatchFetchHandle (List IndexDbQueryDeferredPhotoResultRecord) (Result Int (List PhotoBatchResult))
```

The first argument of `BackendDeferredPhotoBatchFetchHandle` is the original batch of rows we asked about — we need them to reconcile outcomes back into IndexedDB. The second is the decoded port payload.

Also add a third constructor in alphabetical position:

```elm
    | FetchFromIndexDbDeferredPhotoBatch
```

This is the trigger from the deferred-photo phase.

Update the `Msg(..)` exposing list comment line at the top if the module uses one (the existing line uses `Msg(..)` so no per-constructor update needed).

- [ ] **Step 4: Add the session-flag fields to `Model`**

In `client/src/elm/SyncManager/Model.elm`, in the `Model` type alias (find via `grep -n "^type alias Model" client/src/elm/SyncManager/Model.elm`), add three new fields in alphabetical position:

```elm
    , bulkPhotosConsecutiveBatchErrors : Int
    , bulkPhotosEndpointAvailable : Maybe Bool
    , bulkPhotosInFlight : List IndexDbQueryDeferredPhotoResultRecord
```

Update `emptyModel` to initialize them to `0`, `Nothing`, and `[]` respectively.

- [ ] **Step 5: Subscribe to the new incoming port and decode it**

Find the `subscriptions` function for SyncManager (`grep -n "subscriptions" client/src/elm/SyncManager/Update.elm client/src/elm/App/Update.elm`). Add `bulkPhotoFetchHandle` to the batch, decoding via `decodeBulkPhotoFetchHandle`. The in-flight rows are now on `model.bulkPhotosInFlight`, so the subscription passes them to the message:

```elm
        , bulkPhotoFetchHandle
            (\value ->
                let
                    decoded =
                        Json.Decode.decodeValue decodeBulkPhotoFetchHandle value
                            |> Result.withDefault (Err 0)
                in
                BackendDeferredPhotoBatchFetchHandle model.bulkPhotosInFlight decoded
            )
```

Mirror whatever decoding/error pattern the existing port subs use (read `BackendDeferredPhotoFetchHandle` wiring nearby to confirm).

- [ ] **Step 6: Compile**

Run from `client/`: `elm make --output=/dev/null src/elm/Main.elm`
Expected: compiler still passes, with warnings that the new `Msg` constructors aren't yet handled in `update`. Those warnings disappear in Task 10.

- [ ] **Step 7: Ask user to commit**

Ask: "Ports + Msg constructors + session flags added. Compiler clean (with expected unused-constructor warnings). OK to commit as `Add bulk photo fetch ports and Msg constructors`?"

On approval:
```bash
git add client/src/elm/SyncManager/Model.elm <path-to-ports-module>
git commit -m "$(cat <<'EOF'
Add bulk photo fetch ports and Msg constructors

Adds bulkPhotoFetch outgoing port + bulkPhotoFetchHandle incoming port,
plus FetchFromIndexDbDeferredPhotoBatch / BackendDeferredPhotoBatchFetch /
BackendDeferredPhotoBatchFetchHandle Msg variants, and the three
bulkPhotos* session fields. Orchestration logic next.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Task 10: Orchestrate the bulk fetch path with fallback ladder in `Update.elm`

**Files:**
- Modify: `client/src/elm/SyncManager/Update.elm`

This task wires the new `Msg` cases into `update`. Existing single-photo flow stays intact and is reachable as the fallback.

- [ ] **Step 1: Replace the `BackendFetchPhotos` dispatch with bulk-or-single selection**

Find the deferred-photo phase entry point at `Update.elm:574-590` (the `BackendFetchPhotos` branch in `BackendFetchMain`). The current flow:

```elm
BackendFetchPhotos ->
    -- ... existing ...
    FetchFromIndexDbDeferredPhoto
```

Adjust so that when `model.bulkPhotosEndpointAvailable /= Just False`, it dispatches `FetchFromIndexDbDeferredPhotoBatch` instead. Pseudocode:

```elm
BackendFetchPhotos ->
    if model.bulkPhotosEndpointAvailable == Just False then
        update FetchFromIndexDbDeferredPhoto model
    else
        update FetchFromIndexDbDeferredPhotoBatch model
```

Read the actual existing code at line 574 first and integrate the conditional consistently with its surrounding pattern (it may return a `Cmd` directly instead of calling `update`).

- [ ] **Step 2: Handle `FetchFromIndexDbDeferredPhotoBatch`**

Add a new branch in `update` near the existing `FetchFromIndexDbDeferredPhoto` (line 919):

```elm
        FetchFromIndexDbDeferredPhotoBatch ->
            let
                batchSize =
                    100
            in
            ( model
            , sendQueryToIndexDb (IndexDbQueryDeferredPhotoBatch batchSize)
            )
```

`sendQueryToIndexDb` is whatever function the existing `FetchFromIndexDbDeferredPhoto` branch uses to issue an IndexedDB query (around line 943 / 964). Reuse it verbatim.

- [ ] **Step 3: Handle the new result variant in the IndexDB result handler**

Find the case dispatch on `IndexDbQueryTypeResult` (around `Update.elm:1937`). Add a branch:

```elm
                        IndexDbQueryDeferredPhotoBatchResult { rows, remaining } ->
                            if List.isEmpty rows then
                                -- Queue is drained; same idle handling as the
                                -- single-photo path. Mirror what the existing
                                -- IndexDbQueryDeferredPhotoResult Nothing
                                -- branch does at this line.
                                <existing-idle-handling>
                            else
                                update (BackendDeferredPhotoBatchFetch rows) model
```

Read the existing `IndexDbQueryDeferredPhotoResult` branch nearby for the exact idle-state transition; copy that block.

- [ ] **Step 4: Handle `BackendDeferredPhotoBatchFetch` — fire the port**

Add a new branch in `update`:

```elm
        BackendDeferredPhotoBatchFetch rows ->
            let
                urls =
                    List.map .photo rows

                accessToken =
                    -- Same source as the single-photo path uses at Update.elm:1704.
                    -- Look up how device.accessToken is obtained there and mirror.
                    <existing-accessToken-lookup>
            in
            ( { model | bulkPhotosInFlight = rows }
            , bulkPhotoFetch { urls = urls, accessToken = accessToken }
            )
```

Replace `<existing-accessToken-lookup>` with the same expression used at line 1704 of the existing single-photo fetch (likely `device.accessToken` where `device` is destructured from `model.deviceData` or a `Maybe Device` flow).

If the single-photo handler returns early when no device is present, mirror that early-return.

- [ ] **Step 5: Handle `BackendDeferredPhotoBatchFetchHandle` — reconcile outcomes**

Add the branch:

```elm
        BackendDeferredPhotoBatchFetchHandle requestedRows result ->
            case result of
                Err 404 ->
                    -- Endpoint not deployed on this server. Disable bulk
                    -- mode for this session; next idle cycle uses single-photo.
                    ( { model | bulkPhotosEndpointAvailable = Just False, bulkPhotosInFlight = [] }
                    , Cmd.none
                    )

                Err _ ->
                    -- Whole-batch transient failure. Don't mutate rows; the
                    -- same batch will be re-queried next cycle. Track a small
                    -- consecutive-failure budget so a poisonous batch can't
                    -- deadlock — after 3 in a row, drop bulk mode for the
                    -- rest of the cycle.
                    let
                        nextConsecutive =
                            model.bulkPhotosConsecutiveBatchErrors + 1

                        ( disabled, consecutive ) =
                            if nextConsecutive >= 3 then
                                ( Just False, 0 )
                            else
                                ( model.bulkPhotosEndpointAvailable, nextConsecutive )
                    in
                    ( { model
                        | bulkPhotosEndpointAvailable = disabled
                        , bulkPhotosConsecutiveBatchErrors = consecutive
                        , bulkPhotosInFlight = []
                      }
                    , Cmd.none
                    )

                Ok results ->
                    let
                        -- Match each requested row to its outcome by URL.
                        byUrl =
                            results
                                |> List.map (\r -> ( r.url, r ))
                                |> Dict.fromList

                        commands =
                            requestedRows
                                |> List.filterMap
                                    (\row ->
                                        case Dict.get row.photo byUrl of
                                            Just outcome ->
                                                if outcome.ok || outcome.terminal then
                                                    Just (sendQueryToIndexDb (IndexDbQueryRemoveDeferredPhoto row.uuid))
                                                else
                                                    Just (sendQueryToIndexDb (IndexDbQueryUpdateDeferredPhotoAttempts { row | attempts = row.attempts + 1 }))

                                            Nothing ->
                                                -- Server omitted this URL from the manifest entirely.
                                                -- Treat as transient: bump attempts so we don't loop forever.
                                                Just (sendQueryToIndexDb (IndexDbQueryUpdateDeferredPhotoAttempts { row | attempts = row.attempts + 1 }))
                                    )
                    in
                    ( { model
                        | bulkPhotosConsecutiveBatchErrors = 0
                        , bulkPhotosInFlight = []
                        , bulkPhotosEndpointAvailable = Just True
                      }
                    , Cmd.batch commands
                    )
```

The `Dict.fromList` import: add `import Dict` at the top of `Update.elm` if missing.

The exact name `sendQueryToIndexDb` and the `IndexDbQueryUpdateDeferredPhotoAttempts` constructor must match what already exists at `Update.elm:1842-1857`. Read those lines to confirm names before pasting.

- [ ] **Step 6: Ensure existing single-photo path still compiles and is reachable**

Skim `Update.elm` lines 1632-1810 (the existing single-photo flow). Confirm:
- `BackendDeferredPhotoFetch`, `BackendDeferredPhotoFetchHandle`, `FetchFromIndexDbDeferredPhoto`, `IndexDbQueryDeferredPhotoResult` all still exist unchanged.
- The new dispatch in Step 1 routes to `FetchFromIndexDbDeferredPhoto` when bulk is disabled.

- [ ] **Step 7: `elm-format` and compile**

```bash
cd client && elm-format --validate src/elm/SyncManager/Update.elm src/elm/SyncManager/Model.elm
elm make --output=/dev/null src/elm/Main.elm
```
Expected: silent for elm-format; compile succeeds.

- [ ] **Step 8: `elm-review`**

```bash
cd client && rm -rf elm-stuff && npx elm-review
```
(Per `MEMORY.md`, always nuke `elm-stuff` before elm-review.)
Expected: no new issues. Fix anything reported before continuing.

- [ ] **Step 9: Manual smoke test**

Start `ddev gulp` if not running. Open the app in a browser, trigger a sync, watch DevTools → Network for a `POST /api/bulk-photos` request. Verify response is `200 application/octet-stream`. Open Application → Cache Storage → `photos` cache; expect entries to appear in batches rather than one at a time.

If the request fails or the cache stays empty:
- Check DevTools Console for Elm port errors.
- Check DevTools → Network → response body's first 8 bytes to confirm container layout.
- Check `dbSync.deferredPhotos` (Application → IndexedDB) — successful entries should be deleted; transient failures show `attempts: 1`.

- [ ] **Step 10: Ask user to commit**

Ask: "Bulk fetch orchestration is wired into `Update.elm`; manual smoke test passes (or report failure mode for diagnosis). OK to commit as `Orchestrate bulk photo fetch with fallback to single-photo`?"

On approval:
```bash
git add client/src/elm/SyncManager/Update.elm client/src/elm/SyncManager/Model.elm
git commit -m "$(cat <<'EOF'
Orchestrate bulk photo fetch with fallback to single-photo

SyncManager now prefers the bulk endpoint, falls back per-cycle on
repeated batch failures, and disables bulk mode for the session on 404.
Per-photo reconciliation against deferredPhotos mirrors the existing
single-photo path's IndexedDB writes.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Phase 5 — E2E

## Task 11: Playwright E2E covering bulk fetch and the fallback

**Files:**
- Create: `client/e2e/sync-bulk-photos.spec.ts`

- [ ] **Step 1: Read the closest existing E2E test as a template**

Identify an existing sync-related E2E test (per `MEMORY.md` the helpers live in `client/e2e/`). Run:
```bash
ls client/e2e/ | head -20
grep -l "drushEnv\|deferredPhotos\|photos cache\|caches.open" client/e2e/*.ts 2>/dev/null | head -5
```
Read the most relevant one. Notably the E2E framework uses `drushEnv()` from `device.ts` (per `MEMORY.md` — do not hardcode `E2E_DDEV_PROJECT`).

- [ ] **Step 2: Write the bulk-fetch E2E test**

Save to `client/e2e/sync-bulk-photos.spec.ts`:

```typescript
import { test, expect } from '@playwright/test';
import { drushEnv, primeFixtureHealthCenter } from './helpers/device';

test.describe('Bulk photo fetch', () => {
  test('initial sync drains deferredPhotos via bulk endpoint and populates "photos" cache', async ({ page }) => {
    // 1. Reset and seed the backend with a fixture HC carrying >= 5 photo
    //    measurements. The shape of this helper mirrors what other sync
    //    e2e tests use — use the existing one rather than inventing.
    await primeFixtureHealthCenter(/* ... per existing helper signature ... */);

    // 2. Observe bulk endpoint requests.
    const bulkRequests: string[] = [];
    page.on('request', (req) => {
      if (req.url().includes('/api/bulk-photos')) {
        bulkRequests.push(req.method() + ' ' + req.url());
      }
    });

    // 3. Pair the device and let sync run.
    await page.goto('/');
    await pairDeviceAndLogin(page);  // existing helper

    // 4. Wait until deferredPhotos is empty (use the existing helper from
    //    the family-nutrition e2e, which polls `Remaining for Download: 0`
    //    — see MEMORY.md).
    await waitForSyncRemaining(page, 0);

    // 5. Assertions.
    expect(bulkRequests.length, 'at least one POST to bulk endpoint').toBeGreaterThan(0);
    expect(bulkRequests[0]).toContain('POST');

    // 6. The "photos" Cache Storage has entries; navigate to a page that
    //    renders one and verify it loads from cache (network request
    //    intercepted by SW returns the cached blob).
    const cacheHasEntries = await page.evaluate(async () => {
      const cache = await caches.open('photos');
      const keys = await cache.keys();
      return keys.length;
    });
    expect(cacheHasEntries).toBeGreaterThan(0);
  });

  test('client falls back to single-photo fetch when bulk endpoint returns 404', async ({ page }) => {
    // Route /api/bulk-photos to 404 so the client trips the fallback.
    await page.route('**/api/bulk-photos*', (route) => route.fulfill({ status: 404 }));

    const singlePhotoRequests: string[] = [];
    page.on('request', (req) => {
      if (req.url().includes('/sites/') && req.url().includes('/files/styles/')) {
        singlePhotoRequests.push(req.url());
      }
    });

    await primeFixtureHealthCenter(/* ... */);
    await page.goto('/');
    await pairDeviceAndLogin(page);
    await waitForSyncRemaining(page, 0);

    expect(singlePhotoRequests.length, 'single-photo fallback fired').toBeGreaterThan(0);
  });
});
```

The helper names (`primeFixtureHealthCenter`, `pairDeviceAndLogin`, `waitForSyncRemaining`) are placeholders — replace with the actual names you find in Step 1. Do not invent new helpers; if a fixture HC with photos isn't easy to set up via existing helpers, **stop and ask the user** before adding new fixtures.

- [ ] **Step 3: Run the test**

Run from `client/`: `./node_modules/.bin/playwright test sync-bulk-photos.spec.ts`
Expected: both tests pass.

If failures: use `RECORD=1 ./node_modules/.bin/playwright test sync-bulk-photos.spec.ts` to capture video for debugging (per CLAUDE.md).

- [ ] **Step 4: Ask user to commit**

Ask: "E2E covers happy path + 404 fallback. OK to commit as `Add E2E for bulk photo fetch and single-photo fallback`?"

On approval:
```bash
git add client/e2e/sync-bulk-photos.spec.ts
git commit -m "$(cat <<'EOF'
Add E2E for bulk photo fetch and single-photo fallback

Initial sync over a fixture HC with photos: assert at least one POST
hits the bulk endpoint and the "photos" Cache ends populated. Second
test routes bulk endpoint to 404 and asserts the single-photo path
fires as the fallback.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
[ci skip]
EOF
)"
```

---

## Phase 6 — Final verification

## Task 12: Full pre-merge sweep

- [ ] **Step 1: Run all the lint passes one more time**

```bash
# PHP
REVIEW_STANDARD=Drupal ci-scripts/test_coder.sh
REVIEW_STANDARD=DrupalPractice ci-scripts/test_coder.sh

# Elm
cd client && elm-format --validate src/
cd client && rm -rf elm-stuff && npx elm-review

# Shell (no shell scripts added, but standard sweep)
./ci-scripts/test_shell.sh

# Drupal SimpleTest
ddev simpletest --class=HedleyRestfulBulkPhotosTest

# Elm tests
cd client && elm-test

# E2E
cd client && ./node_modules/.bin/playwright test
```

Expected: all green.

- [ ] **Step 2: Run a full sync against a realistic-sized health center**

If a fixture or local clone of a large-HC dataset is available, run an initial sync end-to-end and measure wall-clock time before vs after. Compare against the spec's estimate (~75min → ~28min). Record the result in the commit message for the final consolidation commit, or open a follow-up issue with the numbers.

- [ ] **Step 3: Push the branch (only on user request)**

Ask: "All checks green. Want me to push `bulk-photo-fetch` to `origin`? (PR creation separate — let me know if you want that too.)"

On approval (only):
```bash
git push -u origin bulk-photo-fetch
```

If user wants a PR, follow the gh-cli pattern in the project's commit guidance. Title suggestion: `Bulk photo fetch for deferred-photo download`. Body should reference `docs/superpowers/specs/2026-05-13-bulk-photo-fetch-design.md` and `Issue #N.` per the project's PR convention (`MEMORY.md` — never `Closes`/`Fixes`).

---

## Self-Review Notes (already applied)

- **Spec coverage:** every section of the spec maps to at least one task:
  - §"Server endpoint contract" → Tasks 1–4
  - §"Client orchestration / Elm + JS" → Tasks 6–10
  - §"Cache integration with the service worker" → Task 5
  - §"Sizing and performance estimate" → manual check in Task 12
  - §"Backwards compatibility" → Task 10 Step 5 (404 path) + Task 11 fallback test
  - §"Things to verify during implementation" → embedded as notes in Tasks 3 (`itok`), 6 (browser APIs), 5 (cache key parity)
- **Type consistency:** `PhotoBatchResult` defined in Task 8, used identically in Tasks 9 (Msg signature) and 10 (handler). `IndexDbQueryDeferredPhotoBatchResultRecord` same.
- **Placeholders:** the few `<existing-…>` markers in Task 10 are intentional pointers to read-and-mirror existing code, not TODOs to invent — each is paired with a `grep` or line-number reference to find the source.
- **Open dependencies on existing code that the implementer will resolve by reading the existing file:**
  - Exact name of `sendQueryToIndexDb` helper in `Update.elm` (used in many tasks)
  - Exact shape of port subscriptions (do they pass a decoded value or a raw `Json.Value`?)
  - Exact name of the helper that creates the test device + token + image-style URL in Drupal SimpleTest
  Each is flagged at the point of use so the implementer doesn't guess.
