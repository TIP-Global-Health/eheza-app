import { test, expect } from '@playwright/test';
import { click, login, setupDevice } from './helpers/auth';
import { WAIT } from './helpers/common';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';

// Exercises the /api/bulk-photos pipeline added in PR #1742 (Issue #1741).
//
// `setupDevice` pairs and waits for metadata sync to settle. The
// deferred-photo download phase runs *after* that, so each test polls
// the network for ~2 minutes after setup before asserting on cache
// state. The fixture HC has >100 photos, so multiple batches normally
// fire during this window.
test.describe('Bulk Photo Fetch', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async () => {
    resetDevice();
  });

  // Scenario: initial sync against a fresh device.
  // What's verified:
  //   - At least one POST /api/bulk-photos response with status 200.
  //   - We stay on the Device Status page until bulk fetch fully
  //     settles (network goes quiet on /api/bulk-photos for 5s) before
  //     moving on — mirrors a user who waits for sync to finish.
  //   - The "photos" Cache Storage ends up with entries.
  //   - A real <img> from the Patient Directory renders (naturalWidth > 0),
  //     proving the cached blob is served back to <img> by the SW.
  test('bulk endpoint is used during initial sync and populates "photos" cache', async ({ page }) => {
    const bulkResponses: Array<{ status: number }> = [];
    page.on('response', (res) => {
      if (res.url().includes('/api/bulk-photos')) {
        bulkResponses.push({ status: res.status() });
      }
    });

    // Inline the setupDevice flow so we can stay on the Device Status
    // page until bulk fetch is fully done before navigating back.
    await login(page);
    await click(page.locator('.icon-task-device-status'), page);
    await page.locator('.device-status').waitFor({ timeout: 10000 });

    const hcSection = page.locator('.health-center', {
      has: page.locator('h2', { hasText: 'Nyange Health Center' }),
    });
    await hcSection.waitFor({ timeout: 10000 });

    const startBtn = hcSection.locator('button.ui.button', { hasText: 'Start Syncing' });
    if (await startBtn.isVisible()) {
      await click(startBtn, page);
    }

    // Wait for metadata sync to reach Status: Success.
    await hcSection.locator('.sync-status', { hasText: 'Status: Success' })
      .waitFor({ timeout: 120000 });

    // Wait for at least one bulk POST.
    await expect
      .poll(() => bulkResponses.length, {
        message: 'expected at least one /api/bulk-photos response',
        timeout: 120000,
      })
      .toBeGreaterThan(0);

    // Stay on Device Status until the bulk fetcher has been quiet for 5
    // consecutive seconds — signals deferredPhotos is drained.
    const quietWindowMs = 5000;
    const maxWaitMs = 180000;
    const startedAt = Date.now();
    let lastCount = bulkResponses.length;
    let lastChangeAt = Date.now();
    while (Date.now() - startedAt < maxWaitMs) {
      if (Date.now() - lastChangeAt >= quietWindowMs) break;
      await page.waitForTimeout(500);
      if (bulkResponses.length !== lastCount) {
        lastCount = bulkResponses.length;
        lastChangeAt = Date.now();
      }
    }
    expect(Date.now() - lastChangeAt, 'bulk fetch did not go quiet within timeout').toBeGreaterThanOrEqual(quietWindowMs);

    // Every bulk response should be 200 (no transient failures, no 404).
    expect(bulkResponses.every((r) => r.status === 200)).toBe(true);

    // Navigate back to Dashboard for the visual verification.
    await page.goBack();
    await page.locator('.wrap-cards').waitFor({ timeout: 10000 });

    // The "photos" Cache should have entries written by bulkPhotos.js
    // (and possibly augmented by the service worker for any single-photo
    // requests). Either way, the cache must not be empty.
    const cacheEntries = await page.evaluate(async () => {
      const cache = await caches.open('photos');
      const keys = await cache.keys();
      return keys.length;
    });
    expect(cacheEntries).toBeGreaterThan(0);

    // End-to-end visual check: the user can actually see a downloaded
    // photo. Navigate to Patient Directory, search for "a" (matches many
    // fixture names), and verify at least one result-row thumbnail
    // rendered with naturalWidth > 0 — proving the cached blob was
    // served back to an <img> by the service worker.
    await click(page.locator('.icon-task-participant-directory'), page);
    await page.locator('.page-people').waitFor({ timeout: 15000 });

    await page.locator('input.search-input').fill('a');

    // Wait for the first result with a thumbnail <img>. Persons without
    // an avatar render a `.icon-participant` span instead, so we wait
    // specifically for an img inside a result row.
    await page.locator('.item.participant-view img').first()
      .waitFor({ timeout: 20000 });
    // Allow the browser a moment to decode images from cache.
    await page.waitForTimeout(WAIT.sectionTransition);

    const imgCount = await page.locator('.item.participant-view img').count();
    let foundLoadedImage = false;
    for (let i = 0; i < imgCount; i++) {
      const naturalWidth = await page.locator('.item.participant-view img').nth(i)
        .evaluate((el: HTMLImageElement) => el.naturalWidth);
      if (naturalWidth > 0) {
        foundLoadedImage = true;
        break;
      }
    }
    expect(foundLoadedImage, 'at least one Patient Directory thumbnail rendered with naturalWidth > 0').toBe(true);
  });

  // Scenario: bulk endpoint unavailable (404 — simulates an older
  // server that doesn't have the new route deployed yet).
  // What's verified:
  //   - The bulk endpoint IS attempted at least once.
  //   - After 404, the client falls back to the single-photo path
  //     (GETs to /system/files/styles/.../?itok=…).
  //   - The "photos" Cache still populates — the feature degrades
  //     gracefully rather than breaking photo sync.
  test('falls back to single-photo path when bulk endpoint returns 404', async ({ page }) => {
    // Intercept the new route before any navigation so the very first
    // bulk-fetch attempt sees the 404.
    await page.route('**/api/bulk-photos*', (route) =>
      route.fulfill({ status: 404, body: '' }),
    );

    const bulkAttempts: number[] = [];
    const singlePhotoFetches: string[] = [];
    page.on('response', (res) => {
      const url = res.url();
      if (url.includes('/api/bulk-photos')) {
        bulkAttempts.push(res.status());
      }
      else if (
        res.request().method() === 'GET'
        && /\/system\/files\/styles\/[^?]*\?.*itok=/.test(url)
      ) {
        singlePhotoFetches.push(url);
      }
    });

    await setupDevice(page);

    // The client should attempt bulk fetch at least once (gets 404),
    // then switch to the single-photo path for the remainder.
    await expect
      .poll(() => bulkAttempts.length, {
        message: 'expected at least one /api/bulk-photos attempt (intercepted as 404)',
        timeout: 120000,
      })
      .toBeGreaterThan(0);
    expect(bulkAttempts.every((s) => s === 404)).toBe(true);

    await expect
      .poll(() => singlePhotoFetches.length, {
        message: 'expected single-photo GETs after bulk-endpoint 404',
        timeout: 120000,
      })
      .toBeGreaterThan(0);

    const cacheEntries = await page.evaluate(async () => {
      const cache = await caches.open('photos');
      const keys = await cache.keys();
      return keys.length;
    });
    expect(cacheEntries).toBeGreaterThan(0);
  });
});
