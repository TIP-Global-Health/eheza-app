import { test, expect, Page } from '@playwright/test';

// Chrome stops an idle service worker, for example while a form is being filled
// in. Saving then starts it again with several requests arriving together.
// Each update must still be queued for upload.
const SHARD = 'e2e0c01d-0000-4000-8000-000000000001';
const ROUNDS = 20;

// Loads the app under its service worker. A page the worker has just taken over
// reloads itself, so the test reloads first. Opening `dbSync` creates the 'sync'
// database, which the worker needs before it answers.
async function openApp(page: Page) {
  await page.goto('/');
  await expect.poll(async () => {
    return page.evaluate(() => !!navigator.serviceWorker.controller).catch(() => false);
  }, { timeout: 90000, intervals: [1000] }).toBe(true);
  await page.reload();
  await page.evaluate(async () => {
    // @ts-ignore -- `dbSync` is the app's global Dexie instance (app.js).
    await dbSync.open();
  });
}

// Counts the queued PATCH upload rows for one node.
async function countPatchRows(page: Page, uuid: string): Promise<number> {
  return page.evaluate((uuid) => {
    // @ts-ignore -- `dbSync` is the app's global Dexie instance (app.js).
    return dbSync.shardChanges.where('uuid').equals(uuid).filter((row: any) => row.method === 'PATCH').count();
  }, uuid);
}

test.describe('Offline saves', () => {
  test('an update made right after the service worker starts is queued for upload', async ({ page, context }) => {
    test.setTimeout(300000);
    await openApp(page);

    const created = await page.evaluate(async (shard) => {
      const response = await fetch('/sw/nodes/individual_participant', {
        method: 'POST',
        body: JSON.stringify({ shard, person: 'e2e-person', encounter_type: 'antenatal', deleted: false }),
      });
      return { status: response.status, body: await response.text() };
    }, SHARD);
    expect(created.status, 'the participant was not created').toBe(200);
    const uuid = JSON.parse(created.body).data[0].uuid;

    // Tracks the running status of the active worker.
    const cdp = await context.newCDPSession(page);
    let workerStatus = '';
    cdp.on('ServiceWorker.workerVersionUpdated', (event) => {
      event.versions
        .filter((version) => version.status === 'activated')
        .forEach((version) => { workerStatus = version.runningStatus; });
    });
    await cdp.send('ServiceWorker.enable');

    const failed: string[] = [];
    for (let round = 0; round < ROUNDS; round++) {
      await cdp.send('ServiceWorker.stopAllWorkers');
      await expect.poll(() => workerStatus, { message: 'the service worker did not stop', timeout: 10000 }).toBe('stopped');
      const before = await countPatchRows(page, uuid);

      // The requests a pregnancy dating save sends, staggered by a few
      // milliseconds as they reach a worker that is still starting.
      const statuses = await page.evaluate(async ([uuid, shard, round]) => {
        const later = (ms: number, send: () => Promise<Response>) =>
          new Promise(resolve => setTimeout(resolve, ms)).then(send);
        const stagger = (round % 8) * 3;
        const results = await Promise.allSettled([
          fetch('/sw/nodes/person?limit=1'),
          later(stagger, () => fetch('/sw/nodes/individual_participant/' + uuid)),
          later(stagger * 2, () => fetch('/sw/nodes/last_menstrual_period', {
            method: 'POST',
            body: JSON.stringify({ health_center: shard, prenatal_encounter: 'e2e-encounter', date_measured: '2026-10-25', last_menstrual_period: '2026-08-01', deleted: false }),
          })),
          later(stagger, () => fetch('/sw/nodes/individual_participant/' + uuid, {
            method: 'PATCH',
            body: JSON.stringify({ expected_date_concluded: '2027-05-08' }),
          })),
          later(stagger * 3, () => fetch('/sw/nodes/health_center')),
        ]);
        return results.map(result => result.status === 'fulfilled' ? String(result.value.status) : 'network error');
      }, [uuid, SHARD, round] as const);

      const added = (await countPatchRows(page, uuid)) - before;
      if (added !== 1 || statuses.some(status => status !== '200')) {
        failed.push(`round ${round}: statuses ${statuses.join(', ')}; PATCH upload rows added ${added}`);
      }
    }

    expect(failed).toEqual([]);
  });
});
