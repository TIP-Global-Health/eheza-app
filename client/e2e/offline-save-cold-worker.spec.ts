import { test, expect, Page } from '@playwright/test';

// Chrome stops an idle service worker, for example while a form is being filled
// in. Saving then starts it again with several requests arriving together.
// Each update must still be queued for upload.
const SHARD = 'e2e0c01d-0000-4000-8000-000000000001';
const ROUNDS = 20;

// Opens the app's IndexedDB and waits until its service worker controls the page.
async function openApp(page: Page) {
  await page.goto('/');
  await expect.poll(async () => {
    return page.evaluate(async () => {
      if (!navigator.serviceWorker.controller) {
        return false;
      }
      // @ts-ignore -- `dbSync` is the app's global Dexie instance (app.js).
      await dbSync.open();
      return true;
    }).catch(() => false);
  }, { timeout: 90000, intervals: [2000] }).toBe(true);
}

// Counts the queued PATCH upload rows for one node.
async function countPatchRows(page: Page, uuid: string): Promise<number> {
  return page.evaluate((uuid) => new Promise<number>((resolve, reject) => {
    const open = indexedDB.open('sync');
    open.onsuccess = () => {
      const db = open.result;
      const request = db.transaction('shardChanges').objectStore('shardChanges').getAll();
      request.onsuccess = () => {
        db.close();
        resolve(request.result.filter((row: any) => row.uuid === uuid && row.method === 'PATCH').length);
      };
      request.onerror = () => reject(request.error);
    };
    open.onerror = () => reject(open.error);
  }), uuid);
}

test.describe('Offline saves', () => {
  test('an update made right after the service worker starts is queued for upload', async ({ page, context }) => {
    test.setTimeout(300000);
    await openApp(page);

    const uuid = await page.evaluate(async (shard) => {
      const response = await fetch('/sw/nodes/individual_participant', {
        method: 'POST',
        body: JSON.stringify({ shard, person: 'e2e-person', encounter_type: 'antenatal', deleted: false }),
      });
      return (await response.json()).data[0].uuid;
    }, SHARD);

    const cdp = await context.newCDPSession(page);
    await cdp.send('ServiceWorker.enable');

    const lost: string[] = [];
    for (let round = 0; round < ROUNDS; round++) {
      await cdp.send('ServiceWorker.stopAllWorkers');
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
      if (added !== 1 || statuses[3] !== '200') {
        lost.push(`round ${round}: PATCH ${statuses[3]}, upload rows added ${added}`);
      }
    }

    expect(lost).toEqual([]);
  });
});
