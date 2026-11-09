import { test, expect, Page } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { WAIT } from './helpers/common';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  addChildToMother,
  clickMotherCard,
  completeChildFbf,
  completeFamilyPlanning,
  completeHeight,
  completeLactation,
  completeMotherFbf,
  completeMuac,
  completeNutritionSigns,
  completePhoto,
  completeWeight,
  createMotherOnAttendancePage,
  goToParticipantsPage,
  navigateBackToAttendance,
  navigateToChild,
  navigateToMother,
  navigateToNurseGroupSession,
  openActivity,
} from './helpers/group-session';

const HEIGHT_INPUT = 'input[type="number"][name="height"]';

// Installed before the app loads. Watches the app's requests that create a
// record, and can hold or fail those of one type. A request handled by the
// service worker never reaches `page.route`, so this wraps XMLHttpRequest.
// Also repeats a real tap on Save 50 ms later when asked to, which is a quick
// double tap that does not depend on the helpers' timing.
function installSaveControls() {
  const w = window as any;
  w.__saveControl = { type: null, mode: 'pass', held: [] as Array<() => void> };
  w.__createRequests = [] as Array<{ type: string; person: string }>;
  w.__repeatSaveTap = false;

  const open = XMLHttpRequest.prototype.open as any;
  const send = XMLHttpRequest.prototype.send as any;

  XMLHttpRequest.prototype.open = function (this: any, method: string, url: string, ...rest: any[]) {
    const match = /\/sw\/nodes\/([a-z_]+)(\?|$)/.exec(url);
    this.__nodeType = method === 'POST' && match ? match[1] : null;
    return open.call(this, method, url, ...rest);
  } as any;

  XMLHttpRequest.prototype.send = function (this: any, body?: any) {
    const type = this.__nodeType;
    if (!type) {
      return send.call(this, body);
    }

    let person = null;
    try {
      person = JSON.parse(body).person;
    } catch (error) {
      // Not a JSON request, so it is not one this test counts.
    }
    w.__createRequests.push({ type, person });

    const control = w.__saveControl;
    if (control.type === type && control.mode === 'hold') {
      control.held.push(() => send.call(this, body));
      return undefined;
    }
    if (control.type === type && control.mode === 'fail') {
      this.dispatchEvent(new Event('error'));
      return undefined;
    }
    return send.call(this, body);
  } as any;

  document.addEventListener(
    'click',
    (event) => {
      const button = (event.target as Element | null)?.closest?.('#save-form') as HTMLElement | null;
      if (w.__repeatSaveTap && event.isTrusted && button) {
        setTimeout(() => button.click(), 50);
      }
    },
    true,
  );
}

async function setSaveControl(page: Page, type: string | null, mode: 'pass' | 'hold' | 'fail') {
  await page.evaluate(
    ([type, mode]) => {
      const control = (window as any).__saveControl;
      control.type = type;
      control.mode = mode;
    },
    [type, mode],
  );
}

async function releaseHeldSaves(page: Page) {
  await page.evaluate(() => {
    const control = (window as any).__saveControl;
    control.mode = 'pass';
    control.held.splice(0).forEach((resume: () => void) => resume());
  });
}

async function setRepeatSaveTap(page: Page, repeat: boolean) {
  await page.evaluate((repeat) => {
    (window as any).__repeatSaveTap = repeat;
  }, repeat);
}

// How many requests to create a record of this type the app has sent.
async function createRequests(page: Page, type: string): Promise<number> {
  return page.evaluate(
    (type) => (window as any).__createRequests.filter((request: any) => request.type === type).length,
    type,
  );
}

// The records of this type stored on the device for the person the app saved
// them for. Read from the device, so the count does not depend on a sync.
async function storedRecords(page: Page, type: string): Promise<any[]> {
  return page.evaluate(async (type) => {
    const request = (window as any).__createRequests.find((request: any) => request.type === type);
    if (!request) {
      return [];
    }
    // @ts-ignore -- `dbSync` is the app's global Dexie instance (app.js).
    return dbSync.shards.where('type').equals(type).filter((row: any) => row.person === request.person).toArray();
  }, type);
}

// Registers a mother and a 12-month-old child in an FBF group session, and
// opens the child's page.
async function openChildOfNewFamily(page: Page) {
  await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');

  const mother = await createMotherOnAttendancePage(page, { isChw: false });
  await addChildToMother(page, { ageMonths: 12, isChw: false });

  if (await page.locator('div.page-person').isVisible()) {
    await navigateBackToAttendance(page);
  }
  await page.locator('div.page-attendance').waitFor({ timeout: 10000 });
  await goToParticipantsPage(page);

  await clickMotherCard(page, mother.firstName);
  await navigateToChild(page);
}

test.describe('Group Nutrition: Save button on measurement forms', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    await page.addInitScript(installSaveControls);
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
    await openChildOfNewFamily(page);
  });

  test('Tapping Save twice quickly stores one record on every measurement form', async ({ page }) => {
    // Scenario: every Save below is followed by a second tap 50 ms later.
    // Child activities: Height, Weight, MUAC, Nutrition, Photo, FBF.
    // Mother activities: Family Planning, Lactation, FBF.
    // Verifies: one record of each type on the device for that person.
    await setRepeatSaveTap(page, true);

    await completeHeight(page, '70');
    await completeWeight(page, '8.5');
    await completeMuac(page, '14');
    await completeNutritionSigns(page);
    await completePhoto(page);
    await completeChildFbf(page);

    await navigateToMother(page);
    await completeFamilyPlanning(page);
    await completeLactation(page);
    await completeMotherFbf(page);

    await page.waitForTimeout(WAIT.pageNavigation);

    for (const type of [
      'child_fbf',
      'family_planning',
      'height',
      'lactation',
      'mother_fbf',
      'muac',
      'nutrition',
      'photo',
      'weight',
    ]) {
      expect((await storedRecords(page, type)).length, `records of ${type}`).toBe(1);
    }
  });

  test('While a save is in progress, the button shows that and ignores further taps', async ({ page }) => {
    // Scenario: the Height save is held open, so the form stays in its "saving"
    // state for as long as the test needs.
    // Verifies: the button shows the save in progress, a further tap sends
    // nothing, and one record exists once the save is let through.
    await openActivity(page, 'Height');
    await page.locator(HEIGHT_INPUT).fill('70');
    await page.waitForTimeout(WAIT.formInteraction);

    await setSaveControl(page, 'height', 'hold');
    const save = page.locator('#save-form');
    await click(save, page);
    await expect(save).toHaveClass(/(^| )loading( |$)/, { timeout: 5000 });

    await save.click({ force: true });
    await page.waitForTimeout(WAIT.elmRerender);
    expect(await createRequests(page, 'height'), 'requests to create the record').toBe(1);

    await releaseHeldSaves(page);
    await expect.poll(async () => (await storedRecords(page, 'height')).length).toBe(1);
  });

  test('Once saved, the value can be changed, and Save updates that same record', async ({ page }) => {
    // Scenario: Height is saved, then reopened from the Completed tab and
    // changed.
    // Verifies: still one record for the child, carrying the new value.
    await completeHeight(page, '70');
    await page.waitForTimeout(WAIT.pageNavigation);

    await click(page.locator('#completed-tab'), page);
    await page.waitForTimeout(WAIT.elmRerender);
    await openActivity(page, 'Height');
    await page.locator(HEIGHT_INPUT).fill('72');
    await page.waitForTimeout(WAIT.formInteraction);
    await click(page.locator('#save-form'), page);

    await expect
      .poll(async () => (await storedRecords(page, 'height')).map((record) => record.height), { timeout: 15000 })
      .toEqual([72]);
  });

  test('Saving for one person never blocks Save for the next activity, even immediately afterwards', async ({ page }) => {
    // Scenario: Weight is opened and saved straight after the Height save,
    // without the pause the other helpers leave.
    // Verifies: both records are stored.
    await openActivity(page, 'Height');
    await page.locator(HEIGHT_INPUT).fill('70');
    await page.waitForTimeout(WAIT.formInteraction);
    await click(page.locator('#save-form'), page);

    // The activity cards move when Height leaves the list, so a tap can land
    // on the wrong card. The first try is still immediate.
    await expect(async () => {
      await openActivity(page, 'Weight');
      await page.locator('input[type="number"][name="weight"]').waitFor({ timeout: 2000 });
    }).toPass({ timeout: 20000 });
    await completeWeight(page, '8.5');

    await expect.poll(async () => (await storedRecords(page, 'height')).length).toBe(1);
    await expect.poll(async () => (await storedRecords(page, 'weight')).length).toBe(1);
  });

  test('If a save fails, the error shows and Save works again', async ({ page }) => {
    // Scenario: the first Height save fails, the second goes through.
    // Verifies: the button and the form show the error, the same form saves on
    // the next tap, and one record exists.
    await openActivity(page, 'Height');
    await page.locator(HEIGHT_INPUT).fill('70');
    await page.waitForTimeout(WAIT.formInteraction);

    await setSaveControl(page, 'height', 'fail');
    const save = page.locator('#save-form');
    await click(save, page);

    await expect(save).toHaveClass(/(^| )negative( |$)/, { timeout: 5000 });
    await expect(page.getByText('Save Error', { exact: true })).toBeVisible();
    await expect(save).not.toHaveClass(/(^| )loading( |$)/);

    await setSaveControl(page, null, 'pass');
    await click(save, page);

    await expect.poll(async () => (await storedRecords(page, 'height')).length, { timeout: 15000 }).toBe(1);
  });
});
