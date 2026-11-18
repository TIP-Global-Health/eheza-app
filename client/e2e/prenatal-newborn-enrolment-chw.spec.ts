import { test, expect, Page } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { syncAndWait } from './helpers/common';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  completeChwPostpartumVisit,
  createAdultFemaleAndStartEncounter,
  endPrenatalEncounter,
  queryPregnancyNewborn,
} from './helpers/prenatal';

// The app drops data no page has asked for in five minutes. Leave the form
// open a little longer than that, so at least one check runs past the mark.
const FORM_OPEN_MS = 6 * 60 * 1000;

/**
 * Reload the app on the newborn registration form and get back to it. A
 * reload may ask for the PIN (and the village) again; the form's URL is kept
 * and reopened after signing in. Reloads again on the "Not Found" race.
 */
async function reloadOnRegistrationForm(page: Page) {
  const formUrl = page.url();
  const firstName = page.locator('.ui.grid').filter({ hasText: 'First Name:' }).locator('input');
  const pinInput = page.locator('input[name="pincode"]');

  await page.reload();
  for (let attempt = 0; attempt < 5; attempt++) {
    await Promise.race([
      firstName.first().waitFor({ timeout: 30000 }),
      pinInput.waitFor({ timeout: 30000 }),
      page.getByText('Not Found').first().waitFor({ timeout: 30000 }),
    ]).catch(() => undefined);

    if (await firstName.first().isVisible().catch(() => false)) return;

    if (await pinInput.isVisible().catch(() => false)) {
      await pinInput.fill('2345');
      await click(page.getByRole('button', { name: 'Sign In' }), page);
      const selectLocation = page.locator('p.select-location');
      await Promise.race([
        selectLocation.waitFor({ timeout: 30000 }),
        page.locator('button.ui.button.logout').waitFor({ timeout: 30000 }),
      ]).catch(() => undefined);
      if (await selectLocation.isVisible().catch(() => false)) {
        await click(page.locator('button.ui.primary.button', { hasText: 'Akanduga' }), page);
      }
      await page.goto(formUrl);
      continue;
    }

    await page.reload();
  }
}

test.describe('CHW: Postpartum visit, Enrol newborn', () => {
  test.describe.configure({ timeout: 900000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as CHW Jojo (PIN 2345), select Akanduga village.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  // Scenario: CHW postpartum visit after a live birth. On Next Steps, Enrol
  // newborn opens the registration form, and the app reloads while it is open.
  // The baby is then registered and saved.
  // Checks: back on Next Steps the Enrol newborn task reads done and its button
  // is disabled (inside completeChwPostpartumVisit). After sync, the backend
  // pregnancy links that baby as its newborn, which is what other devices
  // download, and only one person carries the baby's name.
  test('The same holds when the app reloads while the registration form is open, and the baby is then saved', async ({ page }) => {
    const { fullName: motherName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: true,
      encounterType: 'postpartum',
    });

    const newbornName = await completeChwPostpartumVisit(page, {
      onRegistrationForm: reloadOnRegistrationForm,
    });
    expect(newbornName).not.toBeNull();
    await endPrenatalEncounter(page);
    await syncAndWait(page);

    const linked = queryPregnancyNewborn(motherName, newbornName!);
    expect(linked.newborn, 'the pregnancy links the baby as its newborn').toBe(newbornName);
    expect(linked.peopleNamedAsNewborn, 'the baby is registered once').toBe(1);
  });

  // Scenario: as above, but instead of a reload the registration form stays
  // open for six minutes before it is filled in and saved.
  // Checks: the same as above.
  test('Registering the baby from Enrol newborn links it to the pregnancy, however long the registration form was open before Save', async ({ page }) => {
    const { fullName: motherName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: true,
      encounterType: 'postpartum',
    });

    const newbornName = await completeChwPostpartumVisit(page, {
      onRegistrationForm: async (p) => {
        await p.waitForTimeout(FORM_OPEN_MS);
      },
    });
    expect(newbornName).not.toBeNull();
    await endPrenatalEncounter(page);
    await syncAndWait(page);

    const linked = queryPregnancyNewborn(motherName, newbornName!);
    expect(linked.newborn, 'the pregnancy links the baby as its newborn').toBe(newbornName);
    expect(linked.peopleNamedAsNewborn, 'the baby is registered once').toBe(1);
  });
});
