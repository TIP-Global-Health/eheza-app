import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import { setFeatureFlag } from './helpers/feature-flags';

// Standalone spec — verifies that the FeatureGPSCoordinates flag actually
// gates the "GPS Info" section on the patient registration form. Kept
// separate from the encounter specs so the toggle dance does not run on
// every patient registration.

test.describe('Feature: GPS Coordinates gating on registration form', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as nurse (registration form is similar across roles; nurse has
  // the cascading address dropdowns but those are separate from the GPS
  // section).
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('GPS Info section appears and disappears with the flag', async ({ page }) => {
    // Navigate to a registration form. We use the Antenatal Care flow
    // because the form is shared across encounter types and Antenatal
    // is enabled by default for nurse.
    await click(page.locator('.icon-task-clinical'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
    await click(page.locator('button.individual-assessment'), page);
    await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });
    await click(
      page.locator('button.encounter-type', { hasText: 'Antenatal Care' }),
      page,
    );
    await page.locator('div.page-participants').waitFor({ timeout: 10000 });
    await click(
      page.locator('button.ui.primary.button.fluid', {
        hasText: 'Register a new participant',
      }),
      page,
    );
    await page
      .locator('.ui.grid .column', { hasText: 'First Name:' })
      .waitFor({ timeout: 10000 });

    // gpsInfoSection in Pages/Person/View.elm renders a fieldset with
    // class "registration-form gps-info" iff gpsCoordinatesEnabled.
    const gpsSection = page.locator('fieldset.gps-info');

    // Force flag OFF and sync; goBack from device status returns to the
    // registration form (Elm preserves the form state across page nav).
    setFeatureFlag('gps_coordinates', false);
    await syncAndWait(page);
    await expect(
      gpsSection,
      'fieldset.gps-info must be hidden when feature gps_coordinates is off',
    ).toBeHidden();

    // Force flag ON and sync.
    setFeatureFlag('gps_coordinates', true);
    await syncAndWait(page);
    await expect(
      gpsSection,
      'fieldset.gps-info must be visible when feature gps_coordinates is on',
    ).toBeVisible();
  });
});
