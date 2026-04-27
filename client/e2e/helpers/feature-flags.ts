import { Page, expect } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { syncAndWait } from './common';
import { drushEnv } from './device';

/**
 * Toggle a `hedley_admin_feature_<name>_enabled` Drupal variable.
 *
 * After changing a flag, the device must sync (via the sync icon /
 * `syncAndWait`) for the new value to reach the Elm SyncManager
 * `features` set; only then will gated UI re-render.
 *
 * @param name - Short feature name, e.g. 'antenatal', 'gps_coordinates',
 *               'stock_management_hc'. The full variable name is built
 *               by prepending 'hedley_admin_feature_' and appending
 *               '_enabled'.
 * @param enabled - true sets the variable to 1, false sets it to 0.
 *
 * Note: execSync with a hardcoded variable-name pattern — `name` is
 * validated as snake_case to prevent injection. Same pattern as
 * ensureStockManagementHCFeatureEnabled in stock-management.ts.
 */
export function setFeatureFlag(name: string, enabled: boolean) {
  if (!/^[a-z][a-z0-9_]*$/.test(name)) {
    throw new Error(`Invalid feature flag name: "${name}"`);
  }

  const { drushCmd, cwd } = drushEnv();
  const value = enabled ? '1' : '0';
  execSync(`${drushCmd} vset hedley_admin_feature_${name}_enabled ${value}`, {
    cwd,
    timeout: 15000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
}

/**
 * Verify that a SiteFeature flag actually gates an encounter-type
 * button on the Individual Encounter Types page.
 *
 * Pre: device is logged in and sitting on the PinCode dashboard.
 * Post: device is back on the PinCode dashboard with the flag ON, ready
 *       for the existing register-and-encounter flow to take over.
 *
 * Steps:
 *  1. Set flag OFF on backend, sync; device pulls OFF state.
 *  2. Navigate Dashboard → Clinical → Individual Assessment.
 *  3. Assert the button is hidden.
 *  4. Set flag ON, sync.
 *  5. Assert the button is visible.
 *  6. Navigate back to PinCode (two `link-back` clicks).
 *
 * @param page         - Playwright page (logged in, on PinCode).
 * @param flagName     - Feature flag short name, e.g. 'antenatal'.
 * @param buttonText   - Visible label of the encounter-type button,
 *                       e.g. 'Antenatal Care'. Role-dependent for some
 *                       (Well Child = 'Standard Pediatric Visit' for
 *                       nurse, 'Well Child Visit' for CHW).
 */
export async function verifyFeatureGatesEncounterButton(
  page: Page,
  flagName: string,
  buttonText: string,
) {
  // 1. Force feature OFF on backend, sync.
  setFeatureFlag(flagName, false);
  await syncAndWait(page);

  // 2. Navigate Dashboard → Clinical → Individual Assessment.
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // 3. Button must be hidden.
  const button = page.locator('button.encounter-type', { hasText: buttonText });
  await expect(
    button,
    `"${buttonText}" button must be hidden when feature ${flagName} is off`,
  ).toBeHidden();

  // 4. Re-enable feature and sync. After syncAndWait we land back on
  //    the encounter-types page (page.goBack from device status).
  setFeatureFlag(flagName, true);
  await syncAndWait(page);

  // 5. Button must now be visible.
  await expect(
    button,
    `"${buttonText}" button must be visible when feature ${flagName} is on`,
  ).toBeVisible();

  // 6. Navigate back to PinCode so callers can resume from the dashboard.
  await click(page.locator('.link-back .icon-back'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('.link-back .icon-back'), page);
  await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
}
