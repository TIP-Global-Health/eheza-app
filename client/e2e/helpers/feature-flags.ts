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

/**
 * Verify that a SiteFeature flag gates one of the assessment buttons
 * on the Clinical page (Individual / Group / Family Assessment).
 *
 * Used for:
 *  - FeatureFamilyNutrition gates `button.family-assessment` (CHW only)
 *  - FeatureNutritionGroup gates `button.group-assessment` (NURSE only —
 *    on CHW the gate is compound with FeatureGroupEducation, so test
 *    that one via verifyFeatureGatesGroupEncounterButton instead)
 *
 * Pre/Post: device is on PinCode dashboard.
 *
 * @param page        - Playwright page.
 * @param flagName    - Feature flag short name.
 * @param buttonClass - CSS class of the button on Clinical page,
 *                      e.g. 'family-assessment', 'group-assessment'.
 */
export async function verifyFeatureGatesClinicalAssessmentButton(
  page: Page,
  flagName: string,
  buttonClass: string,
) {
  setFeatureFlag(flagName, false);
  await syncAndWait(page);

  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  const button = page.locator(`button.${buttonClass}`);
  await expect(
    button,
    `button.${buttonClass} must be hidden when feature ${flagName} is off`,
  ).toBeHidden();

  setFeatureFlag(flagName, true);
  await syncAndWait(page);

  await expect(
    button,
    `button.${buttonClass} must be visible when feature ${flagName} is on`,
  ).toBeVisible();

  await click(page.locator('.link-back .icon-back'), page);
  await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
}

/**
 * Verify that a SiteFeature flag gates a button on the Group Encounter
 * Types page. Used for CHW where the entry button on Clinical is
 * compound-gated by FeatureNutritionGroup OR FeatureGroupEducation.
 *
 * The caller must specify `partnerFlag` — the OTHER group flag — which
 * the helper sets to ON so the Group Assessment entry button on
 * Clinical remains visible while we toggle the target flag.
 *
 * Pre/Post: device is on PinCode dashboard.
 *
 * @param page         - Playwright page.
 * @param flagName     - Target feature flag (e.g. 'nutrition_group').
 * @param buttonText   - Button label inside Group Encounter Types,
 *                       e.g. 'Child Nutrition' or 'Health Education'.
 * @param partnerFlag  - The other group flag to keep ON for navigation
 *                       (e.g. 'group_education' when testing
 *                       'nutrition_group').
 */
export async function verifyFeatureGatesGroupEncounterButton(
  page: Page,
  flagName: string,
  buttonText: string,
  partnerFlag: string,
) {
  setFeatureFlag(partnerFlag, true);
  setFeatureFlag(flagName, false);
  await syncAndWait(page);

  // Navigate Clinical → Group Assessment → Group Encounter Types.
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('button.group-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  const button = page.locator('button.encounter-type', { hasText: buttonText });
  await expect(
    button,
    `"${buttonText}" button must be hidden when feature ${flagName} is off`,
  ).toBeHidden();

  setFeatureFlag(flagName, true);
  await syncAndWait(page);

  await expect(
    button,
    `"${buttonText}" button must be visible when feature ${flagName} is on`,
  ).toBeVisible();

  // Back: Group Encounter Types → Clinical → PinCode.
  await click(page.locator('.link-back .icon-back'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('.link-back .icon-back'), page);
  await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
}

/**
 * Verify that a SiteFeature flag gates an activity-card icon on the
 * PinCode dashboard. Used for FeatureStockManagementHC (nurse) and
 * FeatureStockManagementVillage (CHW) — both gate `.icon-task-stock-
 * management`.
 *
 * Pre/Post: device is on PinCode dashboard.
 *
 * @param page     - Playwright page.
 * @param flagName - Feature flag short name.
 * @param iconKey  - Icon class suffix, e.g. 'stock-management' for
 *                   `.icon-task-stock-management`.
 */
export async function verifyFeatureGatesPinCodeMenuIcon(
  page: Page,
  flagName: string,
  iconKey: string,
) {
  setFeatureFlag(flagName, false);
  await syncAndWait(page);

  const icon = page.locator(`.icon-task-${iconKey}`);
  await expect(
    icon,
    `.icon-task-${iconKey} must be hidden when feature ${flagName} is off`,
  ).toBeHidden();

  setFeatureFlag(flagName, true);
  await syncAndWait(page);

  await expect(
    icon,
    `.icon-task-${iconKey} must be visible when feature ${flagName} is on`,
  ).toBeVisible();
}
