import { Browser, Page, expect } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { syncAndWait } from './common';
import { drushEnv } from './device';
import {
  NYANGE_HC_ID,
  drupalLogin,
  navigateToCompletionReportPage,
  navigateToHCReportsPage,
  selectReportType,
  setDateRange,
} from './reports';

// Wide date window so Demographics counts include every migration-seeded
// encounter regardless of its created/updated timestamp. Constructed in
// UTC so the calendar popup (which reads via getUTC*) lands on the
// intended day in any timezone — see REPORT_START_DATE in reporting.spec.ts.
const ADMIN_REPORT_START_DATE = new Date(Date.UTC(2018, 0, 1));

/**
 * What admin-Reports surfaces a given SiteFeature flag is expected to gate.
 *
 * Lists are dropdown-option `value` attributes (not translated labels), so
 * matches are exact and stable across translation churn:
 *
 *   sqOptions: ['acute-illness']  -> Statistical Queries dropdown drops the
 *                                     option whose value="acute-illness".
 *   completionOptions: ['prenatal'] -> same idea on the Completion dropdown.
 *
 * Demographics rows have no value attribute so are matched by visible text;
 * use a substring unique to one row (e.g. "ANC Total", "Standard Pediatric
 * Visit").
 */
export interface AdminReportsExpectations {
  sqOptions?: string[];
  sqDemographicsRows?: string[];
  /**
   * FBF Distribution rows to assert by CSS class
   * (`fbf-child` | `fbf-mother` | `fbf-child-achi` | `aheza-child` | `aheza-mother`).
   *
   * When the same call lists `fbf-distribution` in `sqOptions` and `phase` is
   * `'hidden'` the report is unreachable, so the row assertion is skipped —
   * the dropdown assertion already proves the rows are gone.
   */
  sqFbfDistributionRows?: string[];
  completionOptions?: string[];
}

/**
 * Optional admin-side coverage layered onto the four PR #1411
 * verifyFeatureGates* helpers. Pass the test's `browser` fixture and an
 * `admin` payload to interleave server-side asserts inside the helper's
 * existing off/on cycle; omit to keep client-only behaviour.
 */
export interface FeatureGateOptions {
  browser?: Browser;
  admin?: AdminReportsExpectations;
}

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

export async function assertAdminGates(
  browser: Browser,
  expectations: AdminReportsExpectations,
  phase: 'hidden' | 'visible',
): Promise<void> {
  const sqOptions = expectations.sqOptions ?? [];
  const sqRows = expectations.sqDemographicsRows ?? [];
  const sqFbfRows = expectations.sqFbfDistributionRows ?? [];
  const completionOptions = expectations.completionOptions ?? [];
  // Only navigate into fbf-distribution when the dropdown still lists it.
  // If sqOptions already asserts the option is hidden, the report is
  // unreachable and the dropdown assertion subsumes the row check.
  const fbfRowsReachable =
    sqFbfRows.length > 0
    && !(phase === 'hidden' && sqOptions.includes('fbf-distribution'));
  const needsSQ = sqOptions.length > 0 || sqRows.length > 0 || fbfRowsReachable;
  const needsCompletion = completionOptions.length > 0;

  if (!needsSQ && !needsCompletion) {
    return;
  }

  const ctx = await browser.newContext({ ignoreHTTPSErrors: true });
  try {
    const adminPage = await ctx.newPage();
    await drupalLogin(adminPage);

    if (needsSQ) {
      await navigateToHCReportsPage(adminPage, NYANGE_HC_ID);
      const sqDropdown = adminPage
        .locator('.page-content.reports .select-input-wrapper')
        .first()
        .locator('select.select-input');

      for (const value of sqOptions) {
        const option = sqDropdown.locator(`option[value="${value}"]`);
        if (phase === 'hidden') {
          await expect(option, `SQ option "${value}" absent`).toHaveCount(0);
        } else {
          await expect(option, `SQ option "${value}" present`).toHaveCount(1);
        }
      }

      if (sqRows.length > 0) {
        await selectReportType(adminPage, 'demographics');
        // Health-center scope is "wide" (Pages.Reports.Utils.isWideScope),
        // so the Demographics rows are gated behind a date range. Without
        // setting it, the page only shows the WideScopeNote and no rows
        // render -- making both presence and absence assertions fail
        // ambiguously. Pick a wide window covering all migration-seeded
        // encounters; the rows themselves render independently of count.
        await setDateRange(adminPage, ADMIN_REPORT_START_DATE, new Date());
        for (const rowLabel of sqRows) {
          const row = adminPage.locator('.page-content.reports .row', {
            hasText: rowLabel,
          });
          if (phase === 'hidden') {
            await expect(row, `Demographics row "${rowLabel}" absent`).toHaveCount(0);
          } else {
            await expect(row.first(), `Demographics row "${rowLabel}" visible`).toBeVisible();
          }
        }
      }

      if (fbfRowsReachable) {
        await selectReportType(adminPage, 'fbf-distribution');
        // Same wide-scope date-gating as the Demographics block above.
        // setDateRange is idempotent so calling it again after the
        // Demographics path is harmless.
        await setDateRange(adminPage, ADMIN_REPORT_START_DATE, new Date());
        for (const rowClass of sqFbfRows) {
          const row = adminPage.locator(
            `div.report.fbf-distribution div.row.${rowClass}`,
          );
          if (phase === 'hidden') {
            await expect(row, `FBF row ".${rowClass}" absent`).toHaveCount(0);
          } else {
            await expect(row.first(), `FBF row ".${rowClass}" visible`).toBeVisible();
          }
        }
      }
    }

    if (needsCompletion) {
      await navigateToCompletionReportPage(adminPage, NYANGE_HC_ID);
      const completionDropdown = adminPage
        .locator('.page-content.completion .select-input-wrapper')
        .first()
        .locator('select.select-input');

      for (const value of completionOptions) {
        const option = completionDropdown.locator(`option[value="${value}"]`);
        if (phase === 'hidden') {
          await expect(option, `Completion option "${value}" absent`).toHaveCount(0);
        } else {
          await expect(option, `Completion option "${value}" present`).toHaveCount(1);
        }
      }
    }
  } finally {
    await ctx.close();
  }
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
  opts: FeatureGateOptions = {},
) {
  try {
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

    // 3a. Server-side admin gate (when caller asked).
    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'hidden');
    }

    // 4. Re-enable feature and sync. After syncAndWait we land back on
    //    the encounter-types page (page.goBack from device status).
    setFeatureFlag(flagName, true);
    await syncAndWait(page);

    // 5. Button must now be visible.
    await expect(
      button,
      `"${buttonText}" button must be visible when feature ${flagName} is on`,
    ).toBeVisible();

    // 5a. Server-side admin gate (when caller asked).
    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'visible');
    }

    // 6. Navigate back to PinCode so callers can resume from the dashboard.
    await click(page.locator('.link-back .icon-back'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
    await click(page.locator('.link-back .icon-back'), page);
    await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
  } finally {
    // Always restore the flag so a mid-cycle assertion failure can't
    // leak feature state into subsequent tests.
    setFeatureFlag(flagName, true);
  }
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
  opts: FeatureGateOptions = {},
) {
  try {
    setFeatureFlag(flagName, false);
    await syncAndWait(page);

    await click(page.locator('.icon-task-clinical'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

    const button = page.locator(`button.${buttonClass}`);
    await expect(
      button,
      `button.${buttonClass} must be hidden when feature ${flagName} is off`,
    ).toBeHidden();

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'hidden');
    }

    setFeatureFlag(flagName, true);
    await syncAndWait(page);

    await expect(
      button,
      `button.${buttonClass} must be visible when feature ${flagName} is on`,
    ).toBeVisible();

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'visible');
    }

    await click(page.locator('.link-back .icon-back'), page);
    await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
  } finally {
    setFeatureFlag(flagName, true);
  }
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
  opts: FeatureGateOptions = {},
) {
  try {
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

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'hidden');
    }

    setFeatureFlag(flagName, true);
    await syncAndWait(page);

    await expect(
      button,
      `"${buttonText}" button must be visible when feature ${flagName} is on`,
    ).toBeVisible();

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'visible');
    }

    // Back: Group Encounter Types → Clinical → PinCode.
    await click(page.locator('.link-back .icon-back'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
    await click(page.locator('.link-back .icon-back'), page);
    await page.locator('div.page-pincode').waitFor({ timeout: 10000 });
  } finally {
    // Restore both flags so cross-spec state doesn't leak. The helper
    // forces partnerFlag ON for navigation, so we restore it alongside
    // flagName to the project's default "all-on" state.
    setFeatureFlag(flagName, true);
    setFeatureFlag(partnerFlag, true);
  }
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
  opts: FeatureGateOptions = {},
) {
  try {
    setFeatureFlag(flagName, false);
    await syncAndWait(page);

    const icon = page.locator(`.icon-task-${iconKey}`);
    await expect(
      icon,
      `.icon-task-${iconKey} must be hidden when feature ${flagName} is off`,
    ).toBeHidden();

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'hidden');
    }

    setFeatureFlag(flagName, true);
    await syncAndWait(page);

    await expect(
      icon,
      `.icon-task-${iconKey} must be visible when feature ${flagName} is on`,
    ).toBeVisible();

    if (opts.browser && opts.admin) {
      await assertAdminGates(opts.browser, opts.admin, 'visible');
    }
  } finally {
    setFeatureFlag(flagName, true);
  }
}
