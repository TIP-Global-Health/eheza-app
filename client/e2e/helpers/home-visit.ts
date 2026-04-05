import { Page } from '@playwright/test';
import { click } from './auth';
import { answerYesNo, queryMeasurementNodes, selectCheckbox } from './common';

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

/**
 * From the nutrition participant page, start a Home Visit encounter.
 */
export async function startHomeVisit(page: Page) {
  await click(
    page.locator('div.ui.primary.button', { hasText: 'Home Visit' }),
    page,
  );
  await page
    .locator('div.page-encounter.home-visit')
    .waitFor({ timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Activity completion helpers
// ---------------------------------------------------------------------------

/**
 * Navigate to an activity page and wait for its specific content.
 * Uses an activity-specific form element to confirm the right page loaded.
 */
async function openActivity(
  page: Page,
  iconClass: string,
  verifySelector: string,
) {
  const icon = page.locator(`.${iconClass}`);
  await icon.waitFor({ timeout: 10000 });
  await click(icon, page);
  await page.locator(verifySelector).waitFor({ timeout: 10000 });
}

/**
 * Save the current activity and return to the encounter page.
 */
async function saveActivity(page: Page) {
  await click(page.locator('button.ui.fluid.primary.button.active'), page);
  await page
    .locator('div.page-encounter.home-visit')
    .waitFor({ timeout: 10000 });
  // Brief wait for Elm re-render to stabilize card layout.
  await page.waitForTimeout(500);
}

/**
 * Complete Feeding activity — "No" to supplement (simplest path).
 * Fields: receive-supplement, encouraged-to-eat, refusing-to-eat,
 *         breastfeeding, clean-water-available.
 */
export async function completeFeeding(page: Page) {
  await openActivity(page, 'icon-task-feeding', '.form-input.yes-no.receive-supplement');

  await answerYesNo(page, 'receive-supplement', 'No');
  await answerYesNo(page, 'encouraged-to-eat', 'Yes');
  await answerYesNo(page, 'refusing-to-eat', 'No');
  await answerYesNo(page, 'breastfeeding', 'Yes');
  await answerYesNo(page, 'clean-water-available', 'Yes');

  await saveActivity(page);
}

/**
 * Complete Feeding activity — "Yes" to supplement with conditional fields.
 * Selects Fortified Porridge, fills all conditional Yes/No fields.
 */
export async function completeFeedingWithSupplement(page: Page) {
  await openActivity(page, 'icon-task-feeding', '.form-input.yes-no.receive-supplement');

  // Answer "Yes" to receiving supplement — reveals conditional fields.
  await answerYesNo(page, 'receive-supplement', 'Yes');

  // Select supplement type.
  await selectCheckbox(page, 'Fortified Porridge', '.ui.checkbox.activity label');

  // Conditional fields (only shown when supplement = Yes).
  await answerYesNo(page, 'ration-present-at-home', 'Yes');
  await answerYesNo(page, 'enough-till-next-session', 'Yes');
  // Supplement shared: "Only Sick Child" label.
  await click(
    page.locator('.form-input.yes-no.supplement-shared label', {
      hasText: 'Only Sick Child',
    }),
    page,
  );

  // Always-shown fields.
  await answerYesNo(page, 'encouraged-to-eat', 'Yes');
  await answerYesNo(page, 'refusing-to-eat', 'No');
  await answerYesNo(page, 'breastfeeding', 'Yes');
  await answerYesNo(page, 'clean-water-available', 'Yes');

  await saveActivity(page);
}

/**
 * Complete Caring activity.
 * Fields: parents-health, caregiver checkbox, child-clean.
 */
export async function completeCaring(page: Page) {
  await openActivity(page, 'icon-task-caring', '.form-input.yes-no.parents-health');

  await answerYesNo(page, 'parents-health', 'Yes');
  await selectCheckbox(page, 'Parent', '.ui.checkbox.activity label');
  await answerYesNo(page, 'child-clean', 'Yes');

  await saveActivity(page);
}

/**
 * Complete Hygiene activity.
 * Fields: water source checkbox, water preparation checkbox,
 *         soap-in-the-house, wash-hands-before-feeding, food-covered.
 */
export async function completeHygiene(page: Page) {
  await openActivity(page, 'icon-task-hygiene', '.form-input.yes-no.soap-in-the-house');

  await selectCheckbox(page, 'Piped Water to Home', '.ui.checkbox.activity label');
  await selectCheckbox(page, 'Boiled', '.ui.checkbox.activity label');
  await answerYesNo(page, 'soap-in-the-house', 'Yes');
  await answerYesNo(page, 'wash-hands-before-feeding', 'Yes');
  await answerYesNo(page, 'food-covered', 'Yes');

  await saveActivity(page);
}

/**
 * Complete Food Security activity.
 * Fields: income source checkbox, household-got-fFood.
 */
export async function completeFoodSecurity(page: Page) {
  await openActivity(page, 'icon-task-food-security', '.form-input.yes-no.household-got-fFood');

  await selectCheckbox(page, 'Homebased Agriculture / Livestock', '.ui.checkbox.activity label');
  // Note: typo in Elm source — class is "household-got-fFood" (capital F).
  await answerYesNo(page, 'household-got-fFood', 'Yes');

  await saveActivity(page);
}

/**
 * End the Home Visit encounter.
 * Unlike nutrition encounters, Home Visit has no confirmation dialog —
 * clicking End Encounter directly closes the encounter.
 */
export async function endHomeVisit(page: Page) {
  await page.waitForTimeout(2000);

  const endBtn = page.locator('div.actions button.ui.fluid.primary.button', {
    hasText: 'End Encounter',
  });
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // No dialog — Home Visit closes directly.
  // Wait for navigation away from the encounter page.
  await page
    .locator('div.page-encounter.home-visit')
    .waitFor({ state: 'hidden', timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for Home Visit measurement nodes linked to a person.
 */
export function queryHomeVisitNodes(
  personName: string,
  expectedKeys?: string[],
): {
  feeding?: boolean;
  caring?: boolean;
  hygiene?: boolean;
  foodSecurity?: boolean;
} {
  const keyMap: Record<string, string> = {
    nutrition_feeding: 'feeding',
    nutrition_caring: 'caring',
    nutrition_hygiene: 'hygiene',
    nutrition_food_security: 'foodSecurity',
  };
  const nodeTypes = Object.keys(keyMap);
  const expectedNodeTypes = expectedKeys
    ? nodeTypes.filter(t => expectedKeys.includes(keyMap[t]))
    : undefined;

  const raw = queryMeasurementNodes(personName, nodeTypes, expectedNodeTypes);

  const result: Record<string, boolean> = {};
  for (const [nodeType, alias] of Object.entries(keyMap)) {
    if (raw[nodeType] !== undefined) {
      result[alias] = raw[nodeType];
    }
  }
  return result;
}
