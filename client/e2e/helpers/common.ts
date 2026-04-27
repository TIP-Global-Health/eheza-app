import { execSync } from 'child_process';

import { Page } from '@playwright/test';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Timeout constants — named waits for common Elm/Playwright timing patterns
// ---------------------------------------------------------------------------

export const WAIT = {
  /** Brief pause between rapid form inputs (e.g., clicking multiple checkboxes in a loop). */
  quickInput: 200,
  /** Between individual form field interactions (single checkbox, radio click). */
  formInteraction: 300,
  /** Elm framework re-render after state changes (tab switch, dropdown cascade, form toggle). */
  elmRerender: 500,
  /** Between form sections, after search input, or after sub-task save. */
  sectionTransition: 1000,
  /** Page-level navigation after completing multiple activities or before End Encounter. */
  pageNavigation: 2000,
  /** Backend sync, device pairing, PIN retry loops, or data availability retries. */
  heavyOperation: 3000,
} as const;

/**
 * Click the sync icon, wait for sync to complete on a health center,
 * then navigate back.
 *
 * After the initial "Status: Success" is detected, waits 1 second and
 * re-verifies the status is still "Success" to guard against false
 * positives (e.g., download completes but upload is still in progress).
 *
 * @param page - Playwright page
 * @param healthCenter - Health center name to wait for (default: 'Nyange Health Center')
 * @param timeout - Max time in ms to wait for sync success (default: 300000)
 */
export async function syncAndWait(
  page: Page,
  healthCenter = 'Nyange Health Center',
  timeout = 300000,
) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: healthCenter }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  // Wait for sync to complete.
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout });

  // Verify status is stable (not a transient state between phases).
  await page.waitForTimeout(WAIT.sectionTransition);
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout });

  await page.goBack();
}

/**
 * Finds a person node by Drupal title and returns its nid.
 *
 * Retries up to 10 times (5-second delay between attempts) to handle cases
 * where the node has not yet been synced to the backend.
 *
 * @param personName - The full name of the person as stored in the node title
 * @returns The numeric node ID (nid) of the matching person node
 * @throws If the person is not found after 10 attempts
 */
export function findPersonNid(personName: string): number {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo 'NOT_FOUND'; return; }
    echo key(\\$r['node']);
  `;

  for (let attempt = 0; attempt < 10; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
    }).trim();

    if (output !== 'NOT_FOUND') {
      const nid = parseInt(output, 10);
      if (!isNaN(nid)) return nid;
    }

    console.log(`findPersonNid("${personName}") attempt ${attempt + 1}: ${output}`);
    if (attempt < 9) execSync('sleep 5');
  }

  throw new Error(`findPersonNid: person "${personName}" not found after 10 attempts`);
}

/**
 * Queries which measurement node types exist for a given person.
 *
 * Retries up to 10 times (5-second delay between attempts). If `expectedTypes`
 * is provided, keeps retrying until all expected types are present in the
 * result, making this suitable for post-sync verification.
 *
 * @param personName    - The full name of the person as stored in the node title
 * @param nodeTypes     - List of Drupal node type machine names to check
 * @param expectedTypes - Optional subset of `nodeTypes` that must all be `true`
 *                        before returning early
 * @returns A map of node type → boolean indicating whether at least one node
 *          of that type exists for the person
 */
export function queryMeasurementNodes(
  personName: string,
  nodeTypes: string[],
  expectedTypes?: string[],
): Record<string, boolean> {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const typesStr = nodeTypes.map(t => `'${t}'`).join(', ');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo json_encode(['error' => 'Person not found']); return; }
    \\$nid = key(\\$r['node']);

    \\$types = array(${typesStr});
    \\$found = array();
    foreach (\\$types as \\$type) {
      \\$tq = new EntityFieldQuery();
      \\$tr = \\$tq->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$type)
        ->fieldCondition('field_person', 'target_id', \\$nid)
        ->range(0, 1)
        ->execute();
      \\$found[\\$type] = !empty(\\$tr['node']);
    }
    echo json_encode(\\$found);
  `;

  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
      }).trim();

      const parsed = JSON.parse(output);
      if (parsed.error) {
        console.log(`queryMeasurementNodes attempt ${attempt + 1}: ${parsed.error}`);
        if (attempt < 9) { execSync('sleep 5'); continue; }
        return parsed;
      }

      if (expectedTypes) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) return parsed;
        console.log(`queryMeasurementNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
        if (attempt < 9) { execSync('sleep 5'); continue; }
      }

      return parsed;
    } catch (err) {
      console.log(`queryMeasurementNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) execSync('sleep 5');
    }
  }

  return {};
}

/**
 * Backdates the most recent encounter of the given type for a person.
 *
 * Resolves the chain: person → individual_participant → encounter, then sets
 * `field_scheduled_date` to `daysAgo` days in the past. Retries up to 5 times
 * (10-second delay between attempts) and logs each outcome.
 *
 * @param personName    - The full name of the person as stored in the node title
 * @param encounterType - Drupal node type machine name for the encounter
 * @param daysAgo       - How many days back to set the scheduled date (default: 7)
 */
export function backdateEncounter(personName: string, encounterType: string, daysAgo = 7): void {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo 'Person not found'; return; }
    \\$nid = key(\\$r['node']);

    \\$pq = new EntityFieldQuery();
    \\$pr = \\$pq->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'individual_participant')
      ->fieldCondition('field_person', 'target_id', \\$nid)
      ->execute();
    if (empty(\\$pr['node'])) { echo 'No participant found'; return; }

    \\$participant_nids = array_keys(\\$pr['node']);
    \\$eq = new EntityFieldQuery();
    \\$er = \\$eq->entityCondition('entity_type', 'node')
      ->propertyCondition('type', '${encounterType}')
      ->fieldCondition('field_individual_participant', 'target_id', \\$participant_nids, 'IN')
      ->propertyOrderBy('nid', 'DESC')
      ->execute();
    if (empty(\\$er['node'])) { echo 'No encounter found'; return; }

    \\$target_date = date('Y-m-d H:i:s', strtotime('-${daysAgo} days'));
    \\$enc_nid = key(\\$er['node']);
    \\$enc = node_load(\\$enc_nid);
    \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value'] = \\$target_date;
    \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value2'] = \\$target_date;
    node_save(\\$enc);
    echo 'Backdated encounter ' . \\$enc_nid;
  `;

  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
    }).trim();
    console.log(`backdateEncounter(${encounterType}) attempt ${attempt + 1}: ${output}`);
    if (output.startsWith('Backdated')) return;
    if (attempt < 4) execSync('sleep 10');
  }
  console.error(`backdateEncounter(${encounterType}): failed after 5 attempts`);
}

// ---------------------------------------------------------------------------
// Save helpers
// ---------------------------------------------------------------------------

/**
 * Click the Save button and wait for return to the encounter page.
 * @param encounterType - CSS class for the encounter page (e.g., 'ncd', 'prenatal').
 */
export async function saveActivity(page: Page, encounterType: string): Promise<void> {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.locator(`div.page-encounter.${encounterType}`).waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Click the Save button for a sub-task (stays on the activity page).
 */
export async function saveSubTask(page: Page): Promise<void> {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(WAIT.sectionTransition);
}

// ---------------------------------------------------------------------------
// Form interaction helpers
// ---------------------------------------------------------------------------

/**
 * Locate a form input by its label text (grid row pattern).
 * Elm's etaque/elm-form does NOT set HTML name attributes on inputs,
 * so we locate by label text in the .ui.grid layout.
 */
export function formInput(page: Page, labelText: string) {
  return page
    .locator('.ui.grid')
    .filter({ hasText: labelText })
    .locator('input')
    .first();
}

/**
 * Select an option in a form dropdown identified by its label text.
 * @param optionIndex - 1-based index (skips blank default).
 */
export async function selectByLabel(page: Page, labelText: string, optionIndex: number) {
  const row = page.locator('.ui.grid').filter({ hasText: labelText });
  const select = row.locator('select').first();
  const options = select.locator('option');
  const count = await options.count();
  if (count > optionIndex) {
    const value = await options.nth(optionIndex).getAttribute('value');
    if (value !== null) {
      await select.selectOption(value);
    }
  }
}

/**
 * Answer a Yes/No boolean field by its CSS class.
 * Radio inputs are CSS-hidden by Semantic UI; click the label instead.
 */
export async function answerYesNo(
  page: Page,
  fieldClass: string,
  answer: 'Yes' | 'No',
) {
  await click(
    page.locator(`.form-input.yes-no.${fieldClass} label`, {
      hasText: answer,
    }),
    page,
  );
}

/**
 * Select a checkbox option by clicking its label (exact match).
 * @param selector - CSS selector for the checkbox container (default: '.ui.checkbox label').
 */
export async function selectCheckbox(page: Page, optionText: string, selector = '.ui.checkbox label') {
  await click(
    page.locator(selector, {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }),
    page,
  );
}

/**
 * Select a checkbox inside a specific form container.
 */
export async function selectCheckboxInForm(page: Page, formSelector: string, optionText: string) {
  await click(
    page.locator(`${formSelector} .ui.checkbox`, {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }).locator('label'),
    page,
  );
}

/**
 * Click a sub-task tab icon and wait for it to become active.
 */
export async function clickSubTaskTab(page: Page, iconClass: string) {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(WAIT.elmRerender);
  }
}

/**
 * Fill a measurement number input identified by its CSS ID class.
 */
export async function fillMeasurement(page: Page, id: string, value: string) {
  await page
    .locator(`.form-input.measurement.${id} input[type="number"]`)
    .fill(value);
}

/**
 * Open the calendar popup, select a date (UTC), and confirm.
 * Elm date pickers derive dates via Time.utc.
 */
export async function setDate(page: Page, date: Date, triggerSelector = '.date-input') {
  await click(page.locator(triggerSelector).first(), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ timeout: 5000 });

  const year = date.getUTCFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  const monthValue = (date.getUTCMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  const day = date.getUTCDate();
  const dayCell = page.locator(
    'div.calendar table tbody td:not(.date-selector--dimmed)',
    { hasText: new RegExp(`^${day}$`) },
  );
  await dayCell.first().click();

  await click(
    page.locator('.ui.active.modal.calendar-popup div.ui.button'),
    page,
  );

  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ state: 'hidden', timeout: 3000 })
    .catch(() => {});
}

/**
 * Open an activity from the encounter page by clicking its card icon.
 * @param encounterType - CSS class for the encounter page (e.g., 'ncd', 'prenatal').
 * @param activityIcon - icon class suffix (e.g., 'danger-signs', 'history').
 */
export async function openActivity(page: Page, encounterType: string, activityIcon: string) {
  await page.locator(`div.page-encounter.${encounterType}`).waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
  const icon = page.locator(`.icon-task-${activityIcon}`);
  await icon.waitFor({ timeout: 10000 });
  await click(icon, page);
  await page.locator(`div.page-activity.${encounterType}`).waitFor({ timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Participant registration helpers
// ---------------------------------------------------------------------------

/**
 * Navigate from dashboard to individual encounter type and register a new adult.
 *
 * Handles the common flow: Dashboard → Clinical → Individual Assessment →
 * encounter type selection → "Register a new participant" → fill form → submit.
 *
 * @param encounterTypeText - Button text for the encounter type (e.g., 'Noncommunicable Diseases')
 * @param participantClass  - CSS class for the participant page (e.g., 'ncd', 'hiv')
 * @returns Patient name info for backend verification
 */
export async function registerAdult(
  page: Page,
  encounterTypeText: string,
  participantClass: string,
  options?: {
    ageYears?: number;
    firstName?: string;
    isFemale?: boolean;
    isChw?: boolean;
  },
): Promise<{ firstName: string; secondName: string; fullName: string }> {
  const ageYears = options?.ageYears ?? 30;
  const firstName = options?.firstName ?? `TestAdult${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;
  const isChw = options?.isChw ?? false;

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → specific encounter type
  await click(
    page.locator('button.encounter-type', { hasText: encounterTypeText }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // Click "Register a new participant"
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill the registration form.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth.
  const dob = new Date();
  dob.setFullYear(dob.getFullYear() - ageYears);
  await setDate(page, dob);

  // Select gender.
  const genderRadios = page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]');
  if (isFemale) {
    await genderRadios.last().check();
  } else {
    await genderRadios.first().check();
  }

  // Adult required fields.
  await selectByLabel(page, 'Level of Education:', 1);
  await selectByLabel(page, 'Marital Status:', 1);

  if (!isChw) {
    // Nurse: fill address (cascading dropdowns) and health center.
    await selectByLabel(page, 'Province:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'District:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Sector:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Cell:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Village:', 1);

    const hcSelect = page
      .locator('.ui.grid')
      .filter({ hasText: 'Health Center:' })
      .locator('select');
    await hcSelect.selectOption({ label: 'Nyange Health Center' });
  }

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page.
  await page
    .locator(`div.page-participant.individual.${participantClass}`)
    .waitFor({ timeout: 30000 });

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * Navigate from dashboard to individual encounter type and register a new child.
 *
 * @param encounterTypeText - Button text for the encounter type
 * @param participantClass  - CSS class for the participant page
 * @returns Patient name info for backend verification
 */
export async function registerChild(
  page: Page,
  encounterTypeText: string,
  participantClass: string,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isFemale?: boolean;
    isChw?: boolean;
  },
): Promise<{ firstName: string; secondName: string; fullName: string }> {
  const ageMonths = options?.ageMonths ?? 24;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;
  const isChw = options?.isChw ?? false;

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → specific encounter type
  await click(
    page.locator('button.encounter-type', { hasText: encounterTypeText }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // Click "Register a new participant"
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill the registration form.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth.
  const dob = new Date();
  dob.setMonth(dob.getMonth() - ageMonths);
  await setDate(page, dob);

  // Select gender.
  const genderRadios = page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]');
  if (isFemale) {
    await genderRadios.last().check();
  } else {
    await genderRadios.first().check();
  }

  // Child required field.
  await selectByLabel(page, 'Mode of delivery:', 1);

  if (!isChw) {
    // Nurse: fill address (cascading dropdowns) and health center.
    await selectByLabel(page, 'Province:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'District:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Sector:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Cell:', 1);
    await page.waitForTimeout(WAIT.elmRerender);
    await selectByLabel(page, 'Village:', 1);

    const hcSelect = page
      .locator('.ui.grid')
      .filter({ hasText: 'Health Center:' })
      .locator('select');
    await hcSelect.selectOption({ label: 'Nyange Health Center' });
  }

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page.
  await page
    .locator(`div.page-participant.individual.${participantClass}`)
    .waitFor({ timeout: 30000 });

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}
