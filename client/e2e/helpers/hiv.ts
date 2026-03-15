import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Private form helpers
// ---------------------------------------------------------------------------

/**
 * Select an option in a form dropdown identified by its label text.
 * @param optionIndex - 1-based index (skips blank default).
 */
async function selectByLabel(page: Page, labelText: string, optionIndex: number) {
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
 * Locate a form input by its label text (grid row pattern).
 */
function formInput(page: Page, labelText: string) {
  return page
    .locator('.ui.grid')
    .filter({ hasText: labelText })
    .locator('input')
    .first();
}

/**
 * Answer a Yes/No boolean field by its CSS class.
 * Radio inputs are CSS-hidden; click the label instead.
 */
async function answerYesNo(
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
 */
async function selectCheckbox(page: Page, optionText: string) {
  await click(
    page.locator('.ui.checkbox label', {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }),
    page,
  );
}

/**
 * Select a checkbox inside a specific form container.
 */
async function selectCheckboxInForm(page: Page, formSelector: string, optionText: string) {
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
async function clickSubTaskTab(page: Page, iconClass: string) {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(500);
  }
}

/**
 * Open an activity from the HIV encounter page by clicking its card icon.
 */
async function openActivity(page: Page, activityIcon: string) {
  await page.locator('div.page-encounter.hiv').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.hiv').waitFor({ timeout: 10000 });
}

/**
 * Click the Save button and wait to return to the encounter page.
 */
async function saveAndReturn(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.locator('div.page-encounter.hiv').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Click the Save button for a sub-task (doesn't wait for encounter page).
 */
async function saveSubTask(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);
}

/**
 * Open the calendar popup, select a date, and confirm.
 */
async function setDate(page: Page, date: Date, triggerSelector = '.date-input') {
  await click(page.locator(triggerSelector).first(), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ timeout: 5000 });

  const year = date.getFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  const monthValue = (date.getMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  const day = date.getDate();
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

// ---------------------------------------------------------------------------
// Participant registration
// ---------------------------------------------------------------------------

/**
 * Register an adult and start an HIV encounter (CHW flow).
 * Flow: Dashboard → Clinical → Individual Assessment → HIV Management →
 *       Register → fill form → submit → participant page →
 *       click "HIV Encounter"
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createAdultAndStartHIVEncounter(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isFemale?: boolean;
  },
) {
  const ageYears = options?.ageYears ?? 30;
  const firstName = options?.firstName ?? `TestHIV${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → HIV Management
  await click(
    page.locator('button.encounter-type', { hasText: 'HIV Management' }),
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

  // CHW: address fields are auto-filled from assigned village, skip them.

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page.
  await page
    .locator('div.page-participant.individual.hiv')
    .waitFor({ timeout: 30000 });

  // Start HIV encounter.
  await startHIVEncounter(page);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// Diagnostics activity
// ---------------------------------------------------------------------------

/**
 * Complete the Diagnostics activity (initial encounter only).
 *
 * Paths:
 * - 'positive-reported': Patient reports positive diagnosis → set date.
 * - 'no-diagnosis-refuse-test': Not diagnosed, refuses test → end encounter dialog.
 *
 * Creates: hiv_diagnostics
 */
export async function completeDiagnostics(
  page: Page,
  options?: {
    path?: 'positive-reported' | 'no-diagnosis-refuse-test';
  },
) {
  const path = options?.path ?? 'positive-reported';

  await openActivity(page, 'diagnostics');

  if (path === 'positive-reported') {
    // "Have you been diagnosed with HIV?" → Yes
    await answerYesNo(page, 'result-positive', 'Yes');
    await page.waitForTimeout(500);

    // Set positive result date (1 year ago).
    const resultDate = new Date();
    resultDate.setFullYear(resultDate.getFullYear() - 1);
    await setDate(page, resultDate, '.form-input.date');
    await page.waitForTimeout(500);

    await saveAndReturn(page);
  } else if (path === 'no-diagnosis-refuse-test') {
    // "Have you been diagnosed with HIV?" → No
    await answerYesNo(page, 'result-positive', 'No');
    await page.waitForTimeout(500);

    // "Would you like to take an HIV test?" → No
    await answerYesNo(page, 'run-hiv-test', 'No');
    await page.waitForTimeout(500);

    // Click Save — triggers end encounter confirmation dialog.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 5000 });
    await click(saveBtn, page);

    // Wait for the end encounter confirmation dialog.
    const confirmModal = page.locator('div.ui.tiny.active.modal');
    await confirmModal.waitFor({ timeout: 5000 });
    await confirmModal.locator('button', { hasText: 'Continue' }).click({ force: true });

    // Encounter closes — wait for navigation away from encounter page.
    await page
      .locator('div.page-encounter.hiv')
      .waitFor({ state: 'hidden', timeout: 30000 })
      .catch(() => {});
    await page.waitForTimeout(1000);
  }
}

// ---------------------------------------------------------------------------
// Medication activity (PrescribedMedication + TreatmentReview sub-tasks)
// ---------------------------------------------------------------------------

/**
 * Complete the Medication activity.
 *
 * For initial encounters: selects medications, saves, then completes
 * TreatmentReview (which appears after PrescribedMedication is saved).
 *
 * For subsequent encounters: answers "medications not changed" question,
 * then completes TreatmentReview.
 *
 * Creates: hiv_medication, hiv_treatment_review
 */
export async function completeMedication(
  page: Page,
  options?: {
    isSubsequent?: boolean;
    sideEffects?: boolean;
  },
) {
  const isSubsequent = options?.isSubsequent ?? false;
  const sideEffects = options?.sideEffects ?? false;

  await openActivity(page, 'medication');

  // --- Sub-task 1: PrescribedMedication ---
  await clickSubTaskTab(page, 'medication');
  await page.waitForTimeout(500);

  if (isSubsequent) {
    // Subsequent: "Have the medications changed?" → "Yes" means NOT changed.
    // The CSS class is .medications-changed, and "Yes" sets medicationsNotChanged=true.
    await answerYesNo(page, 'medications-changed', 'Yes');
  } else {
    // Initial: select one antiretroviral medication.
    // "Dolutegravir + Lamivudine + Tenofovir (DTG-3TC-TDF)" is the first option.
    const medForm = page.locator('.ui.form.prescribed-medication');
    const firstMed = medForm.locator('.ui.checkbox.activity label').first();
    await click(firstMed, page);
  }

  await saveSubTask(page);
  await page.waitForTimeout(500);

  // --- Sub-task 2: TreatmentReview ---
  // In initial encounters, this tab appears after PrescribedMedication is saved.
  await clickSubTaskTab(page, 'treatment-review');
  await page.waitForTimeout(500);

  // "Has the patient been taking the medication as prescribed?" → Yes
  await answerYesNo(page, 'taken-as-prescribed', 'Yes');
  await page.waitForTimeout(300);

  // "Is the patient feeling better?" → Yes
  await answerYesNo(page, 'feeling-better', 'Yes');
  await page.waitForTimeout(300);

  // "Has the patient missed any doses?" → No (reverted bool input)
  await answerYesNo(page, 'missed-doses', 'No');
  await page.waitForTimeout(300);

  // "Has the medication caused any side effects?" → answer based on option
  if (sideEffects) {
    await answerYesNo(page, 'side-effects', 'Yes');
    await page.waitForTimeout(500);

    // Select adverse events: "Rash or Itching"
    await selectCheckbox(page, 'Rash or Itching');
    await page.waitForTimeout(300);
  } else {
    await answerYesNo(page, 'side-effects', 'No');
  }

  await saveAndReturn(page);
}

// ---------------------------------------------------------------------------
// SymptomReview activity (subsequent encounters only)
// ---------------------------------------------------------------------------

/**
 * Complete the SymptomReview activity.
 * Default: "None of these" (no symptoms).
 *
 * Creates: hiv_symptom_review
 */
export async function completeSymptomReview(
  page: Page,
  options?: {
    symptoms?: string[];
  },
) {
  const symptoms = options?.symptoms ?? [];

  await openActivity(page, 'symptoms');

  const form = page.locator('.ui.form.symptom-review');

  if (symptoms.length === 0) {
    await selectCheckboxInForm(page, '.ui.form.symptom-review', 'None of these');
  } else {
    for (const symptom of symptoms) {
      const checkbox = form.locator('.ui.checkbox', {
        hasText: new RegExp(`^${symptom}$`, 'i'),
      }).locator('label');
      await click(checkbox, page);
      await page.waitForTimeout(300);
    }
  }

  await saveAndReturn(page);
}

// ---------------------------------------------------------------------------
// NextSteps activity (HealthEducation + FollowUp + optional Referral)
// ---------------------------------------------------------------------------

/**
 * Complete the NextSteps activity: iterate visible sub-task tabs.
 * Sub-tasks: HealthEducation (always), FollowUp (always),
 *            Referral (only if symptoms or adverse events reported).
 *
 * Creates: hiv_health_education, hiv_follow_up, hiv_referral (conditional)
 */
export async function completeNextSteps(page: Page) {
  await openActivity(page, 'next-steps');

  // Iterate visible sub-task tabs.
  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(500);

    // Determine which sub-task by checking the active icon.
    const activeTab = page.locator('.link-section.active .icon-activity-task');
    const classAttr = await activeTab.getAttribute('class').catch(() => '');

    if (classAttr?.includes('next-steps-health-education')) {
      // HealthEducation: 4 yes/no questions.
      await answerYesNo(page, 'positive-result', 'Yes');
      await page.waitForTimeout(300);
      await answerYesNo(page, 'safer-sex-practices', 'Yes');
      await page.waitForTimeout(300);
      await answerYesNo(page, 'encouraged-partner-testing', 'Yes');
      await page.waitForTimeout(300);
      await answerYesNo(page, 'family-planning-options', 'Yes');
    } else if (classAttr?.includes('next-steps-follow-up')) {
      // FollowUp: select a follow-up option.
      await selectCheckbox(page, '1 Month');
    } else if (classAttr?.includes('next-steps-send-to-hc')) {
      // Referral: refer to health center.
      await answerYesNo(page, 'refer-to-hc', 'Yes');
      await page.waitForTimeout(500);
      // "Hand referral form?" → Yes
      const handForm = page.locator('.form-input.yes-no.hand-referral-form label', { hasText: 'Yes' });
      if (await handForm.isVisible({ timeout: 2000 }).catch(() => false)) {
        await handForm.click({ force: true });
      }
    }

    // Save sub-task.
    const saveBtn = page.locator('button.ui.fluid.primary.button:not(.disabled)', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 10000 });
    await saveBtn.click({ force: true });
    await page.waitForTimeout(1000);
  }

  // Wait for encounter page.
  await page.locator('div.page-encounter.hiv').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the HIV encounter: click "End Encounter", confirm in the dialog.
 */
export async function endHIVEncounter(page: Page) {
  await page.waitForTimeout(2000);

  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for and confirm the "End Encounter?" dialog.
  const confirmModal = page.locator('div.ui.tiny.active.modal');
  await confirmModal.waitFor({ timeout: 5000 }).catch(() => {});
  if (await confirmModal.isVisible()) {
    await confirmModal.locator('button', { hasText: 'Continue' }).click({ force: true });
  }

  // Wait for navigation away from the encounter page.
  await page
    .locator('div.page-encounter.hiv')
    .waitFor({ state: 'hidden', timeout: 30000 });
}

/**
 * Navigate to the HIV participant page for a given person.
 * Flow: Dashboard → Clinical → Individual Assessment → HIV Management → search.
 */
export async function navigateToParticipantPage(
  page: Page,
  fullName: string,
) {
  const participantPage = page.locator('div.page-participant.individual.hiv');
  if (await participantPage.isVisible({ timeout: 500 }).catch(() => false)) {
    return;
  }

  const dashboard = page.locator('.wrap-cards');
  if (!await dashboard.isVisible({ timeout: 1000 }).catch(() => false)) {
    await page.goto('/');
    await dashboard.waitFor({ timeout: 30000 });
  }

  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  await click(
    page.locator('button.encounter-type', { hasText: 'HIV Management' }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // Search for the participant by name.
  const searchInput = page.getByPlaceholder('Enter participant name here');
  await searchInput.waitFor({ timeout: 5000 });
  await searchInput.fill(fullName);

  // Wait for search results, then click the forward-arrow action icon.
  const resultItem = page.locator('.item.participant-view', {
    hasText: fullName,
  });
  await resultItem.first().waitFor({ timeout: 10000 });
  await click(resultItem.first().locator('.action-icon.forward'), page);

  await participantPage.waitFor({ timeout: 15000 });
}

/**
 * Start a new HIV encounter from the participant page.
 */
export async function startHIVEncounter(page: Page) {
  await click(
    page.locator('div.ui.primary.button', { hasText: 'HIV Encounter' }),
    page,
  );
  await page
    .locator('div.page-encounter.hiv')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Sync (reuse from nutrition.ts pattern)
// ---------------------------------------------------------------------------

/**
 * Sync data and wait for success.
 * Click sync icon → device status → wait for success → go back.
 */
export async function syncAndWait(page: Page) {
  // Navigate to device status.
  await click(page.locator('span.sync-icon'), page);

  // Wait for the Device Status page to be rendered.
  await page.locator('.device-status').waitFor({ timeout: 10000 });

  // Find the health center section for "Nyange Health Center".
  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: 'Nyange Health Center' }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  // Wait for sync success.
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 120000 });

  // Go back.
  await page.goBack();
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the backend for HIV measurement nodes associated with a person.
 * Returns an object mapping node type → boolean (exists).
 *
 * Retries up to 10 times with 5s delay for eventual consistency.
 */
export function queryHIVNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const hivTypes = [
    'hiv_diagnostics',
    'hiv_medication',
    'hiv_treatment_review',
    'hiv_symptom_review',
    'hiv_health_education',
    'hiv_referral',
    'hiv_follow_up',
  ];

  const typesStr = hivTypes.map(t => `'${t}'`).join(', ');

  const php = `
    \\$person_name = base64_decode('${personNameB64}');
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$person_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Person not found']);
      return;
    }
    \\$person_nid = key(\\$result['node']);

    \\$types = array(${typesStr});
    \\$found = array();
    foreach (\\$types as \\$type) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->range(0, 1)
        ->execute();
      \\$found[\\$type] = !empty(\\$r['node']);
    }
    echo json_encode(\\$found);
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd,
        timeout: 30000,
        encoding: 'utf-8',
      }).trim();

      const parsed = JSON.parse(output);
      if (parsed.error) {
        console.log(`queryHIVNodes attempt ${attempt + 1}: ${parsed.error}`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
        return parsed;
      }

      // Check if all expected types are found.
      if (expectedTypes) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) {
          return parsed;
        }
        console.log(`queryHIVNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
      }

      return parsed;
    } catch (err) {
      console.log(`queryHIVNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) {
        execSync('sleep 5');
      }
    }
  }

  return {};
}

/**
 * Backdate the most recent HIV encounter for a person to yesterday.
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function backdateHIVEncounter(personName: string) {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const php = `
    \\$person_name = base64_decode('${personNameB64}');
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$person_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo 'Person not found';
      return;
    }
    \\$person_nid = key(\\$result['node']);

    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'hiv_encounter')
      ->fieldCondition('field_individual_participant', 'target_id', NULL, 'IS NOT NULL')
      ->propertyOrderBy('nid', 'DESC')
      ->range(0, 50)
      ->execute();
    if (empty(\\$r['node'])) {
      echo 'No encounters found';
      return;
    }

    \\$yesterday = date('Y-m-d H:i:s', strtotime('-1 day'));
    foreach (array_keys(\\$r['node']) as \\$enc_nid) {
      \\$enc = node_load(\\$enc_nid);
      \\$participant_nid = \\$enc->field_individual_participant[LANGUAGE_NONE][0]['target_id'];
      \\$participant = node_load(\\$participant_nid);
      if (empty(\\$participant->field_person[LANGUAGE_NONE][0]['target_id'])) continue;
      if (\\$participant->field_person[LANGUAGE_NONE][0]['target_id'] != \\$person_nid) continue;

      \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value'] = \\$yesterday;
      \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value2'] = \\$yesterday;
      node_save(\\$enc);
      echo 'Backdated encounter ' . \\$enc_nid;
      return;
    }
    echo 'No matching encounter found';
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    }).trim();
    console.log(`backdateHIVEncounter attempt ${attempt + 1}:`, output);
    if (output.startsWith('Backdated')) {
      return;
    }
    if (attempt < 4) {
      execSync('sleep 10');
    }
  }
  console.error('backdateHIVEncounter: failed after 5 attempts');
}
