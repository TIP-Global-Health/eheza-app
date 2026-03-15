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
 * Answer a custom boolean field by clicking a label with specific text.
 * Used for fields like is-pulmonary where labels are not "Yes"/"No".
 */
async function answerCustomBool(
  page: Page,
  fieldClass: string,
  labelText: string,
) {
  await click(
    page.locator(`.form-input.yes-no.${fieldClass} label`, {
      hasText: labelText,
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
 * Open an activity from the TB encounter page by clicking its card icon.
 */
async function openActivity(page: Page, activityIcon: string) {
  await page.locator('div.page-encounter.tuberculosis').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.tuberculosis').waitFor({ timeout: 10000 });
}

/**
 * Click the Save button and wait to return to the encounter page.
 */
async function saveAndReturn(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.locator('div.page-encounter.tuberculosis').waitFor({ timeout: 10000 });
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
 * Register an adult and start a TB encounter (CHW flow).
 * Flow: Dashboard -> Clinical -> Individual Assessment -> TB Management ->
 *       Register -> fill form -> submit -> participant page ->
 *       click "Tuberculosis Encounter"
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createAdultAndStartTBEncounter(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isFemale?: boolean;
  },
) {
  const ageYears = options?.ageYears ?? 30;
  const firstName = options?.firstName ?? `TestTB${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;

  // Navigate: Dashboard -> Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical -> Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter -> TB Management
  await click(
    page.locator('button.encounter-type', { hasText: 'TB Management' }),
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
    .locator('div.page-participant.individual.tuberculosis')
    .waitFor({ timeout: 30000 });

  // Start TB encounter.
  await startTBEncounter(page);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// Diagnostics activity
// ---------------------------------------------------------------------------

/**
 * Complete the Diagnostics activity (initial encounter only).
 *
 * Paths:
 * - 'positive-pulmonary': Diagnosed with pulmonary TB.
 * - 'no-diagnosis': Not diagnosed -> end encounter dialog.
 *
 * Creates: tuberculosis_diagnostics
 */
export async function completeDiagnostics(
  page: Page,
  options?: {
    path?: 'positive-pulmonary' | 'no-diagnosis';
  },
) {
  const path = options?.path ?? 'positive-pulmonary';

  await openActivity(page, 'diagnostics');

  if (path === 'positive-pulmonary') {
    // "Was this person diagnosed with Tuberculosis?" -> Yes
    await answerYesNo(page, 'diagnosed', 'Yes');
    await page.waitForTimeout(500);

    // "Where is the Tuberculosis located?" -> Pulmonary (in the lungs)
    await answerCustomBool(page, 'is-pulmonary', 'Pulmonary (in the lungs)');
    await page.waitForTimeout(500);

    await saveAndReturn(page);
  } else if (path === 'no-diagnosis') {
    // "Was this person diagnosed with Tuberculosis?" -> No
    await answerYesNo(page, 'diagnosed', 'No');
    await page.waitForTimeout(500);

    // Click Save -> triggers end encounter confirmation dialog.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 5000 });
    await click(saveBtn, page);

    // Wait for the end encounter confirmation dialog.
    const confirmModal = page.locator('div.ui.tiny.active.modal');
    await confirmModal.waitFor({ timeout: 5000 });
    await confirmModal.locator('button', { hasText: 'Continue' }).click({ force: true });

    // Encounter closes — wait for navigation away from encounter page.
    await page
      .locator('div.page-encounter.tuberculosis')
      .waitFor({ state: 'hidden', timeout: 30000 })
      .catch(() => {});
    await page.waitForTimeout(1000);
  }
}

// ---------------------------------------------------------------------------
// Medication activity (PrescribedMedication + DOT + TreatmentReview)
// ---------------------------------------------------------------------------

/**
 * Complete the Medication activity with 3 sequential sub-tasks.
 *
 * For initial encounters: selects medications, saves, then DOT, then
 * TreatmentReview (DOT and TreatmentReview appear after PrescribedMedication saved).
 *
 * For subsequent encounters: answers "medications not changed" question,
 * then DOT and TreatmentReview.
 *
 * Creates: tuberculosis_medication, tuberculosis_dot, tuberculosis_treatment_review
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
    // Subsequent: "Have the prescribed medications changed?" -> "Yes" means NOT changed.
    await answerYesNo(page, 'medications-changed', 'Yes');
  } else {
    // Initial: select RHZE medication.
    const medForm = page.locator('.ui.form.prescribed-medication');
    const firstMed = medForm.locator('.ui.checkbox.activity label').first();
    await click(firstMed, page);
  }

  await saveSubTask(page);
  await page.waitForTimeout(500);

  // --- Sub-task 2: DOT ---
  await clickSubTaskTab(page, 'dot');
  await page.waitForTimeout(500);

  // "Will you provide DOT/TDO today?" -> Yes
  await answerYesNo(page, 'provide-today', 'Yes');
  await page.waitForTimeout(300);

  // "Did you distribute the following medications?" -> Yes
  await answerYesNo(page, 'distribute-medications', 'Yes');
  await page.waitForTimeout(300);

  await saveSubTask(page);
  await page.waitForTimeout(500);

  // --- Sub-task 3: TreatmentReview ---
  await clickSubTaskTab(page, 'treatment-review');
  await page.waitForTimeout(500);

  // "Has the patient been taking the medication as prescribed?" -> Yes
  await answerYesNo(page, 'taken-as-prescribed', 'Yes');
  await page.waitForTimeout(300);

  // "Is the patient feeling better?" -> Yes
  await answerYesNo(page, 'feeling-better', 'Yes');
  await page.waitForTimeout(300);

  // "Has the patient missed any doses?" -> No (reverted bool input)
  await answerYesNo(page, 'missed-doses', 'No');
  await page.waitForTimeout(300);

  // "Has the medication caused any side effects?" -> answer based on option
  if (sideEffects) {
    await answerYesNo(page, 'side-effects', 'Yes');
    await page.waitForTimeout(500);

    // Select adverse event: "Rash or Itching"
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
 * 4 individual yes/no questions for TB symptoms.
 *
 * Creates: tuberculosis_symptom_review
 */
export async function completeSymptomReview(
  page: Page,
  options?: {
    nightSweats?: boolean;
    bloodInSputum?: boolean;
    weightLoss?: boolean;
    severeFatigue?: boolean;
  },
) {
  const nightSweats = options?.nightSweats ?? false;
  const bloodInSputum = options?.bloodInSputum ?? false;
  const weightLoss = options?.weightLoss ?? false;
  const severeFatigue = options?.severeFatigue ?? false;

  await openActivity(page, 'symptoms');

  // Answer each symptom question.
  await answerYesNo(page, 'night-sweats', nightSweats ? 'Yes' : 'No');
  await page.waitForTimeout(300);

  // Note: CSS class has capital S in "blood-in-Sputum" (matches Elm source).
  await answerYesNo(page, 'blood-in-Sputum', bloodInSputum ? 'Yes' : 'No');
  await page.waitForTimeout(300);

  await answerYesNo(page, 'weight-loss', weightLoss ? 'Yes' : 'No');
  await page.waitForTimeout(300);

  await answerYesNo(page, 'severe-fatigue', severeFatigue ? 'Yes' : 'No');
  await page.waitForTimeout(300);

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
 * Creates: tuberculosis_health_education, tuberculosis_follow_up,
 *          tuberculosis_referral (conditional)
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
      // HealthEducation: "Have you provided the guidance for follow up testing?" -> Yes
      await answerYesNo(page, 'followup-testing', 'Yes');
    } else if (classAttr?.includes('next-steps-follow-up')) {
      // FollowUp: select "1 Week".
      await selectCheckbox(page, '1 Week');
    } else if (classAttr?.includes('next-steps-send-to-hc')) {
      // Referral: refer to health center.
      await answerYesNo(page, 'refer-to-hc', 'Yes');
      await page.waitForTimeout(500);
      // "Hand referral form?" -> Yes
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

  // After saving all sub-tasks, the app may navigate to the encounter page
  // or the progress report page. Wait for either.
  await Promise.race([
    page.locator('div.page-encounter.tuberculosis').waitFor({ timeout: 15000 }),
    page.locator('div.page-report.tuberculosis').waitFor({ timeout: 15000 }),
  ]);
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the TB encounter: click "End Encounter", confirm in the dialog.
 */
export async function endTBEncounter(page: Page) {
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

  // Wait for navigation away from the encounter/report page.
  // After completing all activities, the app may show the progress report
  // instead of the encounter page.
  await Promise.race([
    page.locator('div.page-encounter.tuberculosis').waitFor({ state: 'hidden', timeout: 30000 }),
    page.locator('div.page-report.tuberculosis').waitFor({ state: 'hidden', timeout: 30000 }),
  ]);
}

/**
 * Navigate to the TB participant page for a given person.
 * Flow: Dashboard -> Clinical -> Individual Assessment -> TB Management -> search.
 */
export async function navigateToParticipantPage(
  page: Page,
  fullName: string,
) {
  const participantPage = page.locator('div.page-participant.individual.tuberculosis');
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
    page.locator('button.encounter-type', { hasText: 'TB Management' }),
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
 * Start a new TB encounter from the participant page.
 */
export async function startTBEncounter(page: Page) {
  await click(
    page.locator('div.ui.primary.button', { hasText: 'Tuberculosis Encounter' }),
    page,
  );
  await page
    .locator('div.page-encounter.tuberculosis')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Sync
// ---------------------------------------------------------------------------

/**
 * Sync data and wait for success.
 * Click sync icon -> device status -> wait for success -> go back.
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
 * Query the backend for TB measurement nodes associated with a person.
 * Returns an object mapping node type -> boolean (exists).
 *
 * Uses base64-encoded person name to prevent shell injection.
 * Retries up to 10 times with 5s delay for eventual consistency.
 */
export function queryTBNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const tbTypes = [
    'tuberculosis_diagnostics',
    'tuberculosis_medication',
    'tuberculosis_dot',
    'tuberculosis_treatment_review',
    'tuberculosis_symptom_review',
    'tuberculosis_health_education',
    'tuberculosis_referral',
    'tuberculosis_follow_up',
  ];

  const typesStr = tbTypes.map(t => `'${t}'`).join(', ');

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
        console.log(`queryTBNodes attempt ${attempt + 1}: ${parsed.error}`);
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
        console.log(`queryTBNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
      }

      return parsed;
    } catch (err) {
      console.log(`queryTBNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) {
        execSync('sleep 5');
      }
    }
  }

  return {};
}

/**
 * Backdate the most recent TB encounter for a person to yesterday.
 * Uses base64-encoded person name to prevent shell injection.
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function backdateTBEncounter(personName: string) {
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
      ->propertyCondition('type', 'tuberculosis_encounter')
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
    console.log(`backdateTBEncounter attempt ${attempt + 1}:`, output);
    if (output.startsWith('Backdated')) {
      return;
    }
    if (attempt < 4) {
      execSync('sleep 10');
    }
  }
  console.error('backdateTBEncounter: failed after 5 attempts');
}
