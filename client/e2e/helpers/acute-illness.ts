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
 * Open an activity from the encounter page by clicking its card icon.
 */
async function openActivity(page: Page, activityIcon: string) {
  await page.locator('div.page-encounter.acute-illness').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.acute-illness').waitFor({ timeout: 10000 });
}

/**
 * Dismiss the diagnosis assessment popup if it appears.
 * After completing all mandatory activities, the app may show a modal
 * with the diagnosis and a "Continue" button.
 */
async function dismissDiagnosisPopup(page: Page) {
  const continueBtn = page.locator('.ui.active.modal button', { hasText: 'Continue' });
  if (await continueBtn.isVisible({ timeout: 3000 }).catch(() => false)) {
    await click(continueBtn, page);
    await page.waitForTimeout(1000);
  }
}

/**
 * Save the current activity form and return to the encounter page.
 * @param actionsClass - CSS class on the actions wrapper (e.g., 'symptoms', 'malaria-testing', 'next-steps').
 */
async function saveActivity(page: Page, actionsClass: string) {
  await click(
    page.locator(`.actions.${actionsClass} button.ui.fluid.primary.button`, { hasText: 'Save' }),
    page,
  );
  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.acute-illness')
    .waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Save a next-steps sub-task. After saving, the app auto-navigates to the
 * next sub-task or back to the encounter page.
 */
async function saveNextStepsSubTask(page: Page) {
  const saveBtn = page.locator('.actions.next-steps button.ui.fluid.primary.button');
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);
}

/**
 * Fill a measurement number input identified by its CSS ID class.
 */
async function fillMeasurement(page: Page, id: string, value: string) {
  await page
    .locator(`.form-input.measurement.${id} input[type="number"]`)
    .fill(value);
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
 * Register an adult for acute illness and start an encounter.
 * Flow: Dashboard → Clinical → Individual Assessment → Acute Illness →
 *       Register → fill form → submit → participant page →
 *       click "Start New Acute Illness"
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createAdultAndStartEncounter(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isChw?: boolean;
    gender?: 'male' | 'female';
  },
) {
  const ageYears = options?.ageYears ?? 25;
  const firstName = options?.firstName ?? `TestPatient${Date.now()}`;
  const secondName = 'E2ETest';
  const isChw = options?.isChw ?? false;
  const gender = options?.gender ?? 'female';

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → Acute Illness
  await click(
    page.locator('button.encounter-type', { hasText: 'Acute Illness' }),
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
  if (gender === 'male') {
    await genderRadios.first().check();
  } else {
    await genderRadios.last().check();
  }

  // Adult-only required fields.
  await selectByLabel(page, 'Level of Education:', 1);
  await selectByLabel(page, 'Marital Status:', 1);

  if (!isChw) {
    // Nurse: fill address (cascading dropdowns) and health center.
    await selectByLabel(page, 'Province:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'District:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'Sector:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'Cell:', 1);
    await page.waitForTimeout(500);
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
    .locator('div.page-participant.individual.acute-illness')
    .waitFor({ timeout: 30000 });

  // Start a new acute illness encounter.
  await startNewAcuteIllness(page);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * Start a new acute illness from the participant page.
 * Encounter type is auto-assigned based on nurse/CHW role.
 */
export async function startNewAcuteIllness(page: Page) {
  await click(
    page.locator('div.ui.primary.button', { hasText: /New/ }),
    page,
  );
  await page
    .locator('div.page-encounter.acute-illness')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);
}

/**
 * End the current acute illness encounter: click "End Encounter",
 * confirm in the dialog, wait for navigation away.
 */
export async function endEncounter(page: Page) {
  await page.waitForTimeout(2000);

  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for and confirm the "End Encounter?" dialog.
  const continueBtn = page.locator('button', { hasText: 'Continue' });
  await continueBtn.waitFor({ timeout: 5000 }).catch(() => {});
  if (await continueBtn.isVisible()) {
    await continueBtn.click({ force: true });
  }

  // Wait for navigation away.
  await page.waitForTimeout(5000);
}

/**
 * Navigate to the acute illness participant page for a given person.
 * Works from the dashboard or post-encounter pages.
 * Flow: Dashboard → Clinical → Individual Assessment → Acute Illness
 * → search for participant by name.
 */
export async function navigateToParticipantPage(
  page: Page,
  fullName: string,
) {
  // If already on the participant page, nothing to do.
  const participantPage = page.locator('div.page-participant.individual.acute-illness');
  if (await participantPage.isVisible({ timeout: 500 }).catch(() => false)) {
    return;
  }

  // Navigate from the dashboard.
  // First, make sure we're on the dashboard. Click the back arrow until we get there,
  // or navigate directly.
  const dashboard = page.locator('.wrap-cards');
  if (!await dashboard.isVisible({ timeout: 1000 }).catch(() => false)) {
    // Try clicking back repeatedly to reach dashboard.
    for (let i = 0; i < 5; i++) {
      const backBtn = page.locator('.icon-back').first();
      if (await backBtn.isVisible({ timeout: 500 }).catch(() => false)) {
        await click(backBtn, page);
        await page.waitForTimeout(500);
      }
      if (await dashboard.isVisible({ timeout: 500 }).catch(() => false)) {
        break;
      }
    }
  }

  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  await click(
    page.locator('button.encounter-type', { hasText: 'Acute Illness' }),
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
 * Start a subsequent encounter from the participant page by clicking
 * the existing acute illness button.
 */
export async function startSubsequentEncounter(page: Page) {
  // Step 1: Click the "Existing Acute Illness" button on the participant page.
  await click(
    page.locator('div.ui.primary.button', { hasText: /Existing/i }).first(),
    page,
  );

  // Step 2: The app shows a list of existing acute illnesses.
  // Click the one that says "Subsequent" to start a subsequent encounter.
  const subsequentBtn = page.locator('div.ui.primary.button', {
    hasText: /Subsequent/i,
  });
  await subsequentBtn.first().waitFor({ timeout: 10000 });
  await click(subsequentBtn.first(), page);

  await page
    .locator('div.page-encounter.acute-illness')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);
}

/**
 * Backdate the most recent acute illness encounter for a person to
 * yesterday, allowing a subsequent encounter to be started (same-day
 * block prevents starting a new encounter on the same date).
 *
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function backdateAcuteIllnessEncounter(personName: string) {
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
      ->propertyCondition('type', 'acute_illness_encounter')
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
    console.log(`backdateAcuteIllnessEncounter attempt ${attempt + 1}:`, output);
    if (output.startsWith('Backdated')) {
      return;
    }
    if (attempt < 4) {
      execSync('sleep 10');
    }
  }
  console.error('backdateAcuteIllnessEncounter: failed after 5 attempts');
}

// ---------------------------------------------------------------------------
// Danger Signs activity (subsequent encounter only)
// ---------------------------------------------------------------------------

/**
 * Complete the Danger Signs activity.
 * Default: condition improving, no danger signs.
 *
 * Creates: acute_illness_danger_signs
 */
export async function completeDangerSigns(
  page: Page,
  options?: {
    conditionImproving?: boolean;
    dangerSigns?: string[];
  },
) {
  const improving = options?.conditionImproving ?? true;
  const signs = options?.dangerSigns ?? [];

  await openActivity(page, 'danger-signs');

  // "Is the condition improving?" → Yes/No
  await answerYesNo(page, 'conditionImproving', improving ? 'Yes' : 'No');

  // Select danger signs or "None of the above".
  if (signs.length === 0) {
    await selectCheckbox(page, 'None of the above');
  } else {
    for (const sign of signs) {
      await selectCheckbox(page, sign);
    }
  }

  // Save — actions wrapper uses class "treatment-ongoing".
  await saveActivity(page, 'treatment-ongoing');
}

// ---------------------------------------------------------------------------
// Ongoing Treatment activity (subsequent encounter only)
// ---------------------------------------------------------------------------

/**
 * Complete the Ongoing Treatment activity.
 * Default: taking medication as prescribed, no missed doses, no side effects,
 * feeling better.
 *
 * Creates: treatment_ongoing
 */
export async function completeOngoingTreatment(
  page: Page,
  options?: {
    takenAsPrescribed?: boolean;
    missedDoses?: boolean;
    sideEffects?: boolean;
    feelingBetter?: boolean;
  },
) {
  const takenAsPrescribed = options?.takenAsPrescribed ?? true;
  const missedDoses = options?.missedDoses ?? false;
  const sideEffects = options?.sideEffects ?? false;
  const feelingBetter = options?.feelingBetter ?? true;

  await openActivity(page, 'ongoing-treatment');

  // "Is the patient taking the medication as prescribed?" → Yes
  await answerYesNo(page, 'taken-as-prescribed', takenAsPrescribed ? 'Yes' : 'No');

  // "Did the patient miss any doses?" → No
  await answerYesNo(page, 'missed-doses', missedDoses ? 'Yes' : 'No');

  // "Did the medication cause side effects?" → No
  await answerYesNo(page, 'side-effects', sideEffects ? 'Yes' : 'No');

  // "Is the patient feeling better after taking the medication?" → Yes
  await answerYesNo(page, 'feeling-better', feelingBetter ? 'Yes' : 'No');

  // Save — actions wrapper uses class "treatment-ongoing".
  // After saving, the app may return to the encounter page OR (if this was
  // the last mandatory activity) show a diagnosis popup and auto-navigate
  // to Next Steps. Handle both paths.
  await click(
    page.locator('.actions.treatment-ongoing button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );
  await page.waitForTimeout(1000);

  // Dismiss diagnosis popup if it appears.
  await dismissDiagnosisPopup(page);
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Symptoms activity (initial encounter only)
// ---------------------------------------------------------------------------

/**
 * Complete the Symptoms activity with fever-based symptoms that trigger
 * malaria testing.
 *
 * General: Fever, Chills, BodyAches
 * Respiratory: None
 * GI: None
 *
 * Creates: symptoms_general, symptoms_respiratory, symptoms_gi
 */
export async function completeSymptoms(
  page: Page,
  options?: {
    general?: string[];
    respiratory?: string[];
    coughMoreThan2Weeks?: boolean;
    gi?: string[];
    intractableVomiting?: boolean;
  },
) {
  const generalSigns = options?.general ?? ['Fever', 'Chills', 'Body Aches'];
  const respiratorySigns = options?.respiratory ?? [];
  const giSigns = options?.gi ?? [];

  await openActivity(page, 'symptoms');

  // --- SymptomsGeneral tab (first tab, should be active by default) ---
  await clickSubTaskTab(page, 'symptoms-general');
  if (generalSigns.length === 0) {
    await selectCheckbox(page, 'None of the above');
  } else {
    for (const sign of generalSigns) {
      await selectCheckbox(page, sign);
    }
  }
  // Save general symptoms — advances to respiratory tab.
  await click(
    page.locator('.actions.symptoms button.ui.fluid.primary.button'),
    page,
  );
  await page.waitForTimeout(500);

  // --- SymptomsRespiratory tab ---
  await clickSubTaskTab(page, 'symptoms-respiratory');
  if (respiratorySigns.length === 0) {
    await selectCheckbox(page, 'None of the above');
  } else {
    for (const sign of respiratorySigns) {
      await selectCheckbox(page, sign);
    }
    // Handle cough duration conditional question.
    if (respiratorySigns.includes('Cough')) {
      const durationLabel = (options?.coughMoreThan2Weeks ?? false)
        ? 'More than 2 weeks'
        : '2 weeks or less';
      await page.locator('label', { hasText: durationLabel }).click();
      await page.waitForTimeout(300);
    }
  }
  // Save respiratory — advances to GI tab.
  await click(
    page.locator('.actions.symptoms button.ui.fluid.primary.button'),
    page,
  );
  await page.waitForTimeout(500);

  // --- SymptomsGI tab ---
  await clickSubTaskTab(page, 'symptoms-gi');
  if (giSigns.length === 0) {
    await selectCheckbox(page, 'None of the above');
  } else {
    for (const sign of giSigns) {
      await selectCheckbox(page, sign);
    }
    // Handle intractable vomiting question if Vomiting was selected.
    if (giSigns.includes('Vomiting') && options?.intractableVomiting !== undefined) {
      await answerYesNo(page, 'intractable-vomiting', options.intractableVomiting ? 'Yes' : 'No');
    }
  }
  // Save GI — all tasks complete, returns to encounter page.
  await saveActivity(page, 'symptoms');
}

// ---------------------------------------------------------------------------
// Physical Exam activity
// ---------------------------------------------------------------------------

/**
 * Complete the Physical Exam activity with all 5 sub-tasks.
 *
 * Creates: acute_illness_vitals, acute_illness_core_exam,
 *          acute_illness_muac, acute_findings, acute_illness_nutrition
 */
export async function completePhysicalExam(
  page: Page,
  options?: {
    isChw?: boolean;
    sys?: string;
    dia?: string;
    heartRate?: string;
    respiratoryRate?: string;
    bodyTemp?: string;
    muac?: string;
  },
) {
  const isChw = options?.isChw ?? false;
  const sys = options?.sys ?? '120';
  const dia = options?.dia ?? '80';
  const heartRate = options?.heartRate ?? '80';
  const respiratoryRate = options?.respiratoryRate ?? '18';
  const bodyTemp = options?.bodyTemp ?? '38.5';
  const muac = options?.muac ?? '25';

  await openActivity(page, 'physical-exam');

  // --- Vitals tab ---
  await clickSubTaskTab(page, 'physical-exam-vitals');
  if (!isChw) {
    // Nurse: full vitals form (BP, HR, RR, Temp).
    await fillMeasurement(page, 'sys-blood-pressure', sys);
    await fillMeasurement(page, 'dia-blood-pressure', dia);
    await fillMeasurement(page, 'heart-rate', heartRate);
  }
  // Both CHW (basic) and nurse (full) have respiratory rate + body temp.
  await fillMeasurement(page, 'respiratory-rate', respiratoryRate);
  await fillMeasurement(page, 'body-temperature', bodyTemp);
  // Save vitals — advances to next tab.
  await click(
    page.locator('.actions.symptoms button.ui.fluid.primary.button'),
    page,
  );
  await page.waitForTimeout(500);

  // --- Core Exam tab (nurse only — CHW skips) ---
  const coreExamTab = page.locator('.link-section:has(.icon-activity-task.icon-physical-exam-core-exam)');
  if (await coreExamTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'physical-exam-core-exam');
    // Heart: select "Normal Rate And Rhythm"
    const coreExamForm = page.locator('.ui.form.physical-exam.core-exam');
    await coreExamForm.waitFor({ timeout: 5000 });
    await selectCheckbox(page, 'Normal Rate And Rhythm');
    // Lungs: select "Normal"
    await selectCheckbox(page, 'Normal');
    // Save core exam.
    await click(
      page.locator('.actions.symptoms button.ui.fluid.primary.button'),
      page,
    );
    await page.waitForTimeout(500);
  }

  // --- MUAC tab (children only — may not appear for adults) ---
  const muacTab = page.locator('.link-section:has(.icon-activity-task.icon-physical-exam-muac)');
  if (await muacTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'physical-exam-muac');
    await fillMeasurement(page, 'muac', muac);
    // Save MUAC.
    await click(
      page.locator('.actions.symptoms button.ui.fluid.primary.button'),
      page,
    );
    await page.waitForTimeout(500);
  }

  // --- Nutrition tab (children only — may not appear for adults) ---
  const nutritionTab = page.locator('.link-section:has(.icon-activity-task.icon-physical-exam-nutrition)');
  if (await nutritionTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'physical-exam-nutrition');
    const nutritionForm = page.locator('.ui.form.physical-exam.nutrition');
    await nutritionForm.waitFor({ timeout: 5000 });
    await selectCheckbox(page, 'None of these');
    // Save nutrition.
    await click(
      page.locator('.actions.symptoms button.ui.fluid.primary.button'),
      page,
    );
    await page.waitForTimeout(500);
  }

  // --- Acute Findings tab (may not appear on subsequent encounters) ---
  // After saving the previous tab, we may already be back on the encounter
  // page if there are no more sub-tasks. Check before proceeding.
  const stillOnActivity = await page
    .locator('div.page-activity.acute-illness')
    .isVisible({ timeout: 2000 })
    .catch(() => false);

  if (stillOnActivity) {
    const acuteFindingsTab = page.locator('.link-section:has(.icon-activity-task.icon-acute-findings)');
    if (await acuteFindingsTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'acute-findings');
      const acuteFindingsForm = page.locator('.ui.form.physical-exam.acute-findings');
      await acuteFindingsForm.waitFor({ timeout: 5000 });
      // Select "None of the above" for both general and respiratory findings.
      const noneCheckboxes = acuteFindingsForm.locator('.ui.checkbox', {
        hasText: /^None of the above$/i,
      });
      const count = await noneCheckboxes.count();
      for (let i = 0; i < count; i++) {
        await click(noneCheckboxes.nth(i).locator('label'), page);
      }
      // Save acute findings — all tasks complete, return to encounter page.
      // Physical exam actions div uses class "actions symptoms" (not "physical-exam").
      await saveActivity(page, 'symptoms');
    }
  }
}

// ---------------------------------------------------------------------------
// Prior Treatment activity (initial encounter only)
// ---------------------------------------------------------------------------

/**
 * Complete the Prior Treatment activity.
 * Answers "No" to all prior medication questions.
 *
 * Creates: treatment_history
 */
export async function completePriorTreatment(page: Page) {
  await openActivity(page, 'prior-treatment');

  // Treatment review form: 3 yes/no questions.
  // "Took fever medication in past 6 hours?" → No
  await answerYesNo(page, 'fever-past-6-hours', 'No');
  // "Took malaria treatment today?" → No
  await answerYesNo(page, 'malaria-today', 'No');
  // "Took malaria treatment within past month?" → No
  await answerYesNo(page, 'malaria-within-past-month', 'No');

  // Save treatment review — return to encounter page.
  await saveActivity(page, 'malaria-testing');
}

// ---------------------------------------------------------------------------
// Laboratory activity
// ---------------------------------------------------------------------------

/**
 * Complete the Laboratory activity.
 * For malaria path: RDT positive, COVID not performed.
 *
 * Creates: malaria_testing, covid_testing (if nurse)
 */
export async function completeLaboratory(
  page: Page,
  options?: {
    malariaResult?: string;
    covidTestPerformed?: boolean;
    isPregnant?: boolean;
  },
) {
  const malariaResult = options?.malariaResult ?? 'Positive';
  const isPregnant = options?.isPregnant ?? false;

  await openActivity(page, 'laboratory');

  // --- Malaria Testing tab ---
  await clickSubTaskTab(page, 'laboratory-malaria-testing');
  const malariaForm = page.locator('.ui.form.laboratory.malaria-testing');
  await malariaForm.waitFor({ timeout: 5000 });

  // Select RDT result from dropdown.
  // The select element itself has class "form-input rapid-test-result".
  const rapidTestSelect = malariaForm.locator('select.form-input.rapid-test-result').first();
  await rapidTestSelect.waitFor({ timeout: 5000 });
  await rapidTestSelect.selectOption({ label: malariaResult });

  // If positive and pregnancy question appears, answer it.
  const pregnancyField = page.locator('.form-input.yes-no.is-pregnant');
  if (await pregnancyField.isVisible({ timeout: 2000 }).catch(() => false)) {
    await answerYesNo(page, 'is-pregnant', isPregnant ? 'Yes' : 'No');
  }

  // Save malaria testing — may advance to COVID tab, show diagnosis popup,
  // or return to encounter page.
  await click(
    page.locator('.actions.malaria-testing button.ui.fluid.primary.button'),
    page,
  );
  await page.waitForTimeout(1000);

  // --- COVID Testing tab (if visible) ---
  const covidTab = page.locator('.link-section:has(.icon-activity-task.icon-laboratory-covid-testing)');
  if (await covidTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'laboratory-covid-testing');
    const covidForm = page.locator('.ui.form.laboratory.covid-testing');
    await covidForm.waitFor({ timeout: 5000 });

    // "Test performed?" → No
    await answerYesNo(page, 'test-performed', 'No');

    // "Why not?" — select a reason from the checkbox list.
    const whyNotSection = page.locator('.why-not');
    await whyNotSection.waitFor({ timeout: 3000 }).catch(() => {});
    if (await whyNotSection.isVisible()) {
      // Select "Lack of Stock" as the reason.
      await click(
        whyNotSection.locator('.ui.checkbox.activity').first(),
        page,
      );
    }

    // Save COVID testing.
    await click(
      page.locator('.actions.malaria-testing button.ui.fluid.primary.button'),
      page,
    );
    await page.waitForTimeout(1000);
  }

  // After the last mandatory activity (malaria testing), the app may
  // show a diagnosis popup and auto-navigate to Next Steps.
  await dismissDiagnosisPopup(page);

  // We may now be on the encounter page or on the next-steps activity page.
  // Wait for either.
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Next Steps activity
// ---------------------------------------------------------------------------

/**
 * Complete the Next Steps activity for a malaria uncomplicated diagnosis.
 * Expected sub-tasks: MedicationDistribution, FollowUp.
 *
 * Creates: medication_distribution, acute_illness_follow_up
 */
export async function completeNextSteps(
  page: Page,
  options?: {
    hasMedicationDistribution?: boolean;
    hasSendToHC?: boolean;
    hasFollowUp?: boolean;
    hasHealthEducation?: boolean;
    hasContactTracing?: boolean;
    hasSymptomsRelief?: boolean;
  },
) {
  const hasMedDist = options?.hasMedicationDistribution ?? true;
  const hasFollowUp = options?.hasFollowUp ?? true;
  const hasSendToHC = options?.hasSendToHC ?? false;
  const hasHealthEd = options?.hasHealthEducation ?? false;
  const hasContactTracing = options?.hasContactTracing ?? false;
  const hasSymptomsRelief = options?.hasSymptomsRelief ?? false;

  // After completing all mandatory activities, the app may auto-navigate
  // to Next Steps with the diagnosis popup already dismissed.  Only open
  // the activity explicitly if we're still on the encounter page.
  const alreadyOnActivity = await page
    .locator('div.page-activity.acute-illness')
    .isVisible()
    .catch(() => false);
  if (!alreadyOnActivity) {
    await openActivity(page, 'next-steps');
  }

  // --- Medication Distribution ---
  if (hasMedDist) {
    const medTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-medication-distribution)');
    if (await medTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-medication-distribution');
      await page.locator('.ui.form.medication-distribution').waitFor({ timeout: 5000 });

      // For malaria uncomplicated: "Administered Coartem?" → Yes
      const coartemField = page.locator('.form-input.yes-no.coartem-medication');
      if (await coartemField.isVisible({ timeout: 2000 }).catch(() => false)) {
        await answerYesNo(page, 'coartem-medication', 'Yes');
      }

      // For GI infection: ORS and Zinc
      const orsField = page.locator('.form-input.yes-no.ors-medication');
      if (await orsField.isVisible({ timeout: 1000 }).catch(() => false)) {
        await answerYesNo(page, 'ors-medication', 'Yes');
      }
      const zincField = page.locator('.form-input.yes-no.zinc-medication');
      if (await zincField.isVisible({ timeout: 1000 }).catch(() => false)) {
        await answerYesNo(page, 'zinc-medication', 'Yes');
      }

      // For respiratory: Amoxicillin
      const amoxField = page.locator('.form-input.yes-no.amoxicillin-medication');
      if (await amoxField.isVisible({ timeout: 1000 }).catch(() => false)) {
        await answerYesNo(page, 'amoxicillin-medication', 'Yes');
      }

      await saveNextStepsSubTask(page);
    }
  }

  // --- Send to HC ---
  if (hasSendToHC) {
    const sendTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-send-to-hc)');
    if (await sendTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-send-to-hc');
      // "Referred patient?" → Yes
      await answerYesNo(page, 'refer-to-hc', 'Yes');
      // "Handed referral form?" → Yes
      const handForm = page.locator('.form-input.yes-no.hand-referral-form');
      if (await handForm.isVisible({ timeout: 1000 }).catch(() => false)) {
        await answerYesNo(page, 'hand-referral-form', 'Yes');
      }
      await saveNextStepsSubTask(page);
    }
  }

  // --- Health Education ---
  if (hasHealthEd) {
    const eduTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-health-education)');
    if (await eduTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-health-education');
      await page.locator('.ui.form.health-education').waitFor({ timeout: 5000 });
      await answerYesNo(page, 'education-for-diagnosis', 'Yes');
      await saveNextStepsSubTask(page);
    }
  }

  // --- Symptoms Relief Guidance ---
  if (hasSymptomsRelief) {
    // Symptoms relief reuses the medication-distribution icon.
    const reliefTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-medication-distribution)');
    if (await reliefTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await click(reliefTab, page);
      await page.waitForTimeout(500);
      await page.locator('.ui.form.symptoms-relief').waitFor({ timeout: 5000 });
      await answerYesNo(page, 'education-for-diagnosis', 'Yes');
      await saveNextStepsSubTask(page);
    }
  }

  // --- Contact Tracing ---
  if (hasContactTracing) {
    const ctTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-contacts-tracing)');
    if (await ctTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-contacts-tracing');
      // For now, skip contact tracing (finish without adding contacts).
      await saveNextStepsSubTask(page);
    }
  }

  // --- Follow Up ---
  if (hasFollowUp) {
    const fuTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-follow-up)');
    if (await fuTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-follow-up');
      await page.locator('.ui.form.follow-up').waitFor({ timeout: 5000 });
      // Select "3 Days" follow-up option.
      await selectCheckboxInForm(page, '.ui.form.follow-up', '3 Days');
      await saveNextStepsSubTask(page);
    }
  }

  // After completing all next steps, the app may show the progress report
  // page (with "End Encounter" button) or return to the encounter page.
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Sync helper
// ---------------------------------------------------------------------------

/**
 * Click the sync icon, wait for sync to complete on Nyange HC,
 * then navigate back.
 */
export async function syncAndWait(page: Page) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const nyange = page.locator('.health-center', {
    has: page.locator('h2', { hasText: 'Nyange Health Center' }),
  });
  await nyange.waitFor({ timeout: 10000 });

  await nyange
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 480000 });

  await page.goBack();
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for acute illness measurement nodes linked
 * to a person. Returns an object mapping node type → boolean.
 *
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function queryAcuteIllnessNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
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

    \\$measurements = [];
    \\$types = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_illness_core_exam',
      'acute_illness_muac',
      'acute_findings',
      'acute_illness_nutrition',
      'malaria_testing',
      'covid_testing',
      'treatment_history',
      'treatment_ongoing',
      'medication_distribution',
      'send_to_hc',
      'hc_contact',
      'call_114',
      'isolation',
      'travel_history',
      'exposure',
      'acute_illness_follow_up',
      'acute_illness_danger_signs',
      'acute_illness_contacts_tracing',
      'acute_illness_trace_contact',
      'health_education',
    ];

    foreach (\\$types as \\$node_type) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$node_type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->execute();
      if (!empty(\\$r['node'])) {
        \\$measurements[\\$node_type] = true;
      }
    }

    echo json_encode(\\$measurements);
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    });
    try {
      const parsed = JSON.parse(output.trim());
      if (parsed.error) {
        console.log(`queryAcuteIllnessNodes attempt ${attempt + 1}: ${parsed.error}`);
      } else if (expectedTypes && expectedTypes.length > 0) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) {
          return parsed;
        }
        console.log(`queryAcuteIllnessNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
      } else {
        return parsed;
      }
    } catch (e) {
      if (e instanceof Error && e.message.startsWith('Backend error:')) {
        throw e;
      }
      console.error('Failed to parse drush output:', output);
    }
    if (attempt < 4) {
      execSync('sleep 10');
    }
  }
  return {};
}
