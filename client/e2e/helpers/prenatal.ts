import { Page, Locator } from '@playwright/test';
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
 * Open the calendar popup, select a date, and confirm.
 * @param triggerSelector - CSS selector for the date input that opens the popup.
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

/**
 * Click a sub-task tab icon and wait for the tab to become active.
 * Sub-tasks are rendered as clickable columns in the task bar.
 */
async function clickSubTaskTab(page: Page, iconClass: string) {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  // Only click if not already active.
  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(500);
  }
}

/**
 * Open an activity from the encounter page by clicking its card icon.
 * Waits for the activity page to load.
 */
async function openActivity(page: Page, activityIcon: string) {
  // Ensure encounter page is fully rendered before clicking an activity card.
  await page.locator('div.page-encounter.prenatal').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.prenatal').waitFor({ timeout: 10000 });
}

/**
 * Save current activity and return to encounter page.
 * Clicks the active save button, waits for encounter page.
 */
async function savePrenatalActivity(page: Page) {
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Fill a measurement number input identified by its CSS ID class.
 * Measurement inputs render as: .form-input.measurement.{id} input[type="number"]
 */
async function fillMeasurement(page: Page, id: string, value: string) {
  await page
    .locator(`.form-input.measurement.${id} input[type="number"]`)
    .fill(value);
}

/**
 * Fill a number input identified by its CSS ID class.
 * Number inputs render as: .form-input.number.{id} input[type="number"]
 */
async function fillNumber(page: Page, id: string, value: string) {
  await page
    .locator(`.form-input.number.${id} input[type="number"]`)
    .fill(value);
}

// ---------------------------------------------------------------------------
// Participant registration
// ---------------------------------------------------------------------------

/**
 * Register an adult female for prenatal care and start an encounter.
 * Flow: Dashboard → Clinical → Individual Assessment → Antenatal Care →
 *       Register → fill form → submit → participant page →
 *       click encounter button
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createAdultFemaleAndStartEncounter(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isChw?: boolean;
    encounterType?: 'first' | 'subsequent' | 'postpartum';
  },
) {
  const ageYears = options?.ageYears ?? 25;
  const firstName = options?.firstName ?? `TestMother${Date.now()}`;
  const secondName = 'E2ETest';
  const isChw = options?.isChw ?? false;
  const encounterType = options?.encounterType ?? 'first';

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → Antenatal Care
  await click(
    page.locator('button.encounter-type', { hasText: 'Antenatal Care' }),
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

  // Select gender = Female (only option shown for prenatal / ExpectFemale).
  await page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]')
    .first()
    .check();

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
    .locator('div.page-participant.individual.prenatal')
    .waitFor({ timeout: 30000 });

  // Start the encounter.
  await startPrenatalEncounter(page, encounterType);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * Start a prenatal encounter from the participant page.
 * Clicks the appropriate encounter button and waits for the encounter page.
 */
export async function startPrenatalEncounter(
  page: Page,
  encounterType: 'first' | 'subsequent' | 'postpartum',
) {
  let buttonText: string;
  switch (encounterType) {
    case 'first':
      buttonText = 'First Antenatal';
      break;
    case 'subsequent':
      buttonText = 'Subsequent Antenatal';
      break;
    case 'postpartum':
      buttonText = 'Postpartum';
      break;
  }

  await click(
    page.locator('div.ui.primary.button', { hasText: buttonText }),
    page,
  );
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);

  // Dismiss any informational popup (e.g. "has not yet been seen at the
  // health center for this pregnancy").
  const okBtn = page.locator('button', { hasText: 'OK' });
  if (await okBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
    await okBtn.click({ force: true });
    await page.waitForTimeout(500);
  }
}

/**
 * End the current prenatal encounter: click "End Encounter",
 * confirm in the dialog, wait for navigation away.
 */
export async function endPrenatalEncounter(page: Page) {
  await page.waitForTimeout(2000);

  // "End Encounter" may be on the encounter page or the progress report.
  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for and confirm the "End Encounter?" dialog.
  const continueBtn = page.locator('button', { hasText: 'Continue' });
  await continueBtn.waitFor({ timeout: 5000 }).catch(() => {});
  if (await continueBtn.isVisible()) {
    await continueBtn.click({ force: true });
  }

  // Wait for navigation away (may go to participant page or dashboard).
  await page.waitForTimeout(5000);
}

/**
 * Navigate to the participant page for a given person.
 * Works from the dashboard or encounter page.
 * Navigates: Dashboard → Clinical → Individual Assessment → Antenatal Care
 * → clicks on participant by name.
 */
export async function navigateToParticipantPage(
  page: Page,
  fullName?: string,
) {
  // If already on the participant page, nothing to do.
  const participantPage = page.locator('div.page-participant.individual.prenatal');
  if (await participantPage.isVisible({ timeout: 500 }).catch(() => false)) {
    return;
  }

  // If on the encounter page, just click back.
  const encounterPage = page.locator('div.page-encounter.prenatal');
  if (await encounterPage.isVisible({ timeout: 500 }).catch(() => false)) {
    await click(page.locator('.icon-back').first(), page);
    await participantPage.waitFor({ timeout: 10000 });
    return;
  }

  // Otherwise, navigate from the dashboard.
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  await click(
    page.locator('button.encounter-type', { hasText: 'Antenatal Care' }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // The participants page is a search page — type the name to find results.
  const searchInput = page.getByPlaceholder('Enter participant name here');
  await searchInput.waitFor({ timeout: 5000 });
  await searchInput.fill(fullName || 'E2ETest');

  // Wait for search results, then click the forward-arrow action icon.
  const resultItem = page.locator('.item.participant-view', {
    hasText: fullName || 'E2ETest',
  });
  await resultItem.first().waitFor({ timeout: 10000 });
  await click(resultItem.first().locator('.action-icon.forward'), page);

  await participantPage.waitFor({ timeout: 15000 });
}

// ---------------------------------------------------------------------------
// Simple activity helpers
// ---------------------------------------------------------------------------

/**
 * Complete PregnancyDating: set LMP date, confirm confidence.
 * Creates: last_menstrual_period
 */
export async function completePregnancyDating(page: Page, lmpDate: Date) {
  await openActivity(page, 'pregnancy-dating');

  // Click the LMP date input to open calendar.
  await setDate(page, lmpDate, '.form-input.date');

  // LMP Date Confident? → Yes
  await answerYesNo(page, 'is-confident', 'Yes');

  // If late ANC (past first trimester), a "Why not first trimester?" checkbox
  // group appears. Select a reason if visible.
  const lateAncCheckbox = page.locator('.ui.checkbox label', {
    hasText: /^Lack of awareness/,
  });
  if (await lateAncCheckbox.isVisible({ timeout: 1000 }).catch(() => false)) {
    await click(lateAncCheckbox, page);
  }

  await savePrenatalActivity(page);
}

/**
 * Complete DangerSigns: select "None of These" (normal path).
 * Creates: danger_signs
 */
export async function completeDangerSigns(page: Page) {
  await openActivity(page, 'danger-signs');

  await selectCheckbox(page, 'None of These');

  await savePrenatalActivity(page);
}

/**
 * Complete FamilyPlanning: select one method.
 * Creates: prenatal_family_planning
 */
export async function completeFamilyPlanning(page: Page) {
  await openActivity(page, 'planning');

  // Select "Auto-observation" as the family planning method.
  await selectCheckbox(page, 'Auto-observation');

  await savePrenatalActivity(page);
}

/**
 * Complete MalariaPrevention: answer Yes to mosquito net.
 * Creates: malaria prevention node
 */
export async function completeMalariaPrevention(page: Page) {
  await openActivity(page, 'malaria');

  await answerYesNo(page, 'mosquito-net', 'Yes');

  await savePrenatalActivity(page);
}

/**
 * Complete Laboratory for CHW: select pregnancy test result.
 * Creates: pregnancy_testing
 */
export async function completeLaboratoryChw(page: Page) {
  await openActivity(page, 'laboratory');

  // The CHW lab form is a dropdown for pregnancy test result.
  // Select "Positive".
  const selectEl = page.locator('select').first();
  await selectEl.waitFor({ timeout: 5000 });
  await selectEl.selectOption({ index: 1 });

  await savePrenatalActivity(page);
}

/**
 * Complete HealthEducation: answer Yes to all education questions.
 * Creates: prenatal_health_education
 *
 * @param encounterNum - 1 for first, 2 for second, 3 for third+
 */
export async function completeHealthEducation(
  page: Page,
  encounterNum: number = 1,
) {
  await openActivity(page, 'health-education');

  // First encounter always shows expectations, visits-review, warning-signs.
  // Subsequent encounters only show these if health education was NOT
  // completed at a prior encounter — check visibility before answering.
  const expectationsField = page.locator('.form-input.yes-no.expectations');
  if (await expectationsField.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerYesNo(page, 'expectations', 'Yes');
    await answerYesNo(page, 'visits-review', 'Yes');
    await answerYesNo(page, 'warning-signs', 'Yes');
  }

  // CHW second+ encounter: hemorrhaging
  if (encounterNum >= 2) {
    await answerYesNo(page, 'hemorrhaging', 'Yes');
  }

  // CHW third+ encounter: breastfeeding
  if (encounterNum >= 3) {
    await answerYesNo(page, 'breastfeeding', 'Yes');
  }

  await savePrenatalActivity(page);
}

/**
 * Complete BirthPlan: answer all Yes/No questions + family planning selection.
 * Creates: birth_plan
 */
export async function completeBirthPlan(page: Page) {
  await openActivity(page, 'birth-plan');

  await answerYesNo(page, 'insurance', 'Yes');
  await answerYesNo(page, 'clothes', 'Yes');
  await answerYesNo(page, 'caregiver-accompany', 'Yes');
  // Both "saved money" and "transportation" share the CSS class "saved-money".
  // Use .first() and .nth(1) to disambiguate.
  const savedMoneyFields = page.locator('.form-input.yes-no.saved-money');
  await click(savedMoneyFields.first().locator('label', { hasText: 'Yes' }), page);
  await click(savedMoneyFields.nth(1).locator('label', { hasText: 'Yes' }), page);

  // Family planning selection — select "Auto-observation".
  await selectCheckbox(page, 'Auto-observation');

  await savePrenatalActivity(page);
}

// ---------------------------------------------------------------------------
// Complex activity helpers
// ---------------------------------------------------------------------------

/**
 * Complete History: all sub-tasks (Obstetric step 1 & 2, Medical).
 * Social is never shown. OutsideCare only on subsequent encounters.
 * Creates: obstetric_history, obstetric_history_step2, medical_history
 */
export async function completeHistory(
  page: Page,
  options?: { isSubsequent?: boolean },
) {
  await openActivity(page, 'history');

  if (!options?.isSubsequent) {
    // --- Obstetric History Step 1 (initial encounter only) ---
    await clickSubTaskTab(page, 'obstetric');
    await page.locator('.form.history.obstetric.first').waitFor({ timeout: 5000 });

    await answerYesNo(page, 'currently-pregnant', 'Yes');
    await fillNumber(page, 'term-pregnancy', '2');
    await fillNumber(page, 'preterm-pregnancy', '0');
    await fillNumber(page, 'stillbirths-at-term', '0');
    await fillNumber(page, 'stillbirths-pre-term', '0');
    await fillNumber(page, 'abortions', '0');
    await fillNumber(page, 'live-children', '2');

    await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
    await page.waitForTimeout(1000);

    // --- Obstetric History Step 2 ---
    await page.locator('.form.history.obstetric.second').waitFor({ timeout: 5000 });
    await answerYesNo(page, 'c-section-past', 'No');
    await selectCheckbox(page, 'None of these');
    await selectCheckbox(page, 'None of the above');

    await click(
      page.locator('div.actions button.ui.fluid.primary.button').filter({ hasText: /Save/ }),
      page,
    );
    await page.waitForTimeout(1000);

    // --- Medical History ---
    await clickSubTaskTab(page, 'medical');
    await page.locator('.form.history.medical').waitFor({ timeout: 5000 });

    const noneCheckboxes = page.locator('.ui.checkbox label', {
      hasText: /^None of the above$/i,
    });
    const noneCount = await noneCheckboxes.count();
    for (let i = 0; i < noneCount; i++) {
      await click(noneCheckboxes.nth(i), page);
    }

    const noPreeclampsia = page.locator('.ui.checkbox label', {
      hasText: /^No$/,
    });
    if (await noPreeclampsia.isVisible()) {
      await click(noPreeclampsia, page);
    }

    const medSaveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await click(medSaveBtn, page);
    await page.waitForTimeout(1000);
  }

  // --- OutsideCare (subsequent encounters only) ---
  if (options?.isSubsequent) {
    // "Have you been seen at another facility since your last visit?" → No
    await answerYesNo(page, 'seen-at-another-facility', 'No');

    await click(
      page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
      page,
    );
    await page.waitForTimeout(500);
  }

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });
}

/**
 * Complete Examination: all sub-tasks.
 * Creates: vitals, prenatal_nutrition, core_physical_exam,
 *          obstetrical_exam (not postpartum), breast_exam,
 *          prenatal_gu_exam (postpartum only)
 */
export async function completeExamination(
  page: Page,
  options?: { isPostpartum?: boolean; vitals?: { sys?: string; dia?: string } },
) {
  await openActivity(page, 'examination');

  // --- Vitals ---
  await clickSubTaskTab(page, 'vitals');
  await page.waitForTimeout(500);

  await fillMeasurement(page, 'sys-blood-pressure', options?.vitals?.sys ?? '120');
  await fillMeasurement(page, 'dia-blood-pressure', options?.vitals?.dia ?? '80');
  await fillMeasurement(page, 'heart-rate', '72');
  await fillMeasurement(page, 'respiratory-rate', '18');
  await fillMeasurement(page, 'body-temperature', '36.5');

  await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
  await page.waitForTimeout(1000);

  // --- Nutrition Assessment ---
  await clickSubTaskTab(page, 'nutrition-assessment');
  await page.waitForTimeout(500);

  // Height might be hidden if previously measured; fill if visible.
  const heightInput = page.locator('.form-input.measurement.height input[type="number"]');
  if (await heightInput.isVisible()) {
    await heightInput.fill('160');
  }
  await fillMeasurement(page, 'weight', '60');
  await fillMeasurement(page, 'muac', '25');

  await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
  await page.waitForTimeout(1000);

  // --- Core Physical Exam ---
  await clickSubTaskTab(page, 'core-physical-exam');
  await page.waitForTimeout(500);

  // Head/Hair: bool input (BrittleHair/Normal) — select "Normal".
  await answerYesNo(page, 'head-hair', 'Normal');
  // Eyes: bool input (PaleConjuctiva/Normal) — select "Normal".
  await answerYesNo(page, 'eyes', 'Normal');
  // Heart Murmur: bool input (Yes/No) — select "No".
  await answerYesNo(page, 'heart-murmur', 'No');

  // Checkbox groups (Neck, Heart, Lungs, Abdomen, Hands, Legs) —
  // select every "Normal" checkbox.
  const normalCheckboxes = page.locator('.ui.checkbox label', {
    hasText: /^Normal/i,
  });
  const normalCount = await normalCheckboxes.count();
  for (let i = 0; i < normalCount; i++) {
    await click(normalCheckboxes.nth(i), page);
  }

  await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
  await page.waitForTimeout(1000);

  // --- Obstetrical Exam (not shown for postpartum) ---
  if (!options?.isPostpartum) {
    await clickSubTaskTab(page, 'obstetrical-exam');
    await page.waitForTimeout(500);

    // Is fundal palpable? → Yes
    await answerYesNo(page, 'fundal-palpable', 'Yes');
    await page.waitForTimeout(500);

    // Fundal height measurement (appears after answering Yes above).
    const fundalInput = page.locator(
      '.form-input.measurement.fundal-height input[type="number"]',
    );
    if (await fundalInput.isVisible({ timeout: 3000 }).catch(() => false)) {
      await fundalInput.fill('30');
    }
    await page.waitForTimeout(300);

    // Fetal presentation — select "Cephalic".
    await selectCheckbox(page, 'Cephalic');
    await page.waitForTimeout(300);

    // Fetal movement → Yes
    await answerYesNo(page, 'fetal-movement', 'Yes');
    await page.waitForTimeout(300);

    // Fetal heart rate.
    const heartRateInput = page.locator(
      '.form-input.measurement.fetal-heart-rate input[type="number"]',
    );
    if (await heartRateInput.isVisible({ timeout: 3000 }).catch(() => false)) {
      await heartRateInput.fill('140');
    }
    await page.waitForTimeout(300);

    // Previous c-section scar → None
    await selectCheckbox(page, 'None');

    await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
    await page.waitForTimeout(1000);
  }

  // --- Breast Exam ---
  await clickSubTaskTab(page, 'breast-exam');
  await page.waitForTimeout(500);

  // Breast exam — select "Normal".
  await selectCheckbox(page, 'Normal');

  // Self-examination guidance → Yes
  const selfExamYes = page.locator('.form-input.yes-no label', { hasText: 'Yes' }).first();
  if (await selfExamYes.isVisible()) {
    await click(selfExamYes, page);
  }

  await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
  await page.waitForTimeout(1000);

  // --- GU Exam (postpartum only) ---
  if (options?.isPostpartum) {
    await clickSubTaskTab(page, 'gu-exam');
    await page.waitForTimeout(500);

    // Vaginal Examination: select "Normal".
    await selectCheckbox(page, 'Normal');

    // Episiotomy/perineal tear: select "No".
    await answerYesNo(page, 'episiotomy', 'No');
    await page.waitForTimeout(500);

    // Rectal Hemorrhoids? (appears after answering episiotomy): select "No".
    await answerYesNo(page, 'rectal-hemorrhoids', 'No');

    await click(page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }), page);
    await page.waitForTimeout(1000);
  }

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });
}

/**
 * Complete SymptomReview: Step 1 (select "None"), save.
 * With "None" selected, there's no step 2 (no follow-up questions).
 * Creates: prenatal_symptom_review
 */
export async function completeSymptomReview(page: Page) {
  await openActivity(page, 'symptoms');

  // Wait for the symptom checkboxes to render.
  const noneCheckbox = page.locator('.ui.checkbox.activity label', {
    hasText: /^None of these$/i,
  });
  await noneCheckbox.waitFor({ timeout: 10000 });

  // Click "None of these".
  await click(noneCheckbox, page);

  // Wait for Save button to become active (Elm only attaches onClick when active).
  await page.locator('button.ui.fluid.primary.button.active').waitFor({ timeout: 10000 });

  await savePrenatalActivity(page);
}

/**
 * Complete MaternalMentalHealth: answer all sequential questions.
 * 10 EPDS questions + specialist question.
 * Creates: prenatal_mental_health
 */
export async function completeMentalHealth(page: Page) {
  await openActivity(page, 'mental-health');

  // Questions 1-10: Each shows one question with multiple answer options.
  // Click the best (score=0) answer for each question.
  // Questions 1,2,4 have normal order (Option0 first).
  // Questions 3,5-10 have reversed order (Option3 first, Option0 last).
  const normalOrder = [0, 1, 3]; // 0-indexed question numbers
  for (let q = 0; q < 10; q++) {
    const options = page.locator('.ui.checkbox label');
    if (normalOrder.includes(q)) {
      await click(options.first(), page);
    } else {
      await click(options.last(), page);
    }

    // Click the forward/next button to advance.
    const nextBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await click(nextBtn, page);
    await page.waitForTimeout(500);
  }

  // Question 11: Specialist at HC → Yes
  await answerYesNo(page, 'specialist', 'Yes');

  // With Option0 selected for all questions (including Q10), no suicide
  // risk is diagnosed, so Save navigates directly back to encounter page.
  await savePrenatalActivity(page);
}

/**
 * Complete PrenatalImmunisation: mark tetanus vaccination.
 * Creates: prenatal_tetanus_immunisation
 *
 * Flow: answer "No" to "previously administered?" → "Yes" to "receive today?"
 * This keeps the form in ViewModeInitial so the regular Save button works.
 */
export async function completeImmunisation(page: Page) {
  await openActivity(page, 'immunisation');

  // The Tetanus tab should be available.
  const tetanusTab = page.locator('.link-section:has(.icon-activity-task.icon-tetanus-vaccine)');
  if (await tetanusTab.isVisible()) {
    await click(tetanusTab, page);
    await page.waitForTimeout(500);
  }

  // Question 1: "Was Tetanus Dose 1 previously administered?" → No
  const yesNoInputs = page.locator('.form-input.yes-no');
  await yesNoInputs.first().waitFor({ timeout: 5000 });
  await click(yesNoInputs.first().locator('label', { hasText: 'No' }), page);
  await page.waitForTimeout(500);

  // Question 2: "Will patient receive Tetanus Dose 1 today?" → Yes
  // This second question appears after answering "No" to the first.
  await yesNoInputs.nth(1).waitFor({ timeout: 5000 });
  await click(yesNoInputs.nth(1).locator('label', { hasText: 'Yes' }), page);
  await page.waitForTimeout(500);

  // Save Tetanus tab → auto-switches to Overview tab.
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);

  // Save Overview tab → navigates back to encounter page.
  await savePrenatalActivity(page);
}

/**
 * Complete Medication: iterate through visible medication tabs.
 * Each tab: select whether medication was administered.
 * Creates: prenatal_iron, prenatal_folate, etc.
 */
export async function completeMedication(page: Page): Promise<string[]> {
  await openActivity(page, 'medication');

  const completedMeds: string[] = [];
  const medicationIcons = [
    'calcium', 'fefol', 'folate', 'iron', 'mms', 'mebendezole',
  ];

  for (const med of medicationIcons) {
    const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${med})`);
    if (await tab.isVisible()) {
      await click(tab, page);
      await page.waitForTimeout(500);

      // Each medication form has a "Not Administered" note selection or
      // administration toggle. For simplicity, try to save with default values.
      // First, check if there's a Yes/No toggle for administration.
      const yesLabel = page.locator('.form-input.yes-no label', { hasText: 'Yes' }).first();
      if (await yesLabel.isVisible()) {
        await click(yesLabel, page);
      } else {
        // Try selecting a reason for non-administration.
        const firstCheckbox = page.locator('.ui.checkbox label').first();
        if (await firstCheckbox.isVisible()) {
          await click(firstCheckbox, page);
        }
      }

      // Save this medication tab.
      const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
      if (await saveBtn.isVisible()) {
        await click(saveBtn, page);
        completedMeds.push(med);
        await page.waitForTimeout(500);
      }
    }
  }

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });

  return completedMeds;
}

/**
 * Complete Laboratory for Nurse: iterate through visible lab test tabs.
 * Each test: interact with the form to complete it.
 * Creates: prenatal_hiv_test, prenatal_syphilis_test, etc.
 */
export async function completeLaboratoryNurse(page: Page): Promise<string[]> {
  await openActivity(page, 'laboratory');

  const completedTests: string[] = [];

  // Iterate through all visible task tabs. We can't use icon classes because
  // Partner HIV, HIV, and HIV PCR all share the same icon-laboratory-hiv class.
  const allTabs = page.locator('.link-section');
  const tabCount = await allTabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = allTabs.nth(i);
    if (!(await tab.isVisible())) continue;

    // Skip if already completed.
    const isCompleted = await tab.evaluate(el =>
      el.classList.contains('completed'),
    ).catch(() => false);
    if (isCompleted) continue;

    // Skip if this is the currently active tab (already showing its form).
    const isActive = await tab.evaluate(el =>
      el.classList.contains('active'),
    ).catch(() => false);
    if (!isActive) {
      await click(tab, page);
      await page.waitForTimeout(500);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // Answer yes/no fields by their specific CSS classes.
    // Fields appear sequentially: known-as-positive → test-performed → why-not → blood-smear.

    // 1. "Known as positive?" (HIV, Partner HIV, Hepatitis B) → No
    const knownPositive = page.locator('.form-input.yes-no.known-as-positive');
    if (await knownPositive.isVisible().catch(() => false)) {
      await click(knownPositive.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(500);
    }

    // 2. "Will this test be performed today?" → No
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      await click(testPerformed.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(500);
    }

    // 3. "Why not?" reason checkbox → select first option
    const whyNot = page.locator('.why-not .ui.checkbox label').first();
    if (await whyNot.isVisible().catch(() => false)) {
      await click(whyNot, page);
      await page.waitForTimeout(300);
    }

    // 4. "Did you perform a blood smear?" (Malaria only) → No
    const bloodSmear = page.locator('.form-input.yes-no.got-results-previously');
    if (await bloodSmear.isVisible().catch(() => false)) {
      await click(bloodSmear.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(500);
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(500);
    }
  }

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });

  return completedTests;
}

/**
 * Complete Laboratory for Nurse, ordering tests for lab processing.
 * Unlike completeLaboratoryNurse (which declines all tests), this helper
 * answers "Yes" to performing each test and selects "Lab" (not Point of Care)
 * for the immediate-result question. This leaves results pending for a Lab
 * Technician to enter later via Case Management.
 * Creates: prenatal_hiv_test, prenatal_syphilis_test, etc. with executionNote=RunToday.
 */
export async function completeLaboratoryNurseForLab(page: Page): Promise<string[]> {
  await openActivity(page, 'laboratory');

  const completedTests: string[] = [];
  const allTabs = page.locator('.link-section');
  const tabCount = await allTabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = allTabs.nth(i);
    if (!(await tab.isVisible())) continue;

    const isCompleted = await tab.evaluate(el =>
      el.classList.contains('completed'),
    ).catch(() => false);
    if (isCompleted) continue;

    const isActive = await tab.evaluate(el =>
      el.classList.contains('active'),
    ).catch(() => false);
    if (!isActive) {
      await click(tab, page);
      await page.waitForTimeout(500);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // 1. "Known as positive?" (HIV, Partner HIV, Hepatitis B) → No
    const knownPositive = page.locator('.form-input.yes-no.known-as-positive');
    if (await knownPositive.isVisible().catch(() => false)) {
      await click(knownPositive.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(500);
    }

    // 2. "Will this test be performed today?" → Yes
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      await click(testPerformed.locator('label', { hasText: 'Yes' }), page);
      await page.waitForTimeout(500);
    }

    // 3. "Immediate result?" → Lab (the "No" side of the bool input)
    const immediateResult = page.locator('.form-input.yes-no.immediate-result');
    if (await immediateResult.isVisible().catch(() => false)) {
      await click(immediateResult.locator('label', { hasText: 'Lab' }), page);
      await page.waitForTimeout(500);
    }

    // 4. "Urine Dipstick variant?" → Short Dip (checkbox, appears for Urine Dipstick)
    const shortDip = page.locator('.ui.checkbox label', { hasText: /^Short Dip$/i });
    if (await shortDip.isVisible().catch(() => false)) {
      await click(shortDip, page);
      await page.waitForTimeout(300);
    }

    // 5. "Was this test performed before a meal?" → No (Random Blood Sugar only)
    const patientFasted = page.locator('.form-input.yes-no.patient-fasted');
    if (await patientFasted.isVisible().catch(() => false)) {
      await click(patientFasted.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(300);
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(500);
    }
  }

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });

  return completedTests;
}

/**
 * Complete Lab Results as Lab Technician: iterate through visible lab test
 * tabs on the LabResults recurrent activity page. For each test:
 * 1. Confirm the test was run ("Yes" to test-performed).
 * 2. Enter a result (select from dropdown or fill a numeric input).
 * 3. Save.
 * Returns the list of completed test tab labels.
 */
export async function completeLabResultsAsLabTech(page: Page): Promise<string[]> {
  const completedTests: string[] = [];
  const allTabs = page.locator('.link-section');
  const tabCount = await allTabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = allTabs.nth(i);
    if (!(await tab.isVisible())) continue;

    const isCompleted = await tab.evaluate(el =>
      el.classList.contains('completed'),
    ).catch(() => false);
    if (isCompleted) continue;

    const isActive = await tab.evaluate(el =>
      el.classList.contains('active'),
    ).catch(() => false);
    if (!isActive) {
      await click(tab, page);
      await page.waitForTimeout(500);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // 1. "Will this test be performed today?" → Yes (confirms run by lab tech)
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      await click(testPerformed.locator('label', { hasText: 'Yes' }), page);
      await page.waitForTimeout(500);
    }

    // 2. Enter result — fill all visible select dropdowns and numeric inputs.
    // The select element itself has class "form-input select" (not a wrapper).
    const allSelects = page.locator('select.form-input');
    const selectCount = await allSelects.count();
    for (let s = 0; s < selectCount; s++) {
      const sel = allSelects.nth(s);
      if (await sel.isVisible().catch(() => false)) {
        const opts = sel.locator('option');
        const oCount = await opts.count();
        if (oCount > 1) {
          const currentVal = await sel.inputValue();
          if (!currentVal) {
            const val = await opts.nth(1).getAttribute('value');
            if (val !== null) {
              await sel.selectOption(val);
              await page.waitForTimeout(300);
            }
          }
        }
      }
    }

    // Numeric inputs (hemoglobin count, random blood sugar, etc.)
    // Elm renders these as: <div class="form-input measurement <id>"><input type="number" ...>
    const numericInputs = page.locator('.form-input.measurement input[type="number"]');
    const numCount = await numericInputs.count();
    for (let n = 0; n < numCount; n++) {
      const numInput = numericInputs.nth(n);
      if (await numInput.isVisible().catch(() => false)) {
        const currentVal = await numInput.inputValue();
        if (!currentVal) {
          await numInput.fill('12');
          await page.waitForTimeout(300);
        }
      }
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(500);
    }
  }

  return completedTests;
}

/**
 * Complete NextSteps: iterate through visible sub-task tabs.
 * Creates: appointment_confirmation, prenatal_follow_up, prenatal_send_to_hc, etc.
 */
export async function completeNextSteps(page: Page): Promise<string[]> {
  // For CHW encounters with no danger signs, the icon changes to
  // "appointment-confirmation" instead of "next-steps".
  // For subsequent encounters with no diagnoses, NextSteps may not appear at all.
  const nextStepsIcon = page.locator('.icon-task-next-steps');
  const appointmentIcon = page.locator('.icon-task-appointment-confirmation');
  if (await nextStepsIcon.isVisible({ timeout: 1000 }).catch(() => false)) {
    await openActivity(page, 'next-steps');
  } else if (await appointmentIcon.isVisible({ timeout: 1000 }).catch(() => false)) {
    await openActivity(page, 'appointment-confirmation');
  } else {
    // NextSteps not available for this encounter type/state.
    return [];
  }

  // Dismiss any warning popup that may appear (e.g., "Depression not Likely").
  const warningContinue = page.locator('button', { hasText: 'Continue' });
  if (await warningContinue.isVisible({ timeout: 2000 }).catch(() => false)) {
    await warningContinue.click({ force: true });
    await page.waitForTimeout(500);
  }

  const completedSteps: string[] = [];
  const nextStepIcons = [
    { icon: 'next-steps-send-to-hc', name: 'appointmentConfirmation' }, // AppointmentConfirmation uses send-to-hc icon!
    { icon: 'next-steps-follow-up', name: 'followUp' },
    { icon: 'next-steps-referral', name: 'sendToHC' },
    { icon: 'next-steps-health-education', name: 'healthEducation' },
    { icon: 'next-steps-treatment', name: 'medicationDistribution' },
    { icon: 'next-steps-next-visit', name: 'nextVisit' },
    { icon: 'next-steps-wait', name: 'wait' },
  ];

  for (const step of nextStepIcons) {
    const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${step.icon})`);
    if (await tab.isVisible()) {
      const isCompleted = await tab.evaluate(el => el.classList.contains('completed')).catch(() => false);
      if (isCompleted) continue;

      await click(tab, page);
      await page.waitForTimeout(500);

      // Handle each NextSteps sub-task type.
      if (step.name === 'appointmentConfirmation') {
        // Select an appointment date via calendar popup.
        const dateInput = page.locator('.form-input.date');
        if (await dateInput.isVisible()) {
          const appointmentDate = new Date();
          appointmentDate.setDate(appointmentDate.getDate() + 30);
          await setDate(page, appointmentDate, '.form-input.date');
        }
      } else if (step.name === 'followUp') {
        // Select a follow-up period (e.g., "3 Days").
        const firstOption = page.locator('.ui.checkbox label').first();
        if (await firstOption.isVisible()) {
          await click(firstOption, page);
        }
      } else if (step.name === 'sendToHC') {
        // Referral form — answer Yes/No fields.
        const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
        const yesCount = await yesLabels.count();
        for (let i = 0; i < yesCount; i++) {
          await click(yesLabels.nth(i), page);
        }
      } else if (step.name === 'healthEducation') {
        // Health education — answer Yes/No fields.
        const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
        const yesCount = await yesLabels.count();
        for (let i = 0; i < yesCount; i++) {
          await click(yesLabels.nth(i), page);
        }
      } else if (step.name === 'medicationDistribution') {
        // Medication distribution — try to fill the form.
        const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
        const yesCount = await yesLabels.count();
        for (let i = 0; i < yesCount; i++) {
          await click(yesLabels.nth(i), page);
        }
        // Select recommended treatment if checkbox is visible.
        const treatmentCheckbox = page.locator('.ui.checkbox label').first();
        if (await treatmentCheckbox.isVisible()) {
          await click(treatmentCheckbox, page);
        }
      } else if (step.name === 'nextVisit') {
        // Next visit date — auto-calculated, just save.
        // May have a date input or just display the calculated date.
        const dateInput = page.locator('.form-input.date');
        if (await dateInput.isVisible()) {
          const nextDate = new Date();
          nextDate.setDate(nextDate.getDate() + 30);
          await setDate(page, nextDate, '.form-input.date');
        }
      } else if (step.name === 'wait') {
        // Wait has no form inputs. It shows "Pause Encounter" button instead of "Save".
        // Clicking it closes the encounter and navigates to PinCodePage.
        const pauseBtn = page.locator('div.actions button', { hasText: 'Pause Encounter' });
        if (await pauseBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
          await click(pauseBtn, page);
          completedSteps.push(step.name);
          // App navigates to PinCodePage after pause — return immediately.
          await page.waitForTimeout(2000);
          return completedSteps;
        }
      }

      // Save this sub-task.
      const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
      if (await saveBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(saveBtn, page);
        completedSteps.push(step.name);
        await page.waitForTimeout(500);
      }
    }
  }

  // After saving the last tab, the app may return to the encounter page
  // OR navigate to the progress report (if all activities are now done).
  // Accept either destination.
  await Promise.race([
    page.locator('div.page-encounter.prenatal').waitFor({ timeout: 15000 }),
    page.locator('div.page-activity.prenatal').waitFor({ state: 'hidden', timeout: 15000 }),
  ]).catch(() => {});
  await page.waitForTimeout(500);

  return completedSteps;
}

// ---------------------------------------------------------------------------
// Subsequent/Postpartum activity helpers
// ---------------------------------------------------------------------------

/**
 * Complete TreatmentReview: iterate through medication review tabs.
 * Creates: medication
 */
export async function completeTreatmentReview(page: Page) {
  await openActivity(page, 'prior-treatment');

  // Treatment review has tabs for each medication type being reviewed.
  // All tabs use icon-medication class.
  // Iterate through visible tabs and answer "Taking as prescribed? → Yes".
  const tabs = page.locator('.link-section:has(.icon-activity-task.icon-medication)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = tabs.nth(i);
    const isCompleted = await tab.evaluate(el => el.classList.contains('completed')).catch(() => false);
    if (isCompleted) continue;

    await click(tab, page);
    await page.waitForTimeout(500);

    // Answer all Yes/No questions on this tab with "Yes".
    const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
    const yesCount = await yesLabels.count();
    for (let j = 0; j < yesCount; j++) {
      await click(yesLabels.nth(j), page);
      await page.waitForTimeout(200);
    }

    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      await page.waitForTimeout(500);
    }
  }

  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 10000 });
}

/**
 * Complete Breastfeeding: answer breastfeeding questions.
 * Creates: prenatal_breastfeeding
 */
export async function completeBreastfeeding(page: Page) {
  await openActivity(page, 'breastfeeding');

  // "Are you breastfeeding?" → Yes (triggers 4 more questions).
  await answerYesNo(page, 'is-breastfeeding', 'Yes');
  await page.waitForTimeout(500);

  // Answer the follow-up questions.
  await answerYesNo(page, 'breast-pain', 'No');
  await answerYesNo(page, 'breast-redness', 'No');
  await answerYesNo(page, 'enough-milk', 'Yes');
  await answerYesNo(page, 'latching-well', 'Yes');

  await savePrenatalActivity(page);
}

/**
 * Complete SpecialityCare (if visible).
 * Creates: prenatal_speciality_care
 */
export async function completeSpecialityCare(page: Page): Promise<boolean> {
  // SpecialityCare is conditional — only shown if prior diagnoses require it.
  const activityCard = page.locator('.icon-task-speciality-care');
  if (!(await activityCard.isVisible())) {
    return false;
  }

  await openActivity(page, 'speciality-care');

  // Fill the form — answer Yes/No fields.
  const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
  const count = await yesLabels.count();
  for (let i = 0; i < count; i++) {
    await click(yesLabels.nth(i), page);
  }

  await savePrenatalActivity(page);
  return true;
}

/**
 * Complete PostpartumTreatmentReview: simple treatment review.
 * Creates: medication
 */
export async function completePostpartumTreatmentReview(page: Page) {
  await openActivity(page, 'postpartum-treatment-review');

  // Answer medication questions.
  const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
  const count = await yesLabels.count();
  for (let i = 0; i < count; i++) {
    await click(yesLabels.nth(i), page);
  }

  await savePrenatalActivity(page);
}

/**
 * Complete PregnancyOutcome: fill delivery information.
 * This activity redirects to the outcome page.
 */
export async function completePregnancyOutcome(page: Page) {
  // Pregnancy Outcome uses a special page (div.page-outcome.pregnancy),
  // not the standard activity page.
  await click(page.locator('.icon-task-pregnancy-outcome'), page);
  await page.locator('div.page-outcome.pregnancy').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);

  // 1. Date Pregnancy Concluded — calendar popup via .form-input.date.
  await setDate(page, new Date(), '.form-input.date');

  // 2. Pregnancy Outcome — dropdown, select "Live Birth at Term".
  const outcomeSelect = page.locator('select').first();
  await outcomeSelect.selectOption({ index: 1 });
  await page.waitForTimeout(300);

  // 3. Delivery Location — bool input (Facility/Home).
  await click(
    page.locator('.form-input.yes-no.delivery-location label', { hasText: 'Facility' }),
    page,
  );

  // Save.
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );

  // Wait for return to encounter page.
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 15000 });
}

// ---------------------------------------------------------------------------
// Recurrent encounter helpers
// ---------------------------------------------------------------------------

/**
 * Navigate to Case Management from the nurse menu (PinCodePage when logged in).
 */
export async function navigateToCaseManagement(page: Page) {
  // Menu card icons use class "icon-task icon-task-{name}".
  await click(page.locator('.icon-task-case-management'), page);
  await page.locator('.page-case-management').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Open a recurrent encounter from the Case Management Prenatal Labs pane.
 * Finds the patient entry by name and clicks the forward icon.
 */
export async function openRecurrentEncounterFromCaseManagement(
  page: Page,
  personName: string,
) {
  const entry = page.locator('.follow-up-entry', {
    has: page.locator('.name', { hasText: personName }),
  });
  await entry.waitFor({ timeout: 10000 });
  await click(entry.locator('.icon-forward'), page);
  // Recurrent encounter page has same CSS as initial: div.page-encounter.prenatal.
  await page.locator('div.page-encounter.prenatal').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

/**
 * Complete RecurrentExamination: vitals recheck (BP only).
 * The recurrent vitals form uses VitalsFormRepeated mode — only sys and dia
 * blood pressure fields (no heart rate, respiratory rate, body temperature).
 * After saving the last recurrent activity, the app auto-navigates to the
 * Clinical Progress Report page.
 */
export async function completeRecurrentExamination(
  page: Page,
  options?: { sys?: string; dia?: string },
) {
  await openActivity(page, 'examination');

  // VitalsFormRepeated: only BP fields shown.
  await fillMeasurement(page, 'sys-blood-pressure', options?.sys ?? '120');
  await fillMeasurement(page, 'dia-blood-pressure', options?.dia ?? '80');

  // Save.
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );
  await page.waitForTimeout(2000);

  // After saving the last recurrent activity, the app auto-navigates to
  // ClinicalProgressReportPage (div.page-report.clinical).
  await page
    .locator('div.page-report.clinical')
    .waitFor({ timeout: 15000 });
  await page.waitForTimeout(500);
}

/**
 * End the recurrent encounter from the Clinical Progress Report page.
 * Clicking "End Encounter" navigates directly to PinCodePage (no dialog).
 */
export async function endRecurrentEncounter(page: Page) {
  const endBtn = page.locator('button', { hasText: 'End Encounter' });
  await endBtn.waitFor({ timeout: 10000 });
  await click(endBtn, page);
  await page.waitForTimeout(3000);
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query Drupal for all prenatal-related measurement nodes linked to a person.
 * Returns an object keyed by node type with boolean (exists) values.
 */
/**
 * Backdate the most recent prenatal encounter for a given person to yesterday.
 * This is needed because the app prevents starting a subsequent encounter on
 * the same day the previous encounter was completed.
 */
export function backdatePrenatalEncounter(personName: string) {
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
      ->propertyCondition('type', 'prenatal_encounter')
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

  // Retry: the person may not yet be uploaded after the first sync cycle.
  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    }).trim();
    console.log(`backdatePrenatalEncounter attempt ${attempt + 1}:`, output);
    if (output.startsWith('Backdated')) {
      return;
    }
    if (attempt < 4) {
      execSync('sleep 10');
    }
  }
  console.error('backdatePrenatalEncounter: failed after 5 attempts');
}

export function queryPrenatalNodes(
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
      'last_menstrual_period',
      'obstetric_history',
      'obstetric_history_step2',
      'medical_history',
      'social_history',
      'prenatal_outside_care',
      'vitals',
      'prenatal_nutrition',
      'core_physical_exam',
      'obstetrical_exam',
      'breast_exam',
      'prenatal_gu_exam',
      'prenatal_family_planning',
      'danger_signs',
      'prenatal_symptom_review',
      'prenatal_mental_health',
      'prenatal_tetanus_immunisation',
      'prenatal_iron',
      'prenatal_folate',
      'prenatal_fefol',
      'prenatal_calcium',
      'prenatal_aspirin',
      'prenatal_mms',
      'prenatal_mebendazole',
      'prenatal_hiv_test',
      'prenatal_syphilis_test',
      'prenatal_hepatitis_b_test',
      'prenatal_malaria_test',
      'prenatal_blood_gprs_test',
      'prenatal_urine_dipstick_test',
      'prenatal_hemoglobin_test',
      'prenatal_random_blood_sugar_test',
      'prenatal_hiv_pcr_test',
      'prenatal_partner_hiv_test',
      'prenatal_labs_results',
      'pregnancy_testing',
      'prenatal_health_education',
      'prenatal_follow_up',
      'prenatal_send_to_hc',
      'appointment_confirmation',
      'prenatal_medication_distribution',
      'medication',
      'birth_plan',
      'prenatal_breastfeeding',
      'prenatal_speciality_care',
      'prenatal_photo',
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

  // Retry up to 5 times with 10s delay.  The person node may not yet be
  // uploaded when the first sync cycle only completed downloads.
  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    });
    try {
      const parsed = JSON.parse(output.trim());
      if (parsed.error) {
        console.log(`queryPrenatalNodes attempt ${attempt + 1}: ${parsed.error}`);
      } else if (expectedTypes && expectedTypes.length > 0) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) {
          return parsed;
        }
        console.log(`queryPrenatalNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
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
