import { Page, Locator } from '@playwright/test';
import { click } from './auth';
import {
  WAIT,
  answerYesNo,
  backdateEncounter,
  clickSubTaskTab,
  fillMeasurement,
  formInput,
  openActivity,
  queryMeasurementNodes,
  registerAdult,
  saveActivity,
  saveSubTask,
  selectByLabel,
  selectCheckbox,
  setDate,
} from './common';

// ---------------------------------------------------------------------------
// Private form helpers
// ---------------------------------------------------------------------------

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
  const encounterType = options?.encounterType ?? 'first';

  const result = await registerAdult(page, 'Antenatal Care', 'prenatal', {
    ageYears: options?.ageYears ?? 25,
    firstName: options?.firstName ?? `TestMother${Date.now()}`,
    isFemale: true,
    isChw: options?.isChw,
  });

  // Start the encounter.
  await startPrenatalEncounter(page, encounterType);

  return result;
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

  // Wait for the button to be enabled (not .disabled) — after backdating,
  // the Elm model may need time to re-render with updated encounter data.
  const btn = page.locator('div.ui.primary.button:not(.disabled)', {
    hasText: buttonText,
  });
  await btn.waitFor({ timeout: 30000 });
  await click(btn, page);
  await page
    .locator('div.page-encounter.prenatal')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(WAIT.sectionTransition);

  // Dismiss any informational popup (e.g. "has not yet been seen at the
  // health center for this pregnancy").
  const okBtn = page.locator('button', { hasText: 'OK' });
  if (await okBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
    await okBtn.click({ force: true });
    await page.waitForTimeout(WAIT.elmRerender);
  }
}

/**
 * End the current prenatal encounter: click "End Encounter",
 * confirm in the dialog, wait for navigation away.
 */
export async function endPrenatalEncounter(page: Page) {
  await page.waitForTimeout(WAIT.pageNavigation);

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
  await page.waitForTimeout(WAIT.heavyOperation);
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
  await openActivity(page, 'prenatal', 'pregnancy-dating');

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

  await saveActivity(page, 'prenatal');
}

/**
 * Complete DangerSigns. Default: select "None of These" (normal path).
 * With { premature: true }: select "Premature Onset of Contractions"
 * instead — used to exercise IndicatorPrematureOnsetContractions on the
 * Peripartum report. Selecting that danger sign does NOT trigger any
 * additional prenatal diagnoses (verified against
 * client/src/elm/Pages/Prenatal/Activity/Utils.elm), so existing
 * downstream assertions remain valid.
 * Creates: danger_signs
 */
export async function completeDangerSigns(
  page: Page,
  options?: { premature?: boolean },
) {
  await openActivity(page, 'prenatal', 'danger-signs');

  if (options?.premature) {
    await selectCheckbox(page, 'Premature Onset of Contractions');
  } else {
    await selectCheckbox(page, 'None of These');
  }

  await saveActivity(page, 'prenatal');
}

/**
 * Complete FamilyPlanning: select one method.
 * Creates: prenatal_family_planning
 */
export async function completeFamilyPlanning(page: Page) {
  await openActivity(page, 'prenatal', 'planning');

  // Select "Auto-observation" as the family planning method.
  await selectCheckbox(page, 'Auto-observation');

  await saveActivity(page, 'prenatal');
}

/**
 * Complete MalariaPrevention: answer Yes to mosquito net.
 * Creates: malaria prevention node
 */
export async function completeMalariaPrevention(page: Page) {
  await openActivity(page, 'prenatal', 'malaria');

  await answerYesNo(page, 'mosquito-net', 'Yes');

  await saveActivity(page, 'prenatal');
}

/**
 * Complete Laboratory for CHW: select pregnancy test result.
 * Creates: pregnancy_testing
 */
export async function completeLaboratoryChw(page: Page) {
  await openActivity(page, 'prenatal', 'laboratory');

  // The CHW lab form is a dropdown for pregnancy test result.
  // Select "Positive".
  const selectEl = page.locator('select').first();
  await selectEl.waitFor({ timeout: 5000 });
  await selectEl.selectOption({ index: 1 });

  await saveActivity(page, 'prenatal');
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
  await openActivity(page, 'prenatal', 'health-education');

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

  await saveActivity(page, 'prenatal');
}

/**
 * Complete BirthPlan: answer all Yes/No questions + family planning selection.
 * Creates: birth_plan
 */
export async function completeBirthPlan(page: Page) {
  await openActivity(page, 'prenatal', 'birth-plan');

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

  await saveActivity(page, 'prenatal');
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
  options?: { isSubsequent?: boolean; preeclampsiaPrevious?: boolean },
) {
  await openActivity(page, 'prenatal', 'history');

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

    await saveSubTask(page);

    // --- Obstetric History Step 2 ---
    await page.locator('.form.history.obstetric.second').waitFor({ timeout: 5000 });
    await answerYesNo(page, 'c-section-past', 'No');
    // First checkbox group: "Previous delivery" → "None of these".
    await selectCheckbox(page, 'None of these');
    // Second checkbox group: "conditions during previous pregnancy".
    if (options?.preeclampsiaPrevious) {
      await selectCheckbox(page, 'Preeclampsia');
    } else {
      await selectCheckbox(page, 'None of the above');
    }

    await saveSubTask(page);

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

    await saveSubTask(page);
  }

  // --- OutsideCare (subsequent encounters only) ---
  if (options?.isSubsequent) {
    // "Have you been seen at another facility since your last visit?" → No
    await answerYesNo(page, 'seen-at-another-facility', 'No');

    await saveSubTask(page);
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
  await openActivity(page, 'prenatal', 'examination');

  // --- Vitals ---
  await clickSubTaskTab(page, 'vitals');
  await page.waitForTimeout(WAIT.elmRerender);

  await fillMeasurement(page, 'sys-blood-pressure', options?.vitals?.sys ?? '120');
  await fillMeasurement(page, 'dia-blood-pressure', options?.vitals?.dia ?? '80');
  await fillMeasurement(page, 'heart-rate', '72');
  await fillMeasurement(page, 'respiratory-rate', '18');
  await fillMeasurement(page, 'body-temperature', '36.5');

  await saveSubTask(page);

  // --- Nutrition Assessment ---
  await clickSubTaskTab(page, 'nutrition-assessment');
  await page.waitForTimeout(WAIT.elmRerender);

  // Height might be hidden if previously measured; fill if visible.
  const heightInput = page.locator('.form-input.measurement.height input[type="number"]');
  if (await heightInput.isVisible()) {
    await heightInput.fill('160');
  }
  await fillMeasurement(page, 'weight', '60');
  await fillMeasurement(page, 'muac', '25');

  await saveSubTask(page);

  // --- Core Physical Exam ---
  await clickSubTaskTab(page, 'core-physical-exam');
  await page.waitForTimeout(WAIT.elmRerender);

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

  await saveSubTask(page);

  // --- Obstetrical Exam (not shown for postpartum) ---
  if (!options?.isPostpartum) {
    await clickSubTaskTab(page, 'obstetrical-exam');
    await page.waitForTimeout(WAIT.elmRerender);

    // Is fundal palpable? → Yes
    await answerYesNo(page, 'fundal-palpable', 'Yes');
    await page.waitForTimeout(WAIT.elmRerender);

    // Fundal height measurement (appears after answering Yes above).
    const fundalInput = page.locator(
      '.form-input.measurement.fundal-height input[type="number"]',
    );
    if (await fundalInput.isVisible({ timeout: 3000 }).catch(() => false)) {
      await fundalInput.fill('30');
    }
    await page.waitForTimeout(WAIT.formInteraction);

    // Fetal presentation — select "Cephalic".
    await selectCheckbox(page, 'Cephalic');
    await page.waitForTimeout(WAIT.formInteraction);

    // Fetal movement → Yes
    await answerYesNo(page, 'fetal-movement', 'Yes');
    await page.waitForTimeout(WAIT.formInteraction);

    // Fetal heart rate.
    const heartRateInput = page.locator(
      '.form-input.measurement.fetal-heart-rate input[type="number"]',
    );
    if (await heartRateInput.isVisible({ timeout: 3000 }).catch(() => false)) {
      await heartRateInput.fill('140');
    }
    await page.waitForTimeout(WAIT.formInteraction);

    // Previous c-section scar → None
    await selectCheckbox(page, 'None');

    await saveSubTask(page);
  }

  // --- Breast Exam ---
  await clickSubTaskTab(page, 'breast-exam');
  await page.waitForTimeout(WAIT.elmRerender);

  // Breast exam — select "Normal".
  await selectCheckbox(page, 'Normal');

  // Self-examination guidance → Yes
  const selfExamYes = page.locator('.form-input.yes-no.self-guidance label', { hasText: 'Yes' });
  if (await selfExamYes.isVisible()) {
    await click(selfExamYes, page);
  }

  await saveSubTask(page);

  // --- GU Exam (postpartum only) ---
  if (options?.isPostpartum) {
    await clickSubTaskTab(page, 'gu-exam');
    await page.waitForTimeout(WAIT.elmRerender);

    // Vaginal Examination: select "Normal".
    await selectCheckbox(page, 'Normal');

    // Episiotomy/perineal tear: select "No".
    await answerYesNo(page, 'episiotomy', 'No');
    await page.waitForTimeout(WAIT.elmRerender);

    // Rectal Hemorrhoids? (appears after answering episiotomy): select "No".
    await answerYesNo(page, 'rectal-hemorrhoids', 'No');

    await saveSubTask(page);
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
  await openActivity(page, 'prenatal', 'symptoms');

  // Wait for the symptom checkboxes to render.
  const noneCheckbox = page.locator('.ui.checkbox.activity label', {
    hasText: /^None of these$/i,
  });
  await noneCheckbox.waitFor({ timeout: 10000 });

  // Click "None of these".
  await click(noneCheckbox, page);

  // Wait for Save button to become active (Elm only attaches onClick when active).
  await page.locator('button.ui.fluid.primary.button.active').waitFor({ timeout: 10000 });

  await saveActivity(page, 'prenatal');
}

/**
 * Complete MaternalMentalHealth: answer all sequential questions.
 * 10 EPDS questions + specialist question.
 * Creates: prenatal_mental_health
 */
export async function completeMentalHealth(page: Page) {
  await openActivity(page, 'prenatal', 'mental-health');

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
    await page.waitForTimeout(WAIT.elmRerender);
  }

  // Question 11: Specialist at HC → Yes
  await answerYesNo(page, 'specialist', 'Yes');

  // With Option0 selected for all questions (including Q10), no suicide
  // risk is diagnosed, so Save navigates directly back to encounter page.
  await saveActivity(page, 'prenatal');
}

/**
 * Complete PrenatalImmunisation: mark tetanus vaccination.
 * Creates: prenatal_tetanus_immunisation
 *
 * Flow: answer "No" to "previously administered?" → "Yes" to "receive today?"
 * This keeps the form in ViewModeInitial so the regular Save button works.
 */
export async function completeImmunisation(page: Page) {
  await openActivity(page, 'prenatal', 'immunisation');

  // The Tetanus tab should be available.
  const tetanusTab = page.locator('.link-section:has(.icon-activity-task.icon-tetanus-vaccine)');
  if (await tetanusTab.isVisible()) {
    await click(tetanusTab, page);
    await page.waitForTimeout(WAIT.elmRerender);
  }

  // Question 1: "Was Tetanus Dose 1 previously administered?" → No
  const yesNoInputs = page.locator('.form-input.yes-no');
  await yesNoInputs.first().waitFor({ timeout: 5000 });
  await click(yesNoInputs.first().locator('label', { hasText: 'No' }), page);
  await page.waitForTimeout(WAIT.elmRerender);

  // Question 2: "Will patient receive Tetanus Dose 1 today?" → Yes
  // This second question appears after answering "No" to the first.
  await yesNoInputs.nth(1).waitFor({ timeout: 5000 });
  await click(yesNoInputs.nth(1).locator('label', { hasText: 'Yes' }), page);
  await page.waitForTimeout(WAIT.elmRerender);

  // Save Tetanus tab → auto-switches to Overview tab.
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await click(saveBtn, page);
  await page.waitForTimeout(WAIT.sectionTransition);

  // Save Overview tab → navigates back to encounter page.
  await saveActivity(page, 'prenatal');
}

/**
 * Complete Medication: iterate through visible medication tabs.
 * Each tab: select whether medication was administered.
 * Creates: prenatal_iron, prenatal_folate, etc.
 */
export async function completeMedication(
  page: Page,
  options?: { preferIronFolate?: boolean },
): Promise<string[]> {
  await openActivity(page, 'prenatal', 'medication');

  const completedMeds: string[] = [];
  // Default order: fefol before iron/folate (fefol blocks both).
  // With preferIronFolate: iron+folate first (blocks fefol).
  const medicationIcons = options?.preferIronFolate
    ? ['calcium', 'iron', 'folate', 'fefol', 'mms', 'mebendezole']
    : ['calcium', 'fefol', 'folate', 'iron', 'mms', 'mebendezole'];

  for (const med of medicationIcons) {
    const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${med})`);
    if (await tab.isVisible()) {
      await click(tab, page);
      await page.waitForTimeout(WAIT.elmRerender);

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
        await page.waitForTimeout(WAIT.elmRerender);
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
export async function completeLaboratoryNurse(
  page: Page,
  options?: { hivPositive?: boolean },
): Promise<string[]> {
  const hivPositive = options?.hivPositive ?? false;
  await openActivity(page, 'prenatal', 'laboratory');

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
      await page.waitForTimeout(WAIT.elmRerender);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // Answer yes/no fields by their specific CSS classes.
    // Fields appear sequentially: known-as-positive → test-performed → why-not → blood-smear.

    // Detect if this is the HIV tab (not Partner HIV, not HIV PCR).
    const isHivTab = hivPositive
      && /^\s*HIV\s*$/i.test(tabLabel)
      && !tabLabel.includes('Partner')
      && !tabLabel.includes('PCR');

    // 1. "Known as positive?" (HIV, Partner HIV, Hepatitis B) → No
    const knownPositive = page.locator('.form-input.yes-no.known-as-positive');
    if (await knownPositive.isVisible().catch(() => false)) {
      await click(knownPositive.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 2. "Will this test be performed today?"
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      if (isHivTab) {
        // HIV tab: perform the test with positive result.
        await click(testPerformed.locator('label', { hasText: 'Yes' }), page);
        await page.waitForTimeout(WAIT.elmRerender);

        // "Immediate result?" → Yes (Point of Care) to satisfy immediateResult check.
        const immediateResult = page.locator('.form-input.yes-no.immediate-result');
        if (await immediateResult.isVisible({ timeout: 2000 }).catch(() => false)) {
          await click(immediateResult.locator('label').first(), page);
          await page.waitForTimeout(WAIT.elmRerender);
        }

        // Select result: "Positive"
        const resultSelect = page.locator('select.form-input').first();
        if (await resultSelect.isVisible({ timeout: 2000 }).catch(() => false)) {
          const posOption = resultSelect.locator('option', { hasText: 'Positive' });
          if (await posOption.count() > 0) {
            const val = await posOption.getAttribute('value');
            if (val) await resultSelect.selectOption(val);
          }
          await page.waitForTimeout(WAIT.formInteraction);
        }

        // "Does the health center have an ARV services program?" → Yes
        const hivProgram = page.locator('.form-input.yes-no.hiv-program');
        if (await hivProgram.isVisible({ timeout: 2000 }).catch(() => false)) {
          await click(hivProgram.locator('label', { hasText: 'Yes' }), page);
          await page.waitForTimeout(WAIT.formInteraction);
        }
      } else {
        // All other tabs: test not performed.
        await click(testPerformed.locator('label', { hasText: 'No' }), page);
        await page.waitForTimeout(WAIT.elmRerender);
      }
    }

    // 3. "Why not?" reason checkbox → select first option (only when test not performed)
    const whyNot = page.locator('.why-not .ui.checkbox label').first();
    if (await whyNot.isVisible().catch(() => false)) {
      await click(whyNot, page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // 4. "Did you perform a blood smear?" (Malaria only) → No
    const bloodSmear = page.locator('.form-input.yes-no.got-results-previously');
    if (await bloodSmear.isVisible().catch(() => false)) {
      await click(bloodSmear.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(WAIT.elmRerender);
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
  await openActivity(page, 'prenatal', 'laboratory');

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
      await page.waitForTimeout(WAIT.elmRerender);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // 1. "Known as positive?" (HIV, Partner HIV, Hepatitis B) → No
    const knownPositive = page.locator('.form-input.yes-no.known-as-positive');
    if (await knownPositive.isVisible().catch(() => false)) {
      await click(knownPositive.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 2. "Will this test be performed today?" → Yes
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      await click(testPerformed.locator('label', { hasText: 'Yes' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 3. "Immediate result?" → Lab (the "No" side of the bool input)
    const immediateResult = page.locator('.form-input.yes-no.immediate-result');
    if (await immediateResult.isVisible().catch(() => false)) {
      await click(immediateResult.locator('label', { hasText: 'Lab' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 4. "Urine Dipstick variant?" → Short Dip (checkbox, appears for Urine Dipstick)
    const shortDip = page.locator('.ui.checkbox label', { hasText: /^Short Dip$/i });
    if (await shortDip.isVisible().catch(() => false)) {
      await click(shortDip, page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // 5. "Was this test performed before a meal?" → No (Random Blood Sugar only)
    const patientFasted = page.locator('.form-input.yes-no.patient-fasted');
    if (await patientFasted.isVisible().catch(() => false)) {
      await click(patientFasted.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(WAIT.elmRerender);
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
      await page.waitForTimeout(WAIT.elmRerender);
    }

    const tabLabel = (await tab.textContent()) || `tab-${i}`;

    // 1. "Will this test be performed today?" → Yes (confirms run by lab tech)
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      await click(testPerformed.locator('label', { hasText: 'Yes' }), page);
      await page.waitForTimeout(WAIT.elmRerender);
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
              await page.waitForTimeout(WAIT.formInteraction);
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
          await page.waitForTimeout(WAIT.formInteraction);
        }
      }
    }

    // "Does the health center have an ARV services program?" → Yes (HIV only).
    const hivProgram = page.locator('.form-input.yes-no.hiv-program');
    if (await hivProgram.isVisible({ timeout: 1000 }).catch(() => false)) {
      await click(hivProgram.locator('label', { hasText: 'Yes' }), page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // Conditional symptom/sign checkboxes (e.g., Syphilis positive → symptoms).
    // Select "None of these" if visible.
    const noneCheckbox = page.locator('.ui.checkbox label', { hasText: /^None of these$/i });
    if (await noneCheckbox.isVisible({ timeout: 1000 }).catch(() => false)) {
      await click(noneCheckbox, page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // Save this lab test tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      completedTests.push(tabLabel);
      await page.waitForTimeout(WAIT.elmRerender);
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
    await openActivity(page, 'prenatal', 'next-steps');
  } else if (await appointmentIcon.isVisible({ timeout: 1000 }).catch(() => false)) {
    await openActivity(page, 'prenatal', 'appointment-confirmation');
  } else {
    // NextSteps not available for this encounter type/state.
    return [];
  }

  // Dismiss any warning popup that may appear (e.g., "Depression not Likely").
  const warningContinue = page.locator('button', { hasText: 'Continue' });
  if (await warningContinue.isVisible({ timeout: 2000 }).catch(() => false)) {
    await warningContinue.click({ force: true });
    await page.waitForTimeout(WAIT.elmRerender);
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
      await page.waitForTimeout(WAIT.elmRerender);

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
        // Referral form — answer Yes/No fields sequentially.
        // Some fields appear conditionally after answering "Yes" to previous ones.
        for (let round = 0; round < 5; round++) {
          const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
          const yesCount = await yesLabels.count();
          let clicked = false;
          for (let i = 0; i < yesCount; i++) {
            const label = yesLabels.nth(i);
            // Skip if parent already has a checked radio.
            const parent = label.locator('..');
            const isChecked = await parent.locator('input.checked').count() > 0;
            if (!isChecked) {
              await click(label, page);
              clicked = true;
              await page.waitForTimeout(WAIT.elmRerender);
            }
          }
          if (!clicked) break;
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
          await page.waitForTimeout(WAIT.pageNavigation);
          return completedSteps;
        }
      }

      // Save this sub-task.
      const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
      if (await saveBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(saveBtn, page);
        completedSteps.push(step.name);
        await page.waitForTimeout(WAIT.elmRerender);
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
  await page.waitForTimeout(WAIT.elmRerender);

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
  await openActivity(page, 'prenatal', 'prior-treatment');

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
    await page.waitForTimeout(WAIT.elmRerender);

    // Answer all Yes/No questions on this tab with "Yes".
    const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
    const yesCount = await yesLabels.count();
    for (let j = 0; j < yesCount; j++) {
      await click(yesLabels.nth(j), page);
      await page.waitForTimeout(WAIT.quickInput);
    }

    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible()) {
      await click(saveBtn, page);
      await page.waitForTimeout(WAIT.elmRerender);
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
  await openActivity(page, 'prenatal', 'breastfeeding');

  // "Are you breastfeeding?" → Yes (triggers 5 more questions).
  await answerYesNo(page, 'is-breastfeeding', 'Yes');
  await page.waitForTimeout(WAIT.elmRerender);

  // Answer the follow-up questions.
  await answerYesNo(page, 'breastfed-first-hour', 'Yes');
  await answerYesNo(page, 'breast-pain', 'No');
  await answerYesNo(page, 'breast-redness', 'No');
  await answerYesNo(page, 'enough-milk', 'Yes');
  await answerYesNo(page, 'latching-well', 'Yes');

  await saveActivity(page, 'prenatal');
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

  await openActivity(page, 'prenatal', 'speciality-care');

  // Fill the form — answer Yes/No fields.
  const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
  const count = await yesLabels.count();
  for (let i = 0; i < count; i++) {
    await click(yesLabels.nth(i), page);
  }

  await saveActivity(page, 'prenatal');
  return true;
}

/**
 * Complete PostpartumTreatmentReview: simple treatment review.
 * Creates: medication
 */
export async function completePostpartumTreatmentReview(page: Page) {
  await openActivity(page, 'prenatal', 'postpartum-treatment-review');

  // Answer medication questions.
  const yesLabels = page.locator('.form-input.yes-no label', { hasText: 'Yes' });
  const count = await yesLabels.count();
  for (let i = 0; i < count; i++) {
    await click(yesLabels.nth(i), page);
  }

  await saveActivity(page, 'prenatal');
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
  await page.waitForTimeout(WAIT.elmRerender);

  // 1. Date Pregnancy Concluded — calendar popup via .form-input.date.
  await setDate(page, new Date(), '.form-input.date');

  // 2. Pregnancy Outcome — dropdown, select "Live Birth at Term".
  const outcomeSelect = page.locator('select').first();
  await outcomeSelect.selectOption({ index: 1 });
  await page.waitForTimeout(WAIT.formInteraction);

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

/**
 * Record Pregnancy Outcome from the participant page.
 * This is required before starting a Postpartum encounter.
 * Clicks "Record Pregnancy Outcome", fills the form, and saves.
 * After saving, the app navigates to PinCodePage.
 *
 * Default outcome: "Live Birth at Term" (dropdown index 1).
 * With { preTerm: true }: "Live Birth Pre-Term" (dropdown index 2) —
 * used to exercise the Peripartum report's "Preterm birth newborns"
 * row. The dropdown order is fixed by allPregnancyOutcome in
 * client/src/elm/Backend/IndividualEncounterParticipant/Model.elm
 * (LiveAtTerm, LivePreTerm, StillAtTerm, StillPreTerm, Abortions).
 * `liveChildBorn` treats both LiveAtTerm and LivePreTerm identically,
 * so postpartum-encounter activity expectations are unchanged.
 */
export async function recordPregnancyOutcome(
  page: Page,
  options?: { preTerm?: boolean },
) {
  await click(
    page.locator('div.ui.primary.button', { hasText: 'Record Pregnancy Outcome' }),
    page,
  );
  await page.locator('div.page-outcome.pregnancy').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);

  // 1. Date Pregnancy Concluded — calendar popup via .form-input.date.
  await setDate(page, new Date(), '.form-input.date');

  // 2. Pregnancy Outcome — dropdown.
  const outcomeSelect = page.locator('select').first();
  await outcomeSelect.selectOption({ index: options?.preTerm ? 2 : 1 });
  await page.waitForTimeout(WAIT.formInteraction);

  // 3. Delivery Location — bool input (Facility/Home).
  await click(
    page.locator('.form-input.yes-no.delivery-location label', { hasText: 'Facility' }),
    page,
  );

  // Save — navigates to PinCodePage.
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );
  await page.waitForTimeout(WAIT.heavyOperation);
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
  await page.waitForTimeout(WAIT.elmRerender);
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
  await page.waitForTimeout(WAIT.elmRerender);
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
  await openActivity(page, 'prenatal', 'examination');

  // Wait for form to fully render before filling inputs.
  const sysInput = page.locator(
    '.form-input.measurement.sys-blood-pressure input[type="number"]',
  );
  await sysInput.waitFor({ timeout: 5000 });
  await page.waitForTimeout(WAIT.elmRerender);

  // VitalsFormRepeated: only BP fields shown.
  // Use click + fill to ensure the input is focused and the value registers.
  await sysInput.click();
  await sysInput.fill(options?.sys ?? '120');
  // Wait for Elm to process the input event before filling the next field.
  await page.waitForTimeout(WAIT.elmRerender);

  const diaInput = page.locator(
    '.form-input.measurement.dia-blood-pressure input[type="number"]',
  );
  await diaInput.click();
  await diaInput.fill(options?.dia ?? '80');

  // Wait for save button to become enabled (Elm processed both inputs).
  await page
    .locator('button.ui.fluid.primary.button:not(.disabled)', { hasText: 'Save' })
    .waitFor({ timeout: 10000 });

  // Save.
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: 'Save' }),
    page,
  );
  await page.waitForTimeout(WAIT.pageNavigation);

  // For nurses, the app navigates back to the recurrent encounter page
  // (where "End Encounter" button appears since all activities are done).
  // For lab techs, it navigates to ClinicalProgressReportPage.
  await page
    .locator('div.page-encounter.prenatal')
    .or(page.locator('div.page-report.clinical'))
    .waitFor({ timeout: 15000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * End the recurrent encounter.
 * For nurses: "End Encounter" button on the encounter page (all activities done).
 * For lab techs: "End Encounter" button on the Clinical Progress Report page.
 */
export async function endRecurrentEncounter(page: Page) {
  const endBtn = page.locator('button', { hasText: 'End Encounter' });
  await endBtn.waitFor({ timeout: 10000 });
  await click(endBtn, page);
  await page.waitForTimeout(WAIT.heavyOperation);
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query Drupal for all prenatal-related measurement nodes linked to a person.
 * Returns an object keyed by node type with boolean (exists) values.
 */
/**
 * Backdate the most recent prenatal encounter for a given person.
 * @param daysAgo - how many days to backdate (default: 1 = yesterday)
 * This is needed because the app prevents starting a subsequent encounter on
 * the same day the previous encounter was completed.
 */
export function backdatePrenatalEncounter(personName: string, daysAgo: number = 1) {
  backdateEncounter(personName, 'prenatal_encounter', daysAgo);
}

export function queryPrenatalNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  return queryMeasurementNodes(personName, [
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
  ], expectedTypes);
}
