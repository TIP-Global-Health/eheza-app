import { Page } from '@playwright/test';
import { click } from './auth';
import {
  WAIT,
  queryMeasurementNodes,
  backdateEncounter,
  answerYesNo,
  selectCheckbox,
  clickSubTaskTab,
  openActivity,
  saveActivity,
  saveSubTask,
  registerAdult,
} from './common';

// ---------------------------------------------------------------------------
// Private form helpers
// ---------------------------------------------------------------------------

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
  const result = await registerAdult(page, 'TB Management', 'tuberculosis', {
    ageYears: options?.ageYears,
    firstName: options?.firstName ?? `TestTB${Date.now()}`,
    isFemale: options?.isFemale,
    isChw: true,
  });

  await startTBEncounter(page);

  return result;
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

  await openActivity(page, 'tuberculosis', 'diagnostics');

  if (path === 'positive-pulmonary') {
    // "Was this person diagnosed with Tuberculosis?" -> Yes
    await answerYesNo(page, 'diagnosed', 'Yes');
    await page.waitForTimeout(WAIT.elmRerender);

    // "Where is the Tuberculosis located?" -> Pulmonary (in the lungs)
    await answerCustomBool(page, 'is-pulmonary', 'Pulmonary (in the lungs)');
    await page.waitForTimeout(WAIT.elmRerender);

    await saveActivity(page, 'tuberculosis');
  } else if (path === 'no-diagnosis') {
    // "Was this person diagnosed with Tuberculosis?" -> No
    await answerYesNo(page, 'diagnosed', 'No');
    await page.waitForTimeout(WAIT.elmRerender);

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
    await page.waitForTimeout(WAIT.sectionTransition);
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

  await openActivity(page, 'tuberculosis', 'medication');

  // --- Sub-task 1: PrescribedMedication ---
  await clickSubTaskTab(page, 'medication');
  await page.waitForTimeout(WAIT.elmRerender);

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
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 2: DOT ---
  await clickSubTaskTab(page, 'dot');
  await page.waitForTimeout(WAIT.elmRerender);

  // "Will you provide DOT/TDO today?" -> Yes
  await answerYesNo(page, 'provide-today', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Did you distribute the following medications?" -> Yes
  await answerYesNo(page, 'distribute-medications', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 3: TreatmentReview ---
  await clickSubTaskTab(page, 'treatment-review');
  await page.waitForTimeout(WAIT.elmRerender);

  // "Has the patient been taking the medication as prescribed?" -> Yes
  await answerYesNo(page, 'taken-as-prescribed', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Is the patient feeling better?" -> Yes
  await answerYesNo(page, 'feeling-better', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Has the patient missed any doses?" -> No (reverted bool input)
  await answerYesNo(page, 'missed-doses', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Has the medication caused any side effects?" -> answer based on option
  if (sideEffects) {
    await answerYesNo(page, 'side-effects', 'Yes');
    await page.waitForTimeout(WAIT.elmRerender);

    // Select adverse event: "Rash or Itching"
    await selectCheckbox(page, 'Rash or Itching');
    await page.waitForTimeout(WAIT.formInteraction);
  } else {
    await answerYesNo(page, 'side-effects', 'No');
  }

  await saveActivity(page, 'tuberculosis');
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

  await openActivity(page, 'tuberculosis', 'symptoms');

  // Answer each symptom question.
  await answerYesNo(page, 'night-sweats', nightSweats ? 'Yes' : 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // Note: CSS class has capital S in "blood-in-Sputum" (matches Elm source).
  await answerYesNo(page, 'blood-in-Sputum', bloodInSputum ? 'Yes' : 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  await answerYesNo(page, 'weight-loss', weightLoss ? 'Yes' : 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  await answerYesNo(page, 'severe-fatigue', severeFatigue ? 'Yes' : 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  await saveActivity(page, 'tuberculosis');
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
  await openActivity(page, 'tuberculosis', 'next-steps');

  // Iterate visible sub-task tabs.
  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(WAIT.elmRerender);

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
      await page.waitForTimeout(WAIT.elmRerender);
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
    await page.waitForTimeout(WAIT.sectionTransition);
  }

  // After saving all sub-tasks, the app may navigate to the encounter page
  // or the progress report page. Wait for either.
  await Promise.race([
    page.locator('div.page-encounter.tuberculosis').waitFor({ timeout: 15000 }),
    page.locator('div.page-report.tuberculosis').waitFor({ timeout: 15000 }),
  ]);
  await page.waitForTimeout(WAIT.elmRerender);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the TB encounter: click "End Encounter", confirm in the dialog.
 */
export async function endTBEncounter(page: Page) {
  await page.waitForTimeout(WAIT.pageNavigation);

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
  await page.waitForTimeout(WAIT.sectionTransition);
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
  return queryMeasurementNodes(personName, tbTypes, expectedTypes);
}

/**
 * Backdate the most recent TB encounter for a person to 7 days ago.
 * Uses base64-encoded person name to prevent shell injection.
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function backdateTBEncounter(personName: string) {
  backdateEncounter(personName, 'tuberculosis_encounter', 7);
}
