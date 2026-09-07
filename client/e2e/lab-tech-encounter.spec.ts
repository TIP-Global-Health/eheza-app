import { test, expect, Page } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { getClientPort } from './helpers/client-port';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  WAIT,
  openActivity,
  openEncounterTab,
  syncAndWait,
  queryPrenatalDiagnoses,
  queryPartnerHIVTestExecutionNote,
} from './helpers/common';
import { openReport } from './helpers/progress-report';
import {
  createAdultFemaleAndStartEncounter,
  completePregnancyDating,
  completeHistory,
  completeExamination,
  completeFamilyPlanning,
  completeDangerSigns,
  completeSymptomReview,
  completeMalariaPrevention,
  completeMentalHealth,
  completeImmunisation,
  completeMedication,
  completeLaboratoryNurseForLab,
  completeNextSteps,
  endPrenatalEncounter,
  navigateToCaseManagement,
  openLabsResultsReviewFromCaseManagement,
  acceptLabsResults,
  completeLabResults,
  completeRecurrentNextSteps,
  dismissWarningPopup,
  queryPrenatalNodes,
} from './helpers/prenatal';

/**
 * Sign the current user out and hand the device to another one. Clearing the
 * origin's storage drops the IndexedDB the previous role built up, so nothing
 * it created locally leaks into the next session.
 */
async function switchUser(page: Page, pinCode: string) {
  const client = await page.context().newCDPSession(page);
  await client.send('Storage.clearDataForOrigin', {
    origin: `http://localhost:${getClientPort()}`,
    storageTypes: 'all',
  });
  await client.detach();

  resetDevice();
  await setupDevice(page, pinCode, 'Nyange Health Center');
}

test.describe('Lab Tech: Enter Lab Results via Case Management', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test('nurse orders labs, lab tech enters results, nurse answers the follow ups', async ({ page }) => {
    // Multi-role test: nurse encounter, lab tech results, nurse follow ups —
    // needs extra time.
    test.setTimeout(900000);
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // --- Phase 1: Nurse creates initial encounter with labs ordered for lab ---
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');

    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    // Order labs for lab processing (not point-of-care), except the patient's
    // own HIV test, which is run point of care and is negative. That mix is
    // what leaves the partner's result to arrive at the recurrent phase, with
    // the patient's own result already known.
    await completeLaboratoryNurseForLab(page, { hivPointOfCareNegative: true });
    // NextSteps: the "Wait" sub-task should appear because labs were ordered for lab.
    const completedSteps = await completeNextSteps(page);
    expect(completedSteps, 'completedSteps should contain wait sub-task').toContain('wait');

    // Sync nurse encounter to backend.
    await syncAndWait(page);

    // --- Phase 2: Lab Tech logs in and enters results via Case Management ---
    await switchUser(page, '3333');

    // Verify Lab Tech sees restricted menu (Case Management + Device Status only).
    await page.locator('.icon-task-case-management').waitFor({ timeout: 10000 });
    await page.locator('.icon-task-device-status').waitFor({ timeout: 5000 });
    // Clinical menu should NOT be visible for lab tech.
    expect(await page.locator('.icon-task-clinical').isVisible().catch(() => false), 'clinical menu should not be visible for lab tech').toBe(false);

    // Navigate to Case Management.
    await navigateToCaseManagement(page);

    // Verify Lab Tech Case Management structure: only 2 filter buttons (All + ANC Labs).
    const filterButtons = page.locator('div.ui.segment.filters button');
    await expect(filterButtons, 'Lab Tech Case Management should have exactly 2 filter buttons (All + ANC Labs)').toHaveCount(2);
    // Verify ANC Labs pane heading is visible.
    await expect(
      page.locator('div.pane-heading', { hasText: 'ANC Labs' }),
      'ANC Labs pane heading should be visible',
    ).toBeVisible({ timeout: 5000 });

    // Find the patient in the Prenatal Labs pane.
    const entry = page.locator('.follow-up-entry', {
      has: page.locator('.name', { hasText: fullName }),
    });
    await entry.waitFor({ timeout: 15000 });

    // Click forward icon → navigates directly to LabResults activity page.
    await click(entry.locator('.icon-forward'), page);
    await page.locator('div.page-activity.prenatal').waitFor({ timeout: 15000 });
    await page.waitForTimeout(WAIT.elmRerender);

    // Complete lab results for all visible tests.
    // The blood glucose field is asked for a reading in the wrong unit on
    // the way, and has to refuse it (#2123).
    const completedResults = await completeLabResults(page, {
      checkGlucoseRange: true,
    });
    expect(completedResults.length, 'at least one lab result should have been completed').toBeGreaterThan(0);

    // After completing all tabs, the app should navigate to encounter page
    // or stay on the activity page. Navigate back if needed.
    // For lab tech, after completing all results the encounter auto-completes.
    await page.waitForTimeout(WAIT.pageNavigation);

    // Navigate back to Case Management to verify entry is gone.
    // First go back to main menu.
    const backBtn = page.locator('.icon-back');
    if (await backBtn.isVisible().catch(() => false)) {
      await click(backBtn, page);
      await page.waitForTimeout(WAIT.sectionTransition);
    }
    // If we're on encounter page, go back to main menu.
    const backBtn2 = page.locator('.icon-back');
    if (await backBtn2.isVisible().catch(() => false)) {
      await click(backBtn2, page);
      await page.waitForTimeout(WAIT.sectionTransition);
    }

    // --- Phase 3: sync and verify the backend ---
    await syncAndWait(page);

    // Verify lab test measurement nodes exist in backend.
    const expectedTypes = ['prenatal_labs_results'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    expect(nodes['prenatal_labs_results'], 'prenatal_labs_results should exist').toBe(true);

    // The confirmed-run note is written by the lab technician's own save, so
    // it is the signal that their data arrived. The assertion below expects an
    // absence, and without this anchor a lagging sync would satisfy it.
    expect(
      queryPartnerHIVTestExecutionNote(fullName),
      'partner HIV test should carry the lab technician confirmed-run note',
    ).toBe('run-confirmed-by-lab-tech');

    // A lab technician can not answer the follow up questions about the
    // partner, so nothing is diagnosed yet - whether the partner is on ARVs
    // with a surpressed viral load decides it, and no one has been asked.
    const diagnosesBeforeFollowUps = queryPrenatalDiagnoses(fullName, { allowEmpty: true });
    expect(diagnosesBeforeFollowUps, 'encounter diagnoses should be readable').not.toBeNull();
    expect(
      diagnosesBeforeFollowUps,
      'discordant partnership should NOT be recorded before the follow ups are answered',
    ).not.toContain('partner-hiv-recurrent');

    // --- Phase 4: the nurse answers the follow ups the lab tech left ---
    await switchUser(page, '1234');
    await navigateToCaseManagement(page);

    // Every result is in, so the entry opens the report for the nurse to
    // review. The report states what the partner's ARV status is, and it has
    // nothing to state until the nurse answers the follow ups.
    const reportBeforeFollowUps = await openLabsResultsReviewFromCaseManagement(page, fullName);
    await expect(
      reportBeforeFollowUps.locator('.medical-diagnosis li', { hasText: 'Discordant Couple' }),
      'discordant couple status should not be stated before the follow ups are answered',
    ).toHaveCount(0);

    // Accepting the results is what opens the encounter the follow ups are on.
    await acceptLabsResults(page);

    await click(page.locator('.icon-task-laboratory-follow-ups'), page);
    await page.locator('div.page-activity.prenatal').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);

    // Answers "Is partner taking ARVs?" with No - a positive partner who is
    // not on ARVs is the discordant-partnership condition.
    const completedFollowUps = await completeLabResults(page);
    expect(completedFollowUps.length, 'at least one follow up should have been completed').toBeGreaterThan(0);
    await page.waitForTimeout(WAIT.pageNavigation);

    // Answering the last recurrent activity opens the progress report itself,
    // and the partner's status is stated on it now that it is known.
    const reportAfterFollowUps = await openReport(page, 'prenatal');
    await expect(
      reportAfterFollowUps.locator('.medical-diagnosis li', { hasText: 'Discordant Couple' }),
      'discordant couple status should state that the partner is not taking ARVs',
    ).toHaveText(/Discordant Couple: Partner NOT taking ARVs/);

    // --- Phase 5: sync and read the diagnoses off the encounter ---
    await syncAndWait(page);

    const diagnoses = queryPrenatalDiagnoses(fullName);
    expect(diagnoses, 'encounter diagnoses should be readable').not.toBeNull();
    // The partner's result arrived at the recurrent phase, so the diagnosis
    // belongs to that phase - that is the variant the recurrent Next Steps
    // prescribes PrEP for.
    expect(diagnoses, 'discordant partnership should be recorded for the recurrent phase').toContain('partner-hiv-recurrent');
    expect(diagnoses, 'discordant partnership should NOT be recorded for the initial phase').not.toContain('partner-hiv');
  });

});


// =========================================================================
// A later diagnosis reopens a Next Steps task that was already saved
// =========================================================================

// Scenario: the lab technician enters the results and leaves the follow up
// questions for the nurse. Nothing about the partner is known until the nurse
// answers them, so the encounter carries no diagnosis that needs a medication
// yet - but a low hemoglobin count does put Next Steps on the encounter.
// The nurse saves Next Steps first, and answers the follow ups after.
// Conditions: hemoglobin 9 g/dL -> moderate anemia at the recurrent phase,
// which offers Next Steps while requiring no medication. Partner positive and
// not on ARVs -> a discordant partnership, which requires TDF + 3TC.
// Verifies: the saved task returns to the encounter's pending activities once
// the medication becomes required, and offers the medication.

test.describe('Lab Tech and Nurse: a saved Next Steps task reopened by a later diagnosis', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('a medication required after the task was saved brings the task back', async ({
    page,
  }) => {
    test.setTimeout(600000);
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // --- Phase 1: the nurse runs the initial encounter, labs go to the lab ---
    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    // Her own test is run point of care and is negative, so the partner's
    // result is the one that arrives at the recurrent phase.
    await completeLaboratoryNurseForLab(page, { hivPointOfCareNegative: true });
    const completedSteps = await completeNextSteps(page);
    expect(completedSteps, 'completedSteps should contain wait sub-task').toContain('wait');
    await syncAndWait(page);

    // --- Phase 2: the lab technician enters the results ---
    await switchUser(page, '3333');
    await navigateToCaseManagement(page);

    const entry = page.locator('.follow-up-entry', {
      has: page.locator('.name', { hasText: fullName }),
    });
    await entry.waitFor({ timeout: 15000 });
    await click(entry.locator('.icon-forward'), page);
    await page.locator('div.page-activity.prenatal').waitFor({ timeout: 15000 });
    await page.waitForTimeout(WAIT.elmRerender);

    // 9 g/dL is moderate anemia (7 <= count < 11). It puts Next Steps on the
    // encounter without putting any medication on it.
    const completedResults = await completeLabResults(page, { hemoglobinCount: '9' });
    expect(completedResults.length, 'at least one lab result should have been completed').toBeGreaterThan(0);
    await page.waitForTimeout(WAIT.pageNavigation);
    await syncAndWait(page);

    // --- Phase 3: the nurse saves Next Steps before answering the follow ups ---
    await switchUser(page, '1234');
    await navigateToCaseManagement(page);
    await openLabsResultsReviewFromCaseManagement(page, fullName);
    await acceptLabsResults(page);

    const savedTasks = await completeRecurrentNextSteps(page);
    expect(savedTasks.length, 'at least one Next Steps task should have been saved').toBeGreaterThan(0);

    // Saved with nothing to hand over, the task counts as done.
    await page.locator('div.page-encounter.prenatal').waitFor({ timeout: 10000 });
    await openEncounterTab(page, 'completed');
    await expect(
      page.locator('.icon-task-next-steps'),
      'Next Steps should be listed as completed once it is saved',
    ).toBeVisible({ timeout: 10000 });

    // --- Phase 4: answering the follow ups makes a medication required ---
    await openEncounterTab(page, 'pending');
    await click(page.locator('.icon-task-laboratory-follow-ups'), page);
    await page.locator('div.page-activity.prenatal').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);

    // Answers "Is partner taking ARVs?" with No, which is the discordant
    // partnership condition, and TDF + 3TC is what it prescribes.
    const completedFollowUps = await completeLabResults(page);
    expect(completedFollowUps.length, 'at least one follow up should have been completed').toBeGreaterThan(0);
    await page.waitForTimeout(WAIT.pageNavigation);

    await page.locator('div.page-encounter.prenatal').waitFor({ timeout: 10000 });
    await openEncounterTab(page, 'pending');
    await expect(
      page.locator('.icon-task-next-steps'),
      'Next Steps should be pending again now that a medication is required',
    ).toBeVisible({ timeout: 10000 });

    await openActivity(page, 'prenatal', 'next-steps');
    await dismissWarningPopup(page);
    await expect(
      page.locator('div.page-activity.prenatal'),
      'the medication the new diagnosis requires should be offered',
    ).toContainText('TDF + 3TC');
  });
});
