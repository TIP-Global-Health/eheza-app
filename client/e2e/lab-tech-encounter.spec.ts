import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { getClientPort } from './helpers/client-port';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, syncAndWait } from './helpers/common';
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
  completeLabResultsAsLabTech,
  queryPrenatalNodes,
} from './helpers/prenatal';

test.describe('Lab Tech: Enter Lab Results via Case Management', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test('nurse orders labs, lab tech enters results', async ({ page }) => {
    // Multi-role test: full nurse encounter + lab tech encounter — needs extra time.
    test.setTimeout(600000);
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
    // Order labs for lab processing (not point-of-care).
    await completeLaboratoryNurseForLab(page);
    // NextSteps: the "Wait" sub-task should appear because labs were ordered for lab.
    const completedSteps = await completeNextSteps(page);
    expect(completedSteps, 'completedSteps should contain wait sub-task').toContain('wait');

    // Sync nurse encounter to backend.
    await syncAndWait(page);

    // --- Phase 2: Lab Tech logs in and enters results via Case Management ---
    // Clear all browser state (including IndexedDB) to ensure the nurse's
    // locally-created nodes don't leak into the lab tech session.
    const client = await page.context().newCDPSession(page);
    await client.send('Storage.clearDataForOrigin', {
      origin: `http://localhost:${getClientPort()}`,
      storageTypes: 'all',
    });
    await client.detach();

    resetDevice();
    await setupDevice(page, '3333', 'Nyange Health Center');

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
    const completedResults = await completeLabResultsAsLabTech(page);
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

    // --- Phase 3: Sync and verify backend ---
    await syncAndWait(page);

    // Verify lab test measurement nodes exist in backend.
    const expectedTypes = ['prenatal_labs_results'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    expect(nodes['prenatal_labs_results'], 'prenatal_labs_results should exist').toBe(true);
  });
});
