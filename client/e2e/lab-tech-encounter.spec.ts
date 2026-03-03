import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/nutrition';
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
    expect(completedSteps).toContain('wait');

    // Sync nurse encounter to backend.
    await syncAndWait(page);

    // --- Phase 2: Lab Tech logs in and enters results via Case Management ---
    // Clear all browser state (SW caches, localStorage, sessionStorage) to
    // ensure the nurse's cached data doesn't interfere with the lab tech session.
    await page.evaluate(async () => {
      localStorage.clear();
      sessionStorage.clear();
      const registrations = await navigator.serviceWorker.getRegistrations();
      for (const reg of registrations) {
        await reg.unregister();
      }
      const cacheNames = await caches.keys();
      for (const name of cacheNames) {
        await caches.delete(name);
      }
    });

    resetDevice();
    await setupDevice(page, '3333', 'Nyange Health Center');

    // Verify Lab Tech sees restricted menu (Case Management + Device Status only).
    await page.locator('.icon-task-case-management').waitFor({ timeout: 10000 });
    await page.locator('.icon-task-device-status').waitFor({ timeout: 5000 });
    // Clinical menu should NOT be visible for lab tech.
    expect(await page.locator('.icon-task-clinical').isVisible().catch(() => false)).toBe(false);

    // Navigate to Case Management.
    await navigateToCaseManagement(page);

    // Find the patient in the Prenatal Labs pane.
    const entry = page.locator('.follow-up-entry', {
      has: page.locator('.name', { hasText: fullName }),
    });
    await entry.waitFor({ timeout: 15000 });

    // Click forward icon → navigates directly to LabResults activity page.
    await entry.locator('.icon-forward').click();
    await page.locator('div.page-activity.prenatal').waitFor({ timeout: 15000 });
    await page.waitForTimeout(500);

    // Complete lab results for all visible tests.
    const completedResults = await completeLabResultsAsLabTech(page);
    expect(completedResults.length).toBeGreaterThan(0);

    // After completing all tabs, the app should navigate to encounter page
    // or stay on the activity page. Navigate back if needed.
    // For lab tech, after completing all results the encounter auto-completes.
    await page.waitForTimeout(2000);

    // Navigate back to Case Management to verify entry is gone.
    // First go back to main menu.
    const backBtn = page.locator('.icon-back');
    if (await backBtn.isVisible().catch(() => false)) {
      await backBtn.click();
      await page.waitForTimeout(1000);
    }
    // If we're on encounter page, go back to main menu.
    const backBtn2 = page.locator('.icon-back');
    if (await backBtn2.isVisible().catch(() => false)) {
      await backBtn2.click();
      await page.waitForTimeout(1000);
    }

    // --- Phase 3: Sync and verify backend ---
    await syncAndWait(page);

    // Verify lab test measurement nodes exist in backend.
    const expectedTypes = ['prenatal_labs_results'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    expect(nodes['prenatal_labs_results']).toBe(true);
  });
});
