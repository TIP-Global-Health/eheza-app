import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createChildAndStartEncounter,
  enterWeight,
  enterMuac,
  enterNutritionSigns,
  saveActivity,
  endEncounter,
  syncAndWait,
  completeSendToHC,
  completeHealthEducation,
  completeContributingFactors,
  completeFollowUp,
  queryBackendNodes,
} from './helpers/nutrition';

test.describe('CHW: Individual Nutrition Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as CHW Jojo (PIN 2345), select Akanduga village.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('normal encounter without height (optional for CHW) and backend sync', async ({
    page,
  }) => {
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
      isChw: true,
    });

    // Weight: 12 kg
    await enterWeight(page, '12');
    await saveActivity(page);

    // MUAC: 14 cm
    await enterMuac(page, '14');
    await saveActivity(page);

    // Nutrition signs: None
    await enterNutritionSigns(page, ['None']);
    const diagnosisAppeared = await saveActivity(page);
    expect(diagnosisAppeared).toBe(false);

    // Height is optional for CHW — skip it entirely.
    // End Encounter should be enabled without height.
    const endBtn = page.locator('div.actions button.ui.fluid.button', {
      hasText: 'End Encounter',
    });
    await expect(endBtn).not.toHaveClass(/disabled/);
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify measurements in backend — no height node expected.
    const nodes = queryBackendNodes(fullName);
    expect(nodes.weight).toBe(12);
    expect(nodes.muac).toBe(14);
    expect(nodes.nutrition).toBe(true);
    expect(nodes.height).toBeUndefined();
  });

  test('abnormal MUAC triggers NextSteps with backend sync', async ({
    page,
  }) => {
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
      isChw: true,
    });

    // Weight: 8 kg (underweight)
    await enterWeight(page, '8');
    await saveActivity(page);

    // MUAC: 11 cm (severe acute malnutrition — red zone)
    await enterMuac(page, '11');
    await saveActivity(page);

    // Nutrition signs: Edema — triggers diagnosis popup.
    await enterNutritionSigns(page, ['Edema']);
    await click(page.locator('button.ui.fluid.primary.button.active'), page);

    // Diagnosis popup should appear.
    const popup = page.locator('div.ui.active.modal.diagnosis-popup');
    await popup.waitFor({ timeout: 5000 });
    await click(popup.locator('button.ui.primary.fluid.button'), page);

    // App auto-navigates to NextSteps after diagnosis.
    await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

    // Complete all sub-tasks.
    await completeSendToHC(page);
    await completeHealthEducation(page);
    await completeContributingFactors(page);
    await completeFollowUp(page);

    // Wait for navigation to settle, then ensure we're on the encounter page.
    await page.waitForTimeout(2000);
    await page
      .locator('div.page-encounter.nutrition')
      .waitFor({ timeout: 10000 });

    // End encounter.
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify all measurement nodes in backend (no height for CHW).
    const nodes = queryBackendNodes(fullName);
    expect(nodes.weight).toBe(8);
    expect(nodes.muac).toBe(11);
    expect(nodes.nutrition).toBe(true);
    expect(nodes.height).toBeUndefined();
    expect(nodes.sendToHc).toBe(true);
    expect(nodes.healthEducation).toBe(true);
    expect(nodes.contributingFactors).toBe(true);
    expect(nodes.followUp).toBe(true);
  });
});
