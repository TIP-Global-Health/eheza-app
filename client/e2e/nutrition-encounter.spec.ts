import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, syncAndWait } from './helpers/common';
import {
  createChildAndStartEncounter,
  completeNCDA,
  enterHeight,
  enterWeight,
  enterMuac,
  enterNutritionSigns,
  saveActivity,
  endEncounter,
  completeSendToHC,
  completeHealthEducation,
  completeContributingFactors,
  completeFollowUp,
  queryBackendNodes,
} from './helpers/nutrition';

test.describe('Nurse: Individual Nutrition Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Reset device (fresh pairing code) and do full setup before each test.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page);
  });

  test('complete normal encounter with NCDA and verify backend sync', async ({
    page,
  }) => {
    // Use age 10 months (< 24) so NCDA activity appears for nurse.
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 10,
    });

    // Height: 70 cm
    await enterHeight(page, '70');
    await saveActivity(page);

    // Weight: 8.5 kg
    await enterWeight(page, '8.5');
    await saveActivity(page);

    // MUAC: 14 cm
    await enterMuac(page, '14');
    await saveActivity(page);

    // Nutrition signs: None
    await enterNutritionSigns(page, ['None']);
    const diagnosisAppeared = await saveActivity(page);
    expect(diagnosisAppeared, 'diagnosis popup should not appear for normal signs').toBe(false);

    // Complete NCDA activity (Nurse, atHealthCenter).
    await completeNCDA(page);

    // End encounter.
    const endBtn = page.locator('div.actions button.ui.fluid.button', {
      hasText: 'End Encounter',
    });
    await expect(endBtn, 'End Encounter button should be enabled').not.toHaveClass(/disabled/);
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify measurements in backend.
    const nodes = queryBackendNodes(fullName);
    expect(nodes.height, 'height should be 70 cm').toBe(70);
    expect(nodes.weight, 'weight should be 8.5 kg').toBe(8.5);
    expect(nodes.muac, 'muac should be 14 cm').toBe(14);
    expect(nodes.nutrition, 'nutrition node should exist').toBe(true);
    expect(nodes.ncda, 'ncda node should exist').toBe(true);
  });

  test('measurement validation rejects out-of-range values', async ({
    page,
  }) => {
    await createChildAndStartEncounter(page, { ageMonths: 24 });

    const saveBtn = page.locator('button.ui.fluid.primary.button');

    // --- Height validation ---
    await enterHeight(page, '10');
    await expect(saveBtn, 'save should be disabled for height 10 cm (too low)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.height input[type="number"]')
      .fill('260');
    await expect(saveBtn, 'save should be disabled for height 260 cm (too high)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.height input[type="number"]')
      .fill('85');
    await expect(saveBtn, 'save should be enabled for valid height 85 cm').toHaveClass(/active/);
    await saveActivity(page);

    // --- Weight validation ---
    await enterWeight(page, '0.1');
    await expect(saveBtn, 'save should be disabled for weight 0.1 kg (too low)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.weight input[type="number"]')
      .fill('250');
    await expect(saveBtn, 'save should be disabled for weight 250 kg (too high)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.weight input[type="number"]')
      .fill('12');
    await expect(saveBtn, 'save should be enabled for valid weight 12 kg').toHaveClass(/active/);
    await saveActivity(page);

    // --- MUAC validation ---
    await enterMuac(page, '3');
    await expect(saveBtn, 'save should be disabled for MUAC 3 cm (too low)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.muac input[type="number"]')
      .fill('100');
    await expect(saveBtn, 'save should be disabled for MUAC 100 cm (too high)').toHaveClass(/disabled/);

    await page
      .locator('.form-input.measurement.muac input[type="number"]')
      .fill('14');
    await expect(saveBtn, 'save should be enabled for valid MUAC 14 cm').toHaveClass(/active/);
    await saveActivity(page);
  });

  test('abnormal MUAC triggers assessment and NextSteps with backend sync', async ({
    page,
  }) => {
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
    });

    // Height: 80 cm
    await enterHeight(page, '80');
    await saveActivity(page);

    // Weight: 8 kg (underweight)
    await enterWeight(page, '8');
    await saveActivity(page);

    // MUAC: 11 cm (severe acute malnutrition — red zone)
    await enterMuac(page, '11');
    await saveActivity(page);

    // Nutrition signs: Edema — triggers diagnosis popup and auto-navigates
    // to NextSteps page (doesn't return to encounter page).
    await enterNutritionSigns(page, ['Edema']);
    await click(page.locator('button.ui.fluid.primary.button.active'), page);

    // Diagnosis popup should appear.
    const popup = page.locator('div.ui.active.modal.diagnosis-popup');
    await popup.waitFor({ timeout: 5000 });
    await click(popup.locator('button.ui.primary.fluid.button'), page);

    // App auto-navigates to NextSteps after diagnosis.
    await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

    // Complete all sub-tasks (each saves individually).
    await completeSendToHC(page);
    await completeHealthEducation(page);
    await completeContributingFactors(page);
    await completeFollowUp(page);

    // After completing all sub-tasks, wait for navigation to settle
    // then ensure we're on the encounter page.
    await page.waitForTimeout(WAIT.pageNavigation);
    await page
      .locator('div.page-encounter.nutrition')
      .waitFor({ timeout: 10000 });

    // End encounter.
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify all measurement nodes in backend.
    const nodes = queryBackendNodes(fullName);
    expect(nodes.height, 'height should be 80 cm').toBe(80);
    expect(nodes.weight, 'weight should be 8 kg').toBe(8);
    expect(nodes.muac, 'muac should be 11 cm').toBe(11);
    expect(nodes.nutrition, 'nutrition node should exist').toBe(true);
    expect(nodes.sendToHc, 'sendToHc node should exist').toBe(true);
    expect(nodes.healthEducation, 'healthEducation node should exist').toBe(true);
    expect(nodes.contributingFactors, 'contributingFactors node should exist').toBe(true);
    expect(nodes.followUp, 'followUp node should exist').toBe(true);
  });
});
