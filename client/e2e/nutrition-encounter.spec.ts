import { openReport, closeReport } from './helpers/progress-report';
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
    // Progress report must show what this encounter recorded.
    const report = await openReport(page, 'nutrition');
    await expect(report.locator('.pane.person-details')).toContainText(fullName);
    await closeReport(page, 'nutrition');

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

  // Scenario: each measurement is entered outside the range it can take, at
  // both ends, and the nurse is told which one is wrong rather than being left
  // with a Save button that does not answer (#1980).
  test('an out-of-range measurement is refused with a warning that names it', async ({
    page,
  }) => {
    await createChildAndStartEncounter(page, { ageMonths: 24 });

    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });

    /** Enters a value that is out of range and checks what the nurse is told. */
    const refuses = async (id: string, value: string, named: string) => {
      await page
        .locator(`.form-input.measurement.${id} input[type="number"]`)
        .fill(value);
      await page.waitForTimeout(WAIT.formInteraction);

      // The button answers now; what refuses the value is the warning.
      await expect(saveBtn, `save should answer for ${id} ${value}`).toHaveClass(/active/);
      await click(saveBtn, page);

      const popup = page.locator('div.ui.active.modal.measurement-out-of-range');
      await popup.waitFor({ timeout: 10000 });
      await expect(popup, `the warning should name the ${id}`).toHaveClass(
        new RegExp(`(^| )${named}( |$)`),
      );

      // Still on the form, so nothing out of range was saved.
      await expect(page.locator(`.form-input.measurement.${id}`).first()).toBeVisible();

      await click(popup.locator('button.ui.primary.fluid.button'), page);
      await popup.waitFor({ state: 'hidden', timeout: 10000 });
      await page.waitForTimeout(WAIT.formInteraction);
    };

    // --- Height: 25 to 250 cm ---
    await enterHeight(page, '10');
    await refuses('height', '10', 'height-out-of-range');
    await refuses('height', '260', 'height-out-of-range');
    await page
      .locator('.form-input.measurement.height input[type="number"]')
      .fill('85');
    await expect(saveBtn, 'save should be enabled for valid height 85 cm').toHaveClass(/active/);
    await saveActivity(page);

    // --- Weight: 0.5 to 200 kg ---
    await enterWeight(page, '0.1');
    await refuses('weight', '0.1', 'weight-out-of-range');
    await refuses('weight', '250', 'weight-out-of-range');
    await page
      .locator('.form-input.measurement.weight input[type="number"]')
      .fill('12');
    await expect(saveBtn, 'save should be enabled for valid weight 12 kg').toHaveClass(/active/);
    await saveActivity(page);

    // --- MUAC: 5 to 99, in the unit the site asks for ---
    await enterMuac(page, '3');
    await refuses('muac', '3', 'muac-out-of-range');
    await refuses('muac', '100', 'muac-out-of-range');
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
    // Progress report must show what this encounter recorded.
    const report = await openReport(page, 'nutrition');
    await expect(report.locator('.pane.person-details')).toContainText(fullName);
    await closeReport(page, 'nutrition');

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
