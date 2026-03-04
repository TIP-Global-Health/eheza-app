import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { createChildAndStartEncounter } from './helpers/nutrition';
import {
  startHomeVisit,
  completeFeeding,
  completeFeedingWithSupplement,
  completeCaring,
  completeHygiene,
  completeFoodSecurity,
  endHomeVisit,
  queryHomeVisitNodes,
} from './helpers/home-visit';
import { syncAndWait } from './helpers/nutrition';

test.describe('CHW: Home Visit Encounter', () => {
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

  test('complete all activities and verify backend sync', async ({ page }) => {
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
      isChw: true,
      startEncounter: false,
    });

    await startHomeVisit(page);

    // Complete all 4 mandatory activities.
    await completeFeeding(page);
    await completeCaring(page);
    await completeHygiene(page);
    await completeFoodSecurity(page);

    // End Encounter should be enabled (all activities done).
    const endBtn = page.locator('div.actions button.ui.fluid.button', {
      hasText: 'End Encounter',
    });
    await expect(endBtn).not.toHaveClass(/disabled/);

    await endHomeVisit(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify all 4 measurement nodes exist in backend.
    const nodes = queryHomeVisitNodes(fullName);
    expect(nodes.feeding).toBe(true);
    expect(nodes.caring).toBe(true);
    expect(nodes.hygiene).toBe(true);
    expect(nodes.foodSecurity).toBe(true);
  });

  test('feeding with supplement reveals conditional fields', async ({
    page,
  }) => {
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
      isChw: true,
      startEncounter: false,
    });

    await startHomeVisit(page);

    // Complete feeding with supplement (tests conditional fields).
    await completeFeedingWithSupplement(page);

    // Complete remaining activities.
    await completeCaring(page);
    await completeHygiene(page);
    await completeFoodSecurity(page);

    await endHomeVisit(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify all nodes exist.
    const nodes = queryHomeVisitNodes(fullName);
    expect(nodes.feeding).toBe(true);
    expect(nodes.caring).toBe(true);
    expect(nodes.hygiene).toBe(true);
    expect(nodes.foodSecurity).toBe(true);
  });
});
