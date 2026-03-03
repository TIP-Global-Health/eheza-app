import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/nutrition';
import {
  createAdultFemaleAndStartEncounter,
  startPrenatalEncounter,
  endPrenatalEncounter,
  navigateToParticipantPage,
  completePregnancyDating,
  completeDangerSigns,
  completeLaboratoryChw,
  completeHealthEducation,
  completeBirthPlan,
  completeNextSteps,
  backdatePrenatalEncounter,
  queryPrenatalNodes,
} from './helpers/prenatal';

test.describe('CHW: Prenatal First Encounter', () => {
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
    // LMP date ~30 weeks ago.
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: true,
      encounterType: 'first',
    });

    // Complete all 5 CHW first encounter activities.
    await completePregnancyDating(page, lmpDate);
    await completeLaboratoryChw(page);
    await completeDangerSigns(page);
    await completeHealthEducation(page, 1);
    await completeNextSteps(page);

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify CTs exist in Drupal.
    const nodes = queryPrenatalNodes(fullName);
    expect(nodes['last_menstrual_period']).toBe(true);
    expect(nodes['pregnancy_testing']).toBe(true);
    expect(nodes['danger_signs']).toBe(true);
    expect(nodes['prenatal_health_education']).toBe(true);
    // NextSteps CTs (conditional — at minimum appointment_confirmation
    // and prenatal_follow_up should exist with no danger signs).
    expect(nodes['appointment_confirmation']).toBe(true);
    expect(nodes['prenatal_follow_up']).toBe(true);
  });
});

test.describe('CHW: Prenatal Second Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('complete birth plan and verify backend sync', async ({ page }) => {
    // LMP date ~30 weeks ago.
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // First: complete a minimal CHW first encounter.
    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: true,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeLaboratoryChw(page);
    await completeDangerSigns(page);
    await completeHealthEducation(page, 1);
    await completeNextSteps(page);
    await endPrenatalEncounter(page);

    // Sync first encounter to backend, then backdate it to yesterday so the
    // app allows starting a subsequent encounter (same-day block).
    await syncAndWait(page);
    backdatePrenatalEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start second encounter.
    await navigateToParticipantPage(page, fullName);
    await startPrenatalEncounter(page, 'subsequent');

    // Complete all 4 CHW second encounter activities.
    await completeDangerSigns(page);
    await completeBirthPlan(page);
    await completeHealthEducation(page, 2);
    await completeNextSteps(page);

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify the unique CT: birth_plan.
    const nodes = queryPrenatalNodes(fullName);
    expect(nodes['birth_plan']).toBe(true);
  });
});
