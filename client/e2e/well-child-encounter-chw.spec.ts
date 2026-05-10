import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  WAIT,
  clickSubTaskTab,
  fillMeasurement,
  saveSubTask,
  syncAndWait,
} from './helpers/common';
import { verifyFeatureGatesEncounterButton } from './helpers/feature-flags';
import {
  createChildAndStartWellChildEncounter,
  completePregnancySummary,
  completeNutritionAssessment,
  completeImmunisation,
  completeDangerSigns,
  completeHomeVisit,
  completeNextSteps,
  endWellChildEncounter,
  queryWellChildNodes,
  queryWellChildVitalsValues,
} from './helpers/well-child';

// =========================================================================
// Test 1: CHW NewbornExam (1-month child)
// =========================================================================

test.describe('CHW: Well Child NewbornExam Encounter', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('complete newborn exam with PregnancySummary, NutritionAssessment, Immunisation, verify backend sync', async ({ page, browser }) => {
    // Verify FeatureWellChild flag gates client UI + admin Reports surfaces.
    await verifyFeatureGatesEncounterButton(page, 'well_child', 'Well Child Visit', {
      browser,
      admin: {
        sqOptions: ['postnatal-care'],
        sqDemographicsRows: ['Standard Pediatric Visit'],
        completionOptions: ['newborn-exam', 'well-child'],
      },
    });

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 1,
      isChw: true,
    });

    // 1. Pregnancy Summary: expected date, APGAR scores, birth weight,
    //    no birth length, no complications, no defects.
    await completePregnancySummary(page);

    // 2. Nutrition Assessment: head circumference + nutrition signs + weight.
    //    No height or MUAC for newborns.
    await completeNutritionAssessment(page, {
      headCircumference: '35',
      weight: '4',
      nutritionSigns: [],
    });

    // 3. Immunisation (CHW variant): answer "No" to previous administration.
    await completeImmunisation(page, { isChw: true });

    // 4. Next Steps: triggered after completing nutrition + immunisation.
    //    Includes NextVisit, and possibly HealthEducation + SendToHC
    //    if child is behind on vaccinations.
    await completeNextSteps(page, {
      hasContributingFactors: false,
      hasHealthEducation: true,
      hasSendToHC: true,
      hasFollowUp: false,
    });

    // End encounter.
    await endWellChildEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'well_child_pregnancy_summary',
      'well_child_head_circumference',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_health_education',
      'well_child_send_to_hc',
      'well_child_next_visit',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_pregnancy_summary'], 'well_child_pregnancy_summary should exist').toBe(true);
    expect(nodes['well_child_head_circumference'], 'well_child_head_circumference should exist').toBe(true);
    expect(nodes['well_child_nutrition'], 'well_child_nutrition should exist').toBe(true);
    expect(nodes['well_child_weight'], 'well_child_weight should exist').toBe(true);
    expect(nodes['well_child_health_education'], 'well_child_health_education should exist').toBe(true);
    expect(nodes['well_child_send_to_hc'], 'well_child_send_to_hc should exist').toBe(true);
    expect(nodes['well_child_next_visit'], 'well_child_next_visit should exist').toBe(true);
  });
});

// =========================================================================
// Test 2: CHW PediatricCareChw with HomeVisit (24-month child)
// =========================================================================

test.describe('CHW: Well Child PediatricCareChw Encounter with HomeVisit', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('complete CHW encounter with DangerSigns, NutritionAssessment, HomeVisit, verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 24,
      isChw: true,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page);

    // 2. Nutrition Assessment: height, head circumference, MUAC, nutrition, weight.
    await completeNutritionAssessment(page, {
      height: '85',
      headCircumference: '48',
      muac: '14',
      weight: '12',
      nutritionSigns: [],
    });

    // 3. Home Visit: Feeding, Caring, Hygiene, FoodSecurity.
    await completeHomeVisit(page);

    // 4. Immunisation (CHW variant): complete all pending vaccines.
    await completeImmunisation(page, { isChw: true });

    // 5. Next Steps: triggered after completing nutrition + immunisation.
    await completeNextSteps(page, {
      hasContributingFactors: false,
      hasHealthEducation: true,
      hasSendToHC: true,
      hasFollowUp: false,
    });

    // End encounter.
    await endWellChildEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'well_child_symptoms_review',
      'well_child_vitals',
      'well_child_height',
      'well_child_head_circumference',
      'well_child_muac',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_feeding',
      'well_child_caring',
      'well_child_hygiene',
      'well_child_food_security',
      'well_child_health_education',
      'well_child_send_to_hc',
      'well_child_next_visit',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review'], 'well_child_symptoms_review should exist').toBe(true);
    expect(nodes['well_child_vitals'], 'well_child_vitals should exist').toBe(true);
    expect(nodes['well_child_height'], 'well_child_height should exist').toBe(true);
    expect(nodes['well_child_head_circumference'], 'well_child_head_circumference should exist').toBe(true);
    expect(nodes['well_child_muac'], 'well_child_muac should exist').toBe(true);
    expect(nodes['well_child_nutrition'], 'well_child_nutrition should exist').toBe(true);
    expect(nodes['well_child_weight'], 'well_child_weight should exist').toBe(true);
    expect(nodes['well_child_feeding'], 'well_child_feeding should exist').toBe(true);
    expect(nodes['well_child_caring'], 'well_child_caring should exist').toBe(true);
    expect(nodes['well_child_hygiene'], 'well_child_hygiene should exist').toBe(true);
    expect(nodes['well_child_food_security'], 'well_child_food_security should exist').toBe(true);
    expect(nodes['well_child_health_education'], 'well_child_health_education should exist').toBe(true);
    expect(nodes['well_child_send_to_hc'], 'well_child_send_to_hc should exist').toBe(true);
    expect(nodes['well_child_next_visit'], 'well_child_next_visit should exist').toBe(true);
  });
});

// =========================================================================
// Test 3: CHW PediatricCareChw — Vitals "Unable to take measurement" round-trip
// =========================================================================

test.describe('CHW: Well Child PediatricCareChw — Vitals skip round-trip', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  // Scenario: Single CHW Well Child encounter where the user cycles
  // through the four states of the per-measurement skip (option B):
  //   (1) Both skipped     — RR=null, BT=null on the saved row.
  //   (2) RR skipped only  — RR=null, BT=36.
  //   (3) Neither skipped  — RR=30, BT=36.
  //   (4) BT skipped again — RR=30, BT=null.
  // Each cycle re-opens the SAME encounter's Vitals tab without ending
  // the encounter, syncs, and verifies the well_child_vitals row's null
  // / non-null state on the backend. After all four cycles, the rest of
  // the activities (Nutrition, Home Visit, Immunisation, Next Steps) are
  // completed and the encounter is ended.
  test('Vitals skip round-trip: skip-both -> skip-RR -> none -> skip-BT, end encounter', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 24,
      isChw: true,
    });

    // ---------- Cycle 1: both skipped ----------
    // SymptomsReview: "None of these". Vitals: tick both skip checkboxes.
    await completeDangerSigns(page, {
      respiratoryRateNotTaken: true,
      bodyTemperatureNotTaken: true,
    });
    await syncAndWait(page);
    {
      const vitals = queryWellChildVitalsValues(fullName, { rr: null, temp: null });
      expect(vitals.rr, 'cycle 1: RR should be null (skipped)').toBeNull();
      expect(vitals.temp, 'cycle 1: BT should be null (skipped)').toBeNull();
    }

    // ---------- Cycle 2: untick BT, type 36; RR stays skipped ----------
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    // Danger Signs is now on the Completed tab — switch to it.
    await click(page.locator('#completed-tab'), page);
    await page.waitForTimeout(WAIT.elmRerender);
    await click(page.locator('.icon-task-danger-signs'), page);
    await page.locator('div.page-activity.well-child').waitFor({ timeout: 10000 });
    await clickSubTaskTab(page, 'vitals');

    {
      const vitalsForm = page.locator('.ui.form.vitals');
      await vitalsForm.waitFor({ timeout: 5000 });
      const skipCheckboxes = vitalsForm.locator('.ui.checkbox.activity');
      // Both should be checked from cycle 1 (rehydrated from null fields).
      await expect(skipCheckboxes.nth(0).locator('input[type="checkbox"]'))
        .toHaveClass(/checked/);
      await expect(skipCheckboxes.nth(1).locator('input[type="checkbox"]'))
        .toHaveClass(/checked/);
      // The numeric inputs are NOT rendered while skipped.
      await expect(vitalsForm.locator('.form-input.measurement.respiratory-rate')).toHaveCount(0);
      await expect(vitalsForm.locator('.form-input.measurement.body-temperature')).toHaveCount(0);

      // Untick BT (click the checkbox div — onClick is on it, not the label).
      await click(skipCheckboxes.nth(1), page);
      await page.waitForTimeout(WAIT.formInteraction);
      // BT input should now appear; fill 36.
      await fillMeasurement(page, 'body-temperature', '36');
    }
    await saveSubTask(page);
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    await syncAndWait(page);
    {
      const vitals = queryWellChildVitalsValues(fullName, { rr: null, temp: 36 });
      expect(vitals.rr, 'cycle 2: RR should still be null (skipped)').toBeNull();
      expect(vitals.temp, 'cycle 2: BT should be 36').toBe(36);
    }

    // ---------- Cycle 3: untick RR, type 30; BT stays at 36 ----------
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    // Danger Signs is now on the Completed tab — switch to it.
    await click(page.locator('#completed-tab'), page);
    await page.waitForTimeout(WAIT.elmRerender);
    await click(page.locator('.icon-task-danger-signs'), page);
    await page.locator('div.page-activity.well-child').waitFor({ timeout: 10000 });
    await clickSubTaskTab(page, 'vitals');

    {
      const vitalsForm = page.locator('.ui.form.vitals');
      await vitalsForm.waitFor({ timeout: 5000 });
      const skipCheckboxes = vitalsForm.locator('.ui.checkbox.activity');
      // RR still checked; BT no longer checked.
      await expect(skipCheckboxes.nth(0).locator('input[type="checkbox"]'))
        .toHaveClass(/checked/);
      await expect(skipCheckboxes.nth(1).locator('input[type="checkbox"]'))
        .not.toHaveClass(/checked/);

      // Untick RR (click the checkbox div — onClick is on it, not the label).
      await click(skipCheckboxes.nth(0), page);
      await page.waitForTimeout(WAIT.formInteraction);
      // 30 is between recessed (<24) and elevated (>=40) thresholds for 12–60mo,
      // so the danger-sign alert modal won't fire and block the sync flow.
      await fillMeasurement(page, 'respiratory-rate', '30');
    }
    await saveSubTask(page);
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    await syncAndWait(page);
    {
      const vitals = queryWellChildVitalsValues(fullName, { rr: 30, temp: 36 });
      expect(vitals.rr, 'cycle 3: RR should be 30').toBe(30);
      expect(vitals.temp, 'cycle 3: BT should still be 36').toBe(36);
    }

    // ---------- Cycle 4: re-tick BT (skip again); RR stays at 30 ----------
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    // Danger Signs is now on the Completed tab — switch to it.
    await click(page.locator('#completed-tab'), page);
    await page.waitForTimeout(WAIT.elmRerender);
    await click(page.locator('.icon-task-danger-signs'), page);
    await page.locator('div.page-activity.well-child').waitFor({ timeout: 10000 });
    await clickSubTaskTab(page, 'vitals');

    {
      const vitalsForm = page.locator('.ui.form.vitals');
      await vitalsForm.waitFor({ timeout: 5000 });
      const skipCheckboxes = vitalsForm.locator('.ui.checkbox.activity');
      // Neither checked; both inputs visible.
      await expect(skipCheckboxes.nth(0).locator('input[type="checkbox"]'))
        .not.toHaveClass(/checked/);
      await expect(skipCheckboxes.nth(1).locator('input[type="checkbox"]'))
        .not.toHaveClass(/checked/);

      // Re-tick BT (click the checkbox div — onClick is on it, not the label).
      await click(skipCheckboxes.nth(1), page);
      await page.waitForTimeout(WAIT.formInteraction);
    }
    await saveSubTask(page);
    await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
    await page.waitForTimeout(WAIT.elmRerender);
    await syncAndWait(page);
    {
      const vitals = queryWellChildVitalsValues(fullName, { rr: 30, temp: null });
      expect(vitals.rr, 'cycle 4: RR should be 30').toBe(30);
      expect(vitals.temp, 'cycle 4: BT should be null (re-skipped)').toBeNull();
    }

    // ---------- Complete remaining activities, end encounter ----------
    // Cycle 4 left us on the Completed tab; switch back to To Do so the
    // remaining activity icons are reachable.
    await click(page.locator('#pending-tab'), page);
    await page.waitForTimeout(WAIT.elmRerender);

    await completeNutritionAssessment(page, {
      height: '85',
      headCircumference: '48',
      muac: '14',
      weight: '12',
      nutritionSigns: [],
    });

    await completeHomeVisit(page);

    await completeImmunisation(page, { isChw: true });

    await completeNextSteps(page, {
      hasContributingFactors: false,
      hasHealthEducation: true,
      hasSendToHC: true,
      hasFollowUp: false,
    });

    await endWellChildEncounter(page);
    await syncAndWait(page);

    // Final-state verification.
    const expectedTypes = [
      'well_child_symptoms_review',
      'well_child_vitals',
      'well_child_height',
      'well_child_head_circumference',
      'well_child_muac',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_feeding',
      'well_child_caring',
      'well_child_hygiene',
      'well_child_food_security',
      'well_child_health_education',
      'well_child_send_to_hc',
      'well_child_next_visit',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review'], 'well_child_symptoms_review should exist').toBe(true);
    expect(nodes['well_child_vitals'], 'well_child_vitals should exist').toBe(true);
    expect(nodes['well_child_height'], 'well_child_height should exist').toBe(true);
    expect(nodes['well_child_head_circumference'], 'well_child_head_circumference should exist').toBe(true);
    expect(nodes['well_child_muac'], 'well_child_muac should exist').toBe(true);
    expect(nodes['well_child_nutrition'], 'well_child_nutrition should exist').toBe(true);
    expect(nodes['well_child_weight'], 'well_child_weight should exist').toBe(true);
    expect(nodes['well_child_feeding'], 'well_child_feeding should exist').toBe(true);
    expect(nodes['well_child_caring'], 'well_child_caring should exist').toBe(true);
    expect(nodes['well_child_hygiene'], 'well_child_hygiene should exist').toBe(true);
    expect(nodes['well_child_food_security'], 'well_child_food_security should exist').toBe(true);
    expect(nodes['well_child_health_education'], 'well_child_health_education should exist').toBe(true);
    expect(nodes['well_child_send_to_hc'], 'well_child_send_to_hc should exist').toBe(true);
    expect(nodes['well_child_next_visit'], 'well_child_next_visit should exist').toBe(true);

    // Final vitals row state: RR=30, BT=null.
    const finalVitals = queryWellChildVitalsValues(fullName, { rr: 30, temp: null });
    expect(finalVitals.rr, 'final RR should be 30').toBe(30);
    expect(finalVitals.temp, 'final BT should be null').toBeNull();
  });
});
