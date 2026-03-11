import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createChildAndStartWellChildEncounter,
  completePregnancySummary,
  completeNutritionAssessment,
  completeImmunisation,
  completeDangerSigns,
  completeHomeVisit,
  completeNextSteps,
  endWellChildEncounter,
  syncAndWait,
  queryWellChildNodes,
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

  test('complete newborn exam with PregnancySummary, NutritionAssessment, Immunisation, verify backend sync', async ({ page }) => {

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
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_pregnancy_summary']).toBe(true);
    expect(nodes['well_child_head_circumference']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
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
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_head_circumference']).toBe(true);
    expect(nodes['well_child_muac']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    expect(nodes['well_child_feeding']).toBe(true);
    expect(nodes['well_child_caring']).toBe(true);
    expect(nodes['well_child_hygiene']).toBe(true);
    expect(nodes['well_child_food_security']).toBe(true);
  });
});
