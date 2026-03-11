import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createChildAndStartWellChildEncounter,
  completeDangerSigns,
  completeNutritionAssessment,
  completeECD,
  completeMedication,
  completeImmunisation,
  completeNextSteps,
  endWellChildEncounter,
  syncAndWait,
  queryWellChildNodes,
} from './helpers/well-child';

// =========================================================================
// Test 1: Nurse Normal PediatricCare (24-month child)
// =========================================================================

test.describe('Nurse: Well Child PediatricCare — Normal Encounter', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete normal encounter with all mandatory activities and verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 24,
      isChw: false,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page, {
      respiratoryRate: '20',
      bodyTemp: '36.5',
    });

    // 2. Nutrition Assessment: normal values for 24-month child.
    await completeNutritionAssessment(page, {
      height: '85',
      headCircumference: '48',
      muac: '14',
      weight: '12',
      nutritionSigns: [],
    });

    // 3. ECD: answer all milestone questions "Yes".
    await completeECD(page);

    // 4. Medication: administer all available meds
    //    (Mebendezole + VitaminA for 24mo in Rwanda).
    await completeMedication(page);

    // 5. Immunisation: administer all available vaccines.
    await completeImmunisation(page, { isChw: false });

    // 6. Next Steps: NextVisit is required once
    //    nutrition/immunisation/ECD/medication are all done.
    await completeNextSteps(page, {
      hasContributingFactors: false,
      hasHealthEducation: false,
      hasSendToHC: false,
      hasFollowUp: false,
    });

    // End encounter (skip Photo + NCDA).
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
      'well_child_ecd',
      'well_child_mebendezole',
      'well_child_vitamin_a',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_head_circumference']).toBe(true);
    expect(nodes['well_child_muac']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    expect(nodes['well_child_ecd']).toBe(true);
    expect(nodes['well_child_mebendezole']).toBe(true);
    expect(nodes['well_child_vitamin_a']).toBe(true);
  });
});

// =========================================================================
// Test 2: Nurse Abnormal Nutrition → NextSteps (24-month child)
// =========================================================================

test.describe('Nurse: Well Child PediatricCare — Abnormal Nutrition with NextSteps', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('abnormal nutrition triggers NextSteps, verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 24,
      isChw: false,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page);

    // 2. Nutrition Assessment: abnormal values to trigger diagnosis.
    //    Low weight, low MUAC, and Edema sign.
    await completeNutritionAssessment(page, {
      height: '85',
      headCircumference: '48',
      muac: '11',
      weight: '8',
      nutritionSigns: ['Edema'],
    });

    // 3. NextSteps: triggered by abnormal nutrition assessment.
    //    Complete all available sub-tasks.
    await completeNextSteps(page, {
      hasContributingFactors: true,
      hasHealthEducation: true,
      hasSendToHC: true,
      hasFollowUp: true,
    });

    // 4. ECD: answer all milestone questions "Yes".
    await completeECD(page);

    // 5. Medication: administer all available meds.
    await completeMedication(page);

    // 6. Immunisation: administer all available vaccines.
    await completeImmunisation(page, { isChw: false });

    // 7. Next Steps (round 2): NextVisit is triggered after
    //    nutrition + immunisation + ECD + medication are all done.
    await completeNextSteps(page, {
      hasContributingFactors: false,
      hasHealthEducation: false,
      hasSendToHC: false,
      hasFollowUp: false,
    });

    // End encounter.
    await endWellChildEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes — includes NextSteps measurements.
    const expectedTypes = [
      'well_child_symptoms_review',
      'well_child_vitals',
      'well_child_height',
      'well_child_head_circumference',
      'well_child_muac',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_contributing_factors',
      'well_child_health_education',
      'well_child_send_to_hc',
      'well_child_follow_up',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    expect(nodes['well_child_contributing_factors']).toBe(true);
    expect(nodes['well_child_health_education']).toBe(true);
    expect(nodes['well_child_send_to_hc']).toBe(true);
    expect(nodes['well_child_follow_up']).toBe(true);
  });
});
