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
  completeNCDA,
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

    // Use 23 months (< 24) so NCDA activity appears for nurse.
    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 23,
      isChw: false,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page, {
      respiratoryRate: '20',
      bodyTemp: '36.5',
    });

    // 2. Nutrition Assessment: normal values for 23-month child.
    await completeNutritionAssessment(page, {
      height: '84',
      headCircumference: '48',
      muac: '14',
      weight: '12',
      nutritionSigns: [],
    });

    // 3. ECD: answer all milestone questions "Yes".
    await completeECD(page);

    // 4. Medication: administer all available meds
    //    (Mebendezole + VitaminA for 23mo in Rwanda).
    await completeMedication(page);

    // 5. Immunisation: administer all available vaccines.
    await completeImmunisation(page, { isChw: false });

    // 6. NCDA (Nurse, child < 24 months).
    await completeNCDA(page);

    // 7. Next Steps: NextVisit is required once
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
    // For a 24-month-old male on Rwanda with no vaccination history,
    // all 7 common vaccines are overdue (BCG, OPV, DTP, PCV13, Rotarix, IPV, MR).
    // DTPStandalone is Burundi-only, HPV is female-only.
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
      'well_child_ncda',
      'well_child_next_visit',
      'well_child_bcg_immunisation',
      'well_child_opv_immunisation',
      'well_child_dtp_immunisation',
      'well_child_pcv13_immunisation',
      'well_child_rotarix_immunisation',
      'well_child_ipv_immunisation',
      'well_child_mr_immunisation',
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
    expect(nodes['well_child_ncda']).toBe(true);
    expect(nodes['well_child_next_visit']).toBe(true);
    // Immunisation nodes.
    expect(nodes['well_child_bcg_immunisation']).toBe(true);
    expect(nodes['well_child_opv_immunisation']).toBe(true);
    expect(nodes['well_child_dtp_immunisation']).toBe(true);
    expect(nodes['well_child_pcv13_immunisation']).toBe(true);
    expect(nodes['well_child_rotarix_immunisation']).toBe(true);
    expect(nodes['well_child_ipv_immunisation']).toBe(true);
    expect(nodes['well_child_mr_immunisation']).toBe(true);
    // DTPStandalone is Burundi-only, HPV is female-only — not queried/created.
    expect(nodes['well_child_dtp_sa_immunisation']).toBeFalsy();
    expect(nodes['well_child_hpv_immunisation']).toBeFalsy();
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

    // Verify backend nodes — includes NextSteps + immunisation measurements.
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
      'well_child_next_visit',
      'well_child_bcg_immunisation',
      'well_child_opv_immunisation',
      'well_child_dtp_immunisation',
      'well_child_pcv13_immunisation',
      'well_child_rotarix_immunisation',
      'well_child_ipv_immunisation',
      'well_child_mr_immunisation',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_head_circumference']).toBe(true);
    expect(nodes['well_child_muac']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    expect(nodes['well_child_contributing_factors']).toBe(true);
    expect(nodes['well_child_health_education']).toBe(true);
    expect(nodes['well_child_send_to_hc']).toBe(true);
    expect(nodes['well_child_follow_up']).toBe(true);
    expect(nodes['well_child_next_visit']).toBe(true);
    // Immunisation nodes.
    expect(nodes['well_child_bcg_immunisation']).toBe(true);
    expect(nodes['well_child_opv_immunisation']).toBe(true);
    expect(nodes['well_child_dtp_immunisation']).toBe(true);
    expect(nodes['well_child_pcv13_immunisation']).toBe(true);
    expect(nodes['well_child_rotarix_immunisation']).toBe(true);
    expect(nodes['well_child_ipv_immunisation']).toBe(true);
    expect(nodes['well_child_mr_immunisation']).toBe(true);
  });
});

// =========================================================================
// Test 3: Nurse PediatricCare — 7-year-old Female (Albendazole + HPV vaccine)
// =========================================================================

test.describe('Nurse: Well Child PediatricCare — 7yr Female, Albendazole', () => {
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

  // Scenario: 7-year-old female on Rwanda site.
  // Medication: Albendazole appears (6-12yr), no Mebendezole (1-6yr), no VitaminA (6mo-6yr).
  // Immunisation: All 7 common vaccines + HPV (female, but HPV starts at 12yr so NOT expected at 7yr).
  // Backend: Verifies well_child_albendazole created.
  test('complete encounter for 7yr female with albendazole, verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 84,
      isChw: false,
      isFemale: true,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page, {
      respiratoryRate: '20',
      bodyTemp: '36.5',
    });

    // 2. Nutrition Assessment: normal values for 7-year-old.
    //    Head circumference not expected at this age.
    await completeNutritionAssessment(page, {
      height: '120',
      muac: '17',
      weight: '22',
      nutritionSigns: [],
    });

    // 3. ECD: answer all milestone questions "Yes".
    await completeECD(page);

    // 4. Medication: albendazole only (6-12yr on Rwanda).
    await completeMedication(page);

    // 5. Immunisation: administer all available vaccines.
    await completeImmunisation(page, { isChw: false });

    // 6. Next Steps: NextVisit.
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

    // Verify backend nodes.
    const expectedTypes = [
      'well_child_symptoms_review',
      'well_child_vitals',
      'well_child_height',
      'well_child_muac',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_albendazole',
      'well_child_next_visit',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_muac']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    // Albendazole (6-12yr on Rwanda).
    expect(nodes['well_child_albendazole']).toBe(true);
    // No Mebendezole (1-6yr) or VitaminA (6mo-6yr) at age 7.
    expect(nodes['well_child_mebendezole']).toBeFalsy();
    expect(nodes['well_child_vitamin_a']).toBeFalsy();
    expect(nodes['well_child_next_visit']).toBe(true);
  });
});

// =========================================================================
// Test 4: Nurse PediatricCare — 12.5-year-old Female (HPV vaccine)
// =========================================================================

test.describe('Nurse: Well Child PediatricCare — 12.5yr Female, HPV', () => {
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

  // Scenario: 12.5-year-old female on Rwanda site.
  // Immunisation: HPV appears (female, 12yr+, Rwanda). All common vaccines also overdue.
  // Medication: albendazole (6-12yr), no mebendezole or vitaminA.
  test('complete encounter for 12.5yr female with HPV vaccine, verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartWellChildEncounter(page, {
      ageMonths: 150,
      isChw: false,
      isFemale: true,
    });

    // 1. Danger Signs: no symptoms, normal vitals.
    await completeDangerSigns(page, {
      respiratoryRate: '18',
      bodyTemp: '36.5',
    });

    // 2. Nutrition Assessment: normal values for 12.5-year-old.
    await completeNutritionAssessment(page, {
      height: '150',
      muac: '21',
      weight: '40',
      nutritionSigns: [],
    });

    // 3. ECD: answer all milestone questions "Yes".
    await completeECD(page);

    // No medication at 12.5yr on Rwanda (albendazole < 12yr, mebendezole < 6yr, vitaminA < 6yr).

    // 4. Immunisation: all common vaccines + HPV (female 12yr+).
    await completeImmunisation(page, { isChw: false });

    // 5. Next Steps: NextVisit.
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

    // Verify backend nodes.
    const expectedTypes = [
      'well_child_symptoms_review',
      'well_child_vitals',
      'well_child_height',
      'well_child_muac',
      'well_child_nutrition',
      'well_child_weight',
      'well_child_next_visit',
      'well_child_hpv_immunisation',
    ];
    const nodes = queryWellChildNodes(fullName, expectedTypes);

    expect(nodes['well_child_symptoms_review']).toBe(true);
    expect(nodes['well_child_vitals']).toBe(true);
    expect(nodes['well_child_height']).toBe(true);
    expect(nodes['well_child_muac']).toBe(true);
    expect(nodes['well_child_nutrition']).toBe(true);
    expect(nodes['well_child_weight']).toBe(true);
    expect(nodes['well_child_next_visit']).toBe(true);
    // HPV (female, 12yr+, Rwanda).
    expect(nodes['well_child_hpv_immunisation']).toBe(true);
  });
});
