import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { verifyCaseManagementEntry } from './helpers/case-management';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import { verifyFeatureGatesEncounterButton } from './helpers/feature-flags';
import {
  createAdultAndStartNCDEncounter,
  completeDangerSigns,
  completeSymptomReview,
  completeExamination,
  completeFamilyPlanning,
  completeMedicalHistory,
  completeLaboratory,
  completeOutsideCare,
  completeNextSteps,
  endNCDEncounter,
  queryNCDNodes,
  backdateNCDEncounter,
  navigateToParticipantPage,
  startNCDEncounter,
  navigateToCaseManagement,
  openNCDRecurrentEncounterFromCaseManagement,
  completeLabResults,
  completeRecurrentNextSteps,
  leaveRecurrentEncounter,
} from './helpers/ncd';

// =========================================================================
// Test 1: Nurse First NCD Encounter — Male, Stage 1 Hypertension + Labs
// =========================================================================

test.describe('Nurse: NCD First Encounter — Male, Stage 1 Hypertension', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete first NCD encounter with Stage 1 hypertension, verify backend sync', async ({ page }) => {
    // Verify FeatureNCD flag gates the "Noncommunicable Diseases" button.
    await verifyFeatureGatesEncounterButton(page, 'ncd', 'Noncommunicable Diseases');

    const { fullName } = await createAdultAndStartNCDEncounter(page, {
      isFemale: false,
    });

    // 1. DangerSigns: none.
    await completeDangerSigns(page);

    // 2. SymptomReview: none for all 3 groups.
    await completeSymptomReview(page);

    // 3. Examination: Stage 1 hypertension (sys=145, dia=95).
    await completeExamination(page, {
      sys: '145',
      dia: '95',
      heartRate: '80',
      respiratoryRate: '18',
      bodyTemp: '36.6',
    });

    // 4. MedicalHistory: all 5 sub-tasks with "No"/"None".
    await completeMedicalHistory(page);

    // 5. Laboratory: perform all tests.
    await completeLaboratory(page, { performTests: true });

    // 6. NextSteps: Stage 1 hypertension (first encounter, no complications)
    //    triggers HealthEducation only (no MedicationDistribution, no Referral).
    await completeNextSteps(page);

    // FamilyPlanning should NOT appear (male patient).

    // End encounter.
    await endNCDEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'ncd_danger_signs',
      'ncd_symptom_review',
      'ncd_vitals',
      'ncd_core_exam',
      'ncd_co_morbidities',
      'ncd_medication_history',
      'ncd_social_history',
      'ncd_family_history',
      'ncd_outside_care',
      'ncd_health_education',
      'ncd_hiv_test',
      'ncd_random_blood_sugar_test',
      'ncd_urine_dipstick_test',
      'ncd_creatinine_test',
      'ncd_liver_function_test',
      'ncd_lipid_panel_test',
      'ncd_hba1c_test',
    ];
    const nodes = queryNCDNodes(fullName, expectedTypes);

    expect(nodes['ncd_danger_signs'], 'ncd_danger_signs should exist').toBe(true);
    expect(nodes['ncd_symptom_review'], 'ncd_symptom_review should exist').toBe(true);
    expect(nodes['ncd_vitals'], 'ncd_vitals should exist').toBe(true);
    expect(nodes['ncd_core_exam'], 'ncd_core_exam should exist').toBe(true);
    expect(nodes['ncd_co_morbidities'], 'ncd_co_morbidities should exist').toBe(true);
    expect(nodes['ncd_medication_history'], 'ncd_medication_history should exist').toBe(true);
    expect(nodes['ncd_social_history'], 'ncd_social_history should exist').toBe(true);
    expect(nodes['ncd_family_history'], 'ncd_family_history should exist').toBe(true);
    expect(nodes['ncd_outside_care'], 'ncd_outside_care should exist').toBe(true);
    // Stage 1 hypertension triggers HealthEducation.
    expect(nodes['ncd_health_education'], 'ncd_health_education should exist').toBe(true);
    // Lab test nodes.
    expect(nodes['ncd_hiv_test'], 'ncd_hiv_test should exist').toBe(true);
    expect(nodes['ncd_random_blood_sugar_test'], 'ncd_random_blood_sugar_test should exist').toBe(true);
    expect(nodes['ncd_urine_dipstick_test'], 'ncd_urine_dipstick_test should exist').toBe(true);
    expect(nodes['ncd_creatinine_test'], 'ncd_creatinine_test should exist').toBe(true);
    expect(nodes['ncd_liver_function_test'], 'ncd_liver_function_test should exist').toBe(true);
    expect(nodes['ncd_lipid_panel_test'], 'ncd_lipid_panel_test should exist').toBe(true);
    expect(nodes['ncd_hba1c_test'], 'ncd_hba1c_test should exist').toBe(true);
    // FamilyPlanning not created (male patient).
    expect(nodes['ncd_family_planning'], 'ncd_family_planning should not exist').toBe(false);
    // PregnancyTest not created (male patient).
    expect(nodes['ncd_pregnancy_test'], 'ncd_pregnancy_test should not exist').toBe(false);
    // No MedicationDistribution (Stage 1, first encounter, no complications).
    expect(nodes['ncd_medication_distribution'], 'ncd_medication_distribution should not exist').toBe(false);
    // No Referral (Stage 1, not pregnant).
    expect(nodes['ncd_referral'], 'ncd_referral should not exist').toBe(false);
  });
});

// =========================================================================
// Test 2: Nurse First NCD Encounter — Female, Stage 3 Hypertension + Referral
// =========================================================================

test.describe('Nurse: NCD First Encounter — Female, Stage 3 Hypertension', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete NCD encounter with high BP triggering NextSteps, verify backend sync', async ({ page }) => {
    const { fullName } = await createAdultAndStartNCDEncounter(page, {
      isFemale: true,
      ageYears: 30,
    });

    // 1. DangerSigns: none.
    await completeDangerSigns(page);

    // 2. SymptomReview: none.
    await completeSymptomReview(page);

    // 3. Examination: HIGH BP (sys=180, dia=110 → Hypertension Stage 3).
    await completeExamination(page, {
      sys: '180',
      dia: '110',
      heartRate: '80',
      respiratoryRate: '18',
      bodyTemp: '36.6',
    });

    // 4. FamilyPlanning: select "None" (female patient, activity should appear).
    await completeFamilyPlanning(page);

    // 5. MedicalHistory: all sub-tasks with "No"/"None".
    await completeMedicalHistory(page);

    // 6. Laboratory: perform all tests (female → includes pregnancy test).
    await completeLaboratory(page, { performTests: true });

    // 7. NextSteps: triggered by hypertension diagnosis.
    //    MedicationDistribution + Referral appear (Stage 3 hypertension).
    await completeNextSteps(page);

    // End encounter.
    await endNCDEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes including FamilyPlanning, labs, and NextSteps.
    const expectedTypes = [
      'ncd_danger_signs',
      'ncd_symptom_review',
      'ncd_vitals',
      'ncd_core_exam',
      'ncd_family_planning',
      'ncd_co_morbidities',
      'ncd_medication_history',
      'ncd_social_history',
      'ncd_family_history',
      'ncd_outside_care',
      'ncd_medication_distribution',
      'ncd_hiv_test',
      'ncd_random_blood_sugar_test',
      'ncd_urine_dipstick_test',
      'ncd_pregnancy_test',
      'ncd_creatinine_test',
      'ncd_liver_function_test',
      'ncd_lipid_panel_test',
      'ncd_hba1c_test',
      'ncd_referral',
    ];
    const nodes = queryNCDNodes(fullName, expectedTypes);

    expect(nodes['ncd_danger_signs'], 'ncd_danger_signs should exist').toBe(true);
    expect(nodes['ncd_symptom_review'], 'ncd_symptom_review should exist').toBe(true);
    expect(nodes['ncd_vitals'], 'ncd_vitals should exist').toBe(true);
    expect(nodes['ncd_core_exam'], 'ncd_core_exam should exist').toBe(true);
    expect(nodes['ncd_family_planning'], 'ncd_family_planning should exist').toBe(true);
    expect(nodes['ncd_co_morbidities'], 'ncd_co_morbidities should exist').toBe(true);
    expect(nodes['ncd_medication_history'], 'ncd_medication_history should exist').toBe(true);
    expect(nodes['ncd_social_history'], 'ncd_social_history should exist').toBe(true);
    expect(nodes['ncd_family_history'], 'ncd_family_history should exist').toBe(true);
    expect(nodes['ncd_outside_care'], 'ncd_outside_care should exist').toBe(true);
    expect(nodes['ncd_medication_distribution'], 'ncd_medication_distribution should exist').toBe(true);
    // Lab test nodes (female → pregnancy test also created).
    expect(nodes['ncd_hiv_test'], 'ncd_hiv_test should exist').toBe(true);
    expect(nodes['ncd_random_blood_sugar_test'], 'ncd_random_blood_sugar_test should exist').toBe(true);
    expect(nodes['ncd_urine_dipstick_test'], 'ncd_urine_dipstick_test should exist').toBe(true);
    expect(nodes['ncd_pregnancy_test'], 'ncd_pregnancy_test should exist').toBe(true);
    expect(nodes['ncd_creatinine_test'], 'ncd_creatinine_test should exist').toBe(true);
    expect(nodes['ncd_liver_function_test'], 'ncd_liver_function_test should exist').toBe(true);
    expect(nodes['ncd_lipid_panel_test'], 'ncd_lipid_panel_test should exist').toBe(true);
    expect(nodes['ncd_hba1c_test'], 'ncd_hba1c_test should exist').toBe(true);
    // Referral created (Stage 3 hypertension).
    expect(nodes['ncd_referral'], 'ncd_referral should exist').toBe(true);
  });
});

// =========================================================================
// Test 3: Nurse Subsequent NCD Encounter — OutsideCare replaces MedicalHistory
// =========================================================================

test.describe('Nurse: NCD Subsequent Encounter — OutsideCare', () => {
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

  test('complete subsequent NCD encounter with OutsideCare, verify backend sync', async ({ page }) => {
    // --- PART 1: Complete a first encounter (simplified) ---

    const { fullName } = await createAdultAndStartNCDEncounter(page, {
      isFemale: false,
    });

    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeExamination(page);
    await completeMedicalHistory(page);
    await completeLaboratory(page);
    await endNCDEncounter(page);

    // Sync first encounter.
    await syncAndWait(page);

    // --- PART 2: Backdate and start subsequent encounter ---

    backdateNCDEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start new encounter.
    await navigateToParticipantPage(page, fullName);
    await startNCDEncounter(page);

    // --- PART 3: Complete subsequent encounter activities ---

    // DangerSigns.
    await completeDangerSigns(page);

    // SymptomReview.
    await completeSymptomReview(page);

    // Examination.
    await completeExamination(page);

    // OutsideCare (replaces MedicalHistory for subsequent encounters).
    await completeOutsideCare(page);

    // Laboratory.
    await completeLaboratory(page);

    // End encounter.
    await endNCDEncounter(page);

    // Sync.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'ncd_danger_signs',
      'ncd_symptom_review',
      'ncd_vitals',
      'ncd_core_exam',
      'ncd_outside_care',
    ];
    const nodes = queryNCDNodes(fullName, expectedTypes);

    expect(nodes['ncd_danger_signs'], 'ncd_danger_signs should exist').toBe(true);
    expect(nodes['ncd_symptom_review'], 'ncd_symptom_review should exist').toBe(true);
    expect(nodes['ncd_vitals'], 'ncd_vitals should exist').toBe(true);
    expect(nodes['ncd_core_exam'], 'ncd_core_exam should exist').toBe(true);
    expect(nodes['ncd_outside_care'], 'ncd_outside_care should exist').toBe(true);
  });
});

// =========================================================================
// Test 4: Nurse Recurrent Encounter — Lab Results from Case Management
// =========================================================================

test.describe('Nurse: NCD Recurrent Encounter — Lab Results', () => {
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

  test('complete initial encounter with labs, then review results from Case Management', async ({ page }) => {
    // --- PART 1: Complete initial encounter with lab tests performed ---

    const { fullName } = await createAdultAndStartNCDEncounter(page, {
      isFemale: false,
    });

    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeExamination(page);
    await completeMedicalHistory(page);

    // Laboratory: mark tests as performed (so ncd_labs_results is created).
    await completeLaboratory(page, { performTests: true });

    await endNCDEncounter(page);

    // Sync initial encounter to backend.
    await syncAndWait(page);

    // --- PART 2: Access recurrent encounter from Case Management ---

    // Navigate to Case Management and find patient in NCD Labs pane.
    await navigateToCaseManagement(page);

    // Verify Case Management: NCD Labs pane shows entry for this patient.
    await verifyCaseManagementEntry(page, 'NCD Labs', 'NCD Labs', fullName);

    // Open the recurrent encounter.
    await openNCDRecurrentEncounterFromCaseManagement(page, fullName);

    // --- PART 3: Complete recurrent encounter activities ---

    // LabResults activity must be visible (tests were performed in initial phase).
    const labResultsCard = page.locator('.icon-task-laboratory');
    await expect(
      labResultsCard,
      'Expected Lab Results activity card to be visible in recurrent NCD encounter.',
    ).toBeVisible({ timeout: 5000 });
    await completeLabResults(page);

    // RecurrentNextSteps should appear (lab results may trigger diagnosis).
    const nextStepsCard = page.locator('.icon-task-next-steps');
    if (await nextStepsCard.isVisible({ timeout: 5000 }).catch(() => false)) {
      await completeRecurrentNextSteps(page);
    }

    // Leave recurrent encounter (no confirmation dialog).
    await leaveRecurrentEncounter(page);

    // Sync.
    await syncAndWait(page);

    // Verify backend: initial encounter nodes should exist.
    const expectedTypes = [
      'ncd_danger_signs',
      'ncd_symptom_review',
      'ncd_vitals',
      'ncd_core_exam',
      'ncd_co_morbidities',
      'ncd_medication_history',
      'ncd_social_history',
      'ncd_family_history',
      'ncd_outside_care',
    ];
    const nodes = queryNCDNodes(fullName, expectedTypes);

    expect(nodes['ncd_danger_signs'], 'ncd_danger_signs should exist').toBe(true);
    expect(nodes['ncd_symptom_review'], 'ncd_symptom_review should exist').toBe(true);
    expect(nodes['ncd_vitals'], 'ncd_vitals should exist').toBe(true);
    expect(nodes['ncd_core_exam'], 'ncd_core_exam should exist').toBe(true);
    expect(nodes['ncd_co_morbidities'], 'ncd_co_morbidities should exist').toBe(true);
    expect(nodes['ncd_medication_history'], 'ncd_medication_history should exist').toBe(true);
    expect(nodes['ncd_social_history'], 'ncd_social_history should exist').toBe(true);
    expect(nodes['ncd_family_history'], 'ncd_family_history should exist').toBe(true);
    expect(nodes['ncd_outside_care'], 'ncd_outside_care should exist').toBe(true);
  });
});
