import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
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
  syncAndWait,
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

    expect(nodes['ncd_danger_signs']).toBe(true);
    expect(nodes['ncd_symptom_review']).toBe(true);
    expect(nodes['ncd_vitals']).toBe(true);
    expect(nodes['ncd_core_exam']).toBe(true);
    expect(nodes['ncd_co_morbidities']).toBe(true);
    expect(nodes['ncd_medication_history']).toBe(true);
    expect(nodes['ncd_social_history']).toBe(true);
    expect(nodes['ncd_family_history']).toBe(true);
    expect(nodes['ncd_outside_care']).toBe(true);
    // Stage 1 hypertension triggers HealthEducation.
    expect(nodes['ncd_health_education']).toBe(true);
    // Lab test nodes.
    expect(nodes['ncd_hiv_test']).toBe(true);
    expect(nodes['ncd_random_blood_sugar_test']).toBe(true);
    expect(nodes['ncd_urine_dipstick_test']).toBe(true);
    expect(nodes['ncd_creatinine_test']).toBe(true);
    expect(nodes['ncd_liver_function_test']).toBe(true);
    expect(nodes['ncd_lipid_panel_test']).toBe(true);
    expect(nodes['ncd_hba1c_test']).toBe(true);
    // FamilyPlanning not created (male patient).
    expect(nodes['ncd_family_planning']).toBe(false);
    // PregnancyTest not created (male patient).
    expect(nodes['ncd_pregnancy_test']).toBe(false);
    // No MedicationDistribution (Stage 1, first encounter, no complications).
    expect(nodes['ncd_medication_distribution']).toBe(false);
    // No Referral (Stage 1, not pregnant).
    expect(nodes['ncd_referral']).toBe(false);
  });
});

// =========================================================================
// Test 2: Nurse First NCD Encounter — Female, with Hypertension
// =========================================================================

test.describe('Nurse: NCD First Encounter — Female, Hypertension', () => {
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

    // 3. Examination: HIGH BP (sys=160, dia=100 → Hypertension Stage 2).
    await completeExamination(page, {
      sys: '160',
      dia: '100',
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
    //    MedicationDistribution appears (Stage 2 hypertension).
    //    Referral does NOT appear (requires Stage 3 or pregnant patient).
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
    ];
    const nodes = queryNCDNodes(fullName, expectedTypes);

    expect(nodes['ncd_danger_signs']).toBe(true);
    expect(nodes['ncd_symptom_review']).toBe(true);
    expect(nodes['ncd_vitals']).toBe(true);
    expect(nodes['ncd_core_exam']).toBe(true);
    expect(nodes['ncd_family_planning']).toBe(true);
    expect(nodes['ncd_co_morbidities']).toBe(true);
    expect(nodes['ncd_medication_history']).toBe(true);
    expect(nodes['ncd_social_history']).toBe(true);
    expect(nodes['ncd_family_history']).toBe(true);
    expect(nodes['ncd_outside_care']).toBe(true);
    expect(nodes['ncd_medication_distribution']).toBe(true);
    // Lab test nodes (female → pregnancy test also created).
    expect(nodes['ncd_hiv_test']).toBe(true);
    expect(nodes['ncd_random_blood_sugar_test']).toBe(true);
    expect(nodes['ncd_urine_dipstick_test']).toBe(true);
    expect(nodes['ncd_pregnancy_test']).toBe(true);
    expect(nodes['ncd_creatinine_test']).toBe(true);
    expect(nodes['ncd_liver_function_test']).toBe(true);
    expect(nodes['ncd_lipid_panel_test']).toBe(true);
    expect(nodes['ncd_hba1c_test']).toBe(true);
    // Referral not created (Stage 2 hypertension, non-pregnant).
    expect(nodes['ncd_referral']).toBe(false);
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

    expect(nodes['ncd_danger_signs']).toBe(true);
    expect(nodes['ncd_symptom_review']).toBe(true);
    expect(nodes['ncd_vitals']).toBe(true);
    expect(nodes['ncd_core_exam']).toBe(true);
    expect(nodes['ncd_outside_care']).toBe(true);
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

    expect(nodes['ncd_danger_signs']).toBe(true);
    expect(nodes['ncd_symptom_review']).toBe(true);
    expect(nodes['ncd_vitals']).toBe(true);
    expect(nodes['ncd_core_exam']).toBe(true);
    expect(nodes['ncd_co_morbidities']).toBe(true);
    expect(nodes['ncd_medication_history']).toBe(true);
    expect(nodes['ncd_social_history']).toBe(true);
    expect(nodes['ncd_family_history']).toBe(true);
    expect(nodes['ncd_outside_care']).toBe(true);
  });
});
