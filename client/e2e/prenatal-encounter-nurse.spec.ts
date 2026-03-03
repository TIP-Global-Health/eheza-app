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
  completeHistory,
  completeExamination,
  completeFamilyPlanning,
  completeDangerSigns,
  completeSymptomReview,
  completeMalariaPrevention,
  completeMentalHealth,
  completeImmunisation,
  completeMedication,
  completeLaboratoryNurse,
  completeNextSteps,
  completeTreatmentReview,
  completeBreastfeeding,
  completeSpecialityCare,
  completePostpartumTreatmentReview,
  completePregnancyOutcome,
  backdatePrenatalEncounter,
  queryPrenatalNodes,
  navigateToCaseManagement,
  openRecurrentEncounterFromCaseManagement,
  completeRecurrentExamination,
  endRecurrentEncounter,
} from './helpers/prenatal';

test.describe('Nurse: Prenatal Initial Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as Nurse (PIN 1234), select Nyange Health Center.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete all activities and verify backend sync', async ({ page }) => {
    // Nurse initial encounter completes 12+ activities; needs more than the default 2m.
    test.setTimeout(600000);
    // LMP date ~30 weeks ago — ensures EGA >= 28w for MentalHealth,
    // >= 13w for MalariaPrevention, and triggers all Medication tabs.
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    // Complete all nurse initial encounter activities.
    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    await completeLaboratoryNurse(page);
    await completeNextSteps(page);
    // PrenatalPhoto skipped (file upload; encounter allows ending without it).

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify CTs exist in Drupal.
    const nodes = queryPrenatalNodes(fullName);

    // PregnancyDating
    expect(nodes['last_menstrual_period']).toBe(true);

    // History
    expect(nodes['obstetric_history']).toBe(true);
    expect(nodes['obstetric_history_step2']).toBe(true);
    expect(nodes['medical_history']).toBe(true);

    // Examination
    expect(nodes['vitals']).toBe(true);
    expect(nodes['prenatal_nutrition']).toBe(true);
    expect(nodes['core_physical_exam']).toBe(true);
    expect(nodes['obstetrical_exam']).toBe(true);
    expect(nodes['breast_exam']).toBe(true);

    // Simple activities
    expect(nodes['prenatal_family_planning']).toBe(true);
    expect(nodes['danger_signs']).toBe(true);
    expect(nodes['prenatal_symptom_review']).toBe(true);
    expect(nodes['prenatal_mental_health']).toBe(true);
    expect(nodes['prenatal_tetanus_immunisation']).toBe(true);
  });
});

test.describe('Nurse: Prenatal Subsequent Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete treatment review and verify backend sync', async ({
    page,
  }) => {
    // Subsequent encounter: full initial + backdate + subsequent — needs extra time.
    test.setTimeout(600000);
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // First: complete a minimal nurse initial encounter.
    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    await completeLaboratoryNurse(page);
    await completeNextSteps(page);
    await endPrenatalEncounter(page);

    // Sync first encounter to backend, then backdate it to yesterday so the
    // app allows starting a subsequent encounter (same-day block).
    await syncAndWait(page);
    backdatePrenatalEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start subsequent encounter.
    await navigateToParticipantPage(page, fullName);
    await startPrenatalEncounter(page, 'subsequent');

    // Complete subsequent encounter activities.
    await completeHistory(page, { isSubsequent: true });
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeTreatmentReview(page);
    await completeLaboratoryNurse(page);
    await completeNextSteps(page);

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify unique CTs for subsequent encounter.
    const expectedTypes = ['medication', 'prenatal_outside_care'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    // TreatmentReview creates medication node.
    expect(nodes['medication']).toBe(true);
    // OutsideCare (subsequent only).
    expect(nodes['prenatal_outside_care']).toBe(true);
  });
});

test.describe('Nurse: Prenatal Postpartum Encounter', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('complete breastfeeding and verify backend sync', async ({ page }) => {
    // Postpartum encounter: full initial + backdate + postpartum — needs extra time.
    test.setTimeout(600000);
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // First: complete a minimal nurse initial encounter.
    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    await completeLaboratoryNurse(page);
    await completeNextSteps(page);
    await endPrenatalEncounter(page);

    // Sync first encounter to backend, then backdate it to yesterday so the
    // app allows starting a postpartum encounter (same-day block).
    await syncAndWait(page);
    backdatePrenatalEncounter(fullName);
    await syncAndWait(page);

    // Navigate back and start postpartum encounter.
    await navigateToParticipantPage(page, fullName);
    await startPrenatalEncounter(page, 'postpartum');

    // Complete PregnancyOutcome first (required before other postpartum activities).
    await completePregnancyOutcome(page);

    // Complete postpartum-specific activities (no DangerSigns or NextSteps).
    await completeSymptomReview(page);
    await completeMentalHealth(page);
    await completeBreastfeeding(page);
    await completeExamination(page, { isPostpartum: true });
    await completeFamilyPlanning(page);
    await completePostpartumTreatmentReview(page);

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify postpartum-unique CT.
    const expectedTypes = ['prenatal_breastfeeding'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    expect(nodes['prenatal_breastfeeding']).toBe(true);
  });
});

test.describe('Nurse: Prenatal Recurrent Encounter (BP Recheck)', () => {
  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('marginal BP triggers recurrent phase with vitals recheck', async ({
    page,
  }) => {
    // Recurrent encounter: full initial (with marginal BP) + sync + case management
    // + recurrent vitals recheck — needs extra time.
    test.setTimeout(600000);
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // --- Phase 1: Complete initial encounter with marginal blood pressure ---
    const { fullName } = await createAdultFemaleAndStartEncounter(page, {
      isChw: false,
      encounterType: 'first',
    });

    await completePregnancyDating(page, lmpDate);
    await completeHistory(page);
    // Marginal BP: sys=145 (140 ≤ x < 160), dia=95 (90 ≤ x < 110).
    // This triggers TestVitalsRecheck → Wait task → Pause Encounter flow.
    await completeExamination(page, { vitals: { sys: '145', dia: '95' } });
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    await completeMedication(page);
    await completeLaboratoryNurse(page);
    // NextSteps will show the Wait sub-task due to marginal BP.
    // completeNextSteps handles Wait → clicks "Pause Encounter" → PinCodePage.
    const completedSteps = await completeNextSteps(page);
    expect(completedSteps).toContain('wait');

    // --- Phase 2: Sync and navigate to recurrent encounter via Case Management ---
    // After pause, nurse is on PinCodePage (still logged in).
    await syncAndWait(page);

    // Navigate to Case Management from the nurse menu.
    await navigateToCaseManagement(page);

    // Find the patient in the Prenatal Labs pane and open the recurrent encounter.
    await openRecurrentEncounterFromCaseManagement(page, fullName);

    // --- Phase 3: Complete recurrent encounter (vitals recheck) ---
    // RecurrentExamination: VitalsFormRepeated mode (BP only, no HR/RR/temp).
    // Normal BP (120/80) — no hypertension confirmed.
    await completeRecurrentExamination(page);

    // End the recurrent encounter from the progress report page.
    await endRecurrentEncounter(page);

    // --- Phase 4: Sync and verify backend ---
    await syncAndWait(page);

    const expectedTypes = ['prenatal_labs_results'];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);
    expect(nodes['prenatal_labs_results']).toBe(true);
  });
});
