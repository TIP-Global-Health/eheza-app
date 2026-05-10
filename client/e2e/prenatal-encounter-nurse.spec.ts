import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { verifyCaseManagementEntry } from './helpers/case-management';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import { verifyFeatureGatesEncounterButton } from './helpers/feature-flags';
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

  test('complete all activities and verify backend sync', async ({ page, browser }) => {
    // Nurse initial encounter completes 12+ activities; needs more than the default 2m.
    test.setTimeout(600000);

    // Verify FeatureAntenatal flag gates client UI + admin Reports surfaces.
    await verifyFeatureGatesEncounterButton(page, 'antenatal', 'Antenatal Care', {
      browser,
      admin: {
        sqOptions: ['peripartum', 'prenatal', 'prenatal-contacts', 'prenatal-diagnoses'],
        sqDemographicsRows: ['ANC Total'],
        completionOptions: ['prenatal'],
      },
    });

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
    // Stage 2 hypertension (sys=160, dia=100) → triggers MedicationDistribution.
    // Note: marginal BP (Stage 1) would trigger Wait/Pause flow instead.
    await completeExamination(page, { vitals: { sys: '160', dia: '100' } });
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeMalariaPrevention(page);
    await completeMentalHealth(page);
    await completeImmunisation(page);
    // Prefer iron+folate over fefol (mutually exclusive — iron blocks fefol).
    await completeMedication(page, { preferIronFolate: true });
    // HIV test positive → creates HIV diagnosis, triggers NextSteps
    // (HealthEducation, SendToHC) + HIV PCR in subsequent.
    // Combined with Stage 1 hypertension → also triggers MedicationDistribution.
    await completeLaboratoryNurse(page, { hivPositive: true });
    await completeNextSteps(page);
    // PrenatalPhoto skipped (file upload; encounter allows ending without it).

    // End encounter.
    await endPrenatalEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify CTs exist in Drupal.
    const expectedTypes = [
      'last_menstrual_period',
      'obstetric_history',
      'prenatal_hiv_test',
      'prenatal_calcium',
      'prenatal_send_to_hc',
      'prenatal_medication_distribution',
    ];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);

    // PregnancyDating
    expect(nodes['last_menstrual_period'], 'last_menstrual_period should exist').toBe(true);

    // History
    expect(nodes['obstetric_history'], 'obstetric_history should exist').toBe(true);
    expect(nodes['obstetric_history_step2'], 'obstetric_history_step2 should exist').toBe(true);
    expect(nodes['medical_history'], 'medical_history should exist').toBe(true);

    // Examination
    expect(nodes['vitals'], 'vitals should exist').toBe(true);
    expect(nodes['prenatal_nutrition'], 'prenatal_nutrition should exist').toBe(true);
    expect(nodes['core_physical_exam'], 'core_physical_exam should exist').toBe(true);
    expect(nodes['obstetrical_exam'], 'obstetrical_exam should exist').toBe(true);
    expect(nodes['breast_exam'], 'breast_exam should exist').toBe(true);

    // Simple activities
    expect(nodes['prenatal_family_planning'], 'prenatal_family_planning should exist').toBe(true);
    expect(nodes['danger_signs'], 'danger_signs should exist').toBe(true);
    expect(nodes['prenatal_symptom_review'], 'prenatal_symptom_review should exist').toBe(true);
    expect(nodes['prenatal_mental_health'], 'prenatal_mental_health should exist').toBe(true);
    expect(nodes['prenatal_tetanus_immunisation'], 'prenatal_tetanus_immunisation should exist').toBe(true);

    // Medication (EGA ~30w, first encounter, preferIronFolate):
    // Calcium (>= 14w), Iron+Folate (blocks Fefol), MMS, Mebendazole (>= 24w).
    expect(nodes['prenatal_calcium'], 'prenatal_calcium should exist').toBe(true);
    expect(nodes['prenatal_iron'], 'prenatal_iron should exist').toBe(true);
    expect(nodes['prenatal_folate'], 'prenatal_folate should exist').toBe(true);
    expect(nodes['prenatal_mms'], 'prenatal_mms should exist').toBe(true);
    expect(nodes['prenatal_mebendazole'], 'prenatal_mebendazole should exist').toBe(true);

    // Laboratory (8 standard tests).
    expect(nodes['prenatal_hiv_test'], 'prenatal_hiv_test should exist').toBe(true);
    expect(nodes['prenatal_syphilis_test'], 'prenatal_syphilis_test should exist').toBe(true);
    expect(nodes['prenatal_hepatitis_b_test'], 'prenatal_hepatitis_b_test should exist').toBe(true);
    expect(nodes['prenatal_malaria_test'], 'prenatal_malaria_test should exist').toBe(true);
    expect(nodes['prenatal_blood_gprs_test'], 'prenatal_blood_gprs_test should exist').toBe(true);
    expect(nodes['prenatal_urine_dipstick_test'], 'prenatal_urine_dipstick_test should exist').toBe(true);
    expect(nodes['prenatal_hemoglobin_test'], 'prenatal_hemoglobin_test should exist').toBe(true);
    expect(nodes['prenatal_random_blood_sugar_test'], 'prenatal_random_blood_sugar_test should exist').toBe(true);
    expect(nodes['prenatal_partner_hiv_test'], 'prenatal_partner_hiv_test should exist').toBe(true);

    // NextSteps (HIV diagnosis → referral + education; hypertension → medication).
    expect(nodes['prenatal_health_education'], 'prenatal_health_education should exist').toBe(true);
    expect(nodes['prenatal_send_to_hc'], 'prenatal_send_to_hc should exist').toBe(true);
    expect(nodes['prenatal_medication_distribution'], 'prenatal_medication_distribution should exist').toBe(true);
  });
});

// =========================================================================
// Test 2: Nurse Initial → Subsequent → Postpartum (chained)
// =========================================================================

test.describe('Nurse: Prenatal Initial → Subsequent → Postpartum', () => {
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

  test('complete all three encounter phases and verify backend sync', async ({
    page,
  }) => {
    const lmpDate = new Date();
    lmpDate.setDate(lmpDate.getDate() - 30 * 7);

    // =====================================================================
    // PHASE 1: Initial Encounter
    // =====================================================================

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
    // HIV known positive → triggers HIV PCR in subsequent + SpecialityCare in postpartum.
    await completeLaboratoryNurse(page, { hivPositive: true });
    await completeNextSteps(page);
    await endPrenatalEncounter(page);

    // Sync + backdate initial encounter to 2 weeks ago.
    await syncAndWait(page);
    backdatePrenatalEncounter(fullName, 14);
    await syncAndWait(page);
    // Reload to force Elm model to reinitialize with the backdated data.
    await page.reload();
    await Promise.race([
      page.locator('input[name="pincode"]').waitFor({ timeout: 30000 }),
      page.locator('.wrap-cards').waitFor({ timeout: 30000 }),
    ]);
    if (await page.locator('input[name="pincode"]').isVisible().catch(() => false)) {
      await page.locator('input[name="pincode"]').fill('1234');
      await page.getByRole('button', { name: 'Sign In' }).click();
      await Promise.race([
        page.locator('p.select-location').waitFor({ timeout: 30000 }),
        page.locator('.wrap-cards').waitFor({ timeout: 30000 }),
      ]);
      if (await page.locator('p.select-location').isVisible().catch(() => false)) {
        await page.locator('button.ui.primary.button', { hasText: 'Nyange Health Center' }).click();
      }
    }
    await page.locator('.wrap-cards').waitFor({ timeout: 30000 });
    await syncAndWait(page);

    // =====================================================================
    // PHASE 2: Subsequent Encounter
    // =====================================================================

    await navigateToParticipantPage(page, fullName);
    await startPrenatalEncounter(page, 'subsequent');

    await completeHistory(page, { isSubsequent: true });
    await completeExamination(page);
    await completeFamilyPlanning(page);
    await completeDangerSigns(page);
    await completeSymptomReview(page);
    await completeTreatmentReview(page);
    await completeLaboratoryNurse(page);
    await completeNextSteps(page);
    await endPrenatalEncounter(page);

    // Sync + backdate subsequent encounter to 1 week ago.
    await syncAndWait(page);
    backdatePrenatalEncounter(fullName, 7);
    await syncAndWait(page);
    // Reload to force Elm model to reinitialize with the backdated data.
    await page.reload();
    await Promise.race([
      page.locator('input[name="pincode"]').waitFor({ timeout: 30000 }),
      page.locator('.wrap-cards').waitFor({ timeout: 30000 }),
    ]);
    if (await page.locator('input[name="pincode"]').isVisible().catch(() => false)) {
      await page.locator('input[name="pincode"]').fill('1234');
      await page.getByRole('button', { name: 'Sign In' }).click();
      await Promise.race([
        page.locator('p.select-location').waitFor({ timeout: 30000 }),
        page.locator('.wrap-cards').waitFor({ timeout: 30000 }),
      ]);
      if (await page.locator('p.select-location').isVisible().catch(() => false)) {
        await page.locator('button.ui.primary.button', { hasText: 'Nyange Health Center' }).click();
      }
    }
    await page.locator('.wrap-cards').waitFor({ timeout: 30000 });
    await syncAndWait(page);

    // =====================================================================
    // PHASE 3: Postpartum Encounter
    // =====================================================================

    await navigateToParticipantPage(page, fullName);
    await startPrenatalEncounter(page, 'postpartum');

    await completePregnancyOutcome(page);
    await completeSymptomReview(page);
    await completeMentalHealth(page);
    await completeBreastfeeding(page);
    await completeExamination(page, { isPostpartum: true });
    await completeFamilyPlanning(page);
    await completePostpartumTreatmentReview(page);
    // SpecialityCare: appears because initial encounter set HIV known positive.
    await completeSpecialityCare(page);
    await endPrenatalEncounter(page);

    // =====================================================================
    // Sync + Verify all phases
    // =====================================================================

    await syncAndWait(page);

    const expectedTypes = [
      'medication',
      'prenatal_outside_care',
      'prenatal_hiv_pcr_test',
      'prenatal_breastfeeding',
      'prenatal_gu_exam',
      'prenatal_speciality_care',
    ];
    const nodes = queryPrenatalNodes(fullName, expectedTypes);

    // Subsequent encounter CTs.
    expect(nodes['medication'], 'medication should exist').toBe(true);
    expect(nodes['prenatal_outside_care'], 'prenatal_outside_care should exist').toBe(true);
    // HIV PCR (subsequent — triggered by HIV known positive in initial).
    expect(nodes['prenatal_hiv_pcr_test'], 'prenatal_hiv_pcr_test should exist').toBe(true);
    // Postpartum encounter CTs.
    expect(nodes['prenatal_breastfeeding'], 'prenatal_breastfeeding should exist').toBe(true);
    expect(nodes['prenatal_gu_exam'], 'prenatal_gu_exam should exist').toBe(true);
    // SpecialityCare (postpartum — triggered by HIV known positive in initial).
    expect(nodes['prenatal_speciality_care'], 'prenatal_speciality_care should exist').toBe(true);
  });
});

// =========================================================================
// Test 3: Nurse Recurrent Encounter (BP Recheck)
// =========================================================================

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
    expect(completedSteps, 'completedSteps should contain wait sub-task for marginal BP').toContain('wait');

    // --- Phase 2: Sync and navigate to recurrent encounter via Case Management ---
    // After pause, nurse is on PinCodePage (still logged in).
    await syncAndWait(page);

    // Navigate to Case Management from the nurse menu.
    await navigateToCaseManagement(page);

    // Verify Case Management: ANC Labs pane shows entry for this patient.
    await verifyCaseManagementEntry(page, 'ANC Labs', 'ANC Labs', fullName);

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
    expect(nodes['prenatal_labs_results'], 'prenatal_labs_results should exist').toBe(true);
  });
});
