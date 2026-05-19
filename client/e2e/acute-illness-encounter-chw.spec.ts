import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import {
  navigateToCaseManagement,
  verifyCaseManagementEntry,
} from './helpers/case-management';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, syncAndWait } from './helpers/common';
import { verifyFeatureGatesEncounterButton } from './helpers/feature-flags';
import {
  createAdultAndStartEncounter,
  completeSymptoms,
  completePhysicalExam,
  completePriorTreatment,
  completeLaboratory,
  completeNextSteps,
  endEncounter,
  queryAcuteIllnessNodes,
  backdateAcuteIllnessEncounter,
  navigateToParticipantPage,
  startSubsequentEncounter,
  completeDangerSigns,
  completeOngoingTreatment,
} from './helpers/acute-illness';

test.describe('CHW: Acute Illness Initial Encounter — Uncomplicated Pneumonia', () => {
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

  test('complete CHW initial encounter with respiratory symptoms, verify backend sync', async ({ page, browser }) => {
    // Verify FeatureAcuteIllness flag gates client UI + admin Reports surfaces.
    await verifyFeatureGatesEncounterButton(page, 'acute_illness', 'Acute Illness', {
      browser,
      admin: {
        sqOptions: ['acute-illness'],
        sqDemographicsRows: ['Acute Illness (total)'],
        completionOptions: ['acute-illness'],
      },
    });

    const { fullName } = await createAdultAndStartEncounter(page, {
      isChw: true,
      gender: 'female',
    });

    // 1. Symptoms: Headache only (general); Cough + Nasal Congestion + Sore Throat
    //    (respiratory); None (GI). No Fever to avoid "Fever of Unknown Origin" —
    //    respiratory-only symptoms trigger a respiratory infection diagnosis.
    await completeSymptoms(page, {
      general: ['Headache'],
      respiratory: ['Cough', 'Nasal Congestion', 'Sore Throat'],
      gi: [],
    });

    // 2. Physical Exam: CHW basic vitals (only RR + Temp), no Core Exam.
    //    Temp < 37.5 (no fever) + RR > 30 (elevated for adult) →
    //    triggers Respiratory Infection Uncomplicated diagnosis.
    await completePhysicalExam(page, {
      isChw: true,
      respiratoryRate: '32',
      bodyTemp: '37.0',
    });

    // 3. Prior Treatment: no prior medication.
    await completePriorTreatment(page);

    // After completing all 3 mandatory activities (Symptoms, Physical Exam,
    // Prior Treatment), the app diagnoses "Uncomplicated Pneumonia" and
    // shows the encounter as complete — no Laboratory or Next Steps needed.

    // End encounter.
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify CHW encounter content types.
    // CHW has no Core Exam (nurse only). No Laboratory for this diagnosis.
    const expectedTypes = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_findings',
      'treatment_history',
    ];
    const nodes = queryAcuteIllnessNodes(fullName, expectedTypes);

    expect(nodes['symptoms_general'], 'symptoms_general should exist').toBe(true);
    expect(nodes['symptoms_respiratory'], 'symptoms_respiratory should exist').toBe(true);
    expect(nodes['symptoms_gi'], 'symptoms_gi should exist').toBe(true);
    expect(nodes['acute_illness_vitals'], 'acute_illness_vitals should exist').toBe(true);
    expect(nodes['acute_findings'], 'acute_findings should exist').toBe(true);
    expect(nodes['treatment_history'], 'treatment_history should exist').toBe(true);

    // CHW should NOT have core_exam or malaria_testing.
    expect(nodes['acute_illness_core_exam'], 'acute_illness_core_exam should not exist').toBe(false);
    expect(nodes['malaria_testing'], 'malaria_testing should not exist').toBe(false);
  });
});

// =========================================================================
// Test 2: CHW Initial + Subsequent Encounter
// =========================================================================

test.describe('CHW: Acute Illness Initial + Subsequent Encounter', () => {
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

  test('complete initial and subsequent CHW encounters, verify backend sync', async ({ page }) => {

    // === PART 1: CHW Initial Encounter ===
    const { fullName } = await createAdultAndStartEncounter(page, {
      isChw: true,
      gender: 'male',
    });

    // Symptoms: Fever + Chills (general); None (respiratory); None (GI).
    await completeSymptoms(page, {
      general: ['Fever', 'Chills'],
      respiratory: [],
      gi: [],
    });

    // Physical Exam: CHW basic vitals, acute findings.
    await completePhysicalExam(page, {
      isChw: true,
      respiratoryRate: '18',
      bodyTemp: '38.5',
    });

    // Prior Treatment: no prior medication.
    await completePriorTreatment(page);

    // Laboratory: Malaria RDT positive.
    await completeLaboratory(page, {
      malariaResult: 'Positive',
      isPregnant: false,
    });

    // Next Steps.
    await completeNextSteps(page, {
      hasMedicationDistribution: true,
      hasFollowUp: true,
      hasSendToHC: true,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    // End initial encounter.
    await endEncounter(page);

    // Sync initial encounter.
    await syncAndWait(page);

    // Verify initial encounter.
    const initialTypes = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_findings',
      'treatment_history',
      'malaria_testing',
    ];
    const initialNodes = queryAcuteIllnessNodes(fullName, initialTypes);

    expect(initialNodes['symptoms_general'], 'symptoms_general should exist').toBe(true);
    expect(initialNodes['acute_illness_vitals'], 'acute_illness_vitals should exist').toBe(true);
    expect(initialNodes['malaria_testing'], 'malaria_testing should exist').toBe(true);

    // --- Case Management verification ---
    // After initial encounter, the follow-up should appear in CM.
    // (Must verify before subsequent encounter, which concludes the illness.)
    await navigateToCaseManagement(page);
    await verifyCaseManagementEntry(
      page,
      'Acute Illness',
      'Acute Illness Follow Up',
      fullName,
    );

    // === PART 2: CHW Subsequent Encounter ===

    // Backdate initial encounter to allow subsequent.
    backdateAcuteIllnessEncounter(fullName);
    await syncAndWait(page);

    // Navigate back and start subsequent encounter.
    await navigateToParticipantPage(page, fullName);
    await startSubsequentEncounter(page);

    // 1. Danger Signs: not improving, respiratory distress.
    await completeDangerSigns(page, {
      conditionImproving: false,
      dangerSigns: ['Respiratory Distress'],
    });

    // 2. Physical Exam: CHW basic vitals (still elevated temp).
    await completePhysicalExam(page, {
      isChw: true,
      respiratoryRate: '22',
      bodyTemp: '38.0',
    });

    // 3. Ongoing Treatment.
    await completeOngoingTreatment(page);

    // 4. Next Steps (if available after diagnosis popup).
    await completeNextSteps(page, {
      hasMedicationDistribution: false,
      hasFollowUp: true,
      hasSendToHC: true,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    // End subsequent encounter (may be outcome page).
    // Check if we're on an outcome page or need to click End Encounter.
    const outcomePage = page.locator('h1', { hasText: 'Acute Illness Outcome' });
    if (await outcomePage.isVisible({ timeout: 3000 }).catch(() => false)) {
      const outcomeSelect = page.locator('select').first();
      await outcomeSelect.waitFor({ timeout: 5000 });
      await outcomeSelect.selectOption({ label: 'Referred to Health Center' });
      await page.locator('button', { hasText: 'Save' }).click();
      await page.waitForTimeout(WAIT.heavyOperation);
    } else {
      await endEncounter(page);
    }

    // Sync subsequent encounter.
    await syncAndWait(page);

    // Verify subsequent encounter types.
    const subsequentTypes = [
      'acute_illness_danger_signs',
      'treatment_ongoing',
    ];
    const subsequentNodes = queryAcuteIllnessNodes(fullName, subsequentTypes);

    expect(subsequentNodes['acute_illness_danger_signs'], 'acute_illness_danger_signs should exist').toBe(true);
    expect(subsequentNodes['treatment_ongoing'], 'treatment_ongoing should exist').toBe(true);
  });
});
