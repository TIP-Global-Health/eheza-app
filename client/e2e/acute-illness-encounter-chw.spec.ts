import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createAdultAndStartEncounter,
  completeSymptoms,
  completePhysicalExam,
  completePriorTreatment,
  completeLaboratory,
  completeNextSteps,
  endEncounter,
  syncAndWait,
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

  test('complete CHW initial encounter with respiratory symptoms, verify backend sync', async ({ page }) => {

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

    expect(nodes['symptoms_general']).toBe(true);
    expect(nodes['symptoms_respiratory']).toBe(true);
    expect(nodes['symptoms_gi']).toBe(true);
    expect(nodes['acute_illness_vitals']).toBe(true);
    expect(nodes['acute_findings']).toBe(true);
    expect(nodes['treatment_history']).toBe(true);

    // CHW should NOT have core_exam or malaria_testing.
    expect(nodes['acute_illness_core_exam']).toBeUndefined();
    expect(nodes['malaria_testing']).toBeUndefined();
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

    expect(initialNodes['symptoms_general']).toBe(true);
    expect(initialNodes['acute_illness_vitals']).toBe(true);
    expect(initialNodes['malaria_testing']).toBe(true);

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
      await page.waitForTimeout(3000);
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

    expect(subsequentNodes['acute_illness_danger_signs']).toBe(true);
    expect(subsequentNodes['treatment_ongoing']).toBe(true);
  });
});
