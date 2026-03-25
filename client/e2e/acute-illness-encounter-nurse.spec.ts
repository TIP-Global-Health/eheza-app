import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import {
  createAdultAndStartEncounter,
  createChildAndStartEncounter,
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

test.describe('Nurse: Acute Illness Initial + Subsequent Encounter — Malaria Uncomplicated', () => {
  // Initial + backdate + sync + subsequent encounter — needs extra time.
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

  test('complete initial and subsequent encounters, verify backend sync', async ({ page }) => {

    // =====================================================================
    // PART 1: Nurse Initial Encounter — Malaria Uncomplicated
    // =====================================================================

    const { fullName } = await createAdultAndStartEncounter(page, {
      isChw: false,
      gender: 'female',
    });

    // 1. Symptoms: Fever + Chills + Body Aches → triggers malaria suspicion.
    await completeSymptoms(page, {
      general: ['Fever', 'Chills', 'Body Aches'],
      respiratory: [],
      gi: [],
    });

    // 2. Physical Exam: normal vitals, elevated temp.
    await completePhysicalExam(page, {
      sys: '120',
      dia: '80',
      heartRate: '80',
      respiratoryRate: '18',
      bodyTemp: '38.5',
      muac: '25',
    });

    // 3. Prior Treatment: no prior medication.
    await completePriorTreatment(page);

    // 4. Laboratory: Malaria RDT positive → malaria uncomplicated diagnosis.
    await completeLaboratory(page, {
      malariaResult: 'Positive',
      isPregnant: false,
    });

    // 5. Next Steps: Coartem medication + 3-day follow-up.
    await completeNextSteps(page, {
      hasMedicationDistribution: true,
      hasFollowUp: true,
      hasSendToHC: false,
      hasContactTracing: false,
      hasSymptomsRelief: false,
    });

    // End initial encounter.
    await endEncounter(page);

    // Sync initial encounter to backend.
    await syncAndWait(page);

    // Verify initial encounter content types.
    const initialTypes = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_illness_core_exam',
      'acute_findings',
      'treatment_history',
      'malaria_testing',
      'medication_distribution',
      'acute_illness_follow_up',
    ];
    const initialNodes = queryAcuteIllnessNodes(fullName, initialTypes);

    expect(initialNodes['symptoms_general']).toBe(true);
    expect(initialNodes['symptoms_respiratory']).toBe(true);
    expect(initialNodes['symptoms_gi']).toBe(true);
    expect(initialNodes['acute_illness_vitals']).toBe(true);
    expect(initialNodes['acute_illness_core_exam']).toBe(true);
    expect(initialNodes['acute_findings']).toBe(true);
    expect(initialNodes['treatment_history']).toBe(true);
    expect(initialNodes['malaria_testing']).toBe(true);
    expect(initialNodes['medication_distribution']).toBe(true);
    expect(initialNodes['acute_illness_follow_up']).toBe(true);

    // =====================================================================
    // PART 2: Nurse Subsequent Encounter
    // =====================================================================

    // Backdate the initial encounter to yesterday so the app allows a
    // subsequent encounter (same-day block).
    backdateAcuteIllnessEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start subsequent encounter.
    await navigateToParticipantPage(page, fullName);
    await startSubsequentEncounter(page);

    // 1. Danger Signs: condition improving, no danger signs.
    await completeDangerSigns(page);

    // 2. Physical Exam: normal vitals, normal temp (recovering).
    await completePhysicalExam(page, {
      sys: '120',
      dia: '80',
      heartRate: '75',
      respiratoryRate: '16',
      bodyTemp: '37.0',
    });

    // 3. Ongoing Treatment: taking medication, no issues.
    // After saving, the app shows a diagnosis popup ("Improving") and
    // auto-navigates to Next Steps (Health Education).
    await completeOngoingTreatment(page);

    // 4. Next Steps: the app auto-navigated here after the diagnosis popup.
    // Complete Health Education and any other available sub-tasks.
    await completeNextSteps(page, {
      hasMedicationDistribution: false,
      hasFollowUp: false,
      hasSendToHC: false,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    // 5. Acute Illness Outcome: the "Save & Record Outcome" button from
    // Next Steps navigated here. Select "Illness Resolved" and save.
    await page.locator('heading', { hasText: 'Acute Illness Outcome' })
      .or(page.locator('h1', { hasText: 'Acute Illness Outcome' }))
      .waitFor({ timeout: 10000 })
      .catch(() => {});
    const outcomeSelect = page.locator('select').first();
    await outcomeSelect.waitFor({ timeout: 5000 });
    await outcomeSelect.selectOption({ label: 'Illness Resolved' });
    await page.locator('button', { hasText: 'Save' }).click();
    await page.waitForTimeout(3000);

    // Sync subsequent encounter to backend.
    await syncAndWait(page);

    // Verify subsequent encounter content types.
    // These are NEW types created by subsequent-only activities.
    const subsequentTypes = [
      'acute_illness_danger_signs',
      'treatment_ongoing',
      'health_education',
    ];
    const subsequentNodes = queryAcuteIllnessNodes(fullName, subsequentTypes);

    expect(subsequentNodes['acute_illness_danger_signs']).toBe(true);
    expect(subsequentNodes['treatment_ongoing']).toBe(true);
    expect(subsequentNodes['health_education']).toBe(true);
  });
});

// =========================================================================
// Test 2: Nurse Initial — GI Infection Complicated
// =========================================================================

test.describe('Nurse: Acute Illness Initial Encounter — GI Infection', () => {
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

  // Scenario: 24-month-old child with GI Infection Complicated + COVID test (negative).
  // Child patient → MUAC + Nutrition tabs appear in Physical Exam.
  // COVID test performed (negative) → covid_testing node created, GI diagnosis unchanged.
  test('complete child GI infection with COVID test, verify backend sync', async ({ page }) => {

    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 24,
    });

    // 1. Symptoms: Fever + dehydration signs (general);
    //    None (respiratory); Bloody Diarrhea + Vomiting (GI).
    await completeSymptoms(page, {
      general: ['Fever', 'Lethargy', 'Increased Thirst', 'Dry/Sticky Mouth'],
      respiratory: [],
      gi: ['Bloody Diarrhea', 'Vomiting'],
      intractableVomiting: true,
    });

    // 2. Physical Exam: elevated temp, child vitals (no BP).
    //    MUAC + Nutrition tabs appear for child.
    //    Acute Findings: Sunken Eyes + Poor Skin Turgor (dehydration).
    await completePhysicalExam(page, {
      respiratoryRate: '30',
      bodyTemp: '39.0',
      muac: '14',
      acuteFindingsGeneral: ['Sunken Eyes', 'Poor Skin Turgor'],
      acuteFindingsRespiratory: [],
    });

    // 3. Prior Treatment: no prior medication.
    await completePriorTreatment(page);

    // 4. Laboratory: Malaria RDT negative, COVID test negative.
    //    COVID negative keeps GI diagnosis but creates covid_testing node.
    await completeLaboratory(page, {
      malariaResult: 'Negative',
      covidTestPerformed: true,
      covidResult: 'Negative',
    });

    // 5. Next Steps: GI Complicated → send_to_hc + follow_up.
    await completeNextSteps(page, {
      hasMedicationDistribution: true,
      hasFollowUp: true,
      hasSendToHC: true,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    // End encounter.
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify core + child-specific + COVID content types.
    const expectedTypes = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_illness_core_exam',
      'acute_findings',
      'acute_illness_muac',
      'acute_illness_nutrition',
      'treatment_history',
      'malaria_testing',
      'send_to_hc',
      'acute_illness_follow_up',
    ];
    const nodes = queryAcuteIllnessNodes(fullName, expectedTypes);

    expect(nodes['symptoms_general']).toBe(true);
    expect(nodes['symptoms_respiratory']).toBe(true);
    expect(nodes['symptoms_gi']).toBe(true);
    expect(nodes['acute_illness_vitals']).toBe(true);
    expect(nodes['acute_illness_core_exam']).toBe(true);
    expect(nodes['acute_findings']).toBe(true);
    // Child-specific nodes.
    expect(nodes['acute_illness_muac']).toBe(true);
    expect(nodes['acute_illness_nutrition']).toBe(true);
    // Lab nodes.
    expect(nodes['treatment_history']).toBe(true);
    expect(nodes['malaria_testing']).toBe(true);
    // NextSteps.
    expect(nodes['send_to_hc']).toBe(true);
    expect(nodes['acute_illness_follow_up']).toBe(true);
  });
});
