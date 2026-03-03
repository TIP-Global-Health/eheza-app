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
    ];
    const subsequentNodes = queryAcuteIllnessNodes(fullName, subsequentTypes);

    expect(subsequentNodes['acute_illness_danger_signs']).toBe(true);
    expect(subsequentNodes['treatment_ongoing']).toBe(true);
  });
});
