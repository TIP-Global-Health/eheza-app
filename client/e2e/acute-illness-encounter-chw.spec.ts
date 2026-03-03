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
} from './helpers/acute-illness';

test.describe('CHW: Acute Illness Initial Encounter — Simple Cold and Cough', () => {
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
    //    Acute Findings present on initial encounter.
    await completePhysicalExam(page, {
      isChw: true,
      respiratoryRate: '20',
      bodyTemp: '37.8',
    });

    // 3. Prior Treatment: no prior medication.
    await completePriorTreatment(page);

    // 4. Laboratory: Malaria RDT negative → respiratory path.
    await completeLaboratory(page, {
      malariaResult: 'Negative',
    });

    // 5. Next Steps: the app auto-navigates here after mandatory activities.
    //    Available sub-tasks depend on the diagnosis.
    await completeNextSteps(page, {
      hasMedicationDistribution: true,
      hasFollowUp: true,
      hasSendToHC: true,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    // End encounter — may show "End Encounter" button or progress report.
    await endEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify CHW encounter content types.
    // CHW has no Core Exam (nurse only). No COVID testing.
    const expectedTypes = [
      'symptoms_general',
      'symptoms_respiratory',
      'symptoms_gi',
      'acute_illness_vitals',
      'acute_findings',
      'treatment_history',
      'malaria_testing',
    ];
    const nodes = queryAcuteIllnessNodes(fullName, expectedTypes);

    expect(nodes['symptoms_general']).toBe(true);
    expect(nodes['symptoms_respiratory']).toBe(true);
    expect(nodes['symptoms_gi']).toBe(true);
    expect(nodes['acute_illness_vitals']).toBe(true);
    expect(nodes['acute_findings']).toBe(true);
    expect(nodes['treatment_history']).toBe(true);
    expect(nodes['malaria_testing']).toBe(true);

    // CHW should NOT have core_exam.
    expect(nodes['acute_illness_core_exam']).toBeUndefined();
  });
});
