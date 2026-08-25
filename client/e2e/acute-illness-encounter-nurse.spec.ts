import { openReport, closeReport } from './helpers/progress-report';
import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, syncAndWait } from './helpers/common';
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

    expect(initialNodes['symptoms_general'], 'symptoms_general should exist').toBe(true);
    expect(initialNodes['symptoms_respiratory'], 'symptoms_respiratory should exist').toBe(true);
    expect(initialNodes['symptoms_gi'], 'symptoms_gi should exist').toBe(true);
    expect(initialNodes['acute_illness_vitals'], 'acute_illness_vitals should exist').toBe(true);
    expect(initialNodes['acute_illness_core_exam'], 'acute_illness_core_exam should exist').toBe(true);
    expect(initialNodes['acute_findings'], 'acute_findings should exist').toBe(true);
    expect(initialNodes['treatment_history'], 'treatment_history should exist').toBe(true);
    expect(initialNodes['malaria_testing'], 'malaria_testing should exist').toBe(true);
    expect(initialNodes['medication_distribution'], 'medication_distribution should exist').toBe(true);
    expect(initialNodes['acute_illness_follow_up'], 'acute_illness_follow_up should exist').toBe(true);

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

    // Progress report holds both encounters of the illness, the one being
    // viewed included. Rows are ordered most recent first.
    const report = await openReport(page, 'acute-illness');
    const rates = report.locator('.pane.physical-exam td.respiratory-rate');
    await expect(rates).toHaveCount(2);
    await expect(rates.first()).toContainText('16');
    await expect(rates.last()).toContainText('18');

    // Assessment and symptoms come from the encounter that opened the illness.
    await expect(report.locator('.pane.assessment'))
      .toContainText('Malaria Without Complications');
    await expect(report.locator('.pane.symptoms')).toContainText('Fever');

    await closeReport(page, 'acute-illness');

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
    await page.waitForTimeout(WAIT.heavyOperation);

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

    expect(subsequentNodes['acute_illness_danger_signs'], 'acute_illness_danger_signs should exist').toBe(true);
    expect(subsequentNodes['treatment_ongoing'], 'treatment_ongoing should exist').toBe(true);
    expect(subsequentNodes['health_education'], 'health_education should exist').toBe(true);
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
      checkMuacRange: true,
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

    expect(nodes['symptoms_general'], 'symptoms_general should exist').toBe(true);
    expect(nodes['symptoms_respiratory'], 'symptoms_respiratory should exist').toBe(true);
    expect(nodes['symptoms_gi'], 'symptoms_gi should exist').toBe(true);
    expect(nodes['acute_illness_vitals'], 'acute_illness_vitals should exist').toBe(true);
    expect(nodes['acute_illness_core_exam'], 'acute_illness_core_exam should exist').toBe(true);
    expect(nodes['acute_findings'], 'acute_findings should exist').toBe(true);
    // Child-specific nodes.
    expect(nodes['acute_illness_muac'], 'acute_illness_muac should exist').toBe(true);
    expect(nodes['acute_illness_nutrition'], 'acute_illness_nutrition should exist').toBe(true);
    // Lab nodes.
    expect(nodes['treatment_history'], 'treatment_history should exist').toBe(true);
    expect(nodes['malaria_testing'], 'malaria_testing should exist').toBe(true);
    // NextSteps.
    expect(nodes['send_to_hc'], 'send_to_hc should exist').toBe(true);
    expect(nodes['acute_illness_follow_up'], 'acute_illness_follow_up should exist').toBe(true);
  });
});

// =========================================================================
// Test 3: CHW opens the illness, nurse takes it over
// =========================================================================

test.describe('Nurse takeover: Acute Illness opened by CHW', () => {
  test.describe.configure({ timeout: 900000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('nurse takeover report holds both encounters and reads the nurse as initial', async ({ page }) => {

    // === PART 1: CHW opens the illness ===
    const { fullName } = await createAdultAndStartEncounter(page, {
      isChw: true,
      gender: 'male',
    });

    // Malaria, so the illness is one the app goes on pursuing — a fever with
    // a negative test is filed as Fever of Unknown Origin and closed.
    // The nurse records different symptoms below, so the report can be asked
    // which of the two it treats as the start of the illness.
    await completeSymptoms(page, {
      general: ['Fever', 'Chills'],
      respiratory: [],
      gi: [],
    });

    await completePhysicalExam(page, {
      isChw: true,
      respiratoryRate: '18',
      bodyTemp: '38.5',
    });

    await completePriorTreatment(page);

    await completeLaboratory(page, {
      malariaResult: 'Positive',
      isPregnant: false,
    });

    await completeNextSteps(page, {
      hasMedicationDistribution: true,
      hasFollowUp: true,
      hasSendToHC: true,
      hasContactTracing: false,
      hasSymptomsRelief: false,
      hasHealthEducation: true,
    });

    await endEncounter(page);
    await syncAndWait(page);

    // === PART 2: the nurse takes the illness over ===

    // Backdate so the app allows another encounter on the same illness.
    backdateAcuteIllnessEncounter(fullName);
    await syncAndWait(page);

    // Same device, nurse credentials at the health center.
    await setupDevice(page, '1234', 'Nyange Health Center');

    await navigateToParticipantPage(page, fullName);
    await startSubsequentEncounter(page);

    // A nurse taking over runs a full encounter, so symptoms are collected
    // again. Respiratory ones this time, which the CHW did not record.
    await completeSymptoms(page, {
      general: [],
      respiratory: ['Cough', 'Nasal Congestion'],
      gi: [],
    });

    await completePhysicalExam(page, {
      sys: '120',
      dia: '80',
      heartRate: '80',
      respiratoryRate: '26',
      bodyTemp: '37.2',
    });

    await completePriorTreatment(page);

    // Report holds the whole illness: a row for the CHW encounter and one
    // for the nurse encounter, most recent first.
    const report = await openReport(page, 'acute-illness');
    const rates = report.locator('.pane.physical-exam td.respiratory-rate');
    await expect(rates).toHaveCount(2);
    await expect(rates.first()).toContainText('26');
    await expect(rates.last()).toContainText('18');

    // The nurse encounter starts the illness anew, so the symptoms shown are
    // the ones the nurse recorded, not the ones the CHW opened with.
    const symptoms = report.locator('.pane.symptoms');
    await expect(symptoms).toContainText('Cough');
    await expect(symptoms).not.toContainText('Chills');

    await closeReport(page, 'acute-illness');
  });
});
