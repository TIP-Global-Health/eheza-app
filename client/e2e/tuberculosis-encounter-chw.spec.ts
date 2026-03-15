import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createAdultAndStartTBEncounter,
  completeDiagnostics,
  completeMedication,
  completeSymptomReview,
  completeNextSteps,
  endTBEncounter,
  syncAndWait,
  queryTBNodes,
  backdateTBEncounter,
  navigateToParticipantPage,
  startTBEncounter,
} from './helpers/tuberculosis';

// =========================================================================
// Test 1: CHW Initial TB Encounter — Positive Diagnosis (Pulmonary)
// =========================================================================

test.describe('CHW: Tuberculosis Initial Encounter — Positive Diagnosis', () => {
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

  // Scenario: Initial TB encounter for male adult with positive pulmonary diagnosis.
  // Activities: Diagnostics (Yes + Pulmonary), Medication (PrescribedMedication + DOT + TreatmentReview),
  //             NextSteps (HealthEducation + FollowUp).
  // Conditions: Initial encounter -> Diagnostics shown, SymptomReview NOT shown.
  //             No symptoms/adverse events -> Referral NOT triggered.
  // Backend: Verifies 6 node types created (diagnostics, medication, dot, treatment_review,
  //          health_education, follow_up), confirms symptom_review and referral absent.
  test('complete initial TB encounter with positive pulmonary diagnosis, verify backend sync', async ({ page }) => {
    const { fullName } = await createAdultAndStartTBEncounter(page, {
      isFemale: false,
    });

    // 1. Diagnostics: patient diagnosed with pulmonary TB.
    await completeDiagnostics(page, { path: 'positive-pulmonary' });

    // 2. Medication: select RHZE + DOT (both yes) + treatment review (no side effects).
    await completeMedication(page);

    // 3. NextSteps: HealthEducation + FollowUp (no Referral — no symptoms/adverse events).
    await completeNextSteps(page);

    // End encounter.
    await endTBEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'tuberculosis_diagnostics',
      'tuberculosis_medication',
      'tuberculosis_dot',
      'tuberculosis_treatment_review',
      'tuberculosis_health_education',
      'tuberculosis_follow_up',
    ];
    const nodes = queryTBNodes(fullName, expectedTypes);

    expect(nodes['tuberculosis_diagnostics']).toBe(true);
    expect(nodes['tuberculosis_medication']).toBe(true);
    expect(nodes['tuberculosis_dot']).toBe(true);
    expect(nodes['tuberculosis_treatment_review']).toBe(true);
    expect(nodes['tuberculosis_health_education']).toBe(true);
    expect(nodes['tuberculosis_follow_up']).toBe(true);
    // No symptoms or adverse events -> no referral.
    expect(nodes['tuberculosis_referral']).toBe(false);
    // No symptom review in initial encounter.
    expect(nodes['tuberculosis_symptom_review']).toBe(false);
  });
});

// =========================================================================
// Test 2: CHW Subsequent TB Encounter — Symptoms + Adverse Events -> Referral
// =========================================================================

test.describe('CHW: Tuberculosis Subsequent Encounter — Symptoms + Referral', () => {
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

  // Scenario: Subsequent TB encounter with symptoms and adverse events triggering referral.
  // Activities: Medication (medications-not-changed + DOT + TreatmentReview with side effects),
  //             SymptomReview (night sweats + blood in sputum),
  //             NextSteps (HealthEducation + FollowUp + Referral).
  // Conditions: Subsequent encounter -> no Diagnostics. Adverse events + symptoms -> Referral triggered.
  // Backend: Verifies all 8 node types present (including diagnostics from initial encounter).
  test('complete subsequent TB encounter with symptoms and adverse events triggering referral', async ({ page }) => {
    // --- PART 1: Complete a simplified initial encounter ---

    const { fullName } = await createAdultAndStartTBEncounter(page, {
      isFemale: false,
    });

    // Diagnostics + Medication (minimal to allow encounter to end).
    await completeDiagnostics(page, { path: 'positive-pulmonary' });
    await completeMedication(page);
    await completeNextSteps(page);
    await endTBEncounter(page);

    // Sync initial encounter.
    await syncAndWait(page);

    // --- PART 2: Backdate and start subsequent encounter ---

    backdateTBEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start new encounter.
    await navigateToParticipantPage(page, fullName);
    await startTBEncounter(page);

    // --- PART 3: Complete subsequent encounter activities ---

    // Medication: medications not changed + DOT + treatment review with side effects.
    await completeMedication(page, {
      isSubsequent: true,
      sideEffects: true,
    });

    // SymptomReview: report symptoms (triggers referral along with adverse events).
    await completeSymptomReview(page, {
      nightSweats: true,
      bloodInSputum: true,
    });

    // NextSteps: HealthEducation + FollowUp + Referral (triggered by symptoms + adverse events).
    await completeNextSteps(page);

    // End encounter.
    await endTBEncounter(page);

    // Sync.
    await syncAndWait(page);

    // Verify backend nodes — all 8 types should exist (diagnostics from initial encounter).
    const expectedTypes = [
      'tuberculosis_diagnostics',
      'tuberculosis_medication',
      'tuberculosis_dot',
      'tuberculosis_treatment_review',
      'tuberculosis_symptom_review',
      'tuberculosis_health_education',
      'tuberculosis_follow_up',
      'tuberculosis_referral',
    ];
    const nodes = queryTBNodes(fullName, expectedTypes);

    expect(nodes['tuberculosis_diagnostics']).toBe(true);
    expect(nodes['tuberculosis_medication']).toBe(true);
    expect(nodes['tuberculosis_dot']).toBe(true);
    expect(nodes['tuberculosis_treatment_review']).toBe(true);
    expect(nodes['tuberculosis_symptom_review']).toBe(true);
    expect(nodes['tuberculosis_health_education']).toBe(true);
    expect(nodes['tuberculosis_follow_up']).toBe(true);
    expect(nodes['tuberculosis_referral']).toBe(true);
  });
});

// =========================================================================
// Test 3: CHW Initial TB Encounter — No Diagnosis (End Encounter Dialog)
// =========================================================================

test.describe('CHW: Tuberculosis Initial Encounter — No Diagnosis', () => {
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

  // Scenario: Initial TB encounter where patient is not diagnosed with TB.
  // Activities: Diagnostics only (triggers end-encounter confirmation dialog).
  // Conditions: No diagnosis -> encounter closes via confirmation dialog. No other activities completed.
  // Backend: Verifies only tuberculosis_diagnostics created, all other 7 node types absent.
  test('complete TB encounter with no diagnosis, encounter ends via confirmation dialog', async ({ page }) => {
    const { fullName } = await createAdultAndStartTBEncounter(page, {
      isFemale: false,
    });

    // Diagnostics: not diagnosed -> end encounter dialog.
    await completeDiagnostics(page, { path: 'no-diagnosis' });

    // Encounter should have closed via the confirmation dialog.
    // Sync to backend.
    await syncAndWait(page);

    // Verify only diagnostics node was created.
    const expectedTypes = [
      'tuberculosis_diagnostics',
    ];
    const nodes = queryTBNodes(fullName, expectedTypes);

    expect(nodes['tuberculosis_diagnostics']).toBe(true);
    // No other activities should have been completed.
    expect(nodes['tuberculosis_medication']).toBe(false);
    expect(nodes['tuberculosis_dot']).toBe(false);
    expect(nodes['tuberculosis_treatment_review']).toBe(false);
    expect(nodes['tuberculosis_symptom_review']).toBe(false);
    expect(nodes['tuberculosis_health_education']).toBe(false);
    expect(nodes['tuberculosis_follow_up']).toBe(false);
    expect(nodes['tuberculosis_referral']).toBe(false);
  });
});
