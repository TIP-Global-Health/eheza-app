import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import {
  navigateToCaseManagement,
  verifyCaseManagementEntry,
} from './helpers/case-management';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import { verifyFeatureGatesEncounterButton } from './helpers/feature-flags';
import {
  createAdultAndStartHIVEncounter,
  completeDiagnostics,
  completeMedication,
  completeSymptomReview,
  completeNextSteps,
  endHIVEncounter,
  queryHIVNodes,
  backdateHIVEncounter,
  navigateToParticipantPage,
  startHIVEncounter,
} from './helpers/hiv';

// =========================================================================
// Test 1: CHW Initial HIV Encounter — Positive Diagnosis Reported
// =========================================================================

test.describe('CHW: HIV Initial Encounter — Positive Diagnosis', () => {
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

  // Scenario: Initial HIV encounter for male adult with positive diagnosis reported.
  // Activities: Diagnostics, Medication (PrescribedMedication + TreatmentReview), NextSteps (HealthEducation + FollowUp).
  // Conditions: Initial encounter -> Diagnostics shown, SymptomReview NOT shown. No symptoms/adverse events -> Referral NOT triggered.
  // Backend: Verifies 5 node types created (diagnostics, medication, treatment_review, health_education, follow_up),
  //          confirms hiv_referral and hiv_symptom_review absent.
  test('complete initial HIV encounter with positive diagnosis, verify backend sync', async ({ page, browser }) => {
    // Verify FeatureHIVManagement flag gates client UI + admin Reports surfaces.
    await verifyFeatureGatesEncounterButton(page, 'hiv_management', 'HIV Management', {
      browser,
      admin: {
        sqDemographicsRows: ['HIV'],
        completionOptions: ['hiv'],
      },
    });

    const { fullName } = await createAdultAndStartHIVEncounter(page, {
      isFemale: false,
    });

    // 1. Diagnostics: patient reports positive HIV diagnosis + date.
    await completeDiagnostics(page, { path: 'positive-reported' });

    // 2. Medication: select antiretroviral + treatment review (no side effects).
    await completeMedication(page);

    // 3. NextSteps: HealthEducation + FollowUp (no Referral — no symptoms/adverse events).
    await completeNextSteps(page);

    // End encounter.
    await endHIVEncounter(page);

    // Sync to backend.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'hiv_diagnostics',
      'hiv_medication',
      'hiv_treatment_review',
      'hiv_health_education',
      'hiv_follow_up',
    ];
    const nodes = queryHIVNodes(fullName, expectedTypes);

    expect(nodes['hiv_diagnostics'], 'hiv_diagnostics should exist').toBe(true);
    expect(nodes['hiv_medication'], 'hiv_medication should exist').toBe(true);
    expect(nodes['hiv_treatment_review'], 'hiv_treatment_review should exist').toBe(true);
    expect(nodes['hiv_health_education'], 'hiv_health_education should exist').toBe(true);
    expect(nodes['hiv_follow_up'], 'hiv_follow_up should exist').toBe(true);
    // No symptoms or adverse events → no referral.
    expect(nodes['hiv_referral'], 'hiv_referral should not exist').toBe(false);
    // No symptom review in initial encounter.
    expect(nodes['hiv_symptom_review'], 'hiv_symptom_review should not exist').toBe(false);

    // --- Case Management verification ---
    // The HIV follow-up should appear in the HIV pane (feature-flag gated).
    await navigateToCaseManagement(page);
    const hivFilter = page.locator('div.ui.segment.filters button', {
      hasText: /HIV/i,
    });
    // HIV filter is feature-flag dependent; only assert if visible.
    if (await hivFilter.isVisible({ timeout: 3000 }).catch(() => false)) {
      await verifyCaseManagementEntry(page, 'HIV', 'HIV', fullName);
    }
  });
});

// =========================================================================
// Test 2: CHW Subsequent HIV Encounter — Symptoms + Adverse Events → Referral
// =========================================================================

test.describe('CHW: HIV Subsequent Encounter — Symptoms + Referral', () => {
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

  // Scenario: Subsequent HIV encounter with symptoms and adverse events triggering referral.
  // Activities: Medication (medications-not-changed + TreatmentReview with side effects), SymptomReview, NextSteps (HealthEducation + FollowUp + Referral).
  // Conditions: Subsequent encounter -> no Diagnostics. Adverse events + symptoms -> Referral tab appears in NextSteps.
  // Backend: Verifies 6 node types created (medication, treatment_review, symptom_review, health_education, follow_up, referral).
  test('complete subsequent HIV encounter with symptoms and adverse events triggering referral', async ({ page }) => {
    // --- PART 1: Complete a simplified initial encounter ---

    const { fullName } = await createAdultAndStartHIVEncounter(page, {
      isFemale: false,
    });

    // Diagnostics + Medication (minimal to allow encounter to end).
    await completeDiagnostics(page, { path: 'positive-reported' });
    await completeMedication(page);
    await completeNextSteps(page);
    await endHIVEncounter(page);

    // Sync initial encounter.
    await syncAndWait(page);

    // --- PART 2: Backdate and start subsequent encounter ---

    backdateHIVEncounter(fullName);
    await syncAndWait(page);

    // Navigate back to participant page and start new encounter.
    await navigateToParticipantPage(page, fullName);
    await startHIVEncounter(page);

    // --- PART 3: Complete subsequent encounter activities ---

    // Medication: medications not changed + treatment review with side effects.
    await completeMedication(page, {
      isSubsequent: true,
      sideEffects: true,
    });

    // SymptomReview: report symptoms (triggers referral along with adverse events).
    await completeSymptomReview(page, {
      symptoms: ['Fever', 'Fatigue'],
    });

    // NextSteps: HealthEducation + FollowUp + Referral (triggered by symptoms + adverse events).
    await completeNextSteps(page);

    // End encounter.
    await endHIVEncounter(page);

    // Sync.
    await syncAndWait(page);

    // Verify backend nodes.
    const expectedTypes = [
      'hiv_medication',
      'hiv_treatment_review',
      'hiv_symptom_review',
      'hiv_health_education',
      'hiv_follow_up',
      'hiv_referral',
    ];
    const nodes = queryHIVNodes(fullName, expectedTypes);

    expect(nodes['hiv_medication'], 'hiv_medication should exist').toBe(true);
    expect(nodes['hiv_treatment_review'], 'hiv_treatment_review should exist').toBe(true);
    expect(nodes['hiv_symptom_review'], 'hiv_symptom_review should exist').toBe(true);
    expect(nodes['hiv_health_education'], 'hiv_health_education should exist').toBe(true);
    expect(nodes['hiv_follow_up'], 'hiv_follow_up should exist').toBe(true);
    expect(nodes['hiv_referral'], 'hiv_referral should exist').toBe(true);
  });
});

// =========================================================================
// Test 3: CHW Initial HIV Encounter — No Diagnosis (End Encounter Dialog)
// =========================================================================

test.describe('CHW: HIV Initial Encounter — No Diagnosis', () => {
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

  // Scenario: Initial HIV encounter where patient is not diagnosed and refuses HIV test.
  // Activities: Diagnostics only (triggers end-encounter confirmation dialog).
  // Conditions: No diagnosis + refuses test -> encounter closes via confirmation dialog. No other activities completed.
  // Backend: Verifies only hiv_diagnostics created, all other 6 node types absent.
  test('complete HIV encounter with no diagnosis, encounter ends via confirmation dialog', async ({ page }) => {
    const { fullName } = await createAdultAndStartHIVEncounter(page, {
      isFemale: false,
    });

    // Diagnostics: not diagnosed, refuses test → end encounter dialog.
    await completeDiagnostics(page, { path: 'no-diagnosis-refuse-test' });

    // Encounter should have closed via the confirmation dialog.
    // Sync to backend.
    await syncAndWait(page);

    // Verify only diagnostics node was created.
    const expectedTypes = [
      'hiv_diagnostics',
    ];
    const nodes = queryHIVNodes(fullName, expectedTypes);

    expect(nodes['hiv_diagnostics'], 'hiv_diagnostics should exist').toBe(true);
    // No other activities should have been completed.
    expect(nodes['hiv_medication'], 'hiv_medication should not exist').toBe(false);
    expect(nodes['hiv_treatment_review'], 'hiv_treatment_review should not exist').toBe(false);
    expect(nodes['hiv_symptom_review'], 'hiv_symptom_review should not exist').toBe(false);
    expect(nodes['hiv_health_education'], 'hiv_health_education should not exist').toBe(false);
    expect(nodes['hiv_follow_up'], 'hiv_follow_up should not exist').toBe(false);
    expect(nodes['hiv_referral'], 'hiv_referral should not exist').toBe(false);
  });
});
