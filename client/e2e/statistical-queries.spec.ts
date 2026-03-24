import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { getClientPort } from './helpers/client-port';
import { resetDevice } from './helpers/device';
import { installCursorScript } from './helpers/cursor';
import {
  clearAdvancedQueue,
  drupalLogin,
  generateBaseReportsData,
  processAdvancedQueue,
  recalculateLargeDatasets,
  navigateToHCReportsPage,
  navigateToReportsMenu,
  selectReportType,
  setDateRange,
  readRegisteredPatientsTable,
  readImpactedPatientsTable,
  readEncountersTable,
  findRow,
  findEncounterRow,
  getDdevUrl,
  goToDashboard,
  readAcuteIllnessTable,
  readAIDiagnosisRow,
  readPrenatalDiagnosisRow,
  readPrenatalVisitsTable,
  findPrenatalRow,
  PrenatalVisitsRow,
  findSimpleRow,
  readNutritionTable,
  readNutritionColumnHeaders,
  findNutritionMetric,
  backdateNutritionEncounter,
  NUTRITION_ONE_VISIT_TABLES,
  NutritionMetricRow,
  PatientsTableData,
  EncountersTableData,
  SimpleTableData,
} from './helpers/reports';

// Encounter creation helpers.
import { syncAndWait } from './helpers/common';
import {
  createChildAndStartEncounter as createNutritionChild,
  enterHeight,
  enterWeight,
  enterMuac,
  enterNutritionSigns,
  saveActivity,
} from './helpers/nutrition';
import { createChildAndStartWellChildEncounter } from './helpers/well-child';
import {
  createAdultFemaleAndStartEncounter as createPrenatalAdult,
  completePregnancyDating,
  completeHistory,
  completeExamination,
  completeFamilyPlanning,
  completeDangerSigns as completePrenatalDangerSigns,
  completeSymptomReview,
  completeMalariaPrevention,
  completeMentalHealth,
  completeImmunisation,
  completeMedication,
  completeLaboratoryNurse,
  completeNextSteps as completePrenatalNextSteps,
  endPrenatalEncounter,
} from './helpers/prenatal';
import {
  createAdultAndStartEncounter as createAIAdult,
  completeDangerSigns as completeAIDangerSigns,
  completeSymptoms as completeAISymptoms,
  completePhysicalExam as completeAIPhysicalExam,
  completeLaboratory as completeAILaboratory,
  completePriorTreatment as completeAIPriorTreatment,
} from './helpers/acute-illness';
import { createAdultAndStartNCDEncounter } from './helpers/ncd';
import { createAdultAndStartHIVEncounter } from './helpers/hiv';
import { createAdultAndStartTBEncounter } from './helpers/tuberculosis';
import { createChildAndStartEncounter as createChildScoreboardChild } from './helpers/child-scoreboard';
import { startHomeVisit } from './helpers/home-visit';
import {
  navigateToNurseGroupSession,
  createMotherOnAttendancePage,
  addChildToMother,
  navigateBackToAttendance,
  goToParticipantsPage,
  clickMotherCard,
  navigateToChild,
  completeHeight,
  completeWeight,
} from './helpers/group-session';

const pwaBaseUrl = `http://localhost:${getClientPort()}`;

// Nyange Health Center node ID (from migration CSV).
const NYANGE_HC_ID = 4;

// Start date for report filtering — early enough to include all data.
const REPORT_START_DATE = new Date(2018, 0, 1);

test.describe('Statistical Queries — Demographics Report', () => {
  test.describe.configure({ timeout: 600000 });

  test.beforeEach(async ({ page }) => {
    if (process.env.RECORD) {
      await page.addInitScript(installCursorScript());
    }
  });

  // Scenario: Full pipeline — record baselines from existing demo data, create
  // encounters for every encounter type (nurse + CHW), then verify that the
  // Demographics report tables show the correct deltas.
  //
  // Patients created:
  //   Nurse: NutrChild (M 10mo) — Nutrition (with measurements) + SPV (2 encounters, impacted)
  //          PrenatalMom (F 25y) — Prenatal + AI (2 encounters, impacted)
  //          NCDAdult (M 40y) — NCD
  //          FBF group session (no new patient)
  //   CHW:   PrenatalCHW (F 28y) — Prenatal
  //          AICHW (F 26y) — Acute Illness
  //          HIVAdult (F 35y) — HIV
  //          TBAdult (M 45y) — Tuberculosis
  //          CSChild (M 6mo) — Child Scoreboard
  //
  // Expected Registered Patients deltas:
  //   1M-2Y: male +2 (NutrChild, CSChild)
  //   20Y-50Y: male +2 (NCDAdult, TBAdult), female +4 (PrenatalMom, PrenatalCHW, AICHW, HIVAdult)
  //   Total: +9
  //
  // Expected Impacted Patients deltas:
  //   1M-2Y: male +1 (NutrChild — 2 encounters)
  //   20Y-50Y: female +1 (PrenatalMom — 2 encounters)
  //   Total: +2
  //
  // Expected Encounters deltas (All column, +1 each unless noted):
  //   ANC Total +2 (HC +1, CHW +1), AI Total +2 (HC +1, CHW +1),
  //   SPV +1, Home Visit +1, Child Scoreboard +1, NCD +1, HIV +1, TB +1,
  //   Nutrition Total +2 (Individual +1, FBF +1)
  test('Demographics report reflects new patients and encounters', async ({ page }) => {
    const reportLimitDate = new Date();

    // ── Phase 0: Generate base reports data + record baselines ──

    await test.step('Generate base reports data from existing demo persons', async () => {
      generateBaseReportsData();
      clearAdvancedQueue();
      recalculateLargeDatasets();
    });

    let baselineRegistered: PatientsTableData;
    let baselineImpacted: PatientsTableData;
    let baselineEncounters: EncountersTableData;
    let baselineMalaria: number;
    let baselineResp: number;
    let baselineAITotal: number;
    let baselineAllPregnancies: PrenatalVisitsRow[];
    let baselineActivePregnancies: PrenatalVisitsRow[];
    let baselinePrenatalHIV: number;
    let baselineGestHypertension: number;
    let baselineDepression: number;
    let baselinePrenatalTotal: number;
    let baselineNutrition: Map<number, NutritionMetricRow[]>;
    let nutrChildName: string;
    let hvChildName: string;

    await test.step('Login to Drupal admin and record baseline values', async () => {
      await drupalLogin(page);
      await navigateToHCReportsPage(page, NYANGE_HC_ID);
      await selectReportType(page, 'demographics');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      baselineRegistered = await readRegisteredPatientsTable(page);
      baselineImpacted = await readImpactedPatientsTable(page);
      baselineEncounters = await readEncountersTable(page);

      // Record AI report baseline by CSS class.
      await selectReportType(page, 'acute-illness');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineMalaria = await readAIDiagnosisRow(page, 'diagnosis-malaria-uncomplicated');
      baselineResp = await readAIDiagnosisRow(page, 'diagnosis-respiratory-complicated');
      baselineAITotal = await readAIDiagnosisRow(page, 'totals');

      // Record Prenatal (ANC) report baseline.
      await selectReportType(page, 'prenatal');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineAllPregnancies = await readPrenatalVisitsTable(page, 'all-pregnancies');
      baselineActivePregnancies = await readPrenatalVisitsTable(page, 'active-pregnancies');

      // Record Prenatal Diagnoses report baseline by CSS class.
      await selectReportType(page, 'prenatal-diagnoses');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselinePrenatalHIV = await readPrenatalDiagnosisRow(page, 'diagnosis-hiv');
      baselineGestHypertension = await readPrenatalDiagnosisRow(page, 'diagnosis-gestational-hypertension');
      baselineDepression = await readPrenatalDiagnosisRow(page, 'diagnosis-depression-not-likely');
      baselinePrenatalTotal = await readPrenatalDiagnosisRow(page, 'totals');

      // Record Nutrition report baseline (first column = newest completed month).
      // The nutrition report only shows completed months, so test encounters
      // will be backdated to the previous month after syncing.
      await selectReportType(page, 'nutrition');
      await page.waitForTimeout(2000);
      baselineNutrition = new Map();
      for (const { index } of NUTRITION_ONE_VISIT_TABLES) {
        baselineNutrition.set(index, await readNutritionTable(page, index));
      }

      console.log('Baseline registered total:', baselineRegistered.total);
      console.log('Baseline impacted total:', baselineImpacted.total);
      console.log('Baseline encounters rows:', baselineEncounters.rows.length);
      console.log('Baseline AI: malaria=%d, resp=%d, total=%d', baselineMalaria, baselineResp, baselineAITotal);
      const baseAllTotal = findPrenatalRow(baselineAllPregnancies, 'Total');
      const baseActiveTotal = findPrenatalRow(baselineActivePregnancies, 'Total');
      console.log('Baseline ANC All Total: HC=%d, All=%d', baseAllTotal?.hc ?? 0, baseAllTotal?.all ?? 0);
      console.log('Baseline ANC Active Total: HC=%d, All=%d', baseActiveTotal?.hc ?? 0, baseActiveTotal?.all ?? 0);
      console.log('Baseline Prenatal Dx: hiv=%d, total=%d', baselinePrenatalHIV, baselinePrenatalTotal);
    });

    // ── Phase 1a: Nurse encounters ──

    await test.step('Login as nurse and create encounters', async () => {
      resetDevice();
      await page.goto(pwaBaseUrl);
      await setupDevice(page, '1234', 'Nyange Health Center');

      // --- NutrChild (male, 10 months): Nutrition encounter ---
      // Complete mandatory activities with abnormal measurements to trigger
      // stunting severe (height 60cm at 10mo) + underweight severe (6.0kg at 10mo).
      const nutrChild = await createNutritionChild(page, { ageMonths: 10 });
      nutrChildName = nutrChild.fullName;
      await enterHeight(page, '60');
      await saveActivity(page);
      await enterWeight(page, '6.0');
      await saveActivity(page);
      await enterMuac(page, '11.0');
      await saveActivity(page);
      // Nutrition signs is the last mandatory activity. With abnormal z-scores,
      // saving it triggers a diagnosis popup then NextSteps (Contributing
      // Factors, etc.) instead of returning to the encounter page.
      // Measurements are already persisted, so we dismiss the popup and
      // navigate to the dashboard.
      await enterNutritionSigns(page, ['None']);
      await click(page.locator('button.ui.fluid.primary.button.active'), page);
      const nursePopup = page.locator('div.ui.active.modal.diagnosis-popup');
      try {
        await nursePopup.waitFor({ timeout: 3000 });
        await click(nursePopup.locator('button.ui.primary.fluid.button'), page);
      } catch { /* no popup */ }
      await page.waitForTimeout(2000);
      await goToDashboard(page);
      console.log('Created NutrChild with measurements:', nutrChild.fullName);

      // --- NutrChild: Well Child (SPV) encounter on the SAME child ---
      // Navigate from dashboard: Clinical → Individual → Well Child → search
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await click(page.locator('.icon-task-clinical'), page);
      await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
      await click(page.locator('button.individual-assessment'), page);
      await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });
      await click(
        page.locator('button.encounter-type', { hasText: /Well Child|Standard Pediatric/i }),
        page,
      );
      await page.locator('div.page-participants').waitFor({ timeout: 10000 });

      // Search for the child we just created.
      const spvSearchInput = page.getByPlaceholder('Enter participant name here');
      await spvSearchInput.waitFor({ timeout: 5000 });
      await spvSearchInput.fill(nutrChild.firstName);
      await page.waitForTimeout(1000);

      // Click the child in the search results.
      const spvResult = page.locator('.item.participant-view', {
        hasText: nutrChild.firstName,
      }).first();
      await spvResult.waitFor({ timeout: 10000 });
      await click(spvResult.locator('.action-icon.forward'), page);
      await page.locator('div.page-participant.individual.well-child').waitFor({ timeout: 10000 });

      // Start SPV encounter.
      const spvBtn = page.locator('div.ui.primary.button', {
        hasText: /Standard Pediatric Visit Encounter/i,
      });
      await click(spvBtn, page);
      await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
      await goToDashboard(page);
      console.log('Created SPV encounter for NutrChild');

      // --- PrenatalMom (female, 25 years): Prenatal encounter with HIV diagnosis ---
      // Complete all mandatory activities so the encounter can be ended
      // and the HIV diagnosis is persisted to the encounter node.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const prenatalMom = await createPrenatalAdult(page, { ageYears: 25 });
      const lmpDate = new Date();
      lmpDate.setDate(lmpDate.getDate() - 30 * 7); // ~30 weeks ago
      await completePregnancyDating(page, lmpDate);
      await completeHistory(page);
      await completeExamination(page, { vitals: { sys: '160', dia: '100' } });
      await completeFamilyPlanning(page);
      await completePrenatalDangerSigns(page);
      await completeSymptomReview(page);
      await completeMalariaPrevention(page);
      await completeMentalHealth(page);
      await completeImmunisation(page);
      await completeMedication(page, { preferIronFolate: true });
      await completeLaboratoryNurse(page, { hivPositive: true });
      await completePrenatalNextSteps(page);
      await endPrenatalEncounter(page);
      console.log('Created PrenatalMom (HIV diagnosis):', prenatalMom.fullName);

      // --- AINurse (female, 30 years): Acute Illness with malaria diagnosis ---
      // Create a separate patient (not PrenatalMom) so we get a proper initial
      // encounter with all nurse activities including DangerSigns + Laboratory.
      await goToDashboard(page);
      const aiNurse = await createAIAdult(page, { gender: 'female', ageYears: 30 });
      // Complete activities → Uncomplicated Malaria diagnosis.
      // Nurse initial AI: Symptoms → PhysicalExam → PriorTreatment → Laboratory (appears after first 3).
      await completeAISymptoms(page);
      await completeAIPhysicalExam(page);
      await completeAIPriorTreatment(page);
      await completeAILaboratory(page);
      await goToDashboard(page);
      console.log('Created AINurse (Uncomplicated Malaria):', aiNurse.fullName);

      // --- NCDAdult (male, 40 years): NCD encounter ---
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const ncdAdult = await createAdultAndStartNCDEncounter(page, {
        isFemale: false,
        ageYears: 40,
      });
      await goToDashboard(page);
      console.log('Created NCDAdult:', ncdAdult.fullName);

      // --- FBF Group Session ---
      // For FBF to count in reports, the child needs height + weight
      // measurements (which produce z-scores). Register mother + child,
      // navigate to child, complete height + weight.
      await goToDashboard(page);
      await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');
      const fbfMother = await createMotherOnAttendancePage(page);
      const fbfChild = await addChildToMother(page, { ageMonths: 12 });

      // After addChildToMother we may be on PersonPage or elsewhere.
      // Navigate back to attendance, then to participants.
      if (await page.locator('div.page-person').isVisible({ timeout: 1000 }).catch(() => false)) {
        await navigateBackToAttendance(page);
      }
      // If we overshot past attendance, re-navigate from dashboard.
      if (!(await page.locator('div.page-attendance').isVisible({ timeout: 2000 }).catch(() => false))) {
        await goToDashboard(page);
        await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');
      }
      await goToParticipantsPage(page);
      await clickMotherCard(page, fbfMother.firstName);
      await navigateToChild(page);
      await completeHeight(page, '70');
      await completeWeight(page, '8.5');
      console.log('Created FBF session with child measurements:', fbfChild.fullName);
    });

    await test.step('Sync nurse data to backend', async () => {
      // Navigate to dashboard first (may be on attendance page from FBF session).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1b: CHW encounters ──

    await test.step('Login as CHW and create encounters', async () => {
      // Log out from nurse and log in as CHW — same device, no re-pairing needed.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await click(page.locator('button.ui.button.logout'), page);
      await page.locator('input[name="pincode"]').waitFor({ timeout: 10000 });

      // Login as CHW.
      const pinInput = page.locator('input[name="pincode"]');
      await pinInput.fill('2345');
      await click(page.getByRole('button', { name: 'Sign In' }), page);
      await page.waitForTimeout(3000);

      // Select village.
      const selectLocation = await page.locator('p.select-location').isVisible().catch(() => false);
      if (selectLocation) {
        await click(page.locator('button.ui.primary.button', { hasText: 'Akanduga' }), page);
      }
      await page.locator('.wrap-cards').waitFor({ timeout: 30000 });

      // --- PrenatalCHW (female, 28 years) ---
      const prenatalCHW = await createPrenatalAdult(page, {
        isChw: true,
        ageYears: 28,
      });
      await goToDashboard(page);
      console.log('Created PrenatalCHW:', prenatalCHW.fullName);

      // --- AICHW (female, 26 years) ---
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const aiCHW = await createAIAdult(page, {
        isChw: true,
        gender: 'female',
        ageYears: 26,
      });
      // Complete activities → Uncomplicated Pneumonia diagnosis.
      await completeAISymptoms(page, {
        general: ['Headache'],
        respiratory: ['Cough', 'Nasal Congestion', 'Sore Throat'],
        gi: [],
      });
      await completeAIPhysicalExam(page, {
        isChw: true,
        respiratoryRate: '32',
        bodyTemp: '37.0',
      });
      await completeAIPriorTreatment(page);
      await goToDashboard(page);
      console.log('Created AICHW (Uncomplicated Pneumonia):', aiCHW.fullName);

      // --- HIVAdult (female, 35 years) ---
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const hivAdult = await createAdultAndStartHIVEncounter(page, {
        isFemale: true,
        ageYears: 35,
      });
      await goToDashboard(page);
      console.log('Created HIVAdult:', hivAdult.fullName);

      // --- TBAdult (male, 45 years) ---
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const tbAdult = await createAdultAndStartTBEncounter(page, {
        isFemale: false,
        ageYears: 45,
      });
      await goToDashboard(page);
      console.log('Created TBAdult:', tbAdult.fullName);

      // --- CSChild (male, 6 months): Child Scoreboard ---
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const csChild = await createChildScoreboardChild(page, { ageMonths: 6 });
      await goToDashboard(page);
      console.log('Created CSChild:', csChild.fullName);

      // --- HVChild (male, 8 months): Home Visit ---
      // Create child via nutrition helper (starts encounter), navigate back
      // to participant page (don't end encounter), then start home visit.
      await goToDashboard(page);
      const hvChild = await createNutritionChild(page, {
        ageMonths: 8,
        isChw: true,
      });
      hvChildName = hvChild.fullName;
      // Complete CHW mandatory nutrition activities (no height for Rwanda CHW).
      // Underweight severe: 5.5kg at 8mo (median ~8.6kg, -3SD ~6.2kg).
      await enterWeight(page, '5.5');
      await saveActivity(page);
      await enterMuac(page, '11.0');
      await saveActivity(page);
      // Nutrition signs is the last mandatory activity. Abnormal z-scores
      // trigger a diagnosis popup then NextSteps, so we save, dismiss the
      // popup, and navigate to dashboard instead.
      await enterNutritionSigns(page, ['None']);
      await click(page.locator('button.ui.fluid.primary.button.active'), page);
      const chwPopup = page.locator('div.ui.active.modal.diagnosis-popup');
      try {
        await chwPopup.waitFor({ timeout: 3000 });
        await click(chwPopup.locator('button.ui.primary.fluid.button'), page);
      } catch { /* no popup */ }
      await page.waitForTimeout(2000);
      // Navigate back to participant page for HVChild to start home visit.
      // From NextSteps or encounter page, go to dashboard then re-navigate.
      await goToDashboard(page);
      // Re-find HVChild via nutrition participant flow.
      await click(page.locator('.icon-task-clinical'), page);
      await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
      await click(page.locator('button.individual-assessment'), page);
      await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });
      await click(
        page.locator('button.encounter-type', { hasText: 'Child Nutrition' }),
        page,
      );
      await page.locator('div.page-participants').waitFor({ timeout: 10000 });
      const hvSearch = page.getByPlaceholder('Enter participant name here');
      await hvSearch.waitFor({ timeout: 5000 });
      await hvSearch.fill(hvChild.firstName);
      await page.waitForTimeout(1000);
      const hvResult = page.locator('.item.participant-view', {
        hasText: hvChild.firstName,
      }).first();
      await hvResult.waitFor({ timeout: 10000 });
      await click(hvResult.locator('.action-icon.forward'), page);
      await page.locator('div.page-participant.individual.nutrition').waitFor({ timeout: 10000 });
      // Start home visit from participant page.
      await startHomeVisit(page);
      console.log('Created HVChild + Home Visit:', hvChild.fullName);
    });

    await test.step('Sync CHW data to backend', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1c: Backdate nutrition encounters to previous month ──
    // The nutrition report only shows completed months. Backdating makes
    // our test data appear in the first (newest) column.

    await test.step('Backdate nutrition encounters to previous month', async () => {
      const lastMonth = new Date();
      lastMonth.setMonth(lastMonth.getMonth() - 1);
      backdateNutritionEncounter(nutrChildName, lastMonth);
      backdateNutritionEncounter(hvChildName, lastMonth);
    });

    // ── Phase 2: Process AQ + re-aggregate ──

    await test.step('Process Advanced Queue and recalculate large datasets', async () => {
      // AQ runs once, after all content is generated (including backdating).
      processAdvancedQueue();
      recalculateLargeDatasets();
    });

    // ── Phase 3: Verify Demographics deltas — HC scope ──

    await test.step('Navigate to updated Demographics report', async () => {
      await navigateToHCReportsPage(page, NYANGE_HC_ID);
      await selectReportType(page, 'demographics');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
    });

    await test.step('Verify Registered Patients table deltas', async () => {
      const newRegistered = await readRegisteredPatientsTable(page);

      // Log all rows: baseline → new (expected delta)
      console.log('\n=== REGISTERED PATIENTS ===');
      console.log('Row              | Baseline M/F | New M/F    | Expected delta');
      for (const row of newRegistered.rows) {
        const base = findRow(baselineRegistered, row.label);
        const bm = base?.male ?? 0;
        const bf = base?.female ?? 0;
        console.log(
          `${row.label.padEnd(16)} | ${String(bm).padEnd(5)}/${String(bf).padEnd(5)} | ${String(row.male).padEnd(5)}/${String(row.female).padEnd(5)} | +${row.male - bm}/+${row.female - bf}`,
        );
      }
      console.log(`Total: baseline=${baselineRegistered.total}, new=${newRegistered.total}, delta=+${newRegistered.total - baselineRegistered.total}`);

      // Row "1M - 2Y": male +4 (NutrChild, CSChild, HVChild, FBFChild)
      const row1M2Y = findRow(newRegistered, '1M - 2Y')!;
      const base1M2Y = findRow(baselineRegistered, '1M - 2Y')!;
      expect(row1M2Y.male, '1M-2Y male should increase by 4').toBe(base1M2Y.male + 4);
      expect(row1M2Y.female, '1M-2Y female should be unchanged').toBe(base1M2Y.female);

      // Row "20Y - 50Y": male +2 (NCDAdult, TBAdult), female +6 (PrenatalMom, AINurse, FBFMother, PrenatalCHW, AICHW, HIVAdult)
      const row20Y50Y = findRow(newRegistered, '20Y - 50Y')!;
      const base20Y50Y = findRow(baselineRegistered, '20Y - 50Y')!;
      expect(row20Y50Y.male, '20Y-50Y male should increase by 2').toBe(base20Y50Y.male + 2);
      expect(row20Y50Y.female, '20Y-50Y female should increase by 6').toBe(base20Y50Y.female + 6);

      // Total: +12 (6 nurse patients + 6 CHW patients)
      expect(newRegistered.total, 'Registered total should increase by 12').toBe(
        baselineRegistered.total + 12,
      );

      // Other rows should be unchanged.
      for (const label of ['0 - 1M', '2Y - 5Y', '5Y - 10Y', '10Y - 20Y', '50Y +']) {
        const newRow = findRow(newRegistered, label)!;
        const baseRow = findRow(baselineRegistered, label)!;
        expect(newRow.male, `${label} male should be unchanged`).toBe(baseRow.male);
        expect(newRow.female, `${label} female should be unchanged`).toBe(baseRow.female);
      }
    });

    await test.step('Verify Impacted Patients table deltas', async () => {
      const newImpacted = await readImpactedPatientsTable(page);

      console.log('\n=== IMPACTED PATIENTS ===');
      console.log('Row              | Baseline M/F | New M/F    | Expected delta');
      for (const row of newImpacted.rows) {
        const base = findRow(baselineImpacted, row.label);
        const bm = base?.male ?? 0;
        const bf = base?.female ?? 0;
        console.log(
          `${row.label.padEnd(16)} | ${String(bm).padEnd(5)}/${String(bf).padEnd(5)} | ${String(row.male).padEnd(5)}/${String(row.female).padEnd(5)} | +${row.male - bm}/+${row.female - bf}`,
        );
      }
      console.log(`Total: baseline=${baselineImpacted.total}, new=${newImpacted.total}, delta=+${newImpacted.total - baselineImpacted.total}`);

      // Row "1M - 2Y": male +2 (NutrChild: Nutrition+SPV, HVChild: Nutrition+HomeVisit)
      const imp1M2Y = findRow(newImpacted, '1M - 2Y')!;
      const baseImp1M2Y = findRow(baselineImpacted, '1M - 2Y')!;
      expect(imp1M2Y.male, 'Impacted 1M-2Y male should increase by 2').toBe(
        baseImp1M2Y.male + 2,
      );

      // Row "20Y - 50Y": unchanged (PrenatalMom now has only 1 encounter)
      const imp20Y50Y = findRow(newImpacted, '20Y - 50Y')!;
      const baseImp20Y50Y = findRow(baselineImpacted, '20Y - 50Y')!;
      expect(imp20Y50Y.female, 'Impacted 20Y-50Y female should be unchanged').toBe(
        baseImp20Y50Y.female,
      );

      // Total: +2 (NutrChild + HVChild)
      expect(newImpacted.total, 'Impacted total should increase by 2').toBe(
        baselineImpacted.total + 2,
      );
    });

    await test.step('Verify Encounters table deltas', async () => {
      const newEnc = await readEncountersTable(page);

      console.log('\n=== ENCOUNTERS ===');
      console.log('Row                          | Baseline All | New All | Delta');
      for (const row of newEnc.rows) {
        const base = findEncounterRow(baselineEncounters, row.label);
        const ba = base?.all ?? 0;
        console.log(
          `${row.label.padEnd(28)} | ${String(ba).padEnd(12)} | ${String(row.all).padEnd(7)} | +${row.all - ba}`,
        );
      }

      const assertDelta = (label: string, expectedDelta: number) => {
        const newRow = findEncounterRow(newEnc, label);
        const baseRow = findEncounterRow(baselineEncounters, label);
        expect(newRow, `Encounter row "${label}" should exist`).toBeDefined();
        expect(baseRow, `Baseline row "${label}" should exist`).toBeDefined();
        expect(newRow!.all, `${label} All should increase by ${expectedDelta}`).toBe(
          baseRow!.all + expectedDelta,
        );
      };

      // Labels match Translate.elm: "ANC (total)", "Acute Illness (total)", etc.
      assertDelta('ANC (total)', 2);         // nurse +1, CHW +1
      assertDelta('Acute Illness (total)', 2); // nurse +1, CHW +1
      assertDelta('Standard Pediatric Visit', 1);
      assertDelta('Home Visit', 1);
      assertDelta('Child Scorecard', 1);
      assertDelta('NCD', 1);
      assertDelta('HIV', 1);
      assertDelta('Tuberculosis', 1);
      assertDelta('Nutrition (total)', 3);   // Individual +2 (NutrChild + HVChild) + FBF +1
      assertDelta('FBF', 1);                // FBF child with height + weight measurements
      assertDelta('Individual', 2);         // NutrChild + HVChild each have a nutrition encounter
    });

    await test.step('Verify CSV download button is visible', async () => {
      await expect(page.locator('button.download-csv')).toBeVisible();
    });

    // ── Acute Illness report verification ──

    await test.step('Verify Acute Illness report deltas', async () => {
      await selectReportType(page, 'acute-illness');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      // Read specific rows by CSS class — deterministic, no label ambiguity.
      const newMalaria = await readAIDiagnosisRow(page, 'diagnosis-malaria-uncomplicated');
      const newResp = await readAIDiagnosisRow(page, 'diagnosis-respiratory-complicated');
      const newTotal = await readAIDiagnosisRow(page, 'totals');

      console.log('\n=== ACUTE ILLNESS (by CSS class) ===');
      console.log(`Malaria Uncomplicated:  baseline=${baselineMalaria}, new=${newMalaria}, delta=+${newMalaria - baselineMalaria}`);
      console.log(`Respiratory Complicated: baseline=${baselineResp}, new=${newResp}, delta=+${newResp - baselineResp}`);
      console.log(`Total:                  baseline=${baselineAITotal}, new=${newTotal}, delta=+${newTotal - baselineAITotal}`);

      // "Uncomplicated Malaria": +1 (AINurse with RDT+)
      expect(newMalaria, 'Malaria Uncomplicated should increase by 1').toBe(
        baselineMalaria + 1,
      );

      // "Acute Respiratory Infection with Complications": +1 (AICHW respiratory symptoms)
      expect(newResp, 'Respiratory Complicated should increase by 1').toBe(
        baselineResp + 1,
      );

      // "Total": +2.
      // If this fails due to pre-existing demo data shifting, the root
      // cause is in the reports data generation pipeline, not in targeting.
      expect(newTotal, 'AI Total should increase by 2').toBe(
        baselineAITotal + 2,
      );

      // CSV download button.
      await expect(page.locator('button.download-csv')).toBeVisible();
    });

    // ── Prenatal (ANC) report verification ──

    await test.step('Verify Prenatal (ANC) report deltas', async () => {
      await selectReportType(page, 'prenatal');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      const newAll = await readPrenatalVisitsTable(page, 'all-pregnancies');
      const newActive = await readPrenatalVisitsTable(page, 'active-pregnancies');

      // PrenatalMom: 1 nurse encounter, active pregnancy.
      // All Pregnancies: 1 Visit HC +1, Total HC +1.
      const newAll1Visit = findPrenatalRow(newAll, '1 visit')!;
      const baseAll1Visit = findPrenatalRow(baselineAllPregnancies, '1 visit')!;
      const newAllTotal = findPrenatalRow(newAll, 'Total')!;
      const baseAllTotal2 = findPrenatalRow(baselineAllPregnancies, 'Total')!;

      console.log('\n=== PRENATAL (ANC) ===');
      console.log(`All 1 Visit HC: baseline=${baseAll1Visit.hc}, new=${newAll1Visit.hc}, delta=+${newAll1Visit.hc - baseAll1Visit.hc}`);
      console.log(`All Total HC:   baseline=${baseAllTotal2.hc}, new=${newAllTotal.hc}, delta=+${newAllTotal.hc - baseAllTotal2.hc}`);

      expect(newAll1Visit.hc, 'All Pregnancies 1 Visit HC +1').toBe(baseAll1Visit.hc + 1);
      expect(newAllTotal.hc, 'All Pregnancies Total HC +1').toBe(baseAllTotal2.hc + 1);

      // Active Pregnancies: 1 Visit HC +1, Total HC +1.
      const newActive1Visit = findPrenatalRow(newActive, '1 visit')!;
      const baseActive1Visit = findPrenatalRow(baselineActivePregnancies, '1 visit')!;
      const newActiveTotal = findPrenatalRow(newActive, 'Total')!;
      const baseActiveTotal2 = findPrenatalRow(baselineActivePregnancies, 'Total')!;

      console.log(`Active 1 Visit HC: baseline=${baseActive1Visit.hc}, new=${newActive1Visit.hc}, delta=+${newActive1Visit.hc - baseActive1Visit.hc}`);
      console.log(`Active Total HC:   baseline=${baseActiveTotal2.hc}, new=${newActiveTotal.hc}, delta=+${newActiveTotal.hc - baseActiveTotal2.hc}`);

      expect(newActive1Visit.hc, 'Active Pregnancies 1 Visit HC +1').toBe(baseActive1Visit.hc + 1);
      expect(newActiveTotal.hc, 'Active Pregnancies Total HC +1').toBe(baseActiveTotal2.hc + 1);

      // CSV download button.
      await expect(page.locator('button.download-csv')).toBeVisible();
    });

    // ── Prenatal Diagnoses report verification ──

    await test.step('Verify Prenatal Diagnoses report deltas', async () => {
      await selectReportType(page, 'prenatal-diagnoses');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      // Completing all nurse activities with sys=160/dia=100 + HIV+ generates
      // 4 diagnoses: HIV, Gestational Hypertension, Moderate Preeclampsia,
      // Depression Not Likely.
      const newHIV = await readPrenatalDiagnosisRow(page, 'diagnosis-hiv');
      const newGestHypertension = await readPrenatalDiagnosisRow(page, 'diagnosis-gestational-hypertension');
      const newDepression = await readPrenatalDiagnosisRow(page, 'diagnosis-depression-not-likely');
      const newPrenatalTotal = await readPrenatalDiagnosisRow(page, 'totals');

      console.log('\n=== PRENATAL DIAGNOSES (by CSS class) ===');
      console.log(`HIV:                    baseline=${baselinePrenatalHIV}, new=${newHIV}, delta=+${newHIV - baselinePrenatalHIV}`);
      console.log(`Gestational Hypert.:    baseline=${baselineGestHypertension}, new=${newGestHypertension}, delta=+${newGestHypertension - baselineGestHypertension}`);
      console.log(`Depression Not Likely:  baseline=${baselineDepression}, new=${newDepression}, delta=+${newDepression - baselineDepression}`);
      console.log(`Total:                  baseline=${baselinePrenatalTotal}, new=${newPrenatalTotal}, delta=+${newPrenatalTotal - baselinePrenatalTotal}`);

      expect(newHIV, 'Prenatal HIV +1').toBe(baselinePrenatalHIV + 1);
      expect(newGestHypertension, 'Gestational Hypertension +1').toBe(baselineGestHypertension + 1);
      expect(newDepression, 'Depression Not Likely +1').toBe(baselineDepression + 1);
      // Total: HIV + Gestational Hypertension + Depression Not Likely
      // + NoPrenatalDiagnosis (from CHW encounter with no activities) = +4.
      expect(newPrenatalTotal, 'Prenatal Total +4').toBe(baselinePrenatalTotal + 4);

      // CSV download button.
      await expect(page.locator('button.download-csv')).toBeVisible();
    });

    // ── Nutrition report verification ──

    await test.step('Verify Nutrition report deltas', async () => {
      await selectReportType(page, 'nutrition');
      // No date range — nutrition report always shows last 12 months.
      await page.waitForTimeout(2000);

      await expect(page.locator('div.report.nutrition')).toBeVisible();

      console.log('\n=== NUTRITION ===');

      // Nutrition encounters were backdated to the previous month.
      // Columns are reverse chronological — first column (index 0) is the
      // newest completed month where our test data should now appear.
      const columnHeaders = await readNutritionColumnHeaders(page, 0);
      console.log(`Columns (${columnHeaders.length}): ${columnHeaders.join(' | ')}`);

      // Verify all 4 "One Visit Or More" tables.
      for (const { index, name } of NUTRITION_ONE_VISIT_TABLES) {
        const baseline = baselineNutrition.get(index) ?? [];
        const current = await readNutritionTable(page, index);
        expect(current.length, `${name}: should have 6 metric rows`).toBe(6);

        const colCount = current[0]?.values.length ?? 0;
        expect(colCount, `${name}: should have data columns`).toBeGreaterThan(0);

        // First column = newest completed month (where backdated data lives).
        // NutrChild (height=60 at 10mo) → Stunting Severe.
        const baseStunting = findNutritionMetric(baseline, 'Stunting Severe')?.values[0] ?? 0;
        const newStunting = findNutritionMetric(current, 'Stunting Severe')?.values[0] ?? 0;
        console.log(`${name} — Stunting Severe: baseline=${baseStunting}%, new=${newStunting}%`);
        expect(newStunting, `${name}: Stunting Severe % should increase`)
          .toBeGreaterThanOrEqual(baseStunting);

        // NutrChild (weight=6.0 at 10mo) + HVChild (weight=5.5 at 8mo) → Underweight Severe.
        const baseUnderweight = findNutritionMetric(baseline, 'Underweight Severe')?.values[0] ?? 0;
        const newUnderweight = findNutritionMetric(current, 'Underweight Severe')?.values[0] ?? 0;
        console.log(`${name} — Underweight Severe: baseline=${baseUnderweight}%, new=${newUnderweight}%`);
        expect(newUnderweight, `${name}: Underweight Severe % should increase`)
          .toBeGreaterThanOrEqual(baseUnderweight);
      }

      // CSV download button.
      await expect(page.locator('button.download-csv')).toBeVisible();
    });

    // ── Phase 4: Demographics scope (Province) ──

    await test.step('Verify Demographics report at Province scope', async () => {
      await navigateToReportsMenu(page);

      // Select "Demographic Region" scope.
      const scopeSelect = page
        .locator('.page-content.reports-menu .select-input-wrapper')
        .first()
        .locator('select.select-input');
      await scopeSelect.selectOption('demographics');
      await page.waitForTimeout(500);

      // Select province (first option after blank).
      const provinceSelect = page
        .locator('.page-content.reports-menu .select-input-wrapper')
        .nth(1)
        .locator('select.select-input');
      await provinceSelect.waitFor({ timeout: 5000 });
      // Select "Amajyaruguru" — the province where Nyange HC is located.
      const options = provinceSelect.locator('option');
      const optionCount = await options.count();
      // Find and select the first non-empty option (should be Amajyaruguru).
      for (let i = 0; i < optionCount; i++) {
        const val = await options.nth(i).getAttribute('value');
        if (val && val.trim() !== '') {
          await provinceSelect.selectOption(val);
          break;
        }
      }
      await page.waitForTimeout(500);

      // Click "Load Data" button.
      const loadBtn = page.locator('.page-content.reports-menu div.actions a button');
      await loadBtn.click();

      // Wait for results page.
      await page.locator('.page-content.reports').waitFor({ timeout: 30000 });

      // Verify scope label contains province name.
      const scopeLabel = await page.locator('div.scope').textContent();
      expect(scopeLabel).toBeTruthy();

      // Select Demographics report type and date range.
      await selectReportType(page, 'demographics');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      // Verify report renders with data.
      await expect(page.locator('div.report.demographics')).toBeVisible();
      const provinceRegistered = await readRegisteredPatientsTable(page);
      expect(provinceRegistered.total, 'Province registered total should be > 0').toBeGreaterThan(0);
    });
  });
});
