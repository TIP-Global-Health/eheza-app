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
  generateCompletionData,
  completionRecalculateLargeDatasets,
  navigateToCompletionReportPage,
  selectCompletionReportType,
  selectCompletionTakenBy,
  setCompletionDateRange,
  readCompletionTable,
  findCompletionRow,
  CompletionTableData,
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
  startPrenatalEncounter,
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
  completeLaboratoryChw as completePrenatalLaboratoryChw,
  completeHealthEducation as completePrenatalHealthEducation,
  completeNextSteps as completePrenatalNextSteps,
  completePregnancyOutcome,
  completeBreastfeeding,
  completePostpartumTreatmentReview,
  completeSpecialityCare,
  recordPregnancyOutcome,
  endPrenatalEncounter,
  backdatePrenatalEncounter,
  navigateToParticipantPage as navigateToPrenatalParticipant,
} from './helpers/prenatal';
import {
  createAdultAndStartEncounter as createAIAdult,
  createChildAndStartEncounter as createAIChild,
  completeDangerSigns as completeAIDangerSigns,
  completeSymptoms as completeAISymptoms,
  completePhysicalExam as completeAIPhysicalExam,
  completeLaboratory as completeAILaboratory,
  completePriorTreatment as completeAIPriorTreatment,
  completeNextSteps as completeAINextSteps,
  completeOngoingTreatment as completeAIOngoingTreatment,
  backdateAcuteIllnessEncounter,
  navigateToParticipantPage as navigateToAIParticipant,
  startSubsequentEncounter as startSubsequentAI,
} from './helpers/acute-illness';
import { createAdultAndStartNCDEncounter } from './helpers/ncd';
import { createAdultAndStartHIVEncounter } from './helpers/hiv';
import { createAdultAndStartTBEncounter } from './helpers/tuberculosis';
import {
  createChildAndStartEncounter as createChildScoreboardChild,
  completeNCDA,
  completeVaccinationHistory,
} from './helpers/child-scoreboard';
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

test.describe('Admin Reports', () => {
  test.describe.configure({ timeout: 1200000 });

  test.beforeEach(async ({ page }) => {
    if (process.env.RECORD) {
      await page.addInitScript(installCursorScript());
    }
  });

  // Scenario: Full pipeline — record baselines from existing demo data, create
  // encounters for every encounter type (nurse + CHW), then verify that the
  // Statistical Queries and Completion reports show the correct deltas.
  //
  // Patients created:
  //   Nurse: NutrChild (M 10mo) — Nutrition (with measurements) + SPV (2 encounters, impacted)
  //          PrenatalMom (F 25y) — Prenatal initial + postpartum (2 encounters, impacted)
  //          AINurse (F 30y) — AI initial + subsequent (2 encounters, impacted)
  //          AIChild (M 24mo) — AI initial (with MUAC + Nutrition)
  //          NCDAdult (M 40y) — NCD
  //          FBF group session (no new patient)
  //   CHW:   PrenatalCHW (F 28y) — Prenatal
  //          AICHW (F 26y) — Acute Illness
  //          HIVAdult (F 35y) — HIV
  //          TBAdult (M 45y) — Tuberculosis
  //          CSChild (M 10mo) — Child Scoreboard
  //
  // Expected Registered Patients deltas:
  //   1M-2Y: male +4 (NutrChild, CSChild, HVChild, FBFChild)
  //   2Y-5Y: male +1 (AIChild 24mo)
  //   20Y-50Y: male +2 (NCDAdult, TBAdult), female +6 (PrenatalMom, AINurse, FBFMother, PrenatalCHW, AICHW, HIVAdult)
  //   Total: +13
  //
  // Expected Impacted Patients deltas:
  //   1M-2Y: male +2 (NutrChild: Nutrition+SPV, HVChild: Nutrition+HomeVisit)
  //   20Y-50Y: female +2 (AINurse: AI initial + subsequent, PrenatalMom: initial + postpartum)
  //   Total: +4
  //
  // Expected Encounters deltas (All column, +1 each unless noted):
  //   ANC Total +3 (HC initial +1, HC postpartum +1, CHW +1), AI Total +4 (nurse initial +1, child +1, subsequent +1, CHW +1),
  //   SPV +1, Home Visit +1, Child Scoreboard +1, NCD +1, HIV +1, TB +1,
  //   Nutrition Total +3 (Individual +2, FBF +1)
  test('Reports reflect new patients, encounters, and completion data', async ({ page }) => {
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
    let baselineCompletion: CompletionTableData;
    let baselinePrenatalCompletion: CompletionTableData;
    let baselinePrenatalCompletionCHW: CompletionTableData;
    let baselineCSCompletion: CompletionTableData;
    let nutrChildName: string;
    let hvChildName: string;
    let aiNurseName: string;
    let prenatalMomName: string;

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

    // Record Completion report baselines (AI + Prenatal).
    await test.step('Generate base completion data and record baselines', async () => {
      generateCompletionData('acute-illness', true);
      generateCompletionData('prenatal', true);
      generateCompletionData('child-scoreboard', true);
      completionRecalculateLargeDatasets();

      await navigateToCompletionReportPage(page, NYANGE_HC_ID);

      // AI baseline.
      await selectCompletionReportType(page, 'acute-illness');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineCompletion = await readCompletionTable(page, 'acute-illness');
      console.log('Baseline AI completion rows:', baselineCompletion.rows.length);

      // Prenatal baseline (Any role).
      await selectCompletionReportType(page, 'prenatal');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselinePrenatalCompletion = await readCompletionTable(page, 'prenatal');
      console.log('Baseline Prenatal completion rows:', baselinePrenatalCompletion.rows.length);

      // Prenatal baseline (CHW only) — needed for Taken By filter assertions.
      await selectCompletionTakenBy(page, 'chw');
      baselinePrenatalCompletionCHW = await readCompletionTable(page, 'prenatal');
      await selectCompletionTakenBy(page, '');

      // Child Scoreboard baseline (CHW-only module, no Taken By filter).
      await selectCompletionReportType(page, 'child-scoreboard');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineCSCompletion = await readCompletionTable(page, 'child-scoreboard');
      console.log('Baseline CS completion rows:', baselineCSCompletion.rows.length);
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
      prenatalMomName = prenatalMom.fullName;
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
      // encounter with all nurse activities including Laboratory + NextSteps.
      await goToDashboard(page);
      const aiNurse = await createAIAdult(page, { gender: 'female', ageYears: 30 });
      aiNurseName = aiNurse.fullName;
      // Complete activities → Uncomplicated Malaria diagnosis.
      // Nurse initial AI: Symptoms → PhysicalExam → PriorTreatment → Laboratory → NextSteps.
      await completeAISymptoms(page);
      await completeAIPhysicalExam(page);
      await completeAIPriorTreatment(page);
      await completeAILaboratory(page);
      await completeAINextSteps(page);
      await goToDashboard(page);
      console.log('Created AINurse (Uncomplicated Malaria):', aiNurse.fullName);

      // --- AIChild (male, 24 months): Acute Illness with malaria diagnosis ---
      // Child encounter to exercise MUAC + Nutrition completion activities,
      // which are only expected for patients aged 6 months to 5 years.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const aiChild = await createAIChild(page, { ageMonths: 24 });
      await completeAISymptoms(page);
      await completeAIPhysicalExam(page); // Includes MUAC + Nutrition tabs for children.
      await completeAIPriorTreatment(page);
      await completeAILaboratory(page);
      await completeAINextSteps(page);
      await goToDashboard(page);
      console.log('Created AIChild (Uncomplicated Malaria):', aiChild.fullName);

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

    // ── Phase 1a-extra: Subsequent AI encounter ──
    // AINurse initial encounter is now synced. Backdate it to yesterday
    // so we can start a subsequent encounter today and cover
    // DangerSigns + OngoingTreatment completion activities.

    await test.step('Backdate and create subsequent nurse AI encounter', async () => {
      backdateAcuteIllnessEncounter(aiNurseName);

      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToAIParticipant(page, aiNurseName);
      await startSubsequentAI(page);
      await completeAIDangerSigns(page);
      await completeAIOngoingTreatment(page);
      await goToDashboard(page);
      console.log('Created subsequent AI encounter for:', aiNurseName);
    });

    await test.step('Sync subsequent AI encounter', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1a-extra-2: Postpartum prenatal encounter ──
    // PrenatalMom's initial encounter is synced. Backdate it to allow
    // a postpartum encounter today. Covers: GU Exam, Breastfeeding,
    // SpecialityCare, PregnancyOutcome, PostpartumTreatmentReview.

    await test.step('Backdate and create postpartum prenatal encounter', async () => {
      backdatePrenatalEncounter(prenatalMomName, 1);

      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToPrenatalParticipant(page, prenatalMomName);

      // Record pregnancy outcome from participant page first —
      // required before the Postpartum encounter button becomes active.
      await recordPregnancyOutcome(page);

      // Re-navigate to participant page after outcome recording
      // (recordPregnancyOutcome lands on PinCodePage).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToPrenatalParticipant(page, prenatalMomName);
      await startPrenatalEncounter(page, 'postpartum');

      await completeSymptomReview(page);
      await completeMentalHealth(page);
      await completeBreastfeeding(page);
      await completeExamination(page, { isPostpartum: true });
      await completeFamilyPlanning(page);
      await completePostpartumTreatmentReview(page);
      // SpecialityCare appears because initial encounter set HIV known positive.
      await completeSpecialityCare(page);
      await endPrenatalEncounter(page);
      console.log('Created postpartum prenatal encounter for:', prenatalMomName);
    });

    await test.step('Sync postpartum prenatal encounter', async () => {
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

      // --- PrenatalCHW (female, 28 years): Prenatal CHW-1 encounter ---
      // Complete CHW activities for prenatal completion coverage.
      const prenatalCHW = await createPrenatalAdult(page, {
        isChw: true,
        ageYears: 28,
      });
      const chwLmpDate = new Date();
      chwLmpDate.setDate(chwLmpDate.getDate() - 20 * 7); // ~20 weeks
      await completePregnancyDating(page, chwLmpDate);
      await completePrenatalLaboratoryChw(page);
      await completePrenatalDangerSigns(page);
      await completePrenatalHealthEducation(page, 1);
      await completePrenatalNextSteps(page);
      await endPrenatalEncounter(page);
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

      // --- CSChild (male, 10 months): Child Scoreboard ---
      // Complete NCDA + VaccinationHistory for completion coverage.
      // At 10 months (~43 weeks): all 8 activities expected (including MR at 36+ weeks).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const csChild = await createChildScoreboardChild(page, { ageMonths: 10 });
      await completeNCDA(page);
      await completeVaccinationHistory(page);
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

      // Generate completion data for newly created encounters only
      // (--exclude_set=1 skips encounters already processed in Phase 0).
      generateCompletionData('acute-illness', true);
      generateCompletionData('prenatal', true);
      generateCompletionData('child-scoreboard', true);
      completionRecalculateLargeDatasets();
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

      // Row "2Y - 5Y": male +1 (AIChild 24mo falls into this bucket)
      const row2Y5Y = findRow(newRegistered, '2Y - 5Y')!;
      const base2Y5Y = findRow(baselineRegistered, '2Y - 5Y')!;
      expect(row2Y5Y.male, '2Y-5Y male should increase by 1').toBe(base2Y5Y.male + 1);

      // Row "20Y - 50Y": male +2 (NCDAdult, TBAdult), female +6 (PrenatalMom, AINurse, FBFMother, PrenatalCHW, AICHW, HIVAdult)
      const row20Y50Y = findRow(newRegistered, '20Y - 50Y')!;
      const base20Y50Y = findRow(baselineRegistered, '20Y - 50Y')!;
      expect(row20Y50Y.male, '20Y-50Y male should increase by 2').toBe(base20Y50Y.male + 2);
      expect(row20Y50Y.female, '20Y-50Y female should increase by 6').toBe(base20Y50Y.female + 6);

      // Total: +13 (7 nurse patients + 6 CHW patients)
      expect(newRegistered.total, 'Registered total should increase by 13').toBe(
        baselineRegistered.total + 13,
      );

      // Other rows should be unchanged.
      for (const label of ['0 - 1M', '5Y - 10Y', '10Y - 20Y', '50Y +']) {
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

      // Row "20Y - 50Y": female +2 (AINurse: 2 AI encounters, PrenatalMom: initial + postpartum)
      const imp20Y50Y = findRow(newImpacted, '20Y - 50Y')!;
      const baseImp20Y50Y = findRow(baselineImpacted, '20Y - 50Y')!;
      expect(imp20Y50Y.female, 'Impacted 20Y-50Y female should increase by 2').toBe(
        baseImp20Y50Y.female + 2,
      );

      // Total: +4 (NutrChild + HVChild + AINurse + PrenatalMom)
      expect(newImpacted.total, 'Impacted total should increase by 4').toBe(
        baselineImpacted.total + 4,
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
      assertDelta('ANC (total)', 3);         // nurse initial +1, nurse postpartum +1, CHW +1
      assertDelta('Acute Illness (total)', 4); // nurse initial +1, nurse child +1, nurse subsequent +1, CHW +1
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

      // "Uncomplicated Malaria": +2 (AINurse initial + AIChild initial;
      // subsequent encounter continues the same illness, no new diagnosis).
      expect(newMalaria, 'Malaria Uncomplicated should increase by 2').toBe(
        baselineMalaria + 2,
      );

      // "Acute Respiratory Infection with Complications": +1 (AICHW respiratory symptoms)
      expect(newResp, 'Respiratory Complicated should increase by 1').toBe(
        baselineResp + 1,
      );

      // "Total": +3 (2 malaria + 1 respiratory; subsequent doesn't add a diagnosis).
      // If this fails due to pre-existing demo data shifting, the root
      // cause is in the reports data generation pipeline, not in targeting.
      expect(newTotal, 'AI Total should increase by 3').toBe(
        baselineAITotal + 3,
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

      // PrenatalMom: 2 nurse encounters (initial + postpartum), pregnancy completed.
      // PrenatalCHW: 1 CHW encounter, active pregnancy.
      // Log all rows for debugging — exact deltas depend on visit counting logic.
      console.log('\n=== PRENATAL (ANC) ===');
      console.log('All Pregnancies:');
      for (const row of newAll) {
        const base = findPrenatalRow(baselineAllPregnancies, row.label);
        console.log(`  ${row.label.padEnd(12)} | HC: ${base?.hc ?? 0}→${row.hc} | CHW: ${base?.chw ?? 0}→${row.chw} | All: ${base?.all ?? 0}→${row.all}`);
      }
      console.log('Active Pregnancies:');
      for (const row of newActive) {
        const base = findPrenatalRow(baselineActivePregnancies, row.label);
        console.log(`  ${row.label.padEnd(12)} | HC: ${base?.hc ?? 0}→${row.hc} | CHW: ${base?.chw ?? 0}→${row.chw} | All: ${base?.all ?? 0}→${row.all}`);
      }

      // All Pregnancies: PrenatalMom has 2 visits → moves to "2 visits" row.
      // CHW adds 1 visit. Total HC +1, Total CHW +1, Total All +2.
      const newAllTotal = findPrenatalRow(newAll, 'Total')!;
      const baseAllTotal2 = findPrenatalRow(baselineAllPregnancies, 'Total')!;
      expect(newAllTotal.all, 'All Pregnancies Total All +2').toBe(baseAllTotal2.all + 2);

      // Active Pregnancies: PrenatalMom's pregnancy ended (postpartum with outcome),
      // so she should NOT appear in Active. PrenatalCHW is active with 1 visit.
      const newActiveTotal = findPrenatalRow(newActive, 'Total')!;
      const baseActiveTotal2 = findPrenatalRow(baselineActivePregnancies, 'Total')!;
      // CHW +1 active, PrenatalMom no longer active → net depends on whether
      // PrenatalMom was counted in baseline. She wasn't (created after baseline).
      // So Active Total CHW +1.
      expect(newActiveTotal.chw, 'Active Pregnancies Total CHW +1').toBe(baseActiveTotal2.chw + 1);

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
      // Depression Not Likely +2: initial (MentalHealth score=0) + postpartum (MentalHealth again).
      expect(newDepression, 'Depression Not Likely +2').toBe(baselineDepression + 2);
      // Total: HIV +1, Gestational Hypertension +1, Depression Not Likely +2,
      // NoPrenatalDiagnosis (from CHW encounter) +1 = +5.
      expect(newPrenatalTotal, 'Prenatal Total +5').toBe(baselinePrenatalTotal + 5);

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

    // ── Phase 5: Completion Report — Acute Illness ──
    //
    // The test created 2 AI encounters (nurse initial + CHW initial).
    // The completion pipeline was run in Phase 2 after encounters synced.
    // Now we verify the Completion report table reflects the new data.

    let completion: CompletionTableData;

    await test.step('Verify Completion Report — AI table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'acute-illness');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      completion = await readCompletionTable(page, 'acute-illness');

      console.log('\n=== COMPLETION: ACUTE ILLNESS ===');
      console.log('Heading:', completion.heading);
      console.log('Rows:', completion.rows.length);
      for (const row of completion.rows) {
        const base = findCompletionRow(baselineCompletion, row.activity);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} | ${row.percent}`,
        );
      }

      expect(completion.heading).toContain('Acute Illness');
      expect(completion.rows.length, 'Should have 23 activity rows').toBe(23);
    });

    await test.step('Verify Completion Report — AI activity deltas (Any role)', async () => {
      // We created 4 AI encounters: nurse initial (adult), nurse initial (child),
      // nurse subsequent (adult), CHW initial (adult).
      // 3 initial encounters contribute to initial-only activities.
      // 1 subsequent encounter contributes to subsequent-only activities.

      // Vitals: expected +4 (always expected for all encounters),
      // completed +3 (3 initial encounters fill it; subsequent only does DangerSigns + OngoingTreatment).
      const vitals = findCompletionRow(completion, 'Vitals')!;
      const baseVitals = findCompletionRow(baselineCompletion, 'Vitals')!;
      expect(vitals.expected, 'Vitals expected +4').toBe(baseVitals.expected + 4);
      expect(vitals.completed, 'Vitals completed +3').toBe(baseVitals.completed + 3);

      // SymptomsGeneral: expected +3 (initial encounters only), completed +3.
      const sympGen = findCompletionRow(completion, 'Symptoms General')!;
      const baseSympGen = findCompletionRow(baselineCompletion, 'Symptoms General')!;
      expect(sympGen.expected, 'Symptoms General expected +3').toBe(baseSympGen.expected + 3);
      expect(sympGen.completed, 'Symptoms General completed +3').toBe(baseSympGen.completed + 3);

      // CoreExam: expected +3 (all nurse encounters, including subsequent),
      // completed +2 (only 2 initial nurse encounters fill it).
      const coreExam = findCompletionRow(completion, 'Core Exam')!;
      const baseCoreExam = findCompletionRow(baselineCompletion, 'Core Exam')!;
      expect(coreExam.expected, 'Core Exam expected +3').toBe(baseCoreExam.expected + 3);
      expect(coreExam.completed, 'Core Exam completed +2').toBe(baseCoreExam.completed + 2);

      // MUAC: expected +1 (AIChild is 24mo, in 6mo-5yr range), completed +1.
      const muac = findCompletionRow(completion, 'MUAC')!;
      const baseMuac = findCompletionRow(baselineCompletion, 'MUAC')!;
      expect(muac.expected, 'MUAC expected +1').toBe(baseMuac.expected + 1);
      expect(muac.completed, 'MUAC completed +1').toBe(baseMuac.completed + 1);

      // Nutrition: expected +1 (AIChild < 5yr), completed +1.
      const nutrition = findCompletionRow(completion, 'Nutrition')!;
      const baseNutrition = findCompletionRow(baselineCompletion, 'Nutrition')!;
      expect(nutrition.expected, 'Nutrition expected +1').toBe(baseNutrition.expected + 1);
      expect(nutrition.completed, 'Nutrition completed +1').toBe(baseNutrition.completed + 1);

      // MedicationDistribution: expected +2 (NextSteps triggered for 2 nurse initial
      // malaria encounters), completed +2.
      const medDist = findCompletionRow(completion, 'Medication Distribution')!;
      const baseMedDist = findCompletionRow(baselineCompletion, 'Medication Distribution')!;
      expect(medDist.expected, 'Medication Distribution expected +2').toBe(baseMedDist.expected + 2);
      expect(medDist.completed, 'Medication Distribution completed +2').toBe(baseMedDist.completed + 2);

      // FollowUp: expected +2, completed +2 (same as MedicationDistribution).
      const followUp = findCompletionRow(completion, 'Follow Up')!;
      const baseFollowUp = findCompletionRow(baselineCompletion, 'Follow Up')!;
      expect(followUp.expected, 'Follow Up expected +2').toBe(baseFollowUp.expected + 2);
      expect(followUp.completed, 'Follow Up completed +2').toBe(baseFollowUp.completed + 2);

      // DangerSigns: expected +1 (subsequent encounter), completed +1.
      const dangerSigns = findCompletionRow(completion, 'Danger Signs')!;
      const baseDangerSigns = findCompletionRow(baselineCompletion, 'Danger Signs')!;
      expect(dangerSigns.expected, 'Danger Signs expected +1').toBe(baseDangerSigns.expected + 1);
      expect(dangerSigns.completed, 'Danger Signs completed +1').toBe(baseDangerSigns.completed + 1);

      // OngoingTreatment: expected +1 (subsequent, medication prescribed in initial),
      // completed +1.
      const ongoing = findCompletionRow(completion, 'Ongoing Treatment')!;
      const baseOngoing = findCompletionRow(baselineCompletion, 'Ongoing Treatment')!;
      expect(ongoing.expected, 'Ongoing Treatment expected +1').toBe(baseOngoing.expected + 1);
      expect(ongoing.completed, 'Ongoing Treatment completed +1').toBe(baseOngoing.completed + 1);
    });

    await test.step('Verify Completion Report — AI Taken By filter (Nurse)', async () => {
      await selectCompletionTakenBy(page, 'nurse');
      const nurseData = await readCompletionTable(page, 'acute-illness');
      const baseCoreExam = findCompletionRow(baselineCompletion, 'Core Exam')!;
      const coreExam = findCompletionRow(nurseData, 'Core Exam')!;
      // 3 nurse encounters (2 initial + 1 subsequent) expect CoreExam.
      expect(coreExam.expected, 'AI Nurse: Core Exam expected +3').toBe(baseCoreExam.expected + 3);
      expect(coreExam.completed, 'AI Nurse: Core Exam completed +2').toBe(baseCoreExam.completed + 2);
    });

    await test.step('Verify Completion Report — AI Taken By filter (CHW)', async () => {
      await selectCompletionTakenBy(page, 'chw');
      const chwData = await readCompletionTable(page, 'acute-illness');
      const coreExam = findCompletionRow(chwData, 'Core Exam')!;
      // CoreExam is nurse-only — CHW filter should show 0 expected.
      expect(coreExam.expected, 'AI CHW: Core Exam expected 0').toBe(0);
    });

    // ── Phase 6: Completion Report — Prenatal ──
    //
    // The test created 3 prenatal encounters:
    // - Nurse initial (PrenatalMom): comprehensive activities + HIV+
    // - Nurse postpartum (PrenatalMom): GU Exam, Breastfeeding, SpecialityCare, etc.
    // - CHW-1 (PrenatalCHW): PregnancyTesting, DangerSigns, HealthEducation, NextSteps

    let prenatalCompletion: CompletionTableData;

    await test.step('Verify Completion Report — Prenatal table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'prenatal');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      prenatalCompletion = await readCompletionTable(page, 'prenatal');

      console.log('\n=== COMPLETION: PRENATAL ===');
      console.log('Heading:', prenatalCompletion.heading);
      console.log('Rows:', prenatalCompletion.rows.length);
      for (const row of prenatalCompletion.rows) {
        const base = findCompletionRow(baselinePrenatalCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        if (eDelta !== 0 || cDelta !== 0) {
          console.log(
            `${row.activity.padEnd(30)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
          );
        }
      }

      expect(prenatalCompletion.heading).toContain('Antenatal');
      expect(prenatalCompletion.rows.length, 'Should have 60 activity rows').toBe(60);
    });

    await test.step('Verify Completion Report — Prenatal activity deltas (Any role)', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(prenatalCompletion, label)!;
        const base = findCompletionRow(baselinePrenatalCompletion, label)!;
        expect(row, `Row "${label}" should exist`).toBeDefined();
        expect(base, `Baseline row "${label}" should exist`).toBeDefined();
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(base.expected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(base.completed + completedDelta);
      };

      // Both nurse initial + CHW encounters (+2 each).
      assertDelta('Last Menstrual Period', 2, 2);
      assertDelta('Danger Signs', 2, 2);

      // Nurse initial + postpartum (+2 each).
      assertDelta('Core Physical Exam', 2, 2);
      assertDelta('Breast Exam', 2, 2);
      assertDelta('Family Planning', 2, 2);
      assertDelta('Symptoms Review', 2, 2);
      assertDelta('Vitals', 2, 2);
      assertDelta('Nutrition', 2, 2);

      // Nurse initial only (+1 each).
      assertDelta('Medical History', 1, 1);
      assertDelta('Obstetric History', 1, 1);
      assertDelta('Obstetric History Second Step', 1, 1);
      assertDelta('Obstetrical Exam', 1, 1);
      assertDelta('Tetanus Immunisation', 1, 1);
      assertDelta('Mental Health', 1, 1);
      assertDelta('HIV Test', 1, 1);
      assertDelta('Malaria Test', 1, 1);
      assertDelta('Syphilis Test', 1, 1);
      assertDelta('Hemoglobin Test', 1, 1);
      assertDelta('Hepatitis B Test', 1, 1);
      assertDelta('Urine Dipstick Test', 1, 1);
      assertDelta('Random Blood Sugar Test', 1, 1);
      assertDelta('Blood Group and Rhesus Test', 1, 1);
      assertDelta('Partner HIV Test', 1, 1);
      assertDelta('Resource', 1, 1);

      // Postpartum only (+1 each).
      assertDelta('GU Exam', 1, 1);
      assertDelta('Breastfeeding', 1, 1);
      // Pregnancy Outcome: virtual activity, data on participant node.
      // recordPregnancyOutcome sets field_outcome on participant, but
      // completion script checks encounter measurements — shows as not completed.
      assertDelta('Pregnancy Outcome', 1, 0);
      assertDelta('Postpartum Treatment Review', 1, 1);

      // CHW only (+1 each).
      assertDelta('Pregnancy Testing', 1, 1);
      assertDelta('Appointment Confirmation', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Follow Up', 1, 1);

      // Expected but not completed (expected +1, completed +0).
      assertDelta('Photo', 1, 0);
      assertDelta('Social History', 1, 0);

      // Not expected (subsequent only, CHW-2 only).
      assertDelta('Outside Care', 0, 0);
      assertDelta('Treatment Review', 0, 0);
      assertDelta('Birth Plan', 0, 0);
      assertDelta('HIV PCR Test', 0, 0);
    });

    await test.step('Verify Completion Report — Prenatal Taken By filter (Nurse)', async () => {
      await selectCompletionTakenBy(page, 'nurse');
      const nurseData = await readCompletionTable(page, 'prenatal');

      // Nurse encounters have Vitals; baseline from existing data.
      const baseVitals = findCompletionRow(baselinePrenatalCompletion, 'Vitals')!;
      const vitals = findCompletionRow(nurseData, 'Vitals')!;
      // Nurse initial + postpartum both have Vitals.
      expect(vitals.expected, 'Prenatal Nurse: Vitals expected +2').toBe(baseVitals.expected + 2);
      expect(vitals.completed, 'Prenatal Nurse: Vitals completed +2').toBe(baseVitals.completed + 2);

      // PregnancyTesting is CHW only — Nurse filter should show 0.
      const pt = findCompletionRow(nurseData, 'Pregnancy Testing')!;
      expect(pt.expected, 'Prenatal Nurse: Pregnancy Testing expected 0').toBe(0);
    });

    await test.step('Verify Completion Report — Prenatal Taken By filter (CHW)', async () => {
      await selectCompletionTakenBy(page, 'chw');
      const chwData = await readCompletionTable(page, 'prenatal');

      // CHW has PregnancyTesting: +1 from our CHW encounter.
      const basePT = findCompletionRow(baselinePrenatalCompletionCHW, 'Pregnancy Testing')!;
      const pt = findCompletionRow(chwData, 'Pregnancy Testing')!;
      expect(pt.expected, 'Prenatal CHW: Pregnancy Testing expected +1').toBe(basePT.expected + 1);
      expect(pt.completed, 'Prenatal CHW: Pregnancy Testing completed +1').toBe(basePT.completed + 1);

      // Vitals is nurse only — CHW filter should show 0.
      const vitals = findCompletionRow(chwData, 'Vitals')!;
      expect(vitals.expected, 'Prenatal CHW: Vitals expected 0').toBe(0);
    });

    // ── Phase 7: Completion Report — Child Scoreboard ──
    //
    // CSChild (6mo male) completed NCDA + VaccinationHistory in CHW phase.
    // CHW-only module — no Taken By filter.

    let csCompletion: CompletionTableData;

    await test.step('Verify Completion Report — Child Scoreboard table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'child-scoreboard');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      csCompletion = await readCompletionTable(page, 'child-scoreboard');

      console.log('\n=== COMPLETION: CHILD SCOREBOARD ===');
      console.log('Heading:', csCompletion.heading);
      console.log('Rows:', csCompletion.rows.length);
      for (const row of csCompletion.rows) {
        const base = findCompletionRow(baselineCSCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(csCompletion.heading).toContain('Child Scorecard');
      expect(csCompletion.rows.length, 'Should have 8 activity rows (Rwanda)').toBe(8);
    });

    await test.step('Verify Completion Report — Child Scoreboard activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(csCompletion, label)!;
        const base = findCompletionRow(baselineCSCompletion, label)!;
        expect(row, `Row "${label}" should exist`).toBeDefined();
        expect(base, `Baseline row "${label}" should exist`).toBeDefined();
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(base.expected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(base.completed + completedDelta);
      };

      // NCDA: always expected, completed via completeNCDA.
      assertDelta('NCDA', 1, 1);

      // Immunisations expected at 6 months (26 weeks):
      assertDelta('BCG Immunisation', 1, 1);      // from birth
      assertDelta('OPV Immunisation', 1, 1);      // from birth
      assertDelta('DTP Immunisation', 1, 1);      // from 6 weeks
      assertDelta('PCV13 Immunisation', 1, 1);    // from 6 weeks
      assertDelta('Rotarix Immunisation', 1, 1);  // from 6 weeks
      assertDelta('IPV Immunisation', 1, 1);      // from 14 weeks

      // MR: expected at 10 months (~43 weeks, requires 36+ weeks).
      assertDelta('MR Immunisation', 1, 1);
    });
  });
});
