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
  readFBFDistributionTable,
  readPeripartumTable,
  readPostnatalCareTable,
  findPrenatalRow,
  PrenatalVisitsRow,
  findSimpleRow,
  readNutritionTable,
  readNutritionColumnHeaders,
  findNutritionMetric,
  backdateNutritionEncounter,
  backdateFamilyNutritionEncounter,
  NUTRITION_ONE_VISIT_TABLES,
  NUTRITION_TWO_VISIT_TABLES,
  NutritionMetricRow,
  PatientsTableData,
  EncountersTableData,
  SimpleTableData,
  ensurePrenatalMedicationsVariable,
  generateCompletionData,
  completionRecalculateLargeDatasets,
  navigateToCompletionReportPage,
  selectCompletionReportType,
  selectCompletionTakenBy,
  setCompletionDateRange,
  readCompletionTable,
  findCompletionRow,
  CompletionTableData,
  ensureNCDAFeatureEnabled,
  generateNCDAPersonData,
  ncdaRecalculateLargeDatasets,
  backdatePersonCreated,
  navigateToNCDAScoreboard,
  readScoreboardPane,
  findScoreboardValue,
  getCurrentMonthColumnIndex,
  ScoreboardPaneData,
} from './helpers/reports';

// Encounter creation helpers.
import { WAIT, syncAndWait } from './helpers/common';
import {
  createChildAndStartEncounter as createNutritionChild,
  enterHeight,
  enterWeight,
  enterMuac,
  enterNutritionSigns,
  saveActivity,
  completeNCDA as completeNutritionNCDA,
  completeContributingFactors,
  completeFollowUp as completeNutritionFollowUp,
  completeHealthEducation as completeNutritionHealthEducation,
  completeSendToHC,
} from './helpers/nutrition';
import {
  createChildAndStartWellChildEncounter,
  completeDangerSigns as completeWCDangerSigns,
  completePregnancySummary,
  completeNutritionAssessment as completeWCNutritionAssessment,
  completeECD,
  completeMedication as completeWCMedication,
  completeImmunisation as completeWCImmunisation,
  completeNCDA as completeWCNCDA,
  completeNextSteps as completeWCNextSteps,
  completeHomeVisit as completeWCHomeVisit,
} from './helpers/well-child';
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
  completeLaboratoryNurseForLab,
  navigateToCaseManagement as navigateToPrenatalCaseManagement,
  openRecurrentEncounterFromCaseManagement,
  completeLabResultsAsLabTech,
  endRecurrentEncounter,
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
import {
  createAdultAndStartNCDEncounter,
  completeDangerSigns as completeNCDDangerSigns,
  completeSymptomReview as completeNCDSymptomReview,
  completeExamination as completeNCDExamination,
  completeFamilyPlanning as completeNCDFamilyPlanning,
  completeMedicalHistory as completeNCDMedicalHistory,
  completeLaboratory as completeNCDLaboratory,
  completeNextSteps as completeNCDNextSteps,
  navigateToCaseManagement as navigateToNCDCaseManagement,
  openNCDRecurrentEncounterFromCaseManagement,
  completeLabResults as completeNCDLabResults,
  completeRecurrentNextSteps as completeNCDRecurrentNextSteps,
  leaveRecurrentEncounter as leaveNCDRecurrentEncounter,
} from './helpers/ncd';
import {
  createAdultAndStartHIVEncounter,
  completeDiagnostics as completeHIVDiagnostics,
  completeMedication as completeHIVMedication,
  completeNextSteps as completeHIVNextSteps,
} from './helpers/hiv';
import {
  createAdultAndStartTBEncounter,
  completeDiagnostics as completeTBDiagnostics,
  completeMedication as completeTBMedication,
  completeNextSteps as completeTBNextSteps,
} from './helpers/tuberculosis';
import {
  createChildAndStartEncounter as createChildScoreboardChild,
  completeNCDA,
  completeVaccinationHistory,
} from './helpers/child-scoreboard';
import {
  startHomeVisit,
  completeFeeding,
  completeCaring,
  completeHygiene,
  completeFoodSecurity,
} from './helpers/home-visit';
import {
  createMotherAndNavigateToPersonPage as createFamilyNutritionMother,
  addChild as addFamilyNutritionChild,
  continueToParticipantPage as continueToFamilyNutritionParticipant,
  startFamilyNutritionEncounter,
  selectFamilyMember,
  completeAhezaMother,
  completeAhezaChild,
  completeMuac as completeFamilyNutritionMuac,
  endFamilyNutritionEncounter,
} from './helpers/family-nutrition';
import {
  navigateToNurseGroupSession,
  createMotherOnAttendancePage,
  addChildToMother,
  navigateBackToAttendance,
  goToParticipantsPage,
  clickMotherCard,
  navigateToChild,
  navigateToMother,
  completeHeight,
  completeWeight,
  completeMuac as completeGroupMuac,
  completeNutritionSignsAbnormal,
  completeChildFbf,
  completeNCDA as completeGroupNCDA,
  completeContributingFactors as completeGroupContributingFactors,
  completeHealthEducation as completeGroupHealthEducation,
  completeSendToHC as completeGroupSendToHC,
  completeFollowUp as completeGroupFollowUp,
  completeFamilyPlanning as completeGroupFamilyPlanning,
  completeLactation,
  completeMotherFbf,
} from './helpers/group-session';

const pwaBaseUrl = `http://localhost:${getClientPort()}`;

// Nyange Health Center node ID (from migration CSV).
const NYANGE_HC_ID = 4;

// Start date for report filtering — early enough to include all data.
// Constructed in UTC because the calendar helper reads via getUTC*; using
// `new Date(2018, 0, 1)` (local time) in a positive-offset timezone yields
// 2017-12-31 in UTC, which is outside the Elm date picker's launchDate
// range and causes setDateRange to fail.
const REPORT_START_DATE = new Date(Date.UTC(2018, 0, 1));

test.describe('Admin Reports', () => {
  test.describe.configure({ timeout: 1080000 }); // 18 minutes

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
  //          AIChild (M 30mo) — AI initial (with MUAC + Nutrition)
  //          NCDAdult (F 40y) — NCD
  //          FBF group session (no new patient)
  //   CHW:   PrenatalCHW (F 28y) — Prenatal
  //          AICHW (F 26y) — Acute Illness
  //          HIVAdult (F 35y) — HIV
  //          TBAdult (M 45y) — Tuberculosis
  //          CSChild (M 10mo) — Child Scoreboard
  //          NBChild (M 1mo) — Newborn Exam
  //          FamilyNutritionMother (F 25y) + FamilyMuacChild (M 12mo) —
  //            Family Nutrition encounter with child MUAC = 12 cm (MAM band)
  //
  // Expected Registered Patients deltas:
  //   1M-2Y: male +6 (incl. family-nutrition child); +1 may land in 0-1M
  //     for NBChild (boundary), so test asserts the sum across both buckets
  //   2Y-5Y: male +1 (AIChild 30mo)
  //   20Y-50Y: male +1 (TBAdult), female +8 (incl. family-nutrition mother)
  //   Total: +17
  //
  // Expected Nutrition report deltas (issue #1718):
  //   - All 8 nutrition tables grow from 6 to 8 metric rows: stunting/wasting/
  //     underweight × moderate/severe + new MAM and SAM rows.
  //   - Prevalence (column 0, last completed month): SAM > 0 (NutrChild MUAC 11
  //     + Edema, HVChild MUAC 11) and MAM > 0 (family child MUAC 12 cm).
  test('Reports reflect new patients, encounters, and completion data', async ({ page }) => {
    const reportLimitDate = new Date();

    // Helper to log step name to stdout for progress tracking.
    const step = (name: string, fn: () => Promise<void>) => {
      console.log(`\n▶ ${name}`);
      return test.step(name, fn);
    };

    // ── Phase 0: Generate base reports data + record baselines ──

    await step('Generate base reports data from existing demo persons', async () => {
      ensurePrenatalMedicationsVariable();
      generateBaseReportsData();
      clearAdvancedQueue();
      recalculateLargeDatasets();

      // NCDA scoreboard baseline: generate per-person NCDA data and
      // pre-compute district-level Report Data nodes.
      ensureNCDAFeatureEnabled();
      generateNCDAPersonData(false);
      ncdaRecalculateLargeDatasets();
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
    // Peripartum (PR #1552) report baselines. PrenatalMom's flow is
    // updated below to record a "Live Birth Pre-Term" outcome (rather
    // than the default at-term) and select the "Premature Onset of
    // Contractions" danger sign, so all five rows should grow by +1
    // after sync. Breastfed-first-hour fires from the existing
    // postpartum completeBreastfeeding flow.
    let baselinePeripartumTotalDeliveries: number;
    let baselinePeripartumTotalLiveBirths: number;
    let baselinePeripartumPretermBirths: number;
    let baselinePeripartumPrematureLabour: number;
    let baselinePeripartumBreastfedFirstHour: number;
    // Postnatal Care report baselines (PR #1556). Among the existing test
    // patients, only NutrChild (M, 10mo, full nurse SPV with
    // completeWCImmunisation) contributes — they fall in the "10-19 months
    // UpToDate" bucket. Other test children either lack a well-child
    // encounter (AIChild, CSChild, HVChild) or are below the lowest age
    // bucket (NBChild at 1mo, below the 7-week lower bound). The
    // "within 24h of birth" row stays flat — no test patient has a
    // same-day SPV.
    let baselinePNCWithin24h: number;
    let baselinePNC7To11Weeks: number;
    let baselinePNC11To15Weeks: number;
    let baselinePNC15WeeksTo10Months: number;
    let baselinePNC10To19Months: number;
    let baselinePNC19To24Months: number;
    let baselineFBFDistChild: number;
    let baselineFBFDistMother: number;
    let baselineAhezaDistChild: number;
    let baselineAhezaDistMother: number;
    let baselineFBFDistChildOccurrences: number;
    let baselineFBFDistMotherOccurrences: number;
    let baselineAhezaDistChildOccurrences: number;
    let baselineAhezaDistMotherOccurrences: number;
    let baselineNutrition: Map<number, NutritionMetricRow[]>;
    let baselineCompletion: CompletionTableData;
    let baselinePrenatalCompletion: CompletionTableData;
    let baselinePrenatalCompletionCHW: CompletionTableData;
    let baselineCSCompletion: CompletionTableData;
    let baselineHIVCompletion: CompletionTableData;
    let baselineHVCompletion: CompletionTableData;
    let baselineNCDCompletion: CompletionTableData;
    let baselineNBCompletion: CompletionTableData;
    let baselineSPVCompletion: CompletionTableData;
    let baselineTBCompletion: CompletionTableData;
    let baselineNutrIndCompletion: CompletionTableData;
    let baselineNutrGrpCompletion: CompletionTableData;

    // NCDA Scoreboard baselines (keyed by pane heading).
    let baselineVillage: Record<string, ScoreboardPaneData>;
    let baselineDistrictDemographics: ScoreboardPaneData;

    let nutrChildName: string;
    let hvChildName: string;
    let aiNurseName: string;
    let prenatalMomName: string;
    let ncdAdultName: string;
    let csChildName: string;
    let fbfChildName: string;
    let nbChildName: string;
    let familyMuacChildName: string;

    await step('Login to Drupal admin and record baseline values', async () => {
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

      // Record Peripartum (PR #1552) report baselines.
      await selectReportType(page, 'peripartum');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      const baselinePeripartumTable = await readPeripartumTable(page);
      // Sanity: the report rendered with all 5 rows.
      expect(baselinePeripartumTable.rows.length, 'Peripartum report should have at least 5 rows')
        .toBeGreaterThanOrEqual(5);
      baselinePeripartumTotalDeliveries =
        findSimpleRow(baselinePeripartumTable, 'Total deliveries')?.total ?? 0;
      baselinePeripartumTotalLiveBirths =
        findSimpleRow(baselinePeripartumTable, 'Total live births')?.total ?? 0;
      baselinePeripartumPretermBirths =
        findSimpleRow(baselinePeripartumTable, 'Preterm birth newborns')?.total ?? 0;
      baselinePeripartumPrematureLabour =
        findSimpleRow(baselinePeripartumTable, 'premature labour')?.total ?? 0;
      baselinePeripartumBreastfedFirstHour =
        findSimpleRow(baselinePeripartumTable, 'breastfed within one hour')?.total ?? 0;
      console.log(
        `Baseline Peripartum: deliveries=${baselinePeripartumTotalDeliveries}, ` +
        `liveBirths=${baselinePeripartumTotalLiveBirths}, ` +
        `preterm=${baselinePeripartumPretermBirths}, ` +
        `prematureLabour=${baselinePeripartumPrematureLabour}, ` +
        `breastfedFirstHour=${baselinePeripartumBreastfedFirstHour}`,
      );

      // Record Postnatal Care (PR #1556) report baselines.
      await selectReportType(page, 'postnatal-care');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      const baselinePNCTable = await readPostnatalCareTable(page);
      // Sanity: report rendered with all 6 rows.
      expect(baselinePNCTable.rows.length, 'Postnatal Care report should have at least 6 rows')
        .toBeGreaterThanOrEqual(6);
      baselinePNCWithin24h =
        findSimpleRow(baselinePNCTable, 'within 24 hours of birth')?.total ?? 0;
      baselinePNC7To11Weeks =
        findSimpleRow(baselinePNCTable, 'aged 7-11 weeks')?.total ?? 0;
      baselinePNC11To15Weeks =
        findSimpleRow(baselinePNCTable, 'aged 11-15 weeks')?.total ?? 0;
      baselinePNC15WeeksTo10Months =
        findSimpleRow(baselinePNCTable, '15 weeks - 10 mos')?.total ?? 0;
      baselinePNC10To19Months =
        findSimpleRow(baselinePNCTable, 'aged 10-19 mos')?.total ?? 0;
      baselinePNC19To24Months =
        findSimpleRow(baselinePNCTable, '19 mos - 2 years')?.total ?? 0;
      console.log(
        `Baseline Postnatal Care: within24h=${baselinePNCWithin24h}, ` +
        `7-11w=${baselinePNC7To11Weeks}, 11-15w=${baselinePNC11To15Weeks}, ` +
        `15w-10mo=${baselinePNC15WeeksTo10Months}, ` +
        `10-19mo=${baselinePNC10To19Months}, 19-24mo=${baselinePNC19To24Months}`,
      );

      // Record FBF Distribution report baselines. The migration / demo
      // population can already include FBF and AHEZA records, so the test
      // encounters are verified by delta against this baseline rather than
      // absolute totals. Both the summed amount (Total column) and the
      // event count (Occurrences column) are baselined.
      await selectReportType(page, 'fbf-distribution');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
      const baselineFBFDistTable = await readFBFDistributionTable(page);
      const baselineFBFDistChildRow = findSimpleRow(baselineFBFDistTable, 'FBF Child');
      const baselineFBFDistMotherRow = findSimpleRow(baselineFBFDistTable, 'FBF Mother');
      const baselineAhezaDistChildRow = findSimpleRow(baselineFBFDistTable, 'Aheza Child');
      const baselineAhezaDistMotherRow = findSimpleRow(baselineFBFDistTable, 'Aheza Mother');
      baselineFBFDistChild = baselineFBFDistChildRow?.total ?? 0;
      baselineFBFDistMother = baselineFBFDistMotherRow?.total ?? 0;
      baselineAhezaDistChild = baselineAhezaDistChildRow?.total ?? 0;
      baselineAhezaDistMother = baselineAhezaDistMotherRow?.total ?? 0;
      baselineFBFDistChildOccurrences = baselineFBFDistChildRow?.occurrences ?? 0;
      baselineFBFDistMotherOccurrences = baselineFBFDistMotherRow?.occurrences ?? 0;
      baselineAhezaDistChildOccurrences = baselineAhezaDistChildRow?.occurrences ?? 0;
      baselineAhezaDistMotherOccurrences = baselineAhezaDistMotherRow?.occurrences ?? 0;
      console.log(
        `Baseline FBF Distribution: ` +
        `fbfChild=${baselineFBFDistChild}/${baselineFBFDistChildOccurrences}occ, ` +
        `fbfMother=${baselineFBFDistMother}/${baselineFBFDistMotherOccurrences}occ, ` +
        `ahezaChild=${baselineAhezaDistChild}/${baselineAhezaDistChildOccurrences}occ, ` +
        `ahezaMother=${baselineAhezaDistMother}/${baselineAhezaDistMotherOccurrences}occ`,
      );

      // Record Nutrition report baseline (first column = newest completed month).
      // The nutrition report only shows completed months, so test encounters
      // will be backdated to the previous month after syncing.
      await selectReportType(page, 'nutrition');
      await page.waitForTimeout(WAIT.pageNavigation);
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
    await step('Generate base completion data and record baselines', async () => {
      generateCompletionData('acute-illness', true);
      generateCompletionData('prenatal', true);
      generateCompletionData('child-scoreboard', true);
      generateCompletionData('hiv', true);
      generateCompletionData('home-visit', true);
      generateCompletionData('ncd', true);
      generateCompletionData('well-child', true);
      generateCompletionData('tuberculosis', true);
      generateCompletionData('nutrition-individual', true);
      generateCompletionData('nutrition-group', true);
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

      // HIV baseline (CHW-only module, no Taken By filter).
      await selectCompletionReportType(page, 'hiv');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineHIVCompletion = await readCompletionTable(page, 'hiv');
      console.log('Baseline HIV completion rows:', baselineHIVCompletion.rows.length);

      // Home Visit baseline (CHW-only module, no Taken By filter).
      await selectCompletionReportType(page, 'home-visit');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineHVCompletion = await readCompletionTable(page, 'home-visit');
      console.log('Baseline HV completion rows:', baselineHVCompletion.rows.length);

      // NCD baseline (Nurse-only module, no Taken By filter).
      await selectCompletionReportType(page, 'ncd');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      baselineNCDCompletion = await readCompletionTable(page, 'ncd');
      console.log('Baseline NCD completion rows:', baselineNCDCompletion.rows.length);

      // Newborn Exam baseline (CHW-only, shares CSS class 'well-child' with SPV).
      await selectCompletionReportType(page, 'newborn-exam');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      const nbReport = page.locator('div.report.well-child');
      if (await nbReport.isVisible({ timeout: 3000 }).catch(() => false)) {
        baselineNBCompletion = await readCompletionTable(page, 'well-child');
      } else {
        baselineNBCompletion = { heading: '', rows: [] };
      }
      console.log('Baseline NB completion rows:', baselineNBCompletion.rows.length);

      // Well Child SPV baseline (supports Nurse + CHW Taken By filter).
      await selectCompletionReportType(page, 'well-child');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      const spvReport = page.locator('div.report.well-child');
      if (await spvReport.isVisible({ timeout: 3000 }).catch(() => false)) {
        baselineSPVCompletion = await readCompletionTable(page, 'well-child');
      } else {
        baselineSPVCompletion = { heading: '', rows: [] };
      }
      console.log('Baseline SPV completion rows:', baselineSPVCompletion.rows.length);

      // Tuberculosis baseline (CHW-only module, no Taken By filter).
      await selectCompletionReportType(page, 'tuberculosis');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      const tbReport = page.locator('div.report.tuberculosis');
      if (await tbReport.isVisible({ timeout: 3000 }).catch(() => false)) {
        baselineTBCompletion = await readCompletionTable(page, 'tuberculosis');
      } else {
        baselineTBCompletion = { heading: '', rows: [] };
      }
      console.log('Baseline TB completion rows:', baselineTBCompletion.rows.length);

      // Nutrition Individual baseline (Nurse + CHW, Taken By filter shown).
      await selectCompletionReportType(page, 'nutrition-individual');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      const nutrIndReport = page.locator('div.report.nutrition-individual');
      if (await nutrIndReport.isVisible({ timeout: 3000 }).catch(() => false)) {
        baselineNutrIndCompletion = await readCompletionTable(page, 'nutrition-individual');
      } else {
        baselineNutrIndCompletion = { heading: '', rows: [] };
      }
      console.log('Baseline NutrInd completion rows:', baselineNutrIndCompletion.rows.length);

      // Nutrition Group baseline (Nurse + CHW, Taken By filter shown).
      await selectCompletionReportType(page, 'nutrition-group');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);
      const nutrGrpReport = page.locator('div.report.nutrition-group');
      if (await nutrGrpReport.isVisible({ timeout: 3000 }).catch(() => false)) {
        baselineNutrGrpCompletion = await readCompletionTable(page, 'nutrition-group');
      } else {
        baselineNutrGrpCompletion = { heading: '', rows: [] };
      }
      console.log('Baseline NutrGrp completion rows:', baselineNutrGrpCompletion.rows.length);
    });

    await step('Record NCDA Scoreboard baselines', async () => {
      // Drupal admin login (if not already logged in from completion baselines).
      const currentUrl = page.url();
      if (!currentUrl.includes('/admin/')) {
        await drupalLogin(page);
      }

      // Village-level baseline (Akanduga — only CSChild will land here).
      await navigateToNCDAScoreboard(page, 'Amajyaruguru/Gakenke/Coko/Mbirima/Akanduga');
      const paneNames = [
        'Demographics', 'Acute Malnutrition', 'Stunting',
        'ANC + Newborn', 'Universal Intervention', 'Nutrition Behavior',
        'Targeted Interventions', 'Infrastructure',
      ];
      baselineVillage = {};
      for (const name of paneNames) {
        baselineVillage[name] = await readScoreboardPane(page, name);
      }
      console.log('Village scoreboard baseline panes:', Object.keys(baselineVillage).length);

      // District-level baseline (Gakenke).
      await navigateToNCDAScoreboard(page, 'Amajyaruguru/Gakenke');
      baselineDistrictDemographics = await readScoreboardPane(page, 'Demographics');
      console.log('District demographics baseline rows:', baselineDistrictDemographics.rows.length);
    });

    // ── Phase 1a: Nurse encounters ──

    await step('Login as nurse and create encounters', async () => {
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
      // Nutrition signs: use abnormal sign ('Edema') to trigger NextSteps
      // (Contributing Factors, FollowUp, HealthEducation, SendToHC).
      await enterNutritionSigns(page, ['Edema']);
      await click(page.locator('button.ui.fluid.primary.button.active'), page);
      const nursePopup = page.locator('div.ui.active.modal.diagnosis-popup');
      try {
        await nursePopup.waitFor({ timeout: 3000 });
        await click(nursePopup.locator('button.ui.primary.fluid.button'), page);
      } catch { /* no popup */ }
      await page.waitForTimeout(WAIT.pageNavigation);
      // Complete NextSteps activities triggered by abnormal nutrition signs.
      await completeContributingFactors(page);
      await completeNutritionFollowUp(page);
      await completeNutritionHealthEducation(page);
      await completeSendToHC(page);
      // Complete NCDA (nurse, age < 24 months).
      await completeNutritionNCDA(page);
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
      await page.waitForTimeout(WAIT.sectionTransition);

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
      // Complete nurse SPV activities for completion coverage.
      await completeWCDangerSigns(page);
      await completeWCNutritionAssessment(page, {
        height: '70',
        headCircumference: '45',
        weight: '8',
        muac: '14',
        nutritionSigns: [],
      });
      await completeECD(page);
      await completeWCMedication(page);
      await completeWCImmunisation(page);
      await completeWCNCDA(page);
      await completeWCNextSteps(page, {
        hasContributingFactors: false,
        hasHealthEducation: false,
        hasSendToHC: false,
        hasFollowUp: false,
      });
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
      await completeHistory(page, { preeclampsiaPrevious: true });
      await completeExamination(page, { vitals: { sys: '160', dia: '100' } });
      await completeFamilyPlanning(page);
      // { premature: true } selects "Premature Onset of Contractions" on
      // the danger-signs activity, which fires
      // IndicatorPrematureOnsetContractions on the wire and contributes
      // +1 to the Peripartum report's "Pregnant women with premature
      // labour" row. The selection does not trigger any new prenatal
      // diagnoses (verified against Pages/Prenatal/Activity/Utils.elm).
      await completePrenatalDangerSigns(page, { premature: true });
      await completeSymptomReview(page);
      await completeMalariaPrevention(page);
      await completeMentalHealth(page);
      await completeImmunisation(page);
      await completeMedication(page, { preferIronFolate: true });
      // Send all labs to Lab (not point of care) to create prenatal_labs_results
      // node, which the completion script needs for test result activities.
      await completeLaboratoryNurseForLab(page);
      // NextSteps may show "Wait" (pause) or complete normally depending on
      // diagnosis. Handle both outcomes.
      await completePrenatalNextSteps(page);
      // After NextSteps (with or without Wait/Pause), navigate to dashboard.
      await goToDashboard(page);
      // Sync so lab data is available in Case Management.
      await syncAndWait(page);
      // Enter lab results via Case Management recurrent encounter.
      await navigateToPrenatalCaseManagement(page);
      await openRecurrentEncounterFromCaseManagement(page, prenatalMomName);
      // Open the LAB RESULTS activity on the recurrent encounter page.
      await click(page.locator('.icon-task-laboratory'), page);
      await page.locator('div.page-activity.prenatal').waitFor({ timeout: 10000 });
      await completeLabResultsAsLabTech(page);
      // Return to recurrent encounter page and complete NextSteps.
      await page.locator('div.page-encounter.prenatal').waitFor({ timeout: 10000 });
      await click(page.locator('.icon-task-next-steps'), page);
      await page.locator('div.page-activity.prenatal').waitFor({ timeout: 10000 });
      // Dismiss diagnosis warning popup if present.
      const warningContinue = page.locator('button', { hasText: 'Continue' });
      if (await warningContinue.isVisible({ timeout: 2000 }).catch(() => false)) {
        await warningContinue.click({ force: true });
        await page.waitForTimeout(WAIT.elmRerender);
      }
      // Complete each NextSteps tab on the recurrent encounter.
      // Tabs: Health Education (HIV counseling), Medication Distribution, Referral.
      for (let i = 0; i < 5; i++) {
        const tab = page.locator('.link-section:not(.completed)').first();
        if (!(await tab.isVisible({ timeout: 2000 }).catch(() => false))) break;
        await click(tab, page);
        await page.waitForTimeout(WAIT.elmRerender);
        // Answer all visible Yes/No questions with "Yes".
        // Run multiple rounds: answering "Yes" can reveal new questions.
        for (let round = 0; round < 5; round++) {
          const yesLabels = page.locator('label:text-is("Yes")');
          const yesCount = await yesLabels.count();
          let clicked = false;
          for (let j = 0; j < yesCount; j++) {
            const lbl = yesLabels.nth(j);
            // Skip already-selected radio buttons.
            const radio = lbl.locator('..').locator('input[type="radio"]');
            const isChecked = await radio.isChecked().catch(() => false);
            if (!isChecked) {
              await click(lbl, page);
              clicked = true;
              await page.waitForTimeout(WAIT.formInteraction);
            }
          }
          if (!clicked) break;
        }
        // Medication Distribution may have multiple treatment sections.
        // Selecting a checkbox in one section may reset another section's
        // pre-selection due to shared Elm form state. Select treatments for
        // ALL sections: Coartem for Malaria, Penicillin for Syphilis.
        const treatments = ['Coartem', 'Penicillin'];
        for (const med of treatments) {
          const label = page.locator('label', { hasText: med }).first();
          if (await label.isVisible({ timeout: 500 }).catch(() => false)) {
            await click(label, page);
            await page.waitForTimeout(WAIT.formInteraction);
          }
        }
        // Save this tab.
        const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
        if (await saveBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
          await click(saveBtn, page);
          await page.waitForTimeout(WAIT.elmRerender);
        }
      }
      // After completing all tabs, the app may show the encounter page
      // (with "Leave Encounter") or the Progress Report. Handle both.
      const leaveBtn = page.locator('button', { hasText: /Leave Encounter/i });
      const endBtn = page.locator('button', { hasText: /End Encounter/i }).first();
      const backArrow = page.locator('div.page-report span.icon-back');
      if (await leaveBtn.isVisible({ timeout: 3000 }).catch(() => false)) {
        await click(leaveBtn, page);
      } else if (await endBtn.isVisible({ timeout: 3000 }).catch(() => false)) {
        await endBtn.click({ force: true });
        const continueBtn2 = page.locator('button', { hasText: 'Continue' });
        if (await continueBtn2.isVisible({ timeout: 2000 }).catch(() => false)) {
          await continueBtn2.click({ force: true });
        }
      } else if (await backArrow.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(backArrow, page);
      }
      await page.waitForTimeout(WAIT.pageNavigation);
      console.log('Created PrenatalMom (HIV diagnosis + lab results):', prenatalMom.fullName);

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

      // --- AIChild (male, 30 months): Acute Illness with malaria diagnosis ---
      // Child encounter to exercise MUAC + Nutrition completion activities,
      // which are only expected for patients aged 6 months to 5 years.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const aiChild = await createAIChild(page, { ageMonths: 30 });
      await completeAISymptoms(page);
      await completeAIPhysicalExam(page); // Includes MUAC + Nutrition tabs for children.
      await completeAIPriorTreatment(page);
      await completeAILaboratory(page);
      await completeAINextSteps(page);
      await goToDashboard(page);
      console.log('Created AIChild (Uncomplicated Malaria):', aiChild.fullName);

      // --- NCDAdult (male, 40 years): NCD encounter ---
      // Complete all nurse initial activities for completion coverage.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const ncdAdult = await createAdultAndStartNCDEncounter(page, {
        isFemale: true,
        ageYears: 40,
      });
      ncdAdultName = ncdAdult.fullName;
      await completeNCDDangerSigns(page);
      await completeNCDSymptomReview(page);
      // Stage 1 hypertension (sys=145, dia=95) triggers HealthEducation in NextSteps.
      await completeNCDExamination(page, { sys: '145', dia: '95' });
      await completeNCDFamilyPlanning(page); // Female patient — FamilyPlanning + PregnancyTest expected.
      await completeNCDMedicalHistory(page);
      await completeNCDLaboratory(page, { performTests: true });
      await completeNCDNextSteps(page);
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
      fbfChildName = fbfChild.fullName;

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
      await completeGroupMuac(page, '11.5');
      await completeNutritionSignsAbnormal(page); // Triggers NextSteps.
      await completeChildFbf(page);
      await completeGroupNCDA(page);
      // NextSteps (triggered by abnormal nutrition signs).
      await completeGroupContributingFactors(page);
      await completeGroupHealthEducation(page);
      await completeGroupSendToHC(page);
      await completeGroupFollowUp(page);
      // Navigate to mother and complete mother activities.
      await navigateToMother(page);
      await completeGroupFamilyPlanning(page);
      await completeLactation(page);
      await completeMotherFbf(page);
      console.log('Created FBF session with full activities:', fbfChild.fullName);
    });

    await step('Sync nurse data to backend', async () => {
      // Navigate to dashboard first (may be on attendance page from FBF session).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1a-extra-0: NCD lab results via recurrent encounter ──
    // NCDAdult labs were sent to lab. Enter results via Case Management.

    await step('Enter NCD lab results via recurrent encounter', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToNCDCaseManagement(page);
      await openNCDRecurrentEncounterFromCaseManagement(page, ncdAdultName);
      await completeNCDLabResults(page);
      await completeNCDRecurrentNextSteps(page);
      await leaveNCDRecurrentEncounter(page);
    });

    // ── Phase 1a-extra: Subsequent AI encounter ──
    // AINurse initial encounter is now synced. Backdate it to yesterday
    // so we can start a subsequent encounter today and cover
    // DangerSigns + OngoingTreatment completion activities.

    await step('Backdate and create subsequent nurse AI encounter', async () => {
      backdateAcuteIllnessEncounter(aiNurseName);

      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToAIParticipant(page, aiNurseName);
      await startSubsequentAI(page);
      await completeAIDangerSigns(page);
      await completeAIPhysicalExam(page);
      await completeAIOngoingTreatment(page);
      await completeAINextSteps(page, {
        hasMedicationDistribution: false,
        hasSendToHC: true,
        hasFollowUp: true,
        hasHealthEducation: true,
      });
      await goToDashboard(page);
      console.log('Created subsequent AI encounter for:', aiNurseName);
    });

    await step('Sync subsequent AI encounter', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1a-extra-2: Postpartum prenatal encounter ──
    // PrenatalMom's initial encounter is synced. Backdate it to allow
    // a postpartum encounter today. Covers: GU Exam, Breastfeeding,
    // SpecialityCare, PregnancyOutcome, PostpartumTreatmentReview.

    await step('Backdate and create postpartum prenatal encounter', async () => {
      backdatePrenatalEncounter(prenatalMomName, 7);

      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await navigateToPrenatalParticipant(page, prenatalMomName);

      // Record pregnancy outcome from participant page first —
      // required before the Postpartum encounter button becomes active.
      // { preTerm: true } picks "Live Birth Pre-Term" so the Peripartum
      // report's "Preterm birth newborns" row gets +1. Total deliveries
      // and total live births also +1 (LivePreTerm counts in both).
      await recordPregnancyOutcome(page, { preTerm: true });

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

    await step('Sync postpartum prenatal encounter', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1b: CHW encounters ──

    await step('Login as CHW and create encounters', async () => {
      // Log out from nurse and log in as CHW — same device, no re-pairing needed.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await click(page.locator('button.ui.button.logout'), page);
      await page.locator('input[name="pincode"]').waitFor({ timeout: 10000 });

      // Login as CHW.
      const pinInput = page.locator('input[name="pincode"]');
      await pinInput.fill('2345');
      await click(page.getByRole('button', { name: 'Sign In' }), page);
      await page.waitForTimeout(WAIT.heavyOperation);

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

      // --- HIVAdult (female, 35 years): HIV initial encounter ---
      // Complete Diagnostics (positive) + Medication + NextSteps for completion coverage.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const hivAdult = await createAdultAndStartHIVEncounter(page, {
        isFemale: true,
        ageYears: 35,
      });
      await completeHIVDiagnostics(page);
      await completeHIVMedication(page, { sideEffects: true });
      await completeHIVNextSteps(page);
      await goToDashboard(page);
      console.log('Created HIVAdult:', hivAdult.fullName);

      // --- TBAdult (male, 45 years): TB initial encounter ---
      // Complete Diagnostics (positive) + Medication + NextSteps for completion coverage.
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const tbAdult = await createAdultAndStartTBEncounter(page, {
        isFemale: false,
        ageYears: 45,
      });
      await completeTBDiagnostics(page);
      await completeTBMedication(page, { sideEffects: true }); // Triggers Referral in NextSteps.
      await completeTBNextSteps(page);
      await goToDashboard(page);
      console.log('Created TBAdult:', tbAdult.fullName);

      // --- CSChild (male, 10 months): Child Scoreboard ---
      // Complete NCDA + VaccinationHistory for completion coverage.
      // At 10 months (~43 weeks): all 8 activities expected (including MR at 36+ weeks).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const csChild = await createChildScoreboardChild(page, { ageMonths: 10 });
      csChildName = csChild.fullName;
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
      await page.waitForTimeout(WAIT.pageNavigation);
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
      await page.waitForTimeout(WAIT.sectionTransition);
      const hvResult = page.locator('.item.participant-view', {
        hasText: hvChild.firstName,
      }).first();
      await hvResult.waitFor({ timeout: 10000 });
      await click(hvResult.locator('.action-icon.forward'), page);
      await page.locator('div.page-participant.individual.nutrition').waitFor({ timeout: 10000 });
      // Start home visit from participant page and complete all 4 activities.
      await startHomeVisit(page);
      await completeFeeding(page);
      await completeCaring(page);
      await completeHygiene(page);
      await completeFoodSecurity(page);
      console.log('Created HVChild + Home Visit:', hvChild.fullName);

      // --- NBChild (male, 1 month): Newborn Exam ---
      // CHW Well Child encounter for a 1-month-old triggers Newborn Exam type.
      // Note: at 2 months the app creates PediatricCareChw instead.
      await goToDashboard(page);
      const nbChild = await createChildAndStartWellChildEncounter(page, {
        ageMonths: 1,
        isChw: true,
      });
      nbChildName = nbChild.fullName;
      await completePregnancySummary(page);
      await completeWCNutritionAssessment(page, {
        headCircumference: '35',
        weight: '2',
        nutritionSigns: ['Edema'],
      });
      await completeWCImmunisation(page, { isChw: true });
      await completeWCNextSteps(page, {
        hasContributingFactors: true,
        hasHealthEducation: true,
        hasSendToHC: true,
        hasFollowUp: true,
      });
      await goToDashboard(page);
      console.log('Created NBChild (Newborn Exam):', nbChild.fullName);

      // --- SPVChild (male, 10 months): CHW Well Child encounter ---
      // Abnormal weight triggers nutrition assessment → NextSteps.
      // Covers: Caring, Feeding, FoodSecurity, Hygiene, ContributingFactors,
      // FollowUp, HealthEducation, SendToHC.
      await goToDashboard(page);
      const spvChwChild = await createChildAndStartWellChildEncounter(page, {
        ageMonths: 10,
        isChw: true,
      });
      await completeWCDangerSigns(page);
      await completeWCNutritionAssessment(page, {
        height: '60',
        headCircumference: '45',
        weight: '3',
        muac: '11',
        nutritionSigns: ['Edema'],
      });
      await completeWCHomeVisit(page);
      await completeWCImmunisation(page, { isChw: true });
      await completeWCNextSteps(page, {
        hasContributingFactors: true,
        hasHealthEducation: true,
        hasSendToHC: true,
        hasFollowUp: true,
      });
      await goToDashboard(page);
      console.log('Created SPVChild (CHW Well Child):', spvChwChild.fullName);

      // --- Family Nutrition encounter (CHW): mother + 1 child (12mo) ---
      // Mother MUAC = 25 cm (Normal — required to enable End Encounter for
      // the mother's activities). Child MUAC = 12 cm (= 120 mm = MAM range,
      // ≥115 and <125). Family-nutrition encounters are CHW-only and the
      // child MUAC measurements live on the child's reports data under
      // 'family-nutrition-muac', which is what feeds the MAM/SAM stats
      // rows in the nutrition report. The family encounter creation also
      // contributes +1 to the Demographics "Family Nutrition" encounter
      // row (counted on the mother's reports data under the legacy
      // date-only 'family-nutrition' key).
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      const familyMother = await createFamilyNutritionMother(page, { ageYears: 25 });
      const familyChild = await addFamilyNutritionChild(page, { ageMonths: 12 });
      familyMuacChildName = familyChild.fullName;
      await continueToFamilyNutritionParticipant(page);
      await startFamilyNutritionEncounter(page);
      // Mother is selected by default — complete her activities so the
      // encounter can be ended.
      await completeAhezaMother(page, { amount: '3', reasonIndex: 1 });
      await completeFamilyNutritionMuac(page, { value: '25.0' });
      // Switch to the child (index 1; mother is index 0). Wait for the
      // activity grid to render before clicking activities.
      await selectFamilyMember(page, 1);
      await page
        .locator('.link-section:has(.icon-activity-task.icon-fbf)')
        .waitFor({ timeout: 10000 });
      await completeAhezaChild(page, { amount: '2' });
      // Child MUAC = 12.0 cm = 120 mm = MAM band (>=115 mm and <125 mm).
      // This is the value the test asserts on in the nutrition report.
      await completeFamilyNutritionMuac(page, { value: '12.0' });
      await endFamilyNutritionEncounter(page);
      console.log(
        'Created Family Nutrition encounter:',
        familyMother.fullName,
        '+ child',
        familyChild.fullName,
      );
    });

    await step('Sync CHW data to backend', async () => {
      await page.goto(pwaBaseUrl);
      await page.locator('.wrap-cards').waitFor({ timeout: 10000 });
      await syncAndWait(page);
    });

    // ── Phase 1c: Backdate nutrition encounters to previous month ──
    // The nutrition report only shows completed months. Backdating makes
    // our test data appear in the first (newest) column.

    await step('Backdate nutrition encounters to previous month', async () => {
      const lastMonth = new Date();
      lastMonth.setMonth(lastMonth.getMonth() - 1);
      backdateNutritionEncounter(nutrChildName, lastMonth);
      backdateNutritionEncounter(hvChildName, lastMonth);
      // Family-nutrition: backdate the encounter and child MUAC measurement
      // so the MAM data lands in the same prevalence-by-month column 0 as
      // the nutrition encounters above.
      backdateFamilyNutritionEncounter(familyMuacChildName, lastMonth);
    });

    // ── Phase 2: Process AQ + re-aggregate ──

    await step('Process Advanced Queue and recalculate large datasets', async () => {
      // AQ runs once, after all content is generated (including backdating).
      processAdvancedQueue();
      recalculateLargeDatasets();

      // Generate completion data for newly created encounters only
      // (--exclude_set=1 skips encounters already processed in Phase 0).
      generateCompletionData('acute-illness', true);
      generateCompletionData('prenatal', true);
      generateCompletionData('child-scoreboard', true);
      generateCompletionData('hiv', true);
      generateCompletionData('home-visit', true);
      generateCompletionData('ncd', true);
      generateCompletionData('well-child', true);
      generateCompletionData('tuberculosis', true);
      generateCompletionData('nutrition-individual', true);
      generateCompletionData('nutrition-group', true);
      completionRecalculateLargeDatasets();

      // Re-compute district-level NCDA Report Data nodes with new encounter data.
      // Per-person field_ncda_data is already updated by AQ processing above.
      ncdaRecalculateLargeDatasets();
    });

    // ── Phase 3: Verify Demographics deltas — HC scope ──

    await step('Navigate to updated Demographics report', async () => {
      await navigateToHCReportsPage(page, NYANGE_HC_ID);
      await selectReportType(page, 'demographics');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);
    });

    await step('Verify Registered Patients table deltas', async () => {
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

      // Row "1M - 2Y": male +5 or +6 depending on whether NBChild (1mo) lands
      // in 0-1M or 1M-2Y (date math boundary). Check both buckets sum to +7
      // (the existing 6 male children + the family-nutrition child at 12mo).
      const row0to1M = findRow(newRegistered, '0 - 1M')!;
      const base0to1M = findRow(baselineRegistered, '0 - 1M')!;
      const row1M2Y = findRow(newRegistered, '1M - 2Y')!;
      const base1M2Y = findRow(baselineRegistered, '1M - 2Y')!;
      const nbDelta0to1M = row0to1M.male - base0to1M.male;
      const nbDelta1M2Y = row1M2Y.male - base1M2Y.male;
      expect(nbDelta0to1M + nbDelta1M2Y, '0-1M + 1M-2Y male should increase by 7 total').toBe(7);
      expect(row1M2Y.female, '1M-2Y female should be unchanged').toBe(base1M2Y.female);

      // Row "2Y - 5Y": male +1 (AIChild 30mo falls into this bucket)
      const row2Y5Y = findRow(newRegistered, '2Y - 5Y')!;
      const base2Y5Y = findRow(baselineRegistered, '2Y - 5Y')!;
      expect(row2Y5Y.male, '2Y-5Y male should increase by 1').toBe(base2Y5Y.male + 1);

      // Row "20Y - 50Y": male +1 (TBAdult), female +8 (PrenatalMom, AINurse,
      // FBFMother, NCDAdult, PrenatalCHW, AICHW, HIVAdult, family-nutrition mother)
      const row20Y50Y = findRow(newRegistered, '20Y - 50Y')!;
      const base20Y50Y = findRow(baselineRegistered, '20Y - 50Y')!;
      expect(row20Y50Y.male, '20Y-50Y male should increase by 1').toBe(base20Y50Y.male + 1);
      expect(row20Y50Y.female, '20Y-50Y female should increase by 8').toBe(base20Y50Y.female + 8);

      // Total: +17 (previous +15 + family-nutrition mother + child)
      expect(newRegistered.total, 'Registered total should increase by 17').toBe(
        baselineRegistered.total + 17,
      );

      // Other rows should be unchanged.
      for (const label of ['5Y - 10Y', '10Y - 20Y', '50Y +']) {
        const newRow = findRow(newRegistered, label)!;
        const baseRow = findRow(baselineRegistered, label)!;
        expect(newRow.male, `${label} male should be unchanged`).toBe(baseRow.male);
        expect(newRow.female, `${label} female should be unchanged`).toBe(baseRow.female);
      }
    });

    await step('Verify Impacted Patients table deltas', async () => {
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
      // SPVChild CHW WellChild does not count as impacted.
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

    await step('Verify Encounters table deltas', async () => {
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
      assertDelta('Standard Pediatric Visit', 3); // NutrChild SPV +1, NBChild Newborn Exam +1, SPVChild CHW +1
      assertDelta('Home Visit', 1);
      assertDelta('Child Scorecard', 1);
      assertDelta('NCD', 1);
      assertDelta('HIV', 1);
      assertDelta('Tuberculosis', 1);
      assertDelta('Nutrition (total)', 4);   // Individual +2 (NutrChild + HVChild) + FBF +2 (mother + child)
      assertDelta('FBF', 2);                // FBF mother + child both have measurements
      assertDelta('Individual', 2);         // NutrChild + HVChild each have a nutrition encounter
      assertDelta('Family Nutrition', 1);   // 1 family-nutrition encounter created above
    });

    await step('Verify CSV download button is visible', async () => {
      await expect(page.locator('button.download-csv'), 'Demographics CSV download button should be visible').toBeVisible();
    });

    // ── Acute Illness report verification ──

    await step('Verify Acute Illness report deltas', async () => {
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
      await expect(page.locator('button.download-csv'), 'Acute Illness CSV download button should be visible').toBeVisible();
    });

    // ── Prenatal (ANC) report verification ──

    await step('Verify Prenatal (ANC) report deltas', async () => {
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
      await expect(page.locator('button.download-csv'), 'Prenatal ANC CSV download button should be visible').toBeVisible();
    });

    // ── Prenatal Diagnoses report verification ──

    await step('Verify Prenatal Diagnoses report deltas', async () => {
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
      // NoPrenatalDiagnosis (from CHW) +1, + additional diagnoses from
      // lab results and preeclampsia history +3 = +8.
      expect(newPrenatalTotal, 'Prenatal Total +8').toBe(baselinePrenatalTotal + 8);

      // CSV download button.
      await expect(page.locator('button.download-csv'), 'Prenatal Diagnoses CSV download button should be visible').toBeVisible();
    });

    // ── Peripartum report verification (PR #1552) ──
    //
    // PrenatalMom's flow is wired to fire all five Peripartum rows by +1:
    //   - Pre-term outcome (recordPregnancyOutcome { preTerm: true })
    //     → Total deliveries + Total live births + Preterm birth newborns
    //   - "Premature Onset of Contractions" danger sign
    //     (completeDangerSigns { premature: true })
    //     → Pregnant women with premature labour
    //   - "breastfed-first-hour" answered Yes in postpartum Breastfeeding
    //     (already done by completeBreastfeeding)
    //     → Newborns breastfed within one hour of delivery
    //
    // PrenatalCHW does not record a pregnancy outcome and does not
    // contribute to any of these rows.
    await step('Verify Peripartum report deltas', async () => {
      await selectReportType(page, 'peripartum');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      const peripartumTable = await readPeripartumTable(page);
      const newDeliveries = findSimpleRow(peripartumTable, 'Total deliveries')?.total ?? 0;
      const newLiveBirths = findSimpleRow(peripartumTable, 'Total live births')?.total ?? 0;
      const newPreterm = findSimpleRow(peripartumTable, 'Preterm birth newborns')?.total ?? 0;
      const newPrematureLabour = findSimpleRow(peripartumTable, 'premature labour')?.total ?? 0;
      const newBreastfedFirstHour = findSimpleRow(peripartumTable, 'breastfed within one hour')?.total ?? 0;

      console.log('\n=== PERIPARTUM ===');
      console.log(`Total Deliveries:           baseline=${baselinePeripartumTotalDeliveries}, new=${newDeliveries}, delta=+${newDeliveries - baselinePeripartumTotalDeliveries}`);
      console.log(`Total Live Births:          baseline=${baselinePeripartumTotalLiveBirths}, new=${newLiveBirths}, delta=+${newLiveBirths - baselinePeripartumTotalLiveBirths}`);
      console.log(`Preterm Birth Newborns:     baseline=${baselinePeripartumPretermBirths}, new=${newPreterm}, delta=+${newPreterm - baselinePeripartumPretermBirths}`);
      console.log(`Premature Labour:           baseline=${baselinePeripartumPrematureLabour}, new=${newPrematureLabour}, delta=+${newPrematureLabour - baselinePeripartumPrematureLabour}`);
      console.log(`Breastfed Within One Hour:  baseline=${baselinePeripartumBreastfedFirstHour}, new=${newBreastfedFirstHour}, delta=+${newBreastfedFirstHour - baselinePeripartumBreastfedFirstHour}`);

      expect(newDeliveries, '"Total deliveries" +1').toBe(baselinePeripartumTotalDeliveries + 1);
      expect(newLiveBirths, '"Total live births" +1').toBe(baselinePeripartumTotalLiveBirths + 1);
      expect(newPreterm, '"Preterm birth newborns" +1').toBe(baselinePeripartumPretermBirths + 1);
      expect(newPrematureLabour, '"Pregnant women with premature labour" +1').toBe(baselinePeripartumPrematureLabour + 1);
      expect(newBreastfedFirstHour, '"Newborns breastfed within one hour of delivery" +1').toBe(baselinePeripartumBreastfedFirstHour + 1);

      // CSV download button.
      await expect(page.locator('button.download-csv'), 'Peripartum CSV download button should be visible').toBeVisible();
    });

    // ── Postnatal Care report verification (PR #1556) ──
    //
    // The report has 6 rows: SPV/Newborn-Exam-within-24h-of-birth + 5
    // age-bucket "UpToDate with immunization" rows (7-11w, 11-15w,
    // 15w-10mo, 10-19mo, 19-24mo). NutrChild (M, 10mo) gets a full nurse
    // SPV here with completeWCImmunisation, which administers every
    // age-eligible vaccine today. After sync, NutrChild's wellChildData
    // carries the full immunisation dict; the report's
    // generateFutureVaccinationsData computes the next due dose ~4 weeks
    // out (later than the report's limit date = today), so NutrChild
    // counts as UpToDate in the 10-19 months bucket → delta +1.
    //
    // No other test patient contributes:
    //   - AIChild/CSChild/HVChild: no well-child encounter.
    //   - NBChild (1mo, ~4 weeks): below the 7-week lower bound of the
    //     youngest bucket; not counted.
    //   - "Within 24 hours of birth" row: every well-child encounter in
    //     the test runs today against a birth date weeks-to-months in
    //     the past, so it stays flat.
    await step('Verify Postnatal Care report deltas', async () => {
      await selectReportType(page, 'postnatal-care');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      const pncTable = await readPostnatalCareTable(page);
      const newWithin24h = findSimpleRow(pncTable, 'within 24 hours of birth')?.total ?? 0;
      const new7To11Weeks = findSimpleRow(pncTable, 'aged 7-11 weeks')?.total ?? 0;
      const new11To15Weeks = findSimpleRow(pncTable, 'aged 11-15 weeks')?.total ?? 0;
      const new15WeeksTo10Months = findSimpleRow(pncTable, '15 weeks - 10 mos')?.total ?? 0;
      const new10To19Months = findSimpleRow(pncTable, 'aged 10-19 mos')?.total ?? 0;
      const new19To24Months = findSimpleRow(pncTable, '19 mos - 2 years')?.total ?? 0;

      console.log('\n=== POSTNATAL CARE ===');
      console.log(`Within 24h SPV/NBExam:  baseline=${baselinePNCWithin24h}, new=${newWithin24h}, delta=+${newWithin24h - baselinePNCWithin24h}`);
      console.log(`7-11 weeks UpToDate:    baseline=${baselinePNC7To11Weeks}, new=${new7To11Weeks}, delta=+${new7To11Weeks - baselinePNC7To11Weeks}`);
      console.log(`11-15 weeks UpToDate:   baseline=${baselinePNC11To15Weeks}, new=${new11To15Weeks}, delta=+${new11To15Weeks - baselinePNC11To15Weeks}`);
      console.log(`15w-10mo UpToDate:      baseline=${baselinePNC15WeeksTo10Months}, new=${new15WeeksTo10Months}, delta=+${new15WeeksTo10Months - baselinePNC15WeeksTo10Months}`);
      console.log(`10-19mo UpToDate:       baseline=${baselinePNC10To19Months}, new=${new10To19Months}, delta=+${new10To19Months - baselinePNC10To19Months}`);
      console.log(`19-24mo UpToDate:       baseline=${baselinePNC19To24Months}, new=${new19To24Months}, delta=+${new19To24Months - baselinePNC19To24Months}`);

      // Within-24h row should not move (no test patient has a same-day SPV).
      expect(newWithin24h - baselinePNCWithin24h,
        '"within 24 hours of birth" row should be unchanged').toBe(0);
      // NutrChild (10mo, full SPV with all eligible vaccines administered
      // today) should land in the 10-19 months bucket as UpToDate.
      expect(new10To19Months - baselinePNC10To19Months,
        '"Children aged 10-19 mos UpToDate" row should grow by +1').toBe(1);

      // CSV download button.
      await expect(page.locator('button.download-csv'), 'Postnatal Care CSV download button should be visible').toBeVisible();
    });

    // ── Nutrition report verification ──

    await step('Verify Nutrition report deltas', async () => {
      await selectReportType(page, 'nutrition');
      // No date range — nutrition report always shows last 12 months.
      await page.waitForTimeout(WAIT.pageNavigation);

      await expect(page.locator('div.report.nutrition'), 'Nutrition report container should be visible').toBeVisible();

      console.log('\n=== NUTRITION ===');

      // Nutrition encounters were backdated to the previous month.
      // Columns are reverse chronological — first column (index 0) is the
      // newest completed month where our test data should now appear.
      const columnHeaders = await readNutritionColumnHeaders(page, 0);
      console.log(`Columns (${columnHeaders.length}): ${columnHeaders.join(' | ')}`);

      // Verify all 8 nutrition tables have the new structure.
      // 8 metric rows per table: 6 z-score-based (stunting/wasting/
      // underweight × moderate/severe) + 2 MUAC-based (MAM, SAM) added
      // in issue #1718. Both the "One Visit Or More" tables (the one-
      // visit prevalence/incidence path) and the "Two Visits Or More"
      // tables (the encountersByMonthForImpacted path) must have the
      // new rows present so the precomputed `report_data` JSON is
      // consistent across both paths.
      const allNutritionTables = [
        ...NUTRITION_ONE_VISIT_TABLES,
        ...NUTRITION_TWO_VISIT_TABLES,
      ];
      for (const { index, name } of allNutritionTables) {
        const current = await readNutritionTable(page, index);
        expect(current.length, `${name}: should have 8 metric rows`).toBe(8);

        const colCount = current[0]?.values.length ?? 0;
        expect(colCount, `${name}: should have data columns`).toBeGreaterThan(0);

        // Confirm the new MAM and SAM rows are present in every table.
        expect(
          findNutritionMetric(current, 'MAM'),
          `${name}: MAM row should be present`,
        ).toBeDefined();
        expect(
          findNutritionMetric(current, 'SAM'),
          `${name}: SAM row should be present`,
        ).toBeDefined();
      }

      // Prevalence table (index 0): verify abnormal measurements produce non-zero %.
      // Incidence tables may show 0% if the child had prior encounters in demo data.
      const prevalence = await readNutritionTable(page, 0);
      const newStunting = findNutritionMetric(prevalence, 'Stunting Severe')?.values[0] ?? 0;
      const newUnderweight = findNutritionMetric(prevalence, 'Underweight Severe')?.values[0] ?? 0;
      console.log(`Prevalence — Stunting Severe: ${newStunting}%, Underweight Severe: ${newUnderweight}%`);
      expect(newStunting, 'Prevalence: Stunting Severe % should be > 0').toBeGreaterThan(0);
      expect(newUnderweight, 'Prevalence: Underweight Severe % should be > 0').toBeGreaterThan(0);

      // SAM and MAM rows. The test data produces:
      //   - SAM: NutrChild's nutrition encounter (MUAC 11.0 cm + Edema) and
      //     HVChild's CHW nutrition encounter (MUAC 11.0 cm). Both backdated.
      //   - MAM: family-nutrition child's MUAC = 12.0 cm (= 120 mm, in the
      //     ≥115 / <125 range). Backdated via backdateFamilyNutritionEncounter.
      // Both are expected to be > 0 in the first prevalence column.
      const newSam = findNutritionMetric(prevalence, 'SAM')?.values[0] ?? 0;
      const newMam = findNutritionMetric(prevalence, 'MAM')?.values[0] ?? 0;
      console.log(`Prevalence — MAM: ${newMam}%, SAM: ${newSam}%`);
      expect(newSam, 'Prevalence: SAM % should be > 0').toBeGreaterThan(0);
      expect(newMam, 'Prevalence: MAM % should be > 0').toBeGreaterThan(0);

      // CSV download button.
      await expect(page.locator('button.download-csv'), 'Nutrition CSV download button should be visible').toBeVisible();
    });

    // ── Phase 3b: FBF Distribution report ──
    //
    // Phase 1a created an FBF nurse group session at Nyange I (FBF clinic,
    // group_type='fbf') with completeChildFbf (amount '1') and
    // completeMotherFbf (amount '1'). Phase 1b created a CHW Family
    // Nutrition encounter with completeAhezaMother (amount '3') and
    // completeAhezaChild (amount '2'). All test-created persons land at
    // Nyange HC, so the same HC scope used by the other Phase 3 reports
    // captures the new distribution rows. Pre-existing migration data may
    // already contribute non-zero baseline totals, so we assert deltas
    // against the baseline captured in Phase 0.

    await step('Verify FBF Distribution report deltas', async () => {
      await selectReportType(page, 'fbf-distribution');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      const fbfDistribution = await readFBFDistributionTable(page);

      const fbfChildRow = findSimpleRow(fbfDistribution, 'FBF Child');
      const fbfMotherRow = findSimpleRow(fbfDistribution, 'FBF Mother');
      const ahezaChildRow = findSimpleRow(fbfDistribution, 'Aheza Child');
      const ahezaMotherRow = findSimpleRow(fbfDistribution, 'Aheza Mother');

      const fbfChild = fbfChildRow?.total ?? 0;
      const fbfMother = fbfMotherRow?.total ?? 0;
      const ahezaChild = ahezaChildRow?.total ?? 0;
      const ahezaMother = ahezaMotherRow?.total ?? 0;
      const fbfChildOccurrences = fbfChildRow?.occurrences ?? 0;
      const fbfMotherOccurrences = fbfMotherRow?.occurrences ?? 0;
      const ahezaChildOccurrences = ahezaChildRow?.occurrences ?? 0;
      const ahezaMotherOccurrences = ahezaMotherRow?.occurrences ?? 0;

      console.log('\n=== FBF DISTRIBUTION ===');
      console.log(`FBF Child:    total ${baselineFBFDistChild} → ${fbfChild} (Δ ${fbfChild - baselineFBFDistChild}); occ ${baselineFBFDistChildOccurrences} → ${fbfChildOccurrences} (Δ ${fbfChildOccurrences - baselineFBFDistChildOccurrences})`);
      console.log(`FBF Mother:   total ${baselineFBFDistMother} → ${fbfMother} (Δ ${fbfMother - baselineFBFDistMother}); occ ${baselineFBFDistMotherOccurrences} → ${fbfMotherOccurrences} (Δ ${fbfMotherOccurrences - baselineFBFDistMotherOccurrences})`);
      console.log(`Aheza Child:  total ${baselineAhezaDistChild} → ${ahezaChild} (Δ ${ahezaChild - baselineAhezaDistChild}); occ ${baselineAhezaDistChildOccurrences} → ${ahezaChildOccurrences} (Δ ${ahezaChildOccurrences - baselineAhezaDistChildOccurrences})`);
      console.log(`Aheza Mother: total ${baselineAhezaDistMother} → ${ahezaMother} (Δ ${ahezaMother - baselineAhezaDistMother}); occ ${baselineAhezaDistMotherOccurrences} → ${ahezaMotherOccurrences} (Δ ${ahezaMotherOccurrences - baselineAhezaDistMotherOccurrences})`);

      // Each complete*Fbf / complete*Aheza helper creates exactly one
      // distribution measurement record, so every row's Occurrences
      // delta is +1 regardless of the amount entered.
      // completeChildFbf (amount '1') -> FBF Child total +1, occurrences +1
      expect(fbfChild - baselineFBFDistChild, 'FBF Child total delta should be +1').toBe(1);
      expect(fbfChildOccurrences - baselineFBFDistChildOccurrences, 'FBF Child occurrences delta should be +1').toBe(1);
      // completeMotherFbf (amount '1') -> FBF Mother total +1, occurrences +1
      expect(fbfMother - baselineFBFDistMother, 'FBF Mother total delta should be +1').toBe(1);
      expect(fbfMotherOccurrences - baselineFBFDistMotherOccurrences, 'FBF Mother occurrences delta should be +1').toBe(1);
      // completeAhezaChild (amount '2') -> Aheza Child total +2, occurrences +1
      expect(ahezaChild - baselineAhezaDistChild, 'Aheza Child total delta should be +2').toBe(2);
      expect(ahezaChildOccurrences - baselineAhezaDistChildOccurrences, 'Aheza Child occurrences delta should be +1').toBe(1);
      // completeAhezaMother (amount '3') -> Aheza Mother total +3, occurrences +1
      expect(ahezaMother - baselineAhezaDistMother, 'Aheza Mother total delta should be +3').toBe(3);
      expect(ahezaMotherOccurrences - baselineAhezaDistMotherOccurrences, 'Aheza Mother occurrences delta should be +1').toBe(1);

      // CSV download button.
      await expect(
        page.locator('button.download-csv'),
        'FBF Distribution CSV download button should be visible',
      ).toBeVisible();
    });

    // ── Phase 4: Demographics scope (Province) ──

    await step('Verify Demographics report at Province scope', async () => {
      await navigateToReportsMenu(page);

      // Select "Demographic Region" scope.
      const scopeSelect = page
        .locator('.page-content.reports-menu .select-input-wrapper')
        .first()
        .locator('select.select-input');
      await scopeSelect.selectOption('demographics');
      await page.waitForTimeout(WAIT.elmRerender);

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
      await page.waitForTimeout(WAIT.elmRerender);

      // Click "Load Data" button.
      const loadBtn = page.locator('.page-content.reports-menu div.actions a button');
      await loadBtn.click();

      // Wait for results page.
      await page.locator('.page-content.reports').waitFor({ timeout: 30000 });

      // Verify scope label contains province name.
      const scopeLabel = await page.locator('div.scope').textContent();
      expect(scopeLabel, 'Province scope label should be present').toBeTruthy();

      // Select Demographics report type and date range.
      await selectReportType(page, 'demographics');
      await setDateRange(page, REPORT_START_DATE, reportLimitDate);

      // Verify report renders with data.
      await expect(page.locator('div.report.demographics'), 'Demographics report container should be visible').toBeVisible();
      const provinceRegistered = await readRegisteredPatientsTable(page);
      expect(provinceRegistered.total, 'Province registered total should be > 0').toBeGreaterThan(0);
    });

    // ── Phase 5: Completion Report — Acute Illness ──
    //
    // The test created 2 AI encounters (nurse initial + CHW initial).
    // The completion pipeline was run in Phase 2 after encounters synced.
    // Now we verify the Completion report table reflects the new data.

    let completion: CompletionTableData;

    await step('Verify Completion Report — AI table renders', async () => {
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

      expect(completion.heading, 'AI completion heading should contain Acute Illness').toContain('Acute Illness');
      expect(completion.rows.length, 'Should have 23 activity rows').toBe(23);
    });

    await step('Verify Completion Report — AI activity deltas (Any role)', async () => {
      // We created 4 AI encounters: nurse initial (adult), nurse initial (child),
      // nurse subsequent (adult), CHW initial (adult).
      // 3 initial encounters contribute to initial-only activities.
      // 1 subsequent encounter contributes to subsequent-only activities.

      // Vitals: expected +4 (always expected for all encounters),
      // completed +4 (3 initial + subsequent physical exam).
      const vitals = findCompletionRow(completion, 'Vitals')!;
      const baseVitals = findCompletionRow(baselineCompletion, 'Vitals')!;
      expect(vitals.expected, 'Vitals expected +4').toBe(baseVitals.expected + 4);
      expect(vitals.completed, 'Vitals completed +4').toBe(baseVitals.completed + 4);

      // SymptomsGeneral: expected +3 (initial encounters only), completed +3.
      const sympGen = findCompletionRow(completion, 'Symptoms General')!;
      const baseSympGen = findCompletionRow(baselineCompletion, 'Symptoms General')!;
      expect(sympGen.expected, 'Symptoms General expected +3').toBe(baseSympGen.expected + 3);
      expect(sympGen.completed, 'Symptoms General completed +3').toBe(baseSympGen.completed + 3);

      // CoreExam: expected +3 (all nurse encounters, including subsequent),
      // completed +3 (2 initial + subsequent physical exam).
      const coreExam = findCompletionRow(completion, 'Core Exam')!;
      const baseCoreExam = findCompletionRow(baselineCompletion, 'Core Exam')!;
      expect(coreExam.expected, 'Core Exam expected +3').toBe(baseCoreExam.expected + 3);
      expect(coreExam.completed, 'Core Exam completed +3').toBe(baseCoreExam.completed + 3);

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

      // FollowUp: expected +3, completed +3 (2 initial malaria + 1 subsequent).
      const followUp = findCompletionRow(completion, 'Follow Up')!;
      const baseFollowUp = findCompletionRow(baselineCompletion, 'Follow Up')!;
      expect(followUp.expected, 'Follow Up expected +3').toBe(baseFollowUp.expected + 3);
      expect(followUp.completed, 'Follow Up completed +3').toBe(baseFollowUp.completed + 3);

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

      // HealthEducation: expected +1 (subsequent, no malaria at current encounter),
      // completed +1.
      const healthEd = findCompletionRow(completion, 'Health Education')!;
      const baseHealthEd = findCompletionRow(baselineCompletion, 'Health Education')!;
      expect(healthEd.expected, 'Health Education expected +1').toBe(baseHealthEd.expected + 1);
      expect(healthEd.completed, 'Health Education completed +1').toBe(baseHealthEd.completed + 1);

      // Referral: expected +1 (subsequent, fever triggers send_to_hc),
      // completed +1.
      const referral = findCompletionRow(completion, 'Referral')!;
      const baseReferral = findCompletionRow(baselineCompletion, 'Referral')!;
      expect(referral.expected, 'Referral expected +1').toBe(baseReferral.expected + 1);
      expect(referral.completed, 'Referral completed +1').toBe(baseReferral.completed + 1);
    });

    await step('Verify Completion Report — AI Taken By filter (Nurse)', async () => {
      await selectCompletionTakenBy(page, 'nurse');
      const nurseData = await readCompletionTable(page, 'acute-illness');
      const baseCoreExam = findCompletionRow(baselineCompletion, 'Core Exam')!;
      const coreExam = findCompletionRow(nurseData, 'Core Exam')!;
      // 3 nurse encounters (2 initial + 1 subsequent) expect CoreExam.
      expect(coreExam.expected, 'AI Nurse: Core Exam expected +3').toBe(baseCoreExam.expected + 3);
      expect(coreExam.completed, 'AI Nurse: Core Exam completed +3').toBe(baseCoreExam.completed + 3);
    });

    await step('Verify Completion Report — AI Taken By filter (CHW)', async () => {
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

    await step('Verify Completion Report — Prenatal table renders', async () => {
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

      expect(prenatalCompletion.heading, 'Prenatal completion heading should contain Antenatal').toContain('Antenatal');
      expect(prenatalCompletion.rows.length, 'Should have 59 activity rows').toBe(59);
    });

    await step('Verify Completion Report — Prenatal activity deltas (Any role)', async () => {
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
      // Health Education: +2/+2 — CHW encounter (+1) and nurse NextSteps (+1,
      // triggered by HIV test result, completed before Wait/Pause).
      assertDelta('Health Education', 2, 2);
      assertDelta('Follow Up', 1, 1);

      // Lab test results (all tests performed at point of care).
      assertDelta('HIV Test Result', 1, 1);
      assertDelta('Malaria Test Result', 1, 1);
      assertDelta('Syphilis Test Result', 1, 1);
      assertDelta('Hemoglobin Test Result', 1, 1);
      assertDelta('Hepatitis B Test Result', 1, 1);
      assertDelta('Urine Dipstick Test Result', 1, 1);
      assertDelta('Random Blood Sugar Test Result', 1, 1);
      assertDelta('Blood Group and Rhesus Test Result', 1, 1);
      assertDelta('Partner HIV Test Result', 1, 1);

      // Individual medication activities: expected and completed at nurse initial.
      // Requires hedley_prenatal_change_medications variable to be set.
      assertDelta('Iron', 1, 1);
      assertDelta('Folate', 1, 1);
      assertDelta('MMS', 1, 1);
      assertDelta('Mebendazole', 1, 1);       // EGA 30 >= 24w
      assertDelta('Calcium', 1, 1);            // EGA 30 >= 14w

      // Medication Distribution: expected (HIV diagnosis + hypertension),
      // completed before Wait/Pause in NextSteps.
      assertDelta('Medication Distribution', 1, 1);
      // Referral: expected on recurrent encounter (after lab results),
      // completed via NextSteps on that encounter.
      assertDelta('Referral', 1, 1);

      // Expected but not completed (expected +1, completed +0).
      assertDelta('Photo', 1, 0);
      assertDelta('Social History', 0, 0);  // Disabled since Sep 2024 (#1323)

      // Not expected / blocked / conditional.
      assertDelta('Fefol', 0, 0);           // Blocked by Iron+Folate distributed today
      assertDelta('Vitals Recheck', 0, 0);  // Only for borderline BP, ours is stage 2
      assertDelta('Medication', 0, 0);       // Legacy: no longer expected (variable set)
      assertDelta('Outside Care', 0, 0);    // Subsequent only
      assertDelta('Treatment Review', 0, 0);// Subsequent only
      assertDelta('Birth Plan', 0, 0);      // CHW-2 only
      assertDelta('HIV PCR Test', 0, 0);    // Subsequent only
      assertDelta('HIV PCR Test Result', 0, 0);
    });

    await step('Verify Completion Report — Prenatal Taken By filter (Nurse)', async () => {
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

    await step('Verify Completion Report — Prenatal Taken By filter (CHW)', async () => {
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

    await step('Verify Completion Report — Child Scoreboard table renders', async () => {
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

      expect(csCompletion.heading, 'Child Scoreboard completion heading should contain Child Scorecard').toContain('Child Scorecard');
      expect(csCompletion.rows.length, 'Should have 8 activity rows (Rwanda)').toBe(8);
    });

    await step('Verify Completion Report — Child Scoreboard activity deltas', async () => {
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

    // ── Phase 8: Completion Report — HIV ──
    //
    // HIVAdult (female 35yo, CHW) completed initial encounter:
    // Diagnostics (positive) + Medication + NextSteps (HealthEducation + FollowUp).
    // CHW-only module — no Taken By filter.

    let hivCompletion: CompletionTableData;

    await step('Verify Completion Report — HIV table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'hiv');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      hivCompletion = await readCompletionTable(page, 'hiv');

      console.log('\n=== COMPLETION: HIV ===');
      console.log('Heading:', hivCompletion.heading);
      console.log('Rows:', hivCompletion.rows.length);
      for (const row of hivCompletion.rows) {
        const base = findCompletionRow(baselineHIVCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(hivCompletion.heading, 'HIV completion heading should contain HIV').toContain('HIV');
      expect(hivCompletion.rows.length, 'Should have 7 activity rows').toBe(7);
    });

    await step('Verify Completion Report — HIV activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(hivCompletion, label)!;
        const base = findCompletionRow(baselineHIVCompletion, label)!;
        expect(row, `Row "${label}" should exist`).toBeDefined();
        expect(base, `Baseline row "${label}" should exist`).toBeDefined();
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(base.expected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(base.completed + completedDelta);
      };

      // Initial encounter activities (completed).
      assertDelta('Diagnostics', 1, 1);
      assertDelta('Medication', 1, 1);
      assertDelta('Treatment Review', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Follow Up', 1, 1);

      // Not expected for initial encounter.
      assertDelta('Symptoms Review', 0, 0);

      // Referral: triggered by adverse event (Rash or Itching) in TreatmentReview.
      assertDelta('Referral', 1, 1);
    });

    // ── Phase 9: Completion Report — Home Visit ──
    //
    // HVChild (8mo male, CHW) completed all 4 home visit activities:
    // Feeding, Caring, Hygiene, Food Security.
    // CHW-only module — no Taken By filter.

    let hvCompletion: CompletionTableData;

    await step('Verify Completion Report — Home Visit table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'home-visit');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      hvCompletion = await readCompletionTable(page, 'home-visit');

      console.log('\n=== COMPLETION: HOME VISIT ===');
      console.log('Heading:', hvCompletion.heading);
      console.log('Rows:', hvCompletion.rows.length);
      for (const row of hvCompletion.rows) {
        const base = findCompletionRow(baselineHVCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(20)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(hvCompletion.heading, 'Home Visit completion heading should contain Home Visit').toContain('Home Visit');
      expect(hvCompletion.rows.length, 'Should have 4 activity rows').toBe(4);
    });

    await step('Verify Completion Report — Home Visit activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(hvCompletion, label)!;
        const base = findCompletionRow(baselineHVCompletion, label)!;
        expect(row, `Row "${label}" should exist`).toBeDefined();
        expect(base, `Baseline row "${label}" should exist`).toBeDefined();
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(base.expected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(base.completed + completedDelta);
      };

      // All 4 activities mandatory and completed.
      assertDelta('Caring', 1, 1);
      assertDelta('Feeding', 1, 1);
      assertDelta('Food Security', 1, 1);
      assertDelta('Hygiene', 1, 1);
    });

    // ── Phase 10: Completion Report — NCD ──
    //
    // NCDAdult (male 40yo, Nurse) completed initial encounter:
    // DangerSigns, SymptomReview, Examination, MedicalHistory, Laboratory, NextSteps.
    // Nurse-only module — no Taken By filter.

    let ncdCompletion: CompletionTableData;

    await step('Verify Completion Report — NCD table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'ncd');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      ncdCompletion = await readCompletionTable(page, 'ncd');

      console.log('\n=== COMPLETION: NCD ===');
      console.log('Heading:', ncdCompletion.heading);
      console.log('Rows:', ncdCompletion.rows.length);
      for (const row of ncdCompletion.rows) {
        const base = findCompletionRow(baselineNCDCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        if (eDelta !== 0 || cDelta !== 0) {
          console.log(
            `${row.activity.padEnd(30)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
          );
        }
      }

      expect(ncdCompletion.heading, 'NCD completion heading should contain NCD').toContain('NCD');
      expect(ncdCompletion.rows.length, 'Should have 26 activity rows').toBe(26);
    });

    await step('Verify Completion Report — NCD activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(ncdCompletion, label)!;
        const base = findCompletionRow(baselineNCDCompletion, label)!;
        expect(row, `Row "${label}" should exist`).toBeDefined();
        expect(base, `Baseline row "${label}" should exist`).toBeDefined();
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(base.expected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(base.completed + completedDelta);
      };

      // Base activities (always expected for initial encounter).
      assertDelta('Danger Signs', 1, 1);
      assertDelta('Symptoms Review', 1, 1);
      assertDelta('Vitals', 1, 1);
      assertDelta('Core Exam', 1, 1);

      // History activities (first encounter only).
      assertDelta('Co-Morbidities', 1, 1);
      assertDelta('Medication History', 1, 1);
      assertDelta('Social History', 1, 1);
      assertDelta('Family History', 1, 1);
      assertDelta('Outside Care', 1, 1);

      // Labs (first encounter, never tested → all expected).
      assertDelta('Random Blood Sugar Test', 1, 1);
      assertDelta('Creatinine Test', 1, 1);
      assertDelta('HBA1C Test', 1, 1);
      assertDelta('HIV Test', 1, 1);
      assertDelta('Lipid Panel Test', 1, 1);
      assertDelta('Liver Function Test', 1, 1);
      assertDelta('Urine Dipstick Test', 1, 1);

      // Female patient (age 40, within 13-44 range).
      assertDelta('Family Planning', 1, 1);
      assertDelta('Pregnancy Test', 1, 1);

      // Test results: labs sent to lab, results entered via recurrent encounter.
      assertDelta('Creatinine Result', 1, 1);
      assertDelta('Lipid Panel Result', 1, 1);
      assertDelta('Liver Function Result', 1, 1);
      assertDelta('Random Blood Sugar Test Result', 1, 1);
      assertDelta('Urine Dipstick Test Result', 1, 1);

      // Recurrent NextSteps: expected and completed on recurrent encounter.
      assertDelta('Medication Distribution', 1, 1);
      assertDelta('Referral', 1, 1);
    });

    // ── Phase 11: Completion Report — Newborn Exam ──
    //
    // NBChild (1mo male, CHW) completed Newborn Exam:
    // PregnancySummary, NutritionAssessment (HeadCircumference + Weight),
    // Immunisation, NextSteps (HealthEducation + SendToHC).
    // CHW-only module — no Taken By filter.

    let nbCompletion: CompletionTableData;

    await step('Verify Completion Report — Newborn Exam table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'newborn-exam');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      // Newborn Exam uses same CSS class "report well-child" as SPV.
      nbCompletion = await readCompletionTable(page, 'well-child');

      console.log('\n=== COMPLETION: NEWBORN EXAM ===');
      console.log('Heading:', nbCompletion.heading);
      console.log('Rows:', nbCompletion.rows.length);
      for (const row of nbCompletion.rows) {
        const base = findCompletionRow(baselineNBCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(nbCompletion.heading, 'Newborn Exam completion heading should contain Newborn Exam').toContain('Newborn Exam');
      expect(nbCompletion.rows.length, 'Should have 11 activity rows').toBe(11);
    });

    await step('Verify Completion Report — Newborn Exam activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(nbCompletion, label)!;
        const base = findCompletionRow(baselineNBCompletion, label);
        expect(row, `Row "${label}" should exist`).toBeDefined();
        const baseExpected = base?.expected ?? 0;
        const baseCompleted = base?.completed ?? 0;
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(baseExpected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(baseCompleted + completedDelta);
      };

      // Core activities completed.
      assertDelta('Pregnancy Summary', 1, 1);
      assertDelta('Weight', 1, 1);
      assertDelta('Head Circumference', 1, 1);
      assertDelta('Nutrition', 1, 1);

      // NextSteps completed (triggered by abnormal nutrition signs: Edema).
      assertDelta('Contributing Factors', 1, 1);
      assertDelta('Follow Up', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Referral', 1, 1);

      // Photo: expected but not completed (no upload helper).
      assertDelta('Photo', 1, 0);
    });

    // ── Phase 12: Completion Report — Well Child (SPV) ──
    //
    // NutrChild (10mo male, Nurse) completed SPV encounter:
    // DangerSigns, NutritionAssessment, ECD, Medication, Immunisation, NCDA, NextSteps.
    // Supports Taken By filter (Nurse + CHW).

    let spvCompletion: CompletionTableData;

    await step('Verify Completion Report — Well Child SPV table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'well-child');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      spvCompletion = await readCompletionTable(page, 'well-child');

      console.log('\n=== COMPLETION: WELL CHILD SPV ===');
      console.log('Heading:', spvCompletion.heading);
      console.log('Rows:', spvCompletion.rows.length);
      for (const row of spvCompletion.rows) {
        const base = findCompletionRow(baselineSPVCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        if (eDelta !== 0 || cDelta !== 0) {
          console.log(
            `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
          );
        }
      }

      expect(spvCompletion.rows.length, 'Should have 29 activity rows (Rwanda)').toBe(29);
    });

    await step('Verify Completion Report — Well Child SPV activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(spvCompletion, label)!;
        const base = findCompletionRow(baselineSPVCompletion, label);
        expect(row, `Row "${label}" should exist`).toBeDefined();
        const baseExpected = base?.expected ?? 0;
        const baseCompleted = base?.completed ?? 0;
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(baseExpected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(baseCompleted + completedDelta);
      };

      // DangerSigns = SymptomsReview + Vitals (nurse + CHW).
      assertDelta('Symptoms Review', 2, 2);
      assertDelta('Vitals', 2, 2);

      // NutritionAssessment sub-tasks (nurse + CHW).
      assertDelta('Height', 2, 2);
      assertDelta('Head Circumference', 2, 2);  // age 10mo < 36mo
      assertDelta('MUAC', 2, 2);                // age 10mo >= 6mo
      assertDelta('Nutrition', 2, 2);
      assertDelta('Weight', 2, 2);

      // Nurse-only activities (unchanged).
      assertDelta('ECD', 1, 1);
      assertDelta('NCDA', 1, 1);  // age 10mo < 24mo

      // Photo: expected on both encounters but not completed.
      assertDelta('Photo', 2, 0);

      // CHW HomeVisit activities (+1/+1 from CHW encounter).
      assertDelta('Caring', 1, 1);
      assertDelta('Feeding', 1, 1);
      assertDelta('Food Security', 1, 1);
      assertDelta('Hygiene', 1, 1);

      // NextSteps triggered by abnormal nutrition on CHW encounter.
      assertDelta('Contributing Factors', 1, 1);
      assertDelta('Follow Up', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Referral', 1, 1);

      // HPV: not expected at 10mo (female 12yr+ only).
      assertDelta('HPV Immunisation', 0, 0);
    });

    await step('Verify Completion Report — SPV Taken By filter (Nurse)', async () => {
      await selectCompletionTakenBy(page, 'nurse');
      const nurseData = await readCompletionTable(page, 'well-child');

      // Nurse encounter has Vitals.
      const baseVitals = findCompletionRow(baselineSPVCompletion, 'Vitals');
      const vitals = findCompletionRow(nurseData, 'Vitals')!;
      expect(vitals.expected, 'SPV Nurse: Vitals expected +1').toBe((baseVitals?.expected ?? 0) + 1);
      expect(vitals.completed, 'SPV Nurse: Vitals completed +1').toBe((baseVitals?.completed ?? 0) + 1);
    });

    await step('Verify Completion Report — SPV Taken By filter (CHW)', async () => {
      await selectCompletionTakenBy(page, 'chw');
      const chwData = await readCompletionTable(page, 'well-child');

      // CHW encounter has Caring (CHW-only).
      const caring = findCompletionRow(chwData, 'Caring')!;
      expect(caring.expected, 'SPV CHW: Caring expected +1').toBe(1);
      expect(caring.completed, 'SPV CHW: Caring completed +1').toBe(1);

      // ECD is nurse-only — CHW filter should show 0.
      const ecd = findCompletionRow(chwData, 'ECD')!;
      expect(ecd.expected, 'SPV CHW: ECD expected 0').toBe(0);
    });

    // ── Phase 13: Completion Report — Tuberculosis ──
    //
    // TBAdult (male 45yo, CHW) completed initial encounter:
    // Diagnostics (positive) + Medication (DOT + TreatmentReview) + NextSteps.
    // CHW-only module — no Taken By filter.

    let tbCompletion: CompletionTableData;

    await step('Verify Completion Report — Tuberculosis table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'tuberculosis');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      tbCompletion = await readCompletionTable(page, 'tuberculosis');

      console.log('\n=== COMPLETION: TUBERCULOSIS ===');
      console.log('Heading:', tbCompletion.heading);
      console.log('Rows:', tbCompletion.rows.length);
      for (const row of tbCompletion.rows) {
        const base = findCompletionRow(baselineTBCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(tbCompletion.heading, 'Tuberculosis completion heading should contain Tuberculosis').toContain('Tuberculosis');
      expect(tbCompletion.rows.length, 'Should have 8 activity rows').toBe(8);
    });

    await step('Verify Completion Report — Tuberculosis activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(tbCompletion, label)!;
        const base = findCompletionRow(baselineTBCompletion, label);
        expect(row, `Row "${label}" should exist`).toBeDefined();
        const baseExpected = base?.expected ?? 0;
        const baseCompleted = base?.completed ?? 0;
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(baseExpected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(baseCompleted + completedDelta);
      };

      // Initial encounter activities (completed).
      assertDelta('Diagnostics', 1, 1);
      assertDelta('Medication', 1, 1);
      assertDelta('DOT', 1, 1);
      assertDelta('Treatment Review', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Follow Up', 1, 1);

      // Not expected for initial encounter.
      assertDelta('Symptoms Review', 0, 0);

      // Referral: triggered by adverse events (sideEffects: true in Medication).
      assertDelta('Referral', 1, 1);
    });

    // ── Phase 14: Completion Report — Nutrition Individual ──
    //
    // NutrChild (10mo male, Nurse): Height, Weight, MUAC, Nutrition (Edema),
    //   NCDA, ContributingFactors, FollowUp, HealthEducation, SendToHC.
    // HVChild (8mo male, CHW): Weight, MUAC, Nutrition (None).
    // Supports Taken By filter (Nurse + CHW).

    let nutrIndCompletion: CompletionTableData;

    await step('Verify Completion Report — Nutrition Individual table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'nutrition-individual');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      nutrIndCompletion = await readCompletionTable(page, 'nutrition-individual');

      console.log('\n=== COMPLETION: NUTRITION INDIVIDUAL ===');
      console.log('Heading:', nutrIndCompletion.heading);
      console.log('Rows:', nutrIndCompletion.rows.length);
      for (const row of nutrIndCompletion.rows) {
        const base = findCompletionRow(baselineNutrIndCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        console.log(
          `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
        );
      }

      expect(nutrIndCompletion.rows.length, 'Should have 10 activity rows').toBe(10);
    });

    await step('Verify Completion Report — Nutrition Individual activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(nutrIndCompletion, label)!;
        const base = findCompletionRow(baselineNutrIndCompletion, label);
        expect(row, `Row "${label}" should exist`).toBeDefined();
        const baseExpected = base?.expected ?? 0;
        const baseCompleted = base?.completed ?? 0;
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(baseExpected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(baseCompleted + completedDelta);
      };

      // Both encounters: always expected.
      assertDelta('Weight', 2, 2);
      assertDelta('MUAC', 2, 2);
      assertDelta('Nutrition', 2, 2);

      // Height: both expected, but CHW Rwanda doesn't complete it.
      assertDelta('Height', 2, 1);

      // Photo: both expected, neither completed.
      assertDelta('Photo', 2, 0);

      // Nurse only: NCDA (age < 24mo).
      assertDelta('NCDA', 1, 1);

      // NextSteps: expected +2 (both encounters have abnormal z-scores),
      // completed +1 (only NutrChild completes them).
      assertDelta('Contributing Factors', 2, 1);
      assertDelta('Follow Up', 2, 1);
      assertDelta('Health Education', 2, 1);
      assertDelta('Referral', 2, 1);
    });

    await step('Verify Completion Report — Nutrition Individual Taken By (Nurse)', async () => {
      await selectCompletionTakenBy(page, 'nurse');
      const nurseData = await readCompletionTable(page, 'nutrition-individual');

      const baseNCDA = findCompletionRow(baselineNutrIndCompletion, 'NCDA');
      const ncda = findCompletionRow(nurseData, 'NCDA')!;
      expect(ncda.expected, 'NutrInd Nurse: NCDA expected +1').toBe((baseNCDA?.expected ?? 0) + 1);
      expect(ncda.completed, 'NutrInd Nurse: NCDA completed +1').toBe((baseNCDA?.completed ?? 0) + 1);
    });

    await step('Verify Completion Report — Nutrition Individual Taken By (CHW)', async () => {
      await selectCompletionTakenBy(page, 'chw');
      const chwData = await readCompletionTable(page, 'nutrition-individual');

      // NCDA is nurse-only — CHW should show 0.
      const ncda = findCompletionRow(chwData, 'NCDA')!;
      expect(ncda.expected, 'NutrInd CHW: NCDA expected 0').toBe(0);
    });

    // ── Phase 15: Completion Report — Nutrition Group ──
    //
    // FBF group session (Nurse): Mother (FamilyPlanning, Lactation, MotherFbf)
    // + Child (Height, Weight, MUAC, Nutrition abnormal, ChildFbf, NCDA, NextSteps).
    // Supports Taken By filter (Nurse + CHW).

    let nutrGrpCompletion: CompletionTableData;

    await step('Verify Completion Report — Nutrition Group table renders', async () => {
      await navigateToCompletionReportPage(page, NYANGE_HC_ID);
      await selectCompletionReportType(page, 'nutrition-group');
      await setCompletionDateRange(page, REPORT_START_DATE, reportLimitDate);

      nutrGrpCompletion = await readCompletionTable(page, 'nutrition-group');

      console.log('\n=== COMPLETION: NUTRITION GROUP ===');
      console.log('Heading:', nutrGrpCompletion.heading);
      console.log('Rows:', nutrGrpCompletion.rows.length);
      for (const row of nutrGrpCompletion.rows) {
        const base = findCompletionRow(baselineNutrGrpCompletion, row.activity);
        const eDelta = row.expected - (base?.expected ?? 0);
        const cDelta = row.completed - (base?.completed ?? 0);
        if (eDelta !== 0 || cDelta !== 0) {
          console.log(
            `${row.activity.padEnd(25)} | E: ${String(base?.expected ?? 0).padEnd(3)}→${String(row.expected).padEnd(3)} (+${eDelta}) | C: ${String(base?.completed ?? 0).padEnd(3)}→${String(row.completed).padEnd(3)} (+${cDelta}) | ${row.percent}`,
          );
        }
      }

      expect(nutrGrpCompletion.rows.length, 'Should have 14 activity rows').toBe(14);
    });

    await step('Verify Completion Report — Nutrition Group activity deltas', async () => {
      const assertDelta = (label: string, expectedDelta: number, completedDelta: number) => {
        const row = findCompletionRow(nutrGrpCompletion, label)!;
        const base = findCompletionRow(baselineNutrGrpCompletion, label);
        expect(row, `Row "${label}" should exist`).toBeDefined();
        const baseExpected = base?.expected ?? 0;
        const baseCompleted = base?.completed ?? 0;
        expect(row.expected, `${label} expected +${expectedDelta}`).toBe(baseExpected + expectedDelta);
        expect(row.completed, `${label} completed +${completedDelta}`).toBe(baseCompleted + completedDelta);
      };

      // Mother activities.
      assertDelta('Family Planning', 1, 1);
      assertDelta('Lactation', 1, 1);
      // Mother FBF: conditional on breastfeeding AND ubudehe 1/2 — not triggered.
      assertDelta('Mother FBF', 0, 0);

      // Child: always expected.
      assertDelta('Height', 1, 1);
      assertDelta('Weight', 1, 1);
      assertDelta('Nutrition', 1, 1);
      assertDelta('MUAC', 1, 1);

      // Photo: expected but not completed.
      assertDelta('Photo', 1, 0);

      // FBF-specific.
      assertDelta('Child FBF', 1, 1);

      // Nurse + age < 24mo.
      assertDelta('NCDA', 1, 1);

      // NextSteps (triggered by abnormal nutrition signs).
      assertDelta('Contributing Factors', 1, 1);
      assertDelta('Follow Up', 1, 1);
      assertDelta('Health Education', 1, 1);
      assertDelta('Referral', 1, 1);
    });

    // ── Phase 16: NCDA Scoreboard verification ──

    await step('Regenerate NCDA data with new encounters', async () => {
      // Backdate created timestamps so children pass the Elm scoreboard's
      // existedDuringExaminationMonth check (strict LT against current date).
      backdatePersonCreated(csChildName);
      backdatePersonCreated(nutrChildName);
      backdatePersonCreated(hvChildName);
      backdatePersonCreated(fbfChildName);
      backdatePersonCreated(nbChildName);

      // Regenerate NCDA aggregated data and clear caches.
      generateNCDAPersonData(true);
      ncdaRecalculateLargeDatasets();
    });

    await step('Navigate to NCDA Scoreboard — village level (Akanduga)', async () => {
      // Drupal admin login (if session expired during completion phases).
      const currentUrl = page.url();
      if (!currentUrl.includes('/admin/')) {
        await drupalLogin(page);
      }

      await navigateToNCDAScoreboard(page, 'Amajyaruguru/Gakenke/Coko/Mbirima/Akanduga');
    });

    await step('Verify village scoreboard deltas', async () => {
      const monthIdx = getCurrentMonthColumnIndex();

      // Read panes by heading (robust against reordering).
      const villageDemographics = await readScoreboardPane(page, 'Demographics');
      const villageAcuteMalnutrition = await readScoreboardPane(page, 'Acute Malnutrition');
      const villageStunting = await readScoreboardPane(page, 'Stunting');
      const villageUniversal = await readScoreboardPane(page, 'Universal Intervention');
      const villageNutritionBehavior = await readScoreboardPane(page, 'Nutrition Behavior');
      const villageTargeted = await readScoreboardPane(page, 'Targeted Interventions');
      const villageInfrastructure = await readScoreboardPane(page, 'Infrastructure');

      // assertMinDelta: verify delta >= expected (multiple children may contribute).
      const assertMinDelta = (
        pane: ScoreboardPaneData, baseline: ScoreboardPaneData,
        rowLabel: string, minDelta: number,
      ) => {
        const current = findScoreboardValue(pane, rowLabel, monthIdx);
        const base = findScoreboardValue(baseline, rowLabel, monthIdx);
        const delta = current - base;
        console.log(`${rowLabel}: ${base} → ${current} (delta: ${delta}, expected: >=${minDelta})`);
        expect(delta, `${rowLabel} delta >= ${minDelta}`).toBeGreaterThanOrEqual(minDelta);
      };

      // assertExactDelta: verify delta === expected (CSChild is the only child
      // in Akanduga with NCDA data — CHW-created children skip NCDA).
      const assertExactDelta = (
        pane: ScoreboardPaneData, baseline: ScoreboardPaneData,
        rowLabel: string, expectedDelta: number,
      ) => {
        const current = findScoreboardValue(pane, rowLabel, monthIdx);
        const base = findScoreboardValue(baseline, rowLabel, monthIdx);
        const delta = current - base;
        console.log(`${rowLabel}: ${base} → ${current} (delta: ${delta}, expected: ${expectedDelta})`);
        expect(delta, `${rowLabel} delta`).toBe(expectedDelta);
      };

      // Demographics: multiple CHW-created children under 2 in Akanduga.
      assertMinDelta(villageDemographics, baselineVillage['Demographics'],
        'Children under 2', 1);

      // Nutrition severity panes.
      assertMinDelta(villageAcuteMalnutrition, baselineVillage['Acute Malnutrition'],
        'Moderate Acute Malnutrition', 1);
      assertMinDelta(villageStunting, baselineVillage['Stunting'],
        'No Stunting', 1);

      // Universal Interventions — CSChild answered Yes to Vitamin A, Deworming,
      // Ongera MNP; deliberately No to ECD.
      assertExactDelta(villageUniversal, baselineVillage['Universal Intervention'],
        'Vitamin A', 1);
      assertExactDelta(villageUniversal, baselineVillage['Universal Intervention'],
        'Deworming', 1);
      assertExactDelta(villageUniversal, baselineVillage['Universal Intervention'],
        'Ongera', 1);
      assertExactDelta(villageUniversal, baselineVillage['Universal Intervention'],
        'ECD', 0);

      // Nutrition Behavior — CSChild answered Yes to complementary feeding and
      // diverse diet; deliberately No to meals. Breastfed skipped (age gate < 6mo).
      assertExactDelta(villageNutritionBehavior, baselineVillage['Nutrition Behavior'],
        'complementary feeding', 1);
      assertExactDelta(villageNutritionBehavior, baselineVillage['Nutrition Behavior'],
        'Diverse diet', 1);
      assertExactDelta(villageNutritionBehavior, baselineVillage['Nutrition Behavior'],
        'frequency of meals', 0);

      // Targeted Interventions — CSChild answered Yes to FBF, malnutrition
      // treatment, support; deliberately No to cash transfer and food items.
      assertExactDelta(villageTargeted, baselineVillage['Targeted Interventions'],
        'FBF', 1);
      assertMinDelta(villageTargeted, baselineVillage['Targeted Interventions'],
        'Treatment for acute malnutrition', 1);
      assertExactDelta(villageTargeted, baselineVillage['Targeted Interventions'],
        'support to a child', 1);
      assertExactDelta(villageTargeted, baselineVillage['Targeted Interventions'],
        'conditional cash transfer', 0);
      assertExactDelta(villageTargeted, baselineVillage['Targeted Interventions'],
        'conditional food items', 0);

      // Infrastructure/WASH — CSChild answered Yes to clean water, toilets,
      // handwashing; deliberately No to bed nets and kitchen garden.
      assertExactDelta(villageInfrastructure, baselineVillage['Infrastructure'],
        'clean water', 1);
      assertExactDelta(villageInfrastructure, baselineVillage['Infrastructure'],
        'toilets', 1);
      assertExactDelta(villageInfrastructure, baselineVillage['Infrastructure'],
        'handwashing', 1);
      assertExactDelta(villageInfrastructure, baselineVillage['Infrastructure'],
        'bed nets', 0);
      assertExactDelta(villageInfrastructure, baselineVillage['Infrastructure'],
        'kitchen garden', 0);
    });

    await step('Verify NCDA Scoreboard — district level (Gakenke)', async () => {
      await navigateToNCDAScoreboard(page, 'Amajyaruguru/Gakenke');

      // Structural check: all 9 panes render.
      const paneCount = await page.locator('.pane').count();
      expect(paneCount, 'Should have 9 panes (entity info + 8 data panes)').toBe(9);

      // Demographics: Children Under 2 should have increased.
      const districtDemographics = await readScoreboardPane(page, 'Demographics');
      const monthIdx = getCurrentMonthColumnIndex();
      const districtBaseline = findScoreboardValue(baselineDistrictDemographics, 'Children under 2', monthIdx);
      const districtCurrent = findScoreboardValue(districtDemographics, 'Children under 2', monthIdx);
      console.log(`District Children under 2: ${districtBaseline} → ${districtCurrent}`);
      expect(districtCurrent - districtBaseline, 'District children under 2 delta >= 2').toBeGreaterThanOrEqual(2);

      // Verify entity name.
      const entityPane = await readScoreboardPane(page, 'Aggregated Child Scoreboard');
      expect(entityPane.heading, 'NCDA Scoreboard entity heading should contain Aggregated Child Scoreboard').toContain('Aggregated Child Scoreboard');
    });
  });
});
