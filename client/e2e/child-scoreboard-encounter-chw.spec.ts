import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createChildAndStartEncounter,
  completeNCDA,
  completeVaccinationHistory,
  endChildScoreboardEncounter,
  syncAndWait,
  queryChildScoreboardNodes,
} from './helpers/child-scoreboard';

// =========================================================================
// Test 1: CHW First Child Scoreboard Encounter — NCDA + Vaccination History
// =========================================================================

test.describe('CHW: Child Scoreboard Encounter — First NCDA + Vaccination History', () => {
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

  // Scenario: First encounter for a 10-month-old male child with healthy values.
  // Activities: NCDA (6 steps) + VaccinationHistory (triggered by answering "No"
  //             to ChildBehindOnVaccination in NCDA, contradicting E-Heza's records).
  // Conditions: First NCDA → AntenatalCare step shown. Child 10 months → NutritionBehavior shown.
  //             Normal MUAC (14.0) → TreatedForAcuteMalnutrition NOT shown. No diarrhea → no popup.
  //             ChildBehindOnVaccination → "No" triggers VaccinationHistory activity.
  // Backend: Verifies child_scoreboard_ncda + 7 vaccination nodes created,
  //          confirms child_scoreboard_dtp_sa_iz absent (Burundi-only).
  test('complete NCDA and vaccination history, verify backend sync', async ({ page }) => {
    // 1. Register a 10-month-old child and start the encounter.
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 10,
    });

    // 2. Complete NCDA activity (all 6 steps with healthy values).
    //    ChildBehindOnVaccination answered "No" to trigger VaccinationHistory.
    await completeNCDA(page);

    // 3. Complete VaccinationHistory activity (answer "No" to prior doses for each vaccine).
    await completeVaccinationHistory(page);

    // 4. End encounter (no diarrhea popup since we answered No).
    await endChildScoreboardEncounter(page);

    // 5. Sync to backend.
    await syncAndWait(page);

    // 6. Verify backend nodes.
    // For a 10-month-old male on Rwanda site with no vaccination history,
    // all 7 common vaccines are overdue (BCG, OPV, DTP, PCV13, Rotarix, IPV, MR).
    // DTPStandalone is Burundi-only, so absent on Rwanda.
    const expectedTypes = [
      'child_scoreboard_ncda',
      'child_scoreboard_bcg_iz',
      'child_scoreboard_opv_iz',
      'child_scoreboard_dtp_iz',
      'child_scoreboard_pcv13_iz',
      'child_scoreboard_rotarix_iz',
      'child_scoreboard_ipv_iz',
      'child_scoreboard_mr_iz',
    ];
    const nodes = queryChildScoreboardNodes(fullName, expectedTypes);

    expect(nodes['child_scoreboard_ncda']).toBe(true);
    expect(nodes['child_scoreboard_bcg_iz']).toBe(true);
    expect(nodes['child_scoreboard_opv_iz']).toBe(true);
    expect(nodes['child_scoreboard_dtp_iz']).toBe(true);
    expect(nodes['child_scoreboard_pcv13_iz']).toBe(true);
    expect(nodes['child_scoreboard_rotarix_iz']).toBe(true);
    expect(nodes['child_scoreboard_ipv_iz']).toBe(true);
    expect(nodes['child_scoreboard_mr_iz']).toBe(true);
    // DTPStandalone is Burundi-only, not present on Rwanda site.
    expect(nodes['child_scoreboard_dtp_sa_iz']).toBe(false);
  });
});
