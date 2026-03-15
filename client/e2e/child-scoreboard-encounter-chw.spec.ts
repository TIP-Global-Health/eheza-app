import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createChildAndStartEncounter,
  completeNCDA,
  endChildScoreboardEncounter,
  syncAndWait,
  queryChildScoreboardNodes,
} from './helpers/child-scoreboard';

// =========================================================================
// Test 1: CHW First Child Scoreboard Encounter — Complete NCDA
// =========================================================================

test.describe('CHW: Child Scoreboard Encounter — First NCDA', () => {
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

  // Scenario: First NCDA encounter for a 10-month-old male child with all healthy values.
  // Activities: NCDA (6 steps: AntenatalCare, UniversalInterventions, NutritionBehavior,
  //             NutritionAssessment, TargetedInterventions, InfrastructureEnvironment).
  // Conditions: First NCDA → AntenatalCare step shown. Child 10 months → NutritionBehavior shown.
  //             Normal MUAC (14.0) → TreatedForAcuteMalnutrition NOT shown. No diarrhea → no popup.
  //             VaccinationHistory NOT triggered (healthy values).
  // Backend: Verifies child_scoreboard_ncda created, all 8 vaccination node types absent.
  test('complete first NCDA encounter for healthy child, verify backend sync', async ({ page }) => {
    // 1. Register a 10-month-old child and start the encounter.
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 10,
    });

    // 2. Complete NCDA activity (all 6 steps with healthy values).
    await completeNCDA(page);

    // 3. End encounter (no diarrhea popup since we answered No).
    await endChildScoreboardEncounter(page);

    // 4. Sync to backend.
    await syncAndWait(page);

    // 5. Verify backend nodes.
    const expectedTypes = [
      'child_scoreboard_ncda',
    ];
    const nodes = queryChildScoreboardNodes(fullName, expectedTypes);

    // NCDA node should be created.
    expect(nodes['child_scoreboard_ncda']).toBe(true);
    // No vaccination nodes since VaccinationHistory activity was not triggered.
    expect(nodes['child_scoreboard_bcg_iz']).toBe(false);
    expect(nodes['child_scoreboard_dtp_iz']).toBe(false);
    expect(nodes['child_scoreboard_dtp_sa_iz']).toBe(false);
    expect(nodes['child_scoreboard_ipv_iz']).toBe(false);
    expect(nodes['child_scoreboard_mr_iz']).toBe(false);
    expect(nodes['child_scoreboard_opv_iz']).toBe(false);
    expect(nodes['child_scoreboard_pcv13_iz']).toBe(false);
    expect(nodes['child_scoreboard_rotarix_iz']).toBe(false);
  });
});
