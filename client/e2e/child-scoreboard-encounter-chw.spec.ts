import { openReport, closeReport } from './helpers/progress-report';
import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import {
  createChildAndStartEncounter,
  completeNCDA,
  queryNCDAWeight,
  reopenNCDAAndSayWeightNotTaken,
  completeVaccinationHistory,
  endChildScoreboardEncounter,
  queryChildScoreboardNodes,
  endEncounterDialog,
  diarrheaReferralPopup,
} from './helpers/child-scoreboard';
import { click } from './helpers/auth';

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
  // Also verifies (issue #1981): birth weight is recorded in grams, so a weight
  //             typed in kilograms (3) is refused on the AntenatalCare step —
  //             the warning shows, the form is not left, and only after the
  //             weight is entered in grams (3200) does the form go on.
  // And (issue #2005): the same for the weight and MUAC on the NutritionAssessment
  //             step — the weight is held in kilograms and the MUAC in
  //             centimetres, so 8500 and 120 are refused and named.
  // Backend: Verifies child_scoreboard_ncda + 7 vaccination nodes created,
  //          confirms child_scoreboard_dtp_sa_iz absent (Burundi-only).
  test('complete NCDA and vaccination history, verify backend sync', async ({ page }) => {
    // 1. Register a 10-month-old child and start the encounter.
    const { fullName } = await createChildAndStartEncounter(page, {
      ageMonths: 10,
    });

    // 2. Complete NCDA activity (all 6 steps with healthy values).
    //    ChildBehindOnVaccination answered "No" to trigger VaccinationHistory.
    await completeNCDA(page, { expectMuacAsked: true });

    // 3. Reopen the scorecard and say the weight could not be taken. The form
    //    is saved only at the end of its steps, so this checks the weight saved
    //    a moment ago does not come back and get written again (#2004).
    await reopenNCDAAndSayWeightNotTaken(page);

    // 4. Complete VaccinationHistory activity (answer "No" to prior doses for each vaccine).
    await completeVaccinationHistory(page);

    // 5. End encounter (no diarrhea popup since we answered No).
    // Progress report must show what this encounter recorded.
    const report = await openReport(page, 'child-scoreboard');
    await expect(report.locator('.pane.person-details')).toContainText(fullName);
    await expect(report.locator('.pane.vaccination-history')).toBeVisible();
    await closeReport(page, 'child-scoreboard');

    await endChildScoreboardEncounter(page);

    // 6. Sync to backend.
    await syncAndWait(page);

    // 7. Verify backend nodes.
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

    expect(nodes['child_scoreboard_ncda'], 'child_scoreboard_ncda should exist').toBe(true);

    // The weight was saved, then said not to have been taken: it should be gone
    // rather than written again from what the form was holding.
    expect(queryNCDAWeight(fullName), 'the weight should not be kept').toBeNull();
    expect(nodes['child_scoreboard_bcg_iz'], 'child_scoreboard_bcg_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_opv_iz'], 'child_scoreboard_opv_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_dtp_iz'], 'child_scoreboard_dtp_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_pcv13_iz'], 'child_scoreboard_pcv13_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_rotarix_iz'], 'child_scoreboard_rotarix_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_ipv_iz'], 'child_scoreboard_ipv_iz should exist').toBe(true);
    expect(nodes['child_scoreboard_mr_iz'], 'child_scoreboard_mr_iz should exist').toBe(true);
    // DTPStandalone is Burundi-only, not present on Rwanda site.
    expect(nodes['child_scoreboard_dtp_sa_iz'], 'child_scoreboard_dtp_sa_iz should not exist').toBe(false);
  });
});

test.describe('CHW: Child Scoreboard Encounter — child under six months', () => {
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

  // Scenario: first encounter for a 3-month-old, who is not asked for a MUAC.
  // Conditions: under six months → neither the NutritionBehavior step nor the
  //             MUAC input is part of the form.
  // The regression #1983 documents: the range check must ask only for what the
  // form shows. A MUAC that is checked but never drawn leaves the nurse with a
  // button that will not answer and no input to correct — three earlier attempts
  // were defeated this way. The weight, which is asked at every age, is still
  // refused when it is out of range, so this is not passing by checking nothing.
  test('is not asked for a MUAC, and still saves', async ({ page }) => {
    const { fullName } = await createChildAndStartEncounter(page, { ageMonths: 3 });

    // Asked on the step that draws the MUAC, since that is the only place the
    // absence means anything.
    await completeNCDA(page, { expectMuacAsked: false });

    // Answering the vaccination question on the NCDA asks for this too, and the
    // encounter will not end while it is still pending.
    await completeVaccinationHistory(page);
    // Progress report must show what this encounter recorded.
    const report = await openReport(page, 'child-scoreboard');
    await expect(report.locator('.pane.person-details')).toContainText(fullName);
    await expect(report.locator('.pane.vaccination-history')).toBeVisible();
    await closeReport(page, 'child-scoreboard');

    await endChildScoreboardEncounter(page);
    await syncAndWait(page);

    const nodes = queryChildScoreboardNodes(fullName, ['child_scoreboard_ncda']);
    expect(nodes['child_scoreboard_ncda'], 'child_scoreboard_ncda should exist').toBe(true);
    expect(queryNCDAWeight(fullName), 'the weight entered should be saved').toBe(8.5);
  });
});

// =========================================================================
// Test 3: Ending the encounter from the progress report
// =========================================================================

test.describe('CHW: Child Scoreboard Encounter — ending it from the progress report', () => {
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

  // Scenario: the encounter is ended from the progress report rather than from
  // the encounter page, with diarrhea recorded on the NCDA questionnaire.
  //
  // Covers issue #2062, both halves, in one encounter:
  //   1. Cancel on the confirmation dialog leaves the report open. Before the
  //      fix the report passed "always ShowAIEncounterPopup" for that dialog,
  //      so Cancel set the flag the dialog's own visibility reads and re-opened
  //      it - confirming was the only way out.
  //   2. Confirming refers the child on. Before the fix the report never asked
  //      whether diarrhea was recorded, so the same child was referred or not
  //      depending on which screen the encounter was ended from; and the
  //      referral itself navigated to the PIN code page, because the message
  //      that closed the encounter appended a navigation of its own after it.
  test('cancel keeps the report open, and confirming refers a child with diarrhea', async ({
    page,
  }) => {
    // 1. A child with diarrhea recorded, so ending the encounter should refer.
    await createChildAndStartEncounter(page, { ageMonths: 10 });
    await completeNCDA(page, { expectMuacAsked: true, diarrhea: 'Yes' });
    await completeVaccinationHistory(page);

    // 2. End the encounter from the report, not from the encounter page.
    await openReport(page, 'child-scoreboard');
    await click(page.locator('button', { hasText: 'End Encounter' }).first(), page);

    const dialog = endEncounterDialog(page);
    await expect(dialog).toBeVisible();

    // 3. Cancel means cancel: the dialog closes and the report is still here.
    await click(dialog.locator('button', { hasText: 'Cancel' }), page);
    await expect(dialog).toBeHidden();
    await expect(page.locator('h1', { hasText: 'PROGRESS REPORT' })).toBeVisible();

    // 4. Confirming this time brings up the referral warning.
    await click(page.locator('button', { hasText: 'End Encounter' }).first(), page);
    await expect(dialog).toBeVisible();
    await click(dialog.locator('button.primary', { hasText: 'Continue' }), page);

    const popup = diarrheaReferralPopup(page);
    await expect(popup).toBeVisible();

    // 5. Continue arrives at the acute illness encounter, not the PIN screen.
    await click(popup.locator('button'), page);
    await expect(page.locator('div.page-participant.acute-illness')).toBeVisible({
      timeout: 30000,
    });
  });
});
