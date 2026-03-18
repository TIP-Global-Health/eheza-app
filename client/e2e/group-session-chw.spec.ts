import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  navigateToChwGroupSession,
  createMotherOnAttendancePage,
  addChildToMother,
  navigateBackToAttendance,
  goToParticipantsPage,
  goToActivitiesPage,
  clickMotherCard,
  navigateToChild,
  navigateToMother,
  completeHeight,
  completeWeight,
  completeMuac,
  completeNutritionSigns,
  completeFamilyPlanning,
  endGroupSession,
  syncAndWait,
  queryGroupSessionNodes,
} from './helpers/group-session';

test.describe('CHW: Group Nutrition Session', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as CHW Jojo (PIN 2345), select Akanduga village.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '2345', 'Akanduga');
  });

  test('CHW group session: register mother+child, complete basic activities', async ({
    page,
  }) => {
    // Scenario: CHW creates a group session via GroupEncounterTypesPage,
    //   registers a new mother and child (<24mo), completes basic child
    //   measurements and family planning for mother, ends session.
    // Child activities: Height, Weight, MUAC, NutritionSigns.
    //   (No ChildFbf — Chw clinic type doesn't include FBF activities.)
    // Mother activities: FamilyPlanning only.
    //   (No Lactation or MotherFbf — those are FBF-only.)
    // Conditions: Normal values → NextSteps NOT triggered.
    // Backend: Verifies 6 node types created (attendance, height, weight,
    //   muac, nutrition, family_planning).
    //   Confirms child_fbf, mother_fbf, lactation absent.

    // 1. Navigate to CHW group session.
    await navigateToChwGroupSession(page);

    // 2. Register a new mother (CHW — no address fields).
    const mother = await createMotherOnAttendancePage(page, {
      isChw: true,
    });

    // 3. Add a child (<24 months).
    const child = await addChildToMother(page, {
      ageMonths: 12,
      isChw: true,
    });

    // 4. After saving relationship, app auto-navigates to AttendancePage.
    if (await page.locator('div.page-person').isVisible()) {
      await navigateBackToAttendance(page);
    }

    // 5. From AttendancePage, navigate to ParticipantsPage.
    await page.locator('div.page-attendance').waitFor({ timeout: 10000 });
    await goToParticipantsPage(page);

    // 6. Click the mother's card.
    await clickMotherCard(page, mother.firstName);

    // 7. Navigate to the child's page.
    await navigateToChild(page);

    // 8. Complete child activities (Chw type: no FBF).
    await completeHeight(page, '68');
    await completeWeight(page, '7.5');
    await completeMuac(page, '13.5');
    await completeNutritionSigns(page);

    // 9. Navigate back to the mother's page.
    await navigateToMother(page);

    // 10. Complete mother activities (Chw type: FamilyPlanning only).
    await completeFamilyPlanning(page);

    // 11. Go back to ParticipantsPage, then to ActivitiesPage, and end session.
    await click(page.locator('.link-back'), page);
    await page.locator('div.page-participants').waitFor({ timeout: 10000 });
    await goToActivitiesPage(page);
    await endGroupSession(page);

    // 12. Sync to backend.
    await syncAndWait(page, 'Nyange Health Center');

    // 13. Verify backend nodes.
    const nodes = queryGroupSessionNodes(mother.fullName, child.fullName);

    // Mother measurements.
    expect(nodes.attendance).toBe(true);
    expect(nodes.familyPlanning).toBe(true);

    // Child measurements.
    expect(nodes.height).toBe(true);
    expect(nodes.weight).toBe(true);
    expect(nodes.muac).toBe(true);
    expect(nodes.nutrition).toBe(true);

    // FBF-only activities should be absent for Chw clinic type.
    expect(nodes.childFbf).toBe(false);
    expect(nodes.lactation).toBe(false);
    expect(nodes.motherFbf).toBe(false);

    // NextSteps NOT triggered (normal values).
    expect(nodes.groupHealthEducation).toBe(false);
    expect(nodes.groupSendToHC).toBe(false);
  });
});
