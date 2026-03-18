import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  navigateToNurseGroupSession,
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
  completeChildFbf,
  completeFamilyPlanning,
  completeLactation,
  completeMotherFbf,
  endGroupSession,
  syncAndWait,
  queryGroupSessionNodes,
} from './helpers/group-session';

test.describe('Nurse: FBF Group Nutrition Session', () => {
  test.describe.configure({ timeout: 600000 });

  if (process.env.RECORD) {
    test.beforeEach(async ({ page }) => {
      await page.addInitScript(installCursorScript());
    });
  }

  // Login as nurse (PIN 1234), select Nyange Health Center.
  test.beforeEach(async ({ page }) => {
    resetDevice();
    await setupDevice(page, '1234', 'Nyange Health Center');
  });

  test('FBF group session: register mother+child, complete all activities', async ({
    page,
  }) => {
    // Scenario: Nurse creates an FBF group session, registers a new mother
    //   and child (<24mo), completes all child and mother activities with
    //   normal/healthy values, ends the session, syncs, and verifies backend.
    // Child activities: Height, Weight, MUAC, NutritionSigns, ChildFbf.
    // Mother activities: FamilyPlanning, Lactation, MotherFbf.
    // Conditions: Normal values → NextSteps NOT triggered.
    //   FBF clinic type → ChildFbf + Lactation + MotherFbf available.
    // Backend: Verifies 9 node types created (attendance, height, weight,
    //   muac, nutrition, child_fbf, family_planning, lactation, mother_fbf).
    //   Confirms group_health_education, group_send_to_hc absent.

    // 1. Navigate to FBF group session.
    await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');

    // 2. Register a new mother.
    const mother = await createMotherOnAttendancePage(page, {
      isChw: false,
    });

    // 3. Add a child (<24 months).
    const child = await addChildToMother(page, {
      ageMonths: 12,
      isChw: false,
    });

    // 4. After saving relationship, app auto-navigates to AttendancePage
    //    (PMTCT participant created, mother auto-checked-in).
    //    If we landed on PersonPage instead, navigate back.
    if (await page.locator('div.page-person').isVisible()) {
      await navigateBackToAttendance(page);
    }

    // 5. From AttendancePage, navigate to ParticipantsPage.
    await page.locator('div.page-attendance').waitFor({ timeout: 10000 });
    await goToParticipantsPage(page);

    // 6. Click the mother's card to go to MotherPage.
    await clickMotherCard(page, mother.firstName);

    // 7. Navigate to the child's page.
    await navigateToChild(page);

    // 8. Complete child activities.
    await completeHeight(page, '70');
    await completeWeight(page, '8.5');
    await completeMuac(page, '14');
    await completeNutritionSigns(page);
    await completeChildFbf(page);

    // 9. Navigate back to the mother's page.
    await navigateToMother(page);

    // 10. Complete mother activities.
    await completeFamilyPlanning(page);
    await completeLactation(page);
    await completeMotherFbf(page);

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
    expect(nodes.lactation).toBe(true);
    expect(nodes.motherFbf).toBe(true);

    // Child measurements.
    expect(nodes.height).toBe(true);
    expect(nodes.weight).toBe(true);
    expect(nodes.muac).toBe(true);
    expect(nodes.nutrition).toBe(true);
    expect(nodes.childFbf).toBe(true);

    // NextSteps NOT triggered (normal values).
    expect(nodes.groupHealthEducation).toBe(false);
    expect(nodes.groupSendToHC).toBe(false);
  });
});
