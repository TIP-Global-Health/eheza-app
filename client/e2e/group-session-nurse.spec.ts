import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { syncAndWait } from './helpers/common';
import { setFeatureFlag, verifyFeatureGatesClinicalAssessmentButton } from './helpers/feature-flags';
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
  completeNutritionSignsAbnormal,
  completeChildFbf,
  completeNCDA,
  completeContributingFactors,
  completeHealthEducation,
  completeSendToHC,
  completeFollowUp,
  completeFamilyPlanning,
  completeLactation,
  completeMotherFbf,
  endGroupSession,
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

  test('FBF group session: all activities including NextSteps', async ({
    page,
    browser,
  }) => {
    // Scenario: Nurse creates an FBF group session with abnormal nutrition
    //   values (Edema sign + low MUAC) to trigger all activities including
    //   NextSteps.
    // Child activities: Height, Weight, MUAC (low), NutritionSigns (Edema),
    //   ChildFbf, NCDA, then NextSteps: ContributingFactors, HealthEducation,
    //   SendToHC, FollowUp.
    // Mother activities: FamilyPlanning, Lactation, MotherFbf.
    // Backend: Verifies 14 node types (all group session content types).

    // Verify FeatureNutritionGroup flag gates client UI + admin Reports surfaces.
    // (On nurse the gate is unambiguous: group_education isn't part of the compound for non-CHW.)
    //
    // family_nutrition is dropped for this gate-check so the on/off cycle of
    // nutrition_group exercises both admin Reports gates for FBF data
    // in one pass:
    //   - OFF phase (both flags off): SQ dropdown drops 'fbf-distribution'
    //     entirely (visibleReportTypes gate, both contributing features off).
    //   - ON phase (nutrition_group on, family_nutrition off): dropdown
    //     reappears and the 3 FBF rows render
    //     (visibleFbfDistributionCategories gate, FBF rows track
    //     nutrition_group; Aheza rows correctly absent under family_nutrition).
    try {
      setFeatureFlag('family_nutrition', false);
      await verifyFeatureGatesClinicalAssessmentButton(page, 'nutrition_group', 'group-assessment', {
        browser,
        admin: {
          sqOptions: ['fbf-distribution'],
          sqDemographicsRows: ['PMTCT', 'FBF', 'Sorwathe', 'CBNP', 'ACHI'],
          sqFbfDistributionRows: ['fbf-child', 'fbf-mother', 'fbf-child-achi'],
          completionOptions: ['nutrition-group'],
        },
      });
    } finally {
      setFeatureFlag('family_nutrition', true);
      await syncAndWait(page);
    }

    // 1. Navigate to FBF group session.
    await navigateToNurseGroupSession(page, 'FBF', 'Nyange I');

    // 2. Register mother and child.
    const mother = await createMotherOnAttendancePage(page, {
      isChw: false,
    });
    const child = await addChildToMother(page, {
      ageMonths: 12,
      isChw: false,
    });

    // 3. Navigate to ParticipantsPage.
    if (await page.locator('div.page-person').isVisible()) {
      await navigateBackToAttendance(page);
    }
    await page.locator('div.page-attendance').waitFor({ timeout: 10000 });
    await goToParticipantsPage(page);

    // 4. Go to child page.
    await clickMotherCard(page, mother.firstName);
    await navigateToChild(page);

    // 5. Complete mandatory child activities with abnormal values.
    await completeHeight(page, '70');
    await completeWeight(page, '8.5');
    await completeMuac(page, '11.5'); // Low MUAC → triggers malnutrition
    await completeNutritionSignsAbnormal(page); // Edema → triggers NextSteps
    await completeChildFbf(page);
    await completeNCDA(page);

    // 6. NextSteps triggered by abnormal values. Complete all 4.
    await completeContributingFactors(page);
    await completeHealthEducation(page);
    await completeSendToHC(page);
    await completeFollowUp(page);

    // 7. Navigate to mother and complete her activities.
    await navigateToMother(page);
    await completeFamilyPlanning(page);
    await completeLactation(page);
    await completeMotherFbf(page);

    // 8. End session.
    await click(page.locator('.link-back'), page);
    await page.locator('div.page-participants').waitFor({ timeout: 10000 });
    await goToActivitiesPage(page);
    await endGroupSession(page);

    // 9. Sync and verify.
    await syncAndWait(page, 'Nyange Health Center');
    const nodes = queryGroupSessionNodes(mother.fullName, child.fullName);

    // Mother measurements.
    expect(nodes.attendance, 'attendance should exist').toBe(true);
    expect(nodes.familyPlanning, 'familyPlanning should exist').toBe(true);
    expect(nodes.lactation, 'lactation should exist').toBe(true);
    expect(nodes.motherFbf, 'motherFbf should exist').toBe(true);

    // Child measurements.
    expect(nodes.height, 'height should exist').toBe(true);
    expect(nodes.weight, 'weight should exist').toBe(true);
    expect(nodes.muac, 'muac should exist').toBe(true);
    expect(nodes.nutrition, 'nutrition should exist').toBe(true);
    expect(nodes.childFbf, 'childFbf should exist').toBe(true);
    expect(nodes.groupNcda, 'groupNcda should exist').toBe(true);

    // NextSteps triggered by abnormal values.
    expect(nodes.contributingFactors, 'contributingFactors should exist').toBe(true);
    expect(nodes.groupHealthEducation, 'groupHealthEducation should exist').toBe(true);
    expect(nodes.groupSendToHC, 'groupSendToHC should exist').toBe(true);
    expect(nodes.followUp, 'followUp should exist').toBe(true);
  });
});
