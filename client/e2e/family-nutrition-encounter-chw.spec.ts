import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import { WAIT, syncAndWait } from './helpers/common';
import { verifyFeatureGatesClinicalAssessmentButton } from './helpers/feature-flags';
import {
  createMotherAndNavigateToPersonPage,
  addChild,
  continueToParticipantPage,
  startFamilyNutritionEncounter,
  selectFamilyMember,
  completeAhezaMother,
  completeAhezaChild,
  completeMuac,
  endFamilyNutritionEncounter,
  queryFamilyNutritionNodes,
} from './helpers/family-nutrition';

test.describe('CHW: Family Nutrition Encounter', () => {
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

  test('Case 1: Mother only — complete Aheza and MUAC', async ({ page }) => {
    // Scenario: Family nutrition encounter with mother only (no children added).
    // Activities: Aheza Mother (with distribution reason), MUAC Mother.
    // Conditions: No children → only mother activities shown.
    //   End encounter enabled once mother completes all activities.
    // Backend: Verifies aheza_mother, family_nutrition_muac_mother,
    //   family_nutrition_encounter exist.
    //   Confirms aheza_child, family_nutrition_muac_child,
    //   family_nutrition_photo absent (no children).

    // Verify FeatureFamilyNutrition flag gates the "Family Assessment" button on Clinical (CHW only).
    await verifyFeatureGatesClinicalAssessmentButton(page, 'family_nutrition', 'family-assessment');

    // 1. Register mother, skip adding children, go straight to participant page.
    const mother = await createMotherAndNavigateToPersonPage(page);
    await continueToParticipantPage(page);

    // 2. Start a new Family Nutrition encounter.
    await startFamilyNutritionEncounter(page);

    // 3. Mother is selected by default. Complete Aheza (distribution reason + amount).
    await completeAhezaMother(page, { amount: '3', reasonIndex: 1 });

    // 4. Complete MUAC for mother.
    await completeMuac(page, { value: '25.0' });

    // 5. All mother activities done → End Encounter should be enabled.
    await endFamilyNutritionEncounter(page);

    // 6. Sync to backend.
    await syncAndWait(page);

    // 7. Verify backend nodes.
    const nodes = queryFamilyNutritionNodes(mother.fullName);
    expect(nodes.ahezaMother, 'ahezaMother should exist').toBe(true);
    expect(nodes.muacMother, 'muacMother should exist').toBe(true);
    expect(nodes.encounter, 'encounter should exist').toBe(true);
    // No children → child measurements should not exist.
    expect(nodes.ahezaChild, 'ahezaChild should not exist').toBe(false);
    expect(nodes.muacChild, 'muacChild should not exist').toBe(false);
    expect(nodes.photo, 'photo should not exist').toBe(false);
  });

  test('Case 2: Mother with 2 children — one <6mo, one >=6mo', async ({
    page,
  }) => {
    // Scenario: Family nutrition encounter with mother and 2 children.
    //   Child 1: 3 months old → activities: Aheza + Photo (no MUAC, age <6mo)
    //   Child 2: 12 months old → activities: Aheza + MUAC + Photo
    //   Mother: Aheza + MUAC (no Photo for mother)
    // Activities: Complete Aheza for child 1 (no MUAC due to age).
    //   Complete Aheza + MUAC for child 2. Photo skipped for both
    //   (Dropzone file upload not supported in headless tests).
    //   End encounter requires only mother's activities to be complete.
    // Backend: Verifies 5 node types created:
    //   aheza_mother, aheza_child, family_nutrition_muac_mother,
    //   family_nutrition_muac_child (child 2 only), family_nutrition_encounter.

    // 1. Register mother.
    const mother = await createMotherAndNavigateToPersonPage(page);

    // 2. Add child 1 (3 months old — no MUAC due to age <6mo).
    const child1 = await addChild(page, {
      ageMonths: 3,
      firstName: `TestChild1_${Date.now()}`,
    });

    // 3. Add child 2 (12 months old — all activities).
    const child2 = await addChild(page, {
      ageMonths: 12,
      firstName: `TestChild2_${Date.now()}`,
    });

    // 4. Continue to participant page.
    await continueToParticipantPage(page);

    // 5. Start encounter.
    await startFamilyNutritionEncounter(page);

    // 6. Mother is selected by default — complete her activities.
    await completeAhezaMother(page, { amount: '3', reasonIndex: 1 });
    await completeMuac(page, { value: '24.5' });

    // 7-8. Complete activities for both children.
    //   Children order in the family member list is not guaranteed
    //   (depends on backend relationship ordering), so detect which
    //   child has MUAC (>=6mo) by checking the activity icons.
    await selectFamilyMember(page, 1);
    // Wait for activity grid to render for the selected child.
    await page
      .locator('.link-section:has(.icon-activity-task.icon-fbf)')
      .waitFor({ timeout: 10000 });
    const firstChildHasMuac = await page
      .locator('.link-section:has(.icon-activity-task.icon-muac)')
      .isVisible();

    if (firstChildHasMuac) {
      // Index 1 is the >=6mo child — complete Aheza + MUAC.
      await completeAhezaChild(page, { amount: '2' });
      await completeMuac(page, { value: '14.0' });
      // Switch to the <6mo child at index 2 — complete Aheza only.
      await page.waitForTimeout(WAIT.sectionTransition);
      await selectFamilyMember(page, 2);
      await page.waitForTimeout(WAIT.elmRerender);
      await completeAhezaChild(page, { amount: '1' });
    } else {
      // Index 1 is the <6mo child — complete Aheza only.
      await completeAhezaChild(page, { amount: '1' });
      // Switch to the >=6mo child at index 2 — complete Aheza + MUAC.
      await page.waitForTimeout(WAIT.sectionTransition);
      await selectFamilyMember(page, 2);
      await page.waitForTimeout(WAIT.elmRerender);
      await completeAhezaChild(page, { amount: '2' });
      await completeMuac(page, { value: '14.0' });
    }

    // 9. End Encounter (mother's activities are complete).
    await endFamilyNutritionEncounter(page);

    // 10. Sync to backend.
    await syncAndWait(page);

    // 11. Verify all backend nodes.
    const nodes = queryFamilyNutritionNodes(mother.fullName, [
      child1.fullName,
      child2.fullName,
    ]);
    expect(nodes.ahezaMother, 'ahezaMother should exist').toBe(true);
    expect(nodes.muacMother, 'muacMother should exist').toBe(true);
    expect(nodes.encounter, 'encounter should exist').toBe(true);
    expect(nodes.ahezaChild, 'ahezaChild should exist').toBe(true);
    expect(nodes.muacChild, 'muacChild should exist').toBe(true);
    // Photo skipped — Dropzone file upload not supported in headless tests.
  });
});
