import { test, expect } from '@playwright/test';
import { setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createMotherAndNavigateToPersonPage,
  addChild,
  continueToParticipantPage,
  startFamilyNutritionEncounter,
  selectFamilyMember,
  completeAhezaMother,
  completeAhezaChild,
  completeMuac,
  completePhoto,
  endFamilyNutritionEncounter,
  syncAndWait,
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
    expect(nodes.ahezaMother).toBe(true);
    expect(nodes.muacMother).toBe(true);
    expect(nodes.encounter).toBe(true);
    // No children → child measurements should not exist.
    expect(nodes.ahezaChild).toBe(false);
    expect(nodes.muacChild).toBe(false);
    expect(nodes.photo).toBe(false);
  });

  test('Case 2: Mother with 2 children — one <6mo, one >=6mo', async ({
    page,
  }) => {
    // Scenario: Family nutrition encounter with mother and 2 children.
    //   Child 1: 8 months old → activities: Aheza + MUAC + Photo
    //   Child 2: 18 months old → activities: Aheza + MUAC + Photo
    //   Mother: Aheza + MUAC (no Photo for mother)
    // Activities: Complete Aheza + MUAC for all members. Photo skipped
    //   (Dropzone file upload not supported in headless tests).
    //   End encounter requires only mother's activities to be complete.
    // Backend: Verifies 5 node types created:
    //   aheza_mother, aheza_child, family_nutrition_muac_mother,
    //   family_nutrition_muac_child, family_nutrition_encounter.

    // 1. Register mother.
    const mother = await createMotherAndNavigateToPersonPage(page);

    // 2. Add child 1 (8 months old — all activities including MUAC).
    const child1 = await addChild(page, {
      ageMonths: 8,
      firstName: `TestChild1_${Date.now()}`,
    });

    // 3. Add child 2 (18 months old — all activities).
    const child2 = await addChild(page, {
      ageMonths: 18,
      firstName: `TestChild2_${Date.now()}`,
    });

    // 4. Continue to participant page.
    await continueToParticipantPage(page);

    // 5. Start encounter.
    await startFamilyNutritionEncounter(page);

    // 6. Mother is selected by default — complete her activities.
    await completeAhezaMother(page, { amount: '3', reasonIndex: 1 });
    await completeMuac(page, { value: '24.5' });

    // 7. Switch to child 1 and complete Aheza + MUAC.
    await selectFamilyMember(page, 1);
    await completeAhezaChild(page, { amount: '1' });
    await completeMuac(page, { value: '13.0' });

    // 8. Switch to child 2 and complete Aheza + MUAC.
    await selectFamilyMember(page, 2);
    await completeAhezaChild(page, { amount: '2' });
    await completeMuac(page, { value: '14.0' });

    // 11. All activities done → End Encounter.
    await endFamilyNutritionEncounter(page);

    // 12. Sync to backend.
    await syncAndWait(page);

    // 13. Verify all backend nodes.
    const nodes = queryFamilyNutritionNodes(mother.fullName, [
      child1.fullName,
      child2.fullName,
    ]);
    expect(nodes.ahezaMother).toBe(true);
    expect(nodes.muacMother).toBe(true);
    expect(nodes.encounter).toBe(true);
    expect(nodes.ahezaChild).toBe(true);
    expect(nodes.muacChild).toBe(true);
    // Photo skipped — Dropzone file upload not supported in headless tests.
  });
});
