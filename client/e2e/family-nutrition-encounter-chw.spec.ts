import { test, expect } from '@playwright/test';
import { click, setupDevice } from './helpers/auth';
import { installCursorScript } from './helpers/cursor';
import { resetDevice } from './helpers/device';
import {
  createMotherAndNavigateToPersonPage,
  addChildViaDrush,
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

  // TODO: Blocked by two issues:
  // 1. UI-based child creation: REST relationship endpoint returns "Not Found"
  //    for persons just created via REST POST (backend query can't find them).
  // 2. Drush-based child creation: Children created via drush bypass the sync
  //    revision tracking, so the Elm app doesn't download them.
  // Unskip once one of these approaches is fixed.
  test.skip('Case 2: Mother with 2 children — one <6mo, one >=6mo', async ({
    page,
  }) => {
    // Scenario: Family nutrition encounter with mother and 2 children.
    //   Child 1: 3 months old → activities: Aheza + Photo (no MUAC, age <6mo)
    //   Child 2: 12 months old → activities: Aheza + MUAC + Photo (all 3)
    //   Mother: Aheza + MUAC (no Photo for mother)
    // Activities: Complete all activities for all 3 family members.
    // Backend: Verifies all 6 node types created:
    //   aheza_mother (1), aheza_child (2), family_nutrition_muac_mother (1),
    //   family_nutrition_muac_child (1, child2 only), family_nutrition_photo (2),
    //   family_nutrition_encounter (1).

    // 1. Register mother via UI (stored locally, needs sync to reach backend).
    const mother = await createMotherAndNavigateToPersonPage(page);

    // 2. Sync to push the mother to the Drupal backend.
    await syncAndWait(page);

    // 3. Create children via drush (mother now exists in backend DB).
    const child1 = addChildViaDrush(mother.fullName, {
      ageMonths: 3,
      firstName: `TestChild1_${Date.now()}`,
    });
    const child2 = addChildViaDrush(mother.fullName, {
      ageMonths: 12,
      firstName: `TestChild2_${Date.now()}`,
    });

    // 4. Sync again so the Elm app downloads the drush-created children.
    await syncAndWait(page);

    // 5. Navigate to the encounter: go to root → Clinical → Family Encounter →
    //    search mother → Person page (now shows children) → Continue.
    await page.goto('/');
    await page.locator('.icon-task-clinical').waitFor({ timeout: 10000 });
    await click(page.locator('.icon-task-clinical'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

    await click(page.locator('button.family-assessment'), page);
    await page
      .locator('div.page-encounter-types')
      .waitFor({ timeout: 10000 });

    await click(
      page.locator('button.encounter-type', {
        hasText: 'Nutrition Encounter',
      }),
      page,
    );
    await page
      .locator('div.page-participants')
      .waitFor({ timeout: 10000 });

    // Search for the mother.
    const searchInput = page.locator('input[type="text"]').first();
    await searchInput.fill(mother.firstName);
    await page.waitForTimeout(2000);

    // Click mother in search results.
    await click(
      page
        .locator('.item.participant-view .action-icon.forward')
        .first(),
      page,
    );
    await page.locator('div.page-person').waitFor({ timeout: 10000 });

    // Continue to participant page.
    await continueToParticipantPage(page);

    // 5. Start encounter.
    await startFamilyNutritionEncounter(page);

    // 6. Mother is selected by default — complete her activities.
    await completeAhezaMother(page, { amount: '3', reasonIndex: 1 });
    await completeMuac(page, { value: '24.5' });

    // 7. After completing mother's activities, the app auto-advances
    //    to the next family member with pending activities (child 1).
    //    Ensure we're on child 1 by selecting member index 1.
    await selectFamilyMember(page, 1);

    // 8. Child 1 (3 months): Aheza + Photo (no MUAC due to age <6mo).
    await completeAhezaChild(page, { amount: '1' });
    await completePhoto(page);

    // 9. Switch to child 2.
    await selectFamilyMember(page, 2);

    // 10. Child 2 (12 months): Aheza + MUAC + Photo.
    await completeAhezaChild(page, { amount: '2' });
    await completeMuac(page, { value: '13.5' });
    await completePhoto(page);

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
    expect(nodes.photo).toBe(true);
  });
});
