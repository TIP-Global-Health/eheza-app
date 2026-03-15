import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Private form helpers
// ---------------------------------------------------------------------------

/**
 * Select an option in a form dropdown identified by its label text.
 * @param optionIndex - 1-based index (skips blank default).
 */
async function selectByLabel(page: Page, labelText: string, optionIndex: number) {
  const row = page.locator('.ui.grid').filter({ hasText: labelText });
  const select = row.locator('select').first();
  const options = select.locator('option');
  const count = await options.count();
  if (count > optionIndex) {
    const value = await options.nth(optionIndex).getAttribute('value');
    if (value !== null) {
      await select.selectOption(value);
    }
  }
}

/**
 * Locate a form input by its label text (grid row pattern).
 */
function formInput(page: Page, labelText: string) {
  return page
    .locator('.ui.grid')
    .filter({ hasText: labelText })
    .locator('input')
    .first();
}

/**
 * Open the calendar popup, select a date, and confirm.
 */
async function setDate(page: Page, date: Date, triggerSelector = '.date-input') {
  await click(page.locator(triggerSelector).first(), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ timeout: 5000 });

  const year = date.getFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  const monthValue = (date.getMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  const day = date.getDate();
  const dayCell = page.locator(
    'div.calendar table tbody td:not(.date-selector--dimmed)',
    { hasText: new RegExp(`^${day}$`) },
  );
  await dayCell.first().click();

  await click(
    page.locator('.ui.active.modal.calendar-popup div.ui.button'),
    page,
  );

  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ state: 'hidden', timeout: 3000 })
    .catch(() => {});
}

/**
 * Answer an NCDA yes/no question by its question text.
 *
 * All NCDA yes/no inputs share the same empty CSS class ("form-input yes-no "),
 * so we locate by finding the question label and then navigating to the
 * next sibling .form-input element.
 */
async function answerNCDAYesNo(page: Page, questionSubstring: string, answer: 'Yes' | 'No') {
  const ncdaForm = page.locator('.ui.form.ncda');
  // Find the label containing the question text.
  const questionLabel = ncdaForm.locator('.label', { hasText: questionSubstring }).first();
  await questionLabel.waitFor({ timeout: 5000 });

  // Navigate to the next sibling .form-input element via evaluate.
  // Some questions are wrapped in a .label-with-helper parent, so if
  // the immediate siblings don't have .form-input, walk up to the parent
  // and check its siblings too.
  const yesNoId = await questionLabel.evaluate((el) => {
    function findFormInput(startEl: Element): Element | null {
      let sibling = startEl.nextElementSibling;
      while (sibling) {
        if (sibling.classList.contains('form-input')) {
          return sibling;
        }
        sibling = sibling.nextElementSibling;
      }
      return null;
    }

    // First try siblings of the label itself.
    let formInput = findFormInput(el);

    // If not found, try from the parent (handles .label-with-helper wrapper).
    if (!formInput && el.parentElement) {
      formInput = findFormInput(el.parentElement);
    }

    if (formInput) {
      const tmpId = 'ncda-yn-' + Math.random().toString(36).slice(2);
      formInput.id = tmpId;
      return tmpId;
    }
    return null;
  });

  if (!yesNoId) {
    throw new Error(`Could not find yes/no input after question: "${questionSubstring}"`);
  }

  await click(page.locator(`#${yesNoId} label`, { hasText: answer }), page);
  await page.waitForTimeout(300);
}

/**
 * Select a checkbox option by clicking its label (exact match).
 */
async function selectCheckbox(page: Page, optionText: string) {
  await click(
    page.locator('.ui.checkbox label', {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }),
    page,
  );
}

/**
 * Click an NCDA step tab icon and wait for it to become active.
 */
async function clickNCDAStepTab(page: Page, iconClass: string) {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(500);
  }
}

/**
 * Open an activity from the Child Scoreboard encounter page by its card icon.
 */
async function openActivity(page: Page, activityIcon: string) {
  await page.locator('div.page-encounter.child-scoreboard').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.child-scoreboard').waitFor({ timeout: 10000 });
}

/**
 * Click the Save button. In the NCDA task-tray pattern, this navigates
 * to the next incomplete step unless all steps are done.
 */
async function clickSave(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Participant registration
// ---------------------------------------------------------------------------

/**
 * Register a child and start a Child Scoreboard encounter (CHW flow).
 * Flow: Dashboard → Clinical → Individual Assessment → Child Scorecard →
 *       Register → fill form → submit → participant page →
 *       click "Child Scorecard Encounter"
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createChildAndStartEncounter(
  page: Page,
  options?: {
    ageMonths?: number;
    firstName?: string;
  },
) {
  const ageMonths = options?.ageMonths ?? 10;
  const firstName = options?.firstName ?? `TestCSB${Date.now()}`;
  const secondName = 'E2ETest';

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → Child Scorecard
  await click(
    page.locator('button.encounter-type', { hasText: 'Child Scorecard' }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // Click "Register a new participant"
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill the registration form.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth.
  const dob = new Date();
  dob.setMonth(dob.getMonth() - ageMonths);
  await setDate(page, dob);

  // Select gender = male.
  await page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]')
    .first()
    .check();

  // Select mode of delivery (required for children).
  await selectByLabel(page, 'Mode of delivery:', 1);

  // CHW: address fields are auto-filled from assigned village, skip them.

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page.
  await page
    .locator('div.page-participant.individual.child-scoreboard')
    .waitFor({ timeout: 30000 });

  // Start Child Scoreboard encounter.
  await click(
    page.locator('div.ui.primary.button', { hasText: 'Child Scorecard Encounter' }),
    page,
  );
  await page
    .locator('div.page-encounter.child-scoreboard')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// NCDA activity
// ---------------------------------------------------------------------------

/**
 * Complete the NCDA activity with all 6 steps using healthy values.
 *
 * Steps: AntenatalCare, UniversalInterventions, NutritionBehavior,
 *        NutritionAssessment, TargetedInterventions, InfrastructureEnvironment.
 *
 * Creates: child_scoreboard_ncda
 */
export async function completeNCDA(page: Page) {
  await openActivity(page, 'history');

  // --- Step 1: Antenatal Care ---
  await clickNCDAStepTab(page, 'ncda-antenatal');
  await page.waitForTimeout(500);

  // "Were there any ANC encounters that are not recorded above" → No
  await answerNCDAYesNo(page, 'ANC encounters that are not recorded', 'No');
  await page.waitForTimeout(300);

  // "Did the mother receive Iron, Folic Acid/MMS" → Yes
  await answerNCDAYesNo(page, 'receive Iron, Folic Acid', 'Yes');
  await page.waitForTimeout(300);

  // "Has she taken it as per guidance" → Yes (conditional, shown when above is Yes)
  await answerNCDAYesNo(page, 'taken it as per guidance', 'Yes');
  await page.waitForTimeout(300);

  // Birth weight — appears when newborn exam pregnancy summary has no birth weight.
  // Input class: .form-input.measurement.birth-weight, unit: grams.
  const birthWeightInput = page.locator('.form-input.measurement.birth-weight input[type="number"]');
  if (await birthWeightInput.isVisible({ timeout: 1000 }).catch(() => false)) {
    await birthWeightInput.fill('3200');
    await page.waitForTimeout(300);
  }

  // "Was the child born with a birth defect" → No
  const birthDefectQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'born with a birth defect',
  });
  if (await birthDefectQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'born with a birth defect', 'No');
    await page.waitForTimeout(300);
  }

  // Click Save to proceed to next step.
  await clickSave(page);

  // --- Step 2: Universal Interventions ---
  await clickNCDAStepTab(page, 'ncda-universal-intervention');
  await page.waitForTimeout(500);

  // ChildBehindOnVaccination — conditional, may or may not appear.
  // Answer "No" (caregiver says child is up-to-date) to trigger
  // VaccinationHistory activity after NCDA is saved.
  const vaccinationQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'behind on vaccinations',
  });
  if (await vaccinationQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'behind on vaccinations', 'No');
    await page.waitForTimeout(300);
  }

  // ChildReceivesVitaminA — checkbox select (Yes/No/Not Applicable), NOT yes/no.
  // Scoped to .checkbox-select-input to avoid matching yes/no radio labels.
  await click(
    page.locator('.checkbox-select-input .ui.checkbox label', {
      hasText: /^Yes$/i,
    }).first(),
    page,
  );
  await page.waitForTimeout(300);

  // ChildReceivesDewormer → Yes
  await answerNCDAYesNo(page, 'receive deworming', 'Yes');
  await page.waitForTimeout(300);

  // OngeraMNP → Yes
  await answerNCDAYesNo(page, 'receive Ongera-MNP', 'Yes');
  await page.waitForTimeout(300);

  // TakingOngeraMNP → Yes (conditional, shown when OngeraMNP is Yes)
  await answerNCDAYesNo(page, 'Ongera-MNP being consumed', 'Yes');
  await page.waitForTimeout(300);

  // ChildReceivesECD → Yes
  await answerNCDAYesNo(page, 'sing lullabies', 'Yes');
  await page.waitForTimeout(300);

  await clickSave(page);

  // --- Step 3: Nutrition Behavior (child >= 6 months) ---
  await clickNCDAStepTab(page, 'ncda-nutrition-behavior');
  await page.waitForTimeout(500);

  // FiveFoodGroups → Yes
  await answerNCDAYesNo(page, '5 food groups', 'Yes');
  await page.waitForTimeout(300);

  // BreastfedForSixMonths → Yes (shown for first NCDA on child > 6 months)
  const breastfedQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'breastfed for 6 months',
  });
  if (await breastfedQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'breastfed for 6 months', 'Yes');
    await page.waitForTimeout(300);
  }

  // AppropriateComplementaryFeeding → Yes
  await answerNCDAYesNo(page, 'appropriate complementary feeding', 'Yes');
  await page.waitForTimeout(300);

  // MealsAtRecommendedTimes → Yes
  await answerNCDAYesNo(page, 'eat at the recommended times', 'Yes');
  await page.waitForTimeout(300);

  await clickSave(page);

  // --- Step 4: Nutrition Assessment ---
  await clickNCDAStepTab(page, 'nutrition-assessment');
  await page.waitForTimeout(500);

  // Stunting level: click "Green" checkbox.
  await selectCheckbox(page, 'Green');
  await page.waitForTimeout(300);

  // Weight: enter 8.5 kg.
  const weightInput = page.locator('.form-input.measurement.weight input[type="number"]');
  await weightInput.fill('8.5');
  await page.waitForTimeout(300);

  // MUAC: enter 14.0 cm (green range, >= 12.5 cm).
  const muacInput = page.locator('.form-input.measurement.muac input[type="number"]');
  if (await muacInput.isVisible({ timeout: 1000 }).catch(() => false)) {
    await muacInput.fill('14.0');
    await page.waitForTimeout(300);
  }

  // ShowsEdemaSigns → No
  await answerNCDAYesNo(page, 'signs of edema', 'No');
  await page.waitForTimeout(300);

  await clickSave(page);

  // --- Step 5: Targeted Interventions ---
  await clickNCDAStepTab(page, 'ncda-targeted-intervention');
  await page.waitForTimeout(500);

  // ChildReceivesFBF → No
  await answerNCDAYesNo(page, 'receive FBF', 'No');
  await page.waitForTimeout(300);

  // BeneficiaryCashTransfer → No
  await answerNCDAYesNo(page, 'beneficiary of cash transfer', 'No');
  await page.waitForTimeout(300);

  // ConditionalFoodItems → No
  await answerNCDAYesNo(page, 'other support', 'No');
  await page.waitForTimeout(300);

  // ChildWithDisability → No
  await answerNCDAYesNo(page, 'have disability', 'No');
  await page.waitForTimeout(300);

  // ChildGotDiarrhea → No
  await answerNCDAYesNo(page, 'have diarrhea', 'No');
  await page.waitForTimeout(300);

  await clickSave(page);

  // --- Step 6: Infrastructure & Environment ---
  await clickNCDAStepTab(page, 'ncda-infrastructure-environment');
  await page.waitForTimeout(500);

  // HasCleanWater → Yes
  await answerNCDAYesNo(page, 'clean water', 'Yes');
  await page.waitForTimeout(300);

  // HasHandwashingFacility → Yes
  await answerNCDAYesNo(page, 'handwashing facility', 'Yes');
  await page.waitForTimeout(300);

  // HasToilets → Yes
  await answerNCDAYesNo(page, 'have toilets', 'Yes');
  await page.waitForTimeout(300);

  // HasKitchenGarden → Yes
  await answerNCDAYesNo(page, 'kitchen garden', 'Yes');
  await page.waitForTimeout(300);

  // InsecticideTreatedBednets → Yes
  await answerNCDAYesNo(page, 'insecticide-treated bednets', 'Yes');
  await page.waitForTimeout(300);

  // Final Save — all steps complete, this persists the data.
  await clickSave(page);

  // After saving, the app should navigate to the encounter page.
  // However, it may sometimes land on the progress report page instead.
  const encounterPage = page.locator('div.page-encounter.child-scoreboard');
  const progressReport = page.locator('h1', { hasText: 'PROGRESS REPORT' });

  await Promise.race([
    encounterPage.waitFor({ timeout: 15000 }),
    progressReport.waitFor({ timeout: 15000 }),
  ]);

  // If we landed on the progress report, navigate back to the encounter page.
  if (await progressReport.isVisible({ timeout: 500 }).catch(() => false)) {
    await click(page.locator('span.icon-back').first(), page);
    await encounterPage.waitFor({ timeout: 10000 });
  }

  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Vaccination History activity
// ---------------------------------------------------------------------------

/**
 * Complete the Vaccination History activity by iterating all visible
 * vaccine tabs and answering "No" to "Did the child receive any [vaccine]
 * immunizations prior to today that are not recorded above".
 *
 * In Child Scoreboard, suggestDoseToday is false, so answering "No"
 * to the previous-doses question completes each vaccine tab.
 *
 * Creates: child_scoreboard_*_iz nodes for each vaccine tab completed.
 */
export async function completeVaccinationHistory(page: Page) {
  await openActivity(page, 'immunisation');

  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(500);

    // Answer "No" to "Did the child receive any [vaccine] immunizations
    // prior to today that are not recorded above".
    // The vaccination form (.ui.form.vaccination) has a single yes/no
    // input with empty CSS class, so locate the .form-input.yes-no within it.
    const vaccinationForm = page.locator('.ui.form.vaccination');
    const yesNoInput = vaccinationForm.locator('.form-input.yes-no');
    if (await yesNoInput.isVisible({ timeout: 2000 }).catch(() => false)) {
      await click(yesNoInput.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(300);
    }

    // Save the vaccine tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 5000 });
    await click(saveBtn, page);
    await page.waitForTimeout(1000);
  }

  // After saving the last tab, the app may navigate to the encounter page
  // or the progress report page. Handle both cases.
  const encounterPage = page.locator('div.page-encounter.child-scoreboard');
  const progressReport = page.locator('h1', { hasText: 'PROGRESS REPORT' });

  await Promise.race([
    encounterPage.waitFor({ timeout: 15000 }),
    progressReport.waitFor({ timeout: 15000 }),
  ]);

  if (await progressReport.isVisible({ timeout: 500 }).catch(() => false)) {
    await click(page.locator('span.icon-back').first(), page);
    await encounterPage.waitFor({ timeout: 10000 });
  }

  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the Child Scoreboard encounter.
 * When child does NOT have diarrhea, no popup appears.
 */
export async function endChildScoreboardEncounter(page: Page) {
  await page.waitForTimeout(2000);

  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for navigation away from the encounter page.
  await page
    .locator('div.page-encounter.child-scoreboard')
    .waitFor({ state: 'hidden', timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Sync
// ---------------------------------------------------------------------------

/**
 * Sync data and wait for success.
 * Click sync icon → device status → wait for success → go back.
 */
export async function syncAndWait(page: Page) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: 'Nyange Health Center' }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 120000 });

  await page.goBack();
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the backend for Child Scoreboard measurement nodes associated with a person.
 * Returns an object mapping node type → boolean (exists).
 *
 * Uses base64-encoded person name to prevent shell injection.
 * Retries up to 10 times with 5s delay for eventual consistency.
 */
export function queryChildScoreboardNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const nodeTypes = [
    'child_scoreboard_ncda',
    'child_scoreboard_bcg_iz',
    'child_scoreboard_dtp_iz',
    'child_scoreboard_dtp_sa_iz',
    'child_scoreboard_ipv_iz',
    'child_scoreboard_mr_iz',
    'child_scoreboard_opv_iz',
    'child_scoreboard_pcv13_iz',
    'child_scoreboard_rotarix_iz',
  ];

  const typesStr = nodeTypes.map(t => `'${t}'`).join(', ');

  const php = `
    \\$person_name = base64_decode('${personNameB64}');
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$person_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Person not found']);
      return;
    }
    \\$person_nid = key(\\$result['node']);

    \\$types = array(${typesStr});
    \\$found = array();
    foreach (\\$types as \\$type) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->range(0, 1)
        ->execute();
      \\$found[\\$type] = !empty(\\$r['node']);
    }
    echo json_encode(\\$found);
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd,
        timeout: 30000,
        encoding: 'utf-8',
      }).trim();

      const parsed = JSON.parse(output);
      if (parsed.error) {
        console.log(`queryChildScoreboardNodes attempt ${attempt + 1}: ${parsed.error}`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
        return parsed;
      }

      if (expectedTypes) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) {
          return parsed;
        }
        console.log(`queryChildScoreboardNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
        if (attempt < 9) {
          execSync('sleep 5');
          continue;
        }
      }

      return parsed;
    } catch (err) {
      console.log(`queryChildScoreboardNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) {
        execSync('sleep 5');
      }
    }
  }

  return {};
}
