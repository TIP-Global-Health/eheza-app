import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

/**
 * Select an option in a form dropdown identified by its label text.
 * @param optionIndex - 1-based index of the option to select (skips the blank default).
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
 * Locate a form input by its label text. The Elm form library renders
 * inputs without name attributes, so we find them via the grid row
 * structure: <div class="ui grid"><div>Label:</div><div><input/></div></div>
 */
function formInput(page: Page, labelText: string) {
  return page
    .locator('.ui.grid')
    .filter({ hasText: labelText })
    .locator('input')
    .first();
}

/**
 * Navigate from the dashboard to the nutrition encounter page
 * for a newly registered child.
 *
 * Steps: Clinical → Individual Encounter → Child Nutrition →
 *        Register new participant → fill form → save →
 *        click "Nutrition Encounter"
 */
export async function createChildAndStartEncounter(
  page: Page,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isChw?: boolean;
    startEncounter?: boolean;
  },
) {
  const ageMonths = options?.ageMonths ?? 24;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isChw = options?.isChw ?? false;
  const startEncounter = options?.startEncounter ?? true;

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → Child Nutrition
  await click(
    page.locator('button.encounter-type', { hasText: 'Child Nutrition' }),
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
  // Wait for the form to load — look for the "First Name:" label.
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill the registration form.
  // Elm's form library doesn't set name attributes on inputs, so we
  // locate them via their label row in the grid layout.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth via the calendar popup.
  const dob = new Date();
  dob.setMonth(dob.getMonth() - ageMonths);
  await setDateOfBirth(page, dob);

  // Select gender = male.
  await page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]')
    .first()
    .check();

  // Select mode of delivery (required field).
  await selectByLabel(page, 'Mode of delivery:', 1);

  if (!isChw) {
    // Nurse: fill address (cascading dropdowns) and health center.
    await selectByLabel(page, 'Province:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'District:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'Sector:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'Cell:', 1);
    await page.waitForTimeout(500);
    await selectByLabel(page, 'Village:', 1);

    // Select health center.
    const hcSelect = page
      .locator('.ui.grid')
      .filter({ hasText: 'Health Center:' })
      .locator('select');
    await hcSelect.selectOption({ label: 'Nyange Health Center' });
  }
  // CHW: address fields are auto-filled from the assigned village;
  // the form does not render address or health center sections.

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page to load.
  await page
    .locator('div.page-participant.individual.nutrition')
    .waitFor({ timeout: 30000 });

  if (startEncounter) {
    // Start a new nutrition encounter.
    await click(
      page.locator('div.ui.primary.button', { hasText: 'Nutrition Encounter' }),
      page,
    );
    await page
      .locator('div.page-encounter.nutrition')
      .waitFor({ timeout: 10000 });
  }

  // Drupal stores the person title as "secondName firstName".
  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * Open the DOB calendar popup, select year, month, and day,
 * then confirm.
 */
async function setDateOfBirth(page: Page, dob: Date) {
  // Click the date input to open the calendar popup.
  await click(page.locator('.date-input'), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ timeout: 5000 });

  // Select year.
  const year = dob.getFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  // Select month (JS Date is 0-indexed, Elm select uses 1-indexed string values).
  const monthValue = (dob.getMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  // Click the correct day cell.
  const day = dob.getDate();
  // Day cells contain the day number as text. Find the non-dimmed cell
  // with the matching text.
  const dayCell = page.locator(
    'div.calendar table tbody td:not(.date-selector--dimmed)',
    { hasText: new RegExp(`^${day}$`) },
  );
  await dayCell.first().click();

  // Confirm the date selection (Save is a div, not a button).
  await click(
    page.locator('.ui.active.modal.calendar-popup div.ui.button'),
    page,
  );

  // Wait for the popup to close.
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ state: 'hidden', timeout: 3000 })
    .catch(() => {});
}

// ---------------------------------------------------------------------------
// NCDA helpers (Nurse-only, child < 24 months, atHealthCenter=true)
// ---------------------------------------------------------------------------

/**
 * Answer an NCDA yes/no question by its question text.
 * All NCDA yes/no inputs share the same empty CSS class, so we locate
 * by finding the question label and navigating to the next sibling .form-input.
 */
async function answerNCDAYesNo(page: Page, questionSubstring: string, answer: 'Yes' | 'No') {
  const ncdaForm = page.locator('.ui.form.ncda');
  const questionLabel = ncdaForm.locator('.label', { hasText: questionSubstring }).first();
  await questionLabel.waitFor({ timeout: 5000 });

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
    let formInput = findFormInput(el);
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
 * Click the NCDA Save button (task-tray pattern).
 */
async function clickNCDASave(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);
}

/**
 * Complete the NCDA activity in a Nutrition encounter (Nurse, atHealthCenter).
 *
 * Steps shown at health center differ from CHW/Child Scoreboard:
 * - AntenatalCare: only on first NCDA
 * - UniversalInterventions: only OngeraMNP (no vaccination, vitaminA, dewormer, ECD)
 * - NutritionBehavior: same (child >= 6 months)
 * - NutritionAssessment: NOT shown (atHealthCenter=true)
 * - TargetedInterventions: no FBF, no TreatedForAcuteMalnutrition, no Diarrhea
 * - InfrastructureEnvironment: same
 *
 * Creates: nutrition_ncda
 */
export async function completeNCDA(page: Page) {
  // Open NCDA activity.
  await page.locator('div.page-encounter.nutrition').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await click(page.locator('.icon-task-history'), page);

  // Handle the NCDA confirmation popup: "Would you like to proceed?"
  const confirmPopup = page.locator('div.ui.active.modal');
  await confirmPopup.waitFor({ timeout: 5000 }).catch(() => {});
  if (await confirmPopup.isVisible()) {
    const proceedBtn = confirmPopup.locator('button').first();
    await proceedBtn.click({ force: true });
    await confirmPopup.waitFor({ state: 'hidden', timeout: 5000 });
  }

  await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

  // --- Step 1: Antenatal Care (first fill only) ---
  const antenatalTab = page.locator('.link-section:has(.icon-activity-task.icon-ncda-antenatal)');
  if (await antenatalTab.isVisible({ timeout: 1000 }).catch(() => false)) {
    await clickNCDAStepTab(page, 'ncda-antenatal');
    await page.waitForTimeout(500);

    await answerNCDAYesNo(page, 'ANC encounters that are not recorded', 'No');
    await page.waitForTimeout(300);

    await answerNCDAYesNo(page, 'receive Iron, Folic Acid', 'Yes');
    await page.waitForTimeout(300);

    await answerNCDAYesNo(page, 'taken it as per guidance', 'Yes');
    await page.waitForTimeout(300);

    // Birth weight + birth defect (conditional).
    const birthWeightInput = page.locator('.form-input.measurement.birth-weight input[type="number"]');
    if (await birthWeightInput.isVisible({ timeout: 1000 }).catch(() => false)) {
      await birthWeightInput.fill('3200');
      await page.waitForTimeout(300);
    }
    const birthDefectQuestion = page.locator('.ui.form.ncda .label', { hasText: 'born with a birth defect' });
    if (await birthDefectQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
      await answerNCDAYesNo(page, 'born with a birth defect', 'No');
      await page.waitForTimeout(300);
    }

    await clickNCDASave(page);
  }

  // --- Step 2: Universal Interventions (atHealthCenter: only OngeraMNP) ---
  await clickNCDAStepTab(page, 'ncda-universal-intervention');
  await page.waitForTimeout(500);

  await answerNCDAYesNo(page, 'receive Ongera-MNP', 'Yes');
  await page.waitForTimeout(300);

  await clickNCDASave(page);

  // --- Step 3: Nutrition Behavior (child >= 6 months) ---
  const nutritionBehaviorTab = page.locator('.link-section:has(.icon-activity-task.icon-ncda-nutrition-behavior)');
  if (await nutritionBehaviorTab.isVisible({ timeout: 1000 }).catch(() => false)) {
    await clickNCDAStepTab(page, 'ncda-nutrition-behavior');
    await page.waitForTimeout(500);

    await answerNCDAYesNo(page, '5 food groups', 'Yes');
    await page.waitForTimeout(300);

    const breastfedQuestion = page.locator('.ui.form.ncda .label', { hasText: 'breastfed for 6 months' });
    if (await breastfedQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
      await answerNCDAYesNo(page, 'breastfed for 6 months', 'Yes');
      await page.waitForTimeout(300);
    }

    await answerNCDAYesNo(page, 'appropriate complementary feeding', 'Yes');
    await page.waitForTimeout(300);

    await answerNCDAYesNo(page, 'eat at the recommended times', 'Yes');
    await page.waitForTimeout(300);

    await clickNCDASave(page);
  }

  // --- Step 4: NutritionAssessment — NOT shown at health center ---

  // --- Step 5: Targeted Interventions (atHealthCenter: no FBF, no diarrhea) ---
  await clickNCDAStepTab(page, 'ncda-targeted-intervention');
  await page.waitForTimeout(500);

  await answerNCDAYesNo(page, 'beneficiary of cash transfer', 'No');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'other support', 'No');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'have disability', 'No');
  await page.waitForTimeout(300);

  await clickNCDASave(page);

  // --- Step 6: Infrastructure & Environment ---
  await clickNCDAStepTab(page, 'ncda-infrastructure-environment');
  await page.waitForTimeout(500);

  await answerNCDAYesNo(page, 'clean water', 'Yes');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'handwashing facility', 'Yes');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'have toilets', 'Yes');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'kitchen garden', 'Yes');
  await page.waitForTimeout(300);

  await answerNCDAYesNo(page, 'insecticide-treated bednets', 'Yes');
  await page.waitForTimeout(300);

  // Final Save.
  await clickNCDASave(page);

  // Wait for encounter page (may land on progress report).
  const encounterPage = page.locator('div.page-encounter.nutrition');
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
// Measurement helpers
// ---------------------------------------------------------------------------

/**
 * Enter a height measurement and save.
 */
export async function enterHeight(page: Page, value: string) {
  await click(page.locator('.icon-task-height'), page);
  await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

  await page
    .locator('.form-input.measurement.height input[type="number"]')
    .fill(value);

  return page;
}

/**
 * Enter a weight measurement and save.
 */
export async function enterWeight(page: Page, value: string) {
  await click(page.locator('.icon-task-weight'), page);
  await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

  await page
    .locator('.form-input.measurement.weight input[type="number"]')
    .fill(value);

  return page;
}

/**
 * Enter a MUAC measurement and save.
 */
export async function enterMuac(page: Page, value: string) {
  await click(page.locator('.icon-task-muac'), page);
  await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

  await page
    .locator('.form-input.measurement.muac input[type="number"]')
    .fill(value);

  return page;
}

/**
 * Select nutrition signs and save.
 * @param signs - Array of sign labels, e.g. ['None'] or ['Edema', 'Brittle Hair']
 *   Use 'None' for "None of These".
 */
export async function enterNutritionSigns(page: Page, signs: string[]) {
  await click(page.locator('.icon-task-nutrition'), page);
  await page.locator('div.page-activity.nutrition').waitFor({ timeout: 10000 });

  for (const sign of signs) {
    const label = sign === 'None' ? 'None of These' : sign;
    await click(
      page.locator('.ui.checkbox.activity', { hasText: label }),
      page,
    );
  }

  return page;
}

/**
 * Click the save button on an activity page and wait for return
 * to the encounter page. Handles the diagnosis popup if it appears.
 *
 * Returns true if a diagnosis popup appeared.
 */
export async function saveActivity(page: Page): Promise<boolean> {
  await click(page.locator('button.ui.fluid.primary.button.active'), page);

  // Check for diagnosis popup.
  let diagnosisAppeared = false;
  const popup = page.locator('div.ui.active.modal.diagnosis-popup');
  try {
    await popup.waitFor({ timeout: 3000 });
    diagnosisAppeared = true;
    // Click Continue to dismiss.
    await click(
      popup.locator('button.ui.primary.fluid.button'),
      page,
    );
  } catch {
    // No popup — that's fine.
  }

  // Wait for return to the encounter page.
  await page
    .locator('div.page-encounter.nutrition')
    .waitFor({ timeout: 10000 });

  return diagnosisAppeared;
}

/**
 * End the current encounter: click "End Encounter", confirm in the
 * dialog, and wait for return to the participant page.
 */
export async function endEncounter(page: Page) {
  // Wait for the page to stabilize (Elm re-renders can detach DOM elements).
  await page.waitForTimeout(2000);

  const endBtn = page.locator('div.actions button.ui.fluid.button', {
    hasText: 'End Encounter',
  });
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Confirm in the dialog.
  const dialog = page.locator('div.ui.tiny.active.modal');
  await dialog.waitFor({ timeout: 5000 });
  await click(
    dialog.locator('button.ui.primary.fluid.button'),
    page,
  );

  // Wait for navigation away from the encounter page.
  // The app may return to the participant page or the dashboard.
  await page
    .locator('div.page-encounter.nutrition')
    .waitFor({ state: 'hidden', timeout: 10000 });
}

/**
 * Click the sync icon, wait for sync to complete on Nyange HC,
 * then navigate back.
 */
export async function syncAndWait(page: Page) {
  await click(page.locator('span.sync-icon'), page);

  // Wait for the device status page.
  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const nyange = page.locator('.health-center', {
    has: page.locator('h2', { hasText: 'Nyange Health Center' }),
  });
  await nyange.waitFor({ timeout: 10000 });

  // Wait for sync to complete.
  await nyange
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 120000 });

  await page.goBack();
}

// ---------------------------------------------------------------------------
// NextSteps helpers
// ---------------------------------------------------------------------------

/**
 * Complete the SendToHC next-step: select "Yes" for referral
 * and "Yes" for handing the referral form.
 */
export async function completeSendToHC(page: Page) {
  await click(
    page.locator('#tasks-bar .icon-next-steps-send-to-hc').first(),
    page,
  );
  await page.locator('.ui.form.send-to-hc').waitFor({ timeout: 5000 });

  // "Referred patient to health center?" → Yes
  // Radio inputs are hidden; click the "Yes" label instead.
  await click(
    page.locator('.form-input.yes-no.refer-to-hc label', { hasText: 'Yes' }),
    page,
  );

  // "Handed referral form?" → Yes
  await click(
    page.locator('.form-input.yes-no.hand-referral-form label', { hasText: 'Yes' }),
    page,
  );

  // Save this sub-task.
  await click(page.locator('button.ui.fluid.primary.button.active'), page);
}

/**
 * Complete the HealthEducation next-step: select "Yes" for
 * education provided. Saves the sub-task.
 */
export async function completeHealthEducation(page: Page) {
  await click(
    page.locator('#tasks-bar .icon-next-steps-health-education').first(),
    page,
  );
  await page.locator('.ui.form.health-education').waitFor({ timeout: 5000 });

  // "Provided prevention education?" → Yes
  await click(
    page.locator('.form-input.yes-no.education-for-diagnosis label', { hasText: 'Yes' }),
    page,
  );

  // Save this sub-task.
  await click(page.locator('button.ui.fluid.primary.button.active'), page);
}

/**
 * Complete the ContributingFactors next-step: select a factor.
 * Saves the sub-task.
 */
export async function completeContributingFactors(page: Page) {
  await click(
    page.locator('#tasks-bar .icon-next-steps-contributing-factors').first(),
    page,
  );
  await page
    .locator('.ui.form.contributing-factors')
    .waitFor({ timeout: 5000 });

  // Select "Lack of Breast Milk" as a contributing factor.
  await click(
    page.locator('.ui.form.contributing-factors .ui.checkbox.activity', {
      hasText: 'Lack of Breast Milk',
    }),
    page,
  );

  // Save this sub-task.
  await click(page.locator('button.ui.fluid.primary.button.active'), page);
}

/**
 * Complete the FollowUp next-step: select "1 Day".
 * Saves the sub-task.
 */
export async function completeFollowUp(page: Page) {
  await click(
    page.locator('#tasks-bar .icon-next-steps-follow-up').first(),
    page,
  );
  await page.locator('.ui.form.follow-up').waitFor({ timeout: 5000 });

  // Select "1 Day".
  await click(
    page.locator('.ui.form.follow-up .ui.checkbox.activity', {
      hasText: '1 Day',
    }),
    page,
  );

  // Save this sub-task.
  await click(page.locator('button.ui.fluid.primary.button.active'), page);
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for measurement nodes linked to a person.
 * Returns an object with the measurement values found.
 */
export function queryBackendNodes(personName: string): {
  height?: number;
  weight?: number;
  muac?: number;
  nutrition?: boolean;
  sendToHc?: boolean;
  healthEducation?: boolean;
  contributingFactors?: boolean;
  followUp?: boolean;
  ncda?: boolean;
} {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
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

    \\$measurements = [];
    \\$types = [
      'nutrition_height' => 'height',
      'nutrition_weight' => 'weight',
      'nutrition_muac' => 'muac',
      'nutrition_nutrition' => 'nutrition',
      'nutrition_send_to_hc' => 'sendToHc',
      'nutrition_health_education' => 'healthEducation',
      'nutrition_contributing_factors' => 'contributingFactors',
      'nutrition_follow_up' => 'followUp',
      'nutrition_ncda' => 'ncda',
    ];

    foreach (\\$types as \\$node_type => \\$key) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$node_type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->execute();
      if (!empty(\\$r['node'])) {
        \\$nid = key(\\$r['node']);
        \\$node = node_load(\\$nid);
        \\$wrapper = entity_metadata_wrapper('node', \\$node);

        if (\\$key === 'height') {
          \\$measurements[\\$key] = (float) \\$wrapper->field_height->value();
        } elseif (\\$key === 'weight') {
          \\$measurements[\\$key] = (float) \\$wrapper->field_weight->value();
        } elseif (\\$key === 'muac') {
          \\$measurements[\\$key] = (float) \\$wrapper->field_muac->value();
        } else {
          \\$measurements[\\$key] = true;
        }
      }
    }

    echo json_encode(\\$measurements);
  `;

  const { drushCmd, cwd } = drushEnv();

  const output = execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
  });

  try {
    const parsed = JSON.parse(output.trim());
    if (parsed.error) {
      throw new Error(`Backend error: ${parsed.error}`);
    }
    return parsed;
  } catch (e) {
    if (e instanceof Error && e.message.startsWith('Backend error:')) {
      throw e;
    }
    console.error('Failed to parse drush output:', output);
    return {};
  }
}
