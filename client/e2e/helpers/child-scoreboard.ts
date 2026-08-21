import { Page, expect } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';
import {
  WAIT,
  expectMeasurementsOutOfRangeRefused,
  openActivity,
  queryMeasurementNodes,
  registerChild,
  selectCheckbox,
} from './common';

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
  await page.waitForTimeout(WAIT.formInteraction);
}

/**
 * Click an NCDA step tab icon and wait for it to become active.
 */
async function clickNCDAStepTab(
  page: Page,
  iconClass: string,
  options?: { optional?: boolean },
): Promise<boolean> {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  if (!(await tab.isVisible({ timeout: 1000 }).catch(() => false))) {
    // Not every step is part of every form: some are only asked from a certain
    // age. Only those may be missing - a step that should be there and is not
    // has to fail here, rather than leaving what follows to answer questions on
    // whichever step happens to be showing.
    if (options?.optional) {
      return false;
    }
    throw new Error(`NCDA step "${iconClass}" is not part of this form`);
  }

  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(WAIT.elmRerender);
  }
  return true;
}

/**
 * Click the Save button. In the NCDA task-tray pattern, this navigates
 * to the next incomplete step unless all steps are done.
 */
async function clickSave(page: Page) {
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(WAIT.sectionTransition);
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
  const result = await registerChild(page, 'Child Scorecard', 'child-scoreboard', {
    ageMonths: options?.ageMonths ?? 10,
    firstName: options?.firstName ?? `TestCSB${Date.now()}`,
    isChw: true,
  });

  // Start Child Scoreboard encounter.
  await click(
    page.locator('div.ui.primary.button', { hasText: 'Child Scorecard Encounter' }),
    page,
  );
  await page
    .locator('div.page-encounter.child-scoreboard')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(WAIT.sectionTransition);

  return result;
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
export async function completeNCDA(
  page: Page,
  options?: { expectMuacAsked?: boolean; diarrhea?: 'Yes' | 'No' },
) {
  await openActivity(page, 'child-scoreboard', 'history');

  // --- Step 1: Antenatal Care ---
  await clickNCDAStepTab(page, 'ncda-antenatal');
  await page.waitForTimeout(WAIT.elmRerender);

  // "Were there any ANC encounters that are not recorded above" → No
  await answerNCDAYesNo(page, 'ANC encounters that are not recorded', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Did the mother receive Iron, Folic Acid/MMS" → Yes
  await answerNCDAYesNo(page, 'receive Iron, Folic Acid', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // "Has she taken it as per guidance" → Yes (conditional, shown when above is Yes)
  await answerNCDAYesNo(page, 'taken it as per guidance', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // Birth weight — appears when newborn exam pregnancy summary has no birth weight.
  // Input class: .form-input.measurement.birth-weight, unit: grams.
  const birthWeightInput = page.locator('.form-input.measurement.birth-weight input[type="number"]');
  const birthWeightAsked = await birthWeightInput
    .isVisible({ timeout: 1000 })
    .catch(() => false);

  // "Was the child born with a birth defect" → No
  const birthDefectQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'born with a birth defect',
  });
  if (await birthDefectQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'born with a birth defect', 'No');
    await page.waitForTimeout(WAIT.formInteraction);
  }

  if (birthWeightAsked) {
    // Every other question on this step is answered, so the button is active.
    // The weight is asked on this step but the form is only saved on the last
    // one, so going on from here has to be refused just as saving would be.
    await expectMeasurementsOutOfRangeRefused(page, '.ui.form.ncda', [
      { inputId: 'birth-weight', popupClass: 'birth-weight-out-of-range', bad: '3', good: '3200' },
    ], ['birth-length-out-of-range']);
  }

  // Click Save to proceed to next step.
  await clickSave(page);

  // --- Step 2: Universal Interventions ---
  await clickNCDAStepTab(page, 'ncda-universal-intervention');
  await page.waitForTimeout(WAIT.elmRerender);

  // ChildBehindOnVaccination — conditional, may or may not appear.
  // Answer "No" (caregiver says child is up-to-date) to trigger
  // VaccinationHistory activity after NCDA is saved.
  const vaccinationQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'behind on vaccinations',
  });
  if (await vaccinationQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'behind on vaccinations', 'No');
    await page.waitForTimeout(WAIT.formInteraction);
  }

  // ChildReceivesVitaminA — checkbox select (Yes/No/Not Applicable), NOT yes/no.
  // Scoped to .checkbox-select-input to avoid matching yes/no radio labels.
  await click(
    page.locator('.checkbox-select-input .ui.checkbox label', {
      hasText: /^Yes$/i,
    }).first(),
    page,
  );
  await page.waitForTimeout(WAIT.formInteraction);

  // ChildReceivesDewormer → Yes
  await answerNCDAYesNo(page, 'receive deworming', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // OngeraMNP → Yes
  await answerNCDAYesNo(page, 'receive Ongera-MNP', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // TakingOngeraMNP → Yes (conditional, shown when OngeraMNP is Yes)
  await answerNCDAYesNo(page, 'Ongera-MNP being consumed', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // ChildReceivesECD → No (deliberately No for negative-path verification).
  await answerNCDAYesNo(page, 'sing lullabies', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  await clickSave(page);

  // --- Step 3: Nutrition Behavior (asked from six months of age) ---
  if (await clickNCDAStepTab(page, 'ncda-nutrition-behavior', { optional: true })) {
    await page.waitForTimeout(WAIT.elmRerender);

    // FiveFoodGroups → Yes
    await answerNCDAYesNo(page, '5 food groups', 'Yes');
    await page.waitForTimeout(WAIT.formInteraction);

    // BreastfedForSixMonths → Yes (shown for first NCDA on child > 6 months)
    const breastfedQuestion = page.locator('.ui.form.ncda .label', {
      hasText: 'breastfed for 6 months',
    });
    if (await breastfedQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
      await answerNCDAYesNo(page, 'breastfed for 6 months', 'Yes');
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // AppropriateComplementaryFeeding → Yes
    await answerNCDAYesNo(page, 'appropriate complementary feeding', 'Yes');
    await page.waitForTimeout(WAIT.formInteraction);

    // MealsAtRecommendedTimes → No (deliberately No for negative-path verification).
    await answerNCDAYesNo(page, 'eat at the recommended times', 'No');
    await page.waitForTimeout(WAIT.formInteraction);

    await clickSave(page);
  }

  // --- Step 4: Nutrition Assessment ---
  await clickNCDAStepTab(page, 'nutrition-assessment');
  await page.waitForTimeout(WAIT.elmRerender);

  // Stunting level: click "Green" checkbox.
  await selectCheckbox(page, 'Green');
  await page.waitForTimeout(WAIT.formInteraction);

  // Weight: enter 8.5 kg.
  const weightInput = page.locator('.form-input.measurement.weight input[type="number"]');
  await weightInput.fill('8.5');
  await page.waitForTimeout(WAIT.formInteraction);

  // MUAC: enter 12.0 cm (moderate range, < 12.5 cm. Triggers TreatedForAcuteMalnutrition question visibility).
  // Asked only from six months of age, so a younger child has no input here.
  const muacInput = page.locator('.form-input.measurement.muac input[type="number"]');
  const muacAsked = await muacInput.isVisible({ timeout: 1000 }).catch(() => false);
  if (options?.expectMuacAsked !== undefined) {
    // Asked here, on the step that draws it, because this is the only place the
    // answer means anything: anywhere else the input is absent whatever the age.
    expect(muacAsked, 'whether the form asks for a MUAC').toBe(options.expectMuacAsked);
  }
  if (muacAsked) {
    await muacInput.fill('12.0');
    await page.waitForTimeout(WAIT.formInteraction);
  }

  // ShowsEdemaSigns → No
  await answerNCDAYesNo(page, 'signs of edema', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // Every question on this step is answered, so the button is active. The
  // weight is held in kilograms and the MUAC in centimetres; a value belonging
  // to the other unit has to be refused rather than saved as it stands. The
  // form is only saved on the last step, so going on from here is refused just
  // as saving would be.
  const rangeChecks = [
    { inputId: 'weight', popupClass: 'weight-out-of-range', bad: '8500', good: '8.5' },
  ];
  if (muacAsked) {
    rangeChecks.push({ inputId: 'muac', popupClass: 'muac-out-of-range', bad: '120', good: '12.0' });
  }
  await expectMeasurementsOutOfRangeRefused(page, '.ui.form.ncda', rangeChecks, [
    'birth-weight-out-of-range',
  ]);

  await clickSave(page);

  // --- Step 5: Targeted Interventions ---
  await clickNCDAStepTab(page, 'ncda-targeted-intervention');
  await page.waitForTimeout(WAIT.elmRerender);

  // ChildReceivesFBF → Yes (pane4.row1)
  await answerNCDAYesNo(page, 'receive FBF', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // ChildTakingFBF → Yes (conditional, shown when ChildReceivesFBF is Yes)
  await answerNCDAYesNo(page, 'FBF being consumed', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // BeneficiaryCashTransfer → No (deliberately No for negative-path verification).
  await answerNCDAYesNo(page, 'beneficiary of cash transfer', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // ConditionalFoodItems → No (deliberately No for negative-path verification).
  await answerNCDAYesNo(page, 'other support', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // TreatedForAcuteMalnutrition → Yes (pane4.row2). Asked only when the MUAC is
  // off, so a child too young to be asked for one is never asked this either.
  const treatedQuestion = page.locator('.ui.form.ncda .label', {
    hasText: 'child being treated',
  });
  if (await treatedQuestion.isVisible({ timeout: 1000 }).catch(() => false)) {
    await answerNCDAYesNo(page, 'child being treated', 'Yes');
    await page.waitForTimeout(WAIT.formInteraction);
  }

  // ChildWithDisability → Yes (pane4.row4)
  await answerNCDAYesNo(page, 'have disability', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // ReceivingSupport → Yes (conditional, shown when ChildWithDisability is Yes)
  await answerNCDAYesNo(page, 'receive support', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // ChildGotDiarrhea → No by default (it contributes to no scoreboard pane —
  // pane4.row3 requires ORS/Zinc medication, not this sign). Yes is what sends
  // the child on to an acute illness encounter when this one ends, so tests
  // that want that path ask for it.
  await answerNCDAYesNo(page, 'have diarrhea', options?.diarrhea ?? 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  await clickSave(page);

  // --- Step 6: Infrastructure & Environment ---
  await clickNCDAStepTab(page, 'ncda-infrastructure-environment');
  await page.waitForTimeout(WAIT.elmRerender);

  // HasCleanWater → Yes
  await answerNCDAYesNo(page, 'clean water', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // HasHandwashingFacility → Yes
  await answerNCDAYesNo(page, 'handwashing facility', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // HasToilets → Yes
  await answerNCDAYesNo(page, 'have toilets', 'Yes');
  await page.waitForTimeout(WAIT.formInteraction);

  // HasKitchenGarden → No (deliberately No for negative-path verification).
  await answerNCDAYesNo(page, 'kitchen garden', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

  // InsecticideTreatedBednets → No (deliberately No for negative-path verification).
  await answerNCDAYesNo(page, 'insecticide-treated bednets', 'No');
  await page.waitForTimeout(WAIT.formInteraction);

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

  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Reopens the saved Child Scorecard, says the weight could not be taken, and
 * saves again.
 *
 * The form is filled over several steps and saved only at the end, so what it
 * holds between them is what gets saved. Ticking the box empties the input and
 * hides it; this checks the weight saved a moment ago does not come back in its
 * place and get written again.
 */
export async function reopenNCDAAndSayWeightNotTaken(page: Page) {
  await page.locator('div.page-encounter.child-scoreboard').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);

  await click(page.locator('#completed-tab'), page);
  await page.waitForTimeout(WAIT.elmRerender);

  // Child Scorecard and Birth History share an icon, so pick it by name.
  await click(
    page.locator('.card', { hasText: 'CHILD SCORECARD' }).locator('.icon-task-history'),
    page,
  );
  await page.locator('div.page-activity.child-scoreboard').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);

  await clickNCDAStepTab(page, 'nutrition-assessment');
  await page.waitForTimeout(WAIT.elmRerender);

  // The weight saved a moment ago is on the form.
  const weightInput = page.locator('.form-input.measurement.weight input[type="number"]');
  await expect(weightInput, 'the saved weight should be on the form').toHaveValue('8.5');

  // Three measurements on this step carry the same label, so the box is picked
  // by the measurement it belongs to.
  await click(page.locator('div.ui.checkbox.skip-step.weight'), page);
  await page.waitForTimeout(WAIT.formInteraction);

  // The input goes with it.
  await expect(weightInput, 'the input should go when the box is ticked').toHaveCount(0);

  // Save through whatever steps remain. A form that is already complete goes
  // back to the encounter sooner than one being filled for the first time, so
  // press Save until it does rather than a fixed number of times.
  const encounterPage = page.locator('div.page-encounter.child-scoreboard');
  const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });

  for (let step = 0; step < 6; step += 1) {
    if (await encounterPage.isVisible({ timeout: 1000 }).catch(() => false)) {
      break;
    }
    if (!(await saveBtn.isVisible({ timeout: 2000 }).catch(() => false))) {
      break;
    }
    await click(saveBtn, page);
    await page.waitForTimeout(WAIT.sectionTransition);
  }

  await encounterPage.waitFor({ timeout: 15000 });
  await page.waitForTimeout(WAIT.elmRerender);

  // Back to the things still to do, or the activity that follows cannot be
  // reached: this left the page on the completed ones.
  await click(page.locator('#pending-tab'), page);
  await page.waitForTimeout(WAIT.elmRerender);
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
  await openActivity(page, 'child-scoreboard', 'immunisation');

  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(WAIT.elmRerender);

    // Answer "No" to "Did the child receive any [vaccine] immunizations
    // prior to today that are not recorded above".
    // The vaccination form (.ui.form.vaccination) has a single yes/no
    // input with empty CSS class, so locate the .form-input.yes-no within it.
    const vaccinationForm = page.locator('.ui.form.vaccination');
    const yesNoInput = vaccinationForm.locator('.form-input.yes-no');
    if (await yesNoInput.isVisible({ timeout: 2000 }).catch(() => false)) {
      await click(yesNoInput.locator('label', { hasText: 'No' }), page);
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // Save the vaccine tab.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 5000 });
    await click(saveBtn, page);
    await page.waitForTimeout(WAIT.sectionTransition);
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

  await page.waitForTimeout(WAIT.elmRerender);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the Child Scoreboard encounter.
 * When child does NOT have diarrhea, no popup appears.
 */
export async function endChildScoreboardEncounter(page: Page) {
  await page.waitForTimeout(WAIT.pageNavigation);

  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for navigation away from the encounter page.
  await page
    .locator('div.page-encounter.child-scoreboard')
    .waitFor({ state: 'hidden', timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the backend for Child Scoreboard measurement nodes associated with a person.
 * Returns an object mapping node type → boolean (exists).
 */
export function queryChildScoreboardNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  return queryMeasurementNodes(personName, [
    'child_scoreboard_ncda',
    'child_scoreboard_bcg_iz',
    'child_scoreboard_dtp_iz',
    'child_scoreboard_dtp_sa_iz',
    'child_scoreboard_ipv_iz',
    'child_scoreboard_mr_iz',
    'child_scoreboard_opv_iz',
    'child_scoreboard_pcv13_iz',
    'child_scoreboard_rotarix_iz',
  ], expectedTypes);
}

/**
 * The weight held on the saved Child Scorecard, or null when it holds none.
 *
 * Read as a field rather than as "does the node exist", because the node is
 * always there - the question is whether the measurement was written into it.
 */
export function queryNCDAWeight(personName: string): number | null {
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

    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'child_scoreboard_ncda')
      ->fieldCondition('field_person', 'target_id', \\$person_nid)
      ->propertyOrderBy('nid', 'DESC')
      ->range(0, 1)
      ->execute();
    if (empty(\\$r['node'])) {
      echo json_encode(['error' => 'NCDA not found']);
      return;
    }
    \\$node = node_load(key(\\$r['node']));
    \\$wrapper = entity_metadata_wrapper('node', \\$node);
    \\$weight = \\$wrapper->field_weight->value();
    echo json_encode(['weight' => \\$weight]);
  `;

  const { drushCmd, cwd } = drushEnv();
  const output = execSync(`${drushCmd} eval "${php}"`, { cwd, timeout: 30000, encoding: 'utf-8' });
  const parsed = JSON.parse(output.trim());
  if (parsed.error) {
    throw new Error(`queryNCDAWeight: ${parsed.error}`);
  }
  return parsed.weight === null || parsed.weight === undefined ? null : Number(parsed.weight);
}


/**
 * Open the progress report (the "scorecard" tab of the encounter).
 */
export async function openScorecardReport(page: Page) {
  await click(page.locator('#scorecard-tab'), page);
  await page.locator('h1', { hasText: 'PROGRESS REPORT' }).waitFor({ timeout: 30000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * The confirmation dialog the report shows before ending an encounter.
 * Distinct from the diarrhea warning, which is a .danger-signs-popup - both
 * carry a Continue button, so tests have to say which one they mean.
 */
export function endEncounterDialog(page: Page) {
  return page.locator('div.ui.tiny.active.modal');
}

/**
 * The warning shown when a child recorded with diarrhea is referred on.
 */
export function diarrheaReferralPopup(page: Page) {
  return page.locator('div.ui.active.modal.danger-signs-popup');
}
