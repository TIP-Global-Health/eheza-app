import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Private form helpers (copy-pasted per module convention)
// ---------------------------------------------------------------------------

/**
 * Select an option in a form dropdown identified by its label text.
 * @param optionIndex - 1-based index (skips blank default).
 */
async function selectByLabel(
  page: Page,
  labelText: string,
  optionIndex: number,
) {
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
 * Open the calendar popup, select year, month, and day, then confirm.
 */
async function setDate(page: Page, dob: Date) {
  await click(page.locator('.date-input'), page);
  await page
    .locator('.ui.active.modal.calendar-popup')
    .waitFor({ timeout: 5000 });

  const year = dob.getFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  const monthValue = (dob.getMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  const day = dob.getDate();
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

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

/**
 * Navigate from the dashboard to the group session attendance page
 * using the nurse flow: Clinical → ClinicsPage → select program → select group.
 */
export async function navigateToNurseGroupSession(
  page: Page,
  program: string,
  group: string,
) {
  // Navigate: Dashboard → Clinical → Group Encounter
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  await click(page.locator('button.group-assessment'), page);

  // ClinicsPage may show "Not Found" if clinics data hasn't loaded yet.
  // Retry with page reload.
  const programBtn = page.locator('button.ui.fluid.primary.button', {
    hasText: program,
  });

  for (let attempt = 0; attempt < 5; attempt++) {
    const programVisible = await programBtn
      .waitFor({ timeout: 3000 })
      .then(() => true)
      .catch(() => false);

    if (programVisible) break;

    if (attempt === 4) {
      throw new Error(
        `Program type "${program}" button not found on ClinicsPage after retries.`,
      );
    }

    await page.reload();
    await page.waitForTimeout(1000);
  }

  await click(programBtn, page);

  // Select specific group (e.g., "Nyange I")
  await page
    .locator('button.ui.fluid.primary.button', { hasText: group })
    .waitFor({ timeout: 10000 });
  await click(
    page.locator('button.ui.fluid.primary.button', { hasText: group }),
    page,
  );

  // Wait for AttendancePage
  await page.locator('div.page-attendance').waitFor({ timeout: 30000 });
}

/**
 * Navigate from the dashboard to the group session attendance page
 * using the CHW flow: Clinical → GroupEncounterTypesPage → "Child Nutrition".
 */
export async function navigateToChwGroupSession(page: Page) {
  // Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // The GroupEncounterTypesPage fetch doesn't load clinics data.
  // Navigate to ClinicsPage first to trigger FetchClinics, then go back.
  await click(page.locator('button.group-assessment'), page);

  // CHW goes to GroupEncounterTypesPage, but we need clinics loaded.
  // Navigate back to Clinical and briefly visit ClinicsPage via the
  // nurse path (which triggers FetchClinics), then return.
  // Check if we're on GroupEncounterTypesPage.
  const onEncounterTypes = await page
    .locator('div.page-encounter-types')
    .waitFor({ timeout: 10000 })
    .then(() => true)
    .catch(() => false);

  if (onEncounterTypes) {
    // Go back to Clinical page.
    await click(page.locator('.link-back'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  }

  // Try clicking Group Assessment again — with retry/reload to ensure
  // village clinic data is available.
  for (let attempt = 0; attempt < 5; attempt++) {
    await click(page.locator('button.group-assessment'), page);
    await page
      .locator('div.page-encounter-types')
      .waitFor({ timeout: 10000 });

    await click(
      page.locator('button.encounter-type', { hasText: 'Child Nutrition' }),
      page,
    );

    const attendanceVisible = await page
      .locator('div.page-attendance')
      .waitFor({ timeout: 3000 })
      .then(() => true)
      .catch(() => false);

    if (attendanceVisible) return;

    if (attempt === 4) {
      throw new Error(
        'AttendancePage not reached after clicking Child Nutrition. Village clinic data may not be synced.',
      );
    }

    // Go back and reload to trigger data fetch.
    await click(page.locator('.link-back'), page);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
    await page.reload();
    await page.waitForTimeout(1000);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
  }
}

/**
 * Navigate to ParticipantsPage from the header tab icons.
 */
export async function goToParticipantsPage(page: Page) {
  await click(
    page.locator('ul.links-head li').filter({ has: page.locator('.icon-mother') }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });
}

/**
 * Navigate to ActivitiesPage from the header tab icons.
 */
export async function goToActivitiesPage(page: Page) {
  await click(
    page.locator('ul.links-head li').filter({ has: page.locator('.icon-measurements') }),
    page,
  );
  await page.waitForTimeout(1000);
}

/**
 * Click a mother's card on the ParticipantsPage.
 */
export async function clickMotherCard(page: Page, motherName: string) {
  const card = page.locator('.ui.four.cards .card', {
    has: page.locator('p.mother', { hasText: motherName }),
  });
  await click(card, page);
  await page.waitForTimeout(1000);
}

/**
 * From the MotherPage, navigate to a child's page by clicking the child icon
 * in the family links bar. The links-body contains icon-only li elements
 * (icon-mother, icon-baby with count), not text names.
 *
 * @param childIndex 0-based index of the child (default: 0 = first child).
 */
export async function navigateToChild(page: Page, childIndex: number = 0) {
  // Child icons have .icon-baby class inside the links-body.
  // The first non-active child icon is the one we want to click.
  const childLinks = page.locator('ul.links-body li:has(.icon-baby)');
  await childLinks.nth(childIndex).click();
  await page.waitForTimeout(1000);
}

/**
 * Navigate back to the MotherPage from a ChildPage by clicking
 * the mother icon in the family links bar.
 */
export async function navigateToMother(page: Page) {
  // Dismiss any overlay/modal that might be blocking (e.g., NCDA skip dialog).
  await dismissOverlay(page);

  const motherLink = page.locator('ul.links-body li:has(.icon-mother)');
  await motherLink.click();
  await page.waitForTimeout(1000);
}

/**
 * Dismiss any overlay or modal dialog that may be blocking clicks.
 * Common overlays: NCDA skip dialog, diagnosis popups, warning alerts.
 */
async function dismissOverlay(page: Page) {
  // Check for overlay (e.g., NCDA "Child Scorecard" prompt).
  const overlay = page.locator('div.overlay');
  if (await overlay.isVisible().catch(() => false)) {
    // Try clicking any button inside the overlay to dismiss it.
    const buttons = overlay.locator('button');
    const count = await buttons.count();
    if (count > 0) {
      // Prefer "No, skip" or the last button (usually dismiss/cancel).
      const skipBtn = overlay.locator('button', { hasText: /skip|cancel|close/i });
      if (await skipBtn.first().isVisible().catch(() => false)) {
        await skipBtn.first().click({ force: true });
      } else {
        await buttons.last().click({ force: true });
      }
    }
    await overlay.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
    await page.waitForTimeout(300);
  }

  // Check for modal dialog.
  const modal = page.locator('div.ui.tiny.active.modal');
  if (await modal.isVisible().catch(() => false)) {
    const skipBtn = modal.locator('button', { hasText: /skip|cancel/i });
    if (await skipBtn.first().isVisible().catch(() => false)) {
      await skipBtn.first().click({ force: true });
    } else {
      await modal.locator('button').last().click({ force: true });
    }
    await modal.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
    await page.waitForTimeout(300);
  }
}

// ---------------------------------------------------------------------------
// Participant registration
// ---------------------------------------------------------------------------

/**
 * From the attendance page, register a new adult female (mother).
 * Clicks "Add New Participant" → People search → Register → fill form → submit.
 *
 * Returns on PersonPage with { firstName, secondName, fullName }.
 */
export async function createMotherOnAttendancePage(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isChw?: boolean;
  },
) {
  const ageYears = options?.ageYears ?? 25;
  const firstName = options?.firstName ?? `TestMother${Date.now()}`;
  const secondName = 'E2ETest';
  const isChw = options?.isChw ?? false;

  // Click "Add New Participant" on the attendance page.
  await click(
    page.locator('.register-actions button.ui.primary.button.fluid', {
      hasText: 'Add New Participant',
    }),
    page,
  );

  // Wait for People search page.
  await page.locator('div.page-people').waitFor({ timeout: 10000 });

  // Click "Register a new participant".
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill registration form.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth.
  const dob = new Date();
  dob.setFullYear(dob.getFullYear() - ageYears);
  await setDate(page, dob);

  // Select gender = female (last radio).
  await page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]')
    .last()
    .check();

  // Adult required fields.
  await selectByLabel(page, 'Level of Education:', 1);
  await selectByLabel(page, 'Marital Status:', 1);

  if (!isChw) {
    // Nurse: fill cascading address dropdowns.
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

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the Person page.
  await page.locator('div.page-person').waitFor({ timeout: 30000 });

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * From the mother's PersonPage, add a child by registering a new person
 * and creating a relationship.
 *
 * Flow: PersonPage → "Add Child" → People search → Register → fill form →
 *       Relationship page → select "My Child" → Save → back to PersonPage.
 */
export async function addChildToMother(
  page: Page,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isFemale?: boolean;
    isChw?: boolean;
  },
) {
  const ageMonths = options?.ageMonths ?? 12;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;
  const isChw = options?.isChw ?? false;

  // Click the "Add Child" area on the PersonPage.
  await click(page.locator('.add-participant-icon'), page);

  // Wait for People search page.
  await page.locator('div.page-people').waitFor({ timeout: 10000 });

  // Click "Register a new participant".
  await click(
    page.locator('button.ui.primary.button.fluid', {
      hasText: 'Register a new participant',
    }),
    page,
  );
  await page
    .locator('.ui.grid .column', { hasText: 'First Name:' })
    .waitFor({ timeout: 10000 });

  // Fill child registration form.
  await formInput(page, 'First Name:').fill(firstName);
  await formInput(page, 'Second Name:').fill(secondName);

  // Set date of birth.
  const dob = new Date();
  dob.setMonth(dob.getMonth() - ageMonths);
  await setDate(page, dob);

  // Select gender.
  const genderRadios = page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]');
  if (isFemale) {
    await genderRadios.last().check();
  } else {
    await genderRadios.first().check();
  }

  // Mode of delivery (required for children).
  await selectByLabel(page, 'Mode of delivery:', 1);

  if (!isChw) {
    // Nurse: fill cascading address dropdowns.
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

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the Relationship page.
  await page.locator('div.page-relationship').waitFor({ timeout: 30000 });

  // Wait for the relationship form to appear.
  const relationForm = page.locator('.ui.form.relationship-selector');
  let formVisible = await relationForm
    .waitFor({ timeout: 15000 })
    .then(() => true)
    .catch(() => false);

  // Retry with reloads if form didn't appear.
  if (!formVisible) {
    for (let attempt = 0; attempt < 3; attempt++) {
      await page.waitForTimeout(3000);
      await page.reload();
      await page
        .locator('div.page-relationship')
        .waitFor({ timeout: 10000 });

      formVisible = await relationForm
        .waitFor({ timeout: 15000 })
        .then(() => true)
        .catch(() => false);

      if (formVisible) {
        break;
      }
    }
  }

  if (!formVisible) {
    throw new Error(
      'Relationship form never appeared after retries. ' +
        'The backend may not have indexed the newly created person yet.',
    );
  }

  // Select relationship type: "My Child" (first radio for adults).
  const relationRadio = page.locator(
    '.ui.radio.checkbox label.relationship-selection',
  ).first();
  await click(relationRadio, page);
  await page.waitForTimeout(500);

  // For GroupEncounterOrigin, the group is auto-selected (only the session's clinic).
  // No need to manually select a group.

  // Click Save.
  await click(
    page.locator('.save-buttons button.ui.button.primary.fluid', {
      hasText: 'Save',
    }),
    page,
  );

  // For GroupEncounterOrigin, saving the relationship creates a PMTCT
  // participant and auto-navigates back to the AttendancePage (with the
  // mother auto-checked-in). It may also land on PersonPage first.
  const personPage = page.locator('div.page-person');
  const attendancePage = page.locator('div.page-attendance');

  await Promise.race([
    personPage.waitFor({ timeout: 30000 }),
    attendancePage.waitFor({ timeout: 30000 }),
  ]);

  await page.waitForTimeout(1000);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * From the PersonPage, navigate back to the AttendancePage.
 * Clicks the back arrow which goes through People search → AttendancePage.
 */
export async function navigateBackToAttendance(page: Page) {
  // Click back from PersonPage.
  await click(page.locator('.link-back'), page);

  // May land on People search page first — check and navigate if needed.
  const peoplePage = page.locator('div.page-people');
  const attendancePage = page.locator('div.page-attendance');

  // Wait for either page.
  await Promise.race([
    peoplePage.waitFor({ timeout: 10000 }),
    attendancePage.waitFor({ timeout: 10000 }),
  ]);

  // If on People search page, click back again.
  if (await peoplePage.isVisible()) {
    await click(page.locator('.link-back'), page);
    await attendancePage.waitFor({ timeout: 10000 });
  }

  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Child activity helpers
// ---------------------------------------------------------------------------

/**
 * Click an activity tab on the ChildPage or MotherPage.
 * Activities are rendered as clickable divs in a grid, each with
 * the activity name as text (e.g., "Height", "MUAC", "Weight").
 */
async function openActivity(page: Page, activityName: string) {
  // Dismiss any overlay that might be blocking (e.g., nutrition warning).
  await dismissOverlay(page);

  // The activity tabs are in the .ui.task.segment grid.
  // Each tab is a div with the activity icon and name text.
  const tab = page.locator('.ui.task.segment div', {
    hasText: new RegExp(`^${activityName}$`),
  });

  // If the tab doesn't exist (maybe it's already selected), try a broader match.
  const visible = await tab
    .first()
    .waitFor({ timeout: 5000 })
    .then(() => true)
    .catch(() => false);

  if (visible) {
    await click(tab.first(), page);
  }

  await page.waitForTimeout(500);
}

/**
 * Click Save on the current activity form (group session context).
 * The save button has id="save-form".
 */
async function saveActivity(page: Page) {
  const saveBtn = page.locator('#save-form');
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(1000);
}

/**
 * Complete the Height activity for a child.
 */
export async function completeHeight(page: Page, value: string) {
  await openActivity(page, 'Height');
  await page.locator('input[type="number"][name="height"]').fill(value);
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Weight activity for a child.
 */
export async function completeWeight(page: Page, value: string) {
  await openActivity(page, 'Weight');
  await page.locator('input[type="number"][name="weight"]').fill(value);
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the MUAC activity for a child.
 */
export async function completeMuac(page: Page, value: string) {
  await openActivity(page, 'MUAC');
  // MUAC uses a number input (spinbutton) — find it near the Save button.
  const muacInput = page.locator('input[type="number"]').first();
  await muacInput.fill(value);
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the NutritionSigns activity for a child.
 * Selects "Normal" (NormalChildNutrition) to indicate no signs.
 */
export async function completeNutritionSigns(page: Page) {
  await openActivity(page, 'Nutrition');
  // Click "None of these" checkbox to indicate no nutrition signs.
  // The checkbox input is CSS-hidden; click its label instead.
  const label = page.getByText('None of these', { exact: true });
  await label.scrollIntoViewIfNeeded();
  await label.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Child FBF distribution activity.
 * Selects quantity=1 from the dropdown.
 */
export async function completeChildFbf(page: Page) {
  await openActivity(page, 'FBF Child');
  // Select quantity from the FBF distribution dropdown.
  const fbfSelect = page.locator('select').first();
  await fbfSelect.selectOption('1');
  await page.waitForTimeout(300);
  await saveActivity(page);
}

// ---------------------------------------------------------------------------
// Private NCDA helpers for group-session step-by-step completion
// ---------------------------------------------------------------------------

/**
 * Answer all visible Yes/No questions with "No" and fill empty number inputs.
 * Used as the first pass before overriding specific answers to "Yes".
 */
async function answerAllNoAndFillNumbers(page: Page) {
  await page.waitForTimeout(500);

  // Click all visible "No" labels.
  const noButtons = page.locator('.form-input label', { hasText: /^No$/ });
  const noCount = await noButtons.count();
  for (let i = 0; i < noCount; i++) {
    const btn = noButtons.nth(i);
    if (await btn.isVisible().catch(() => false)) {
      await btn.scrollIntoViewIfNeeded();
      await btn.click({ force: true });
      await page.waitForTimeout(200);
    }
  }

  // Fill all empty visible number inputs with "3000".
  const numberInputs = page.locator('.form-input input[type="number"]');
  const numCount = await numberInputs.count();
  for (let i = 0; i < numCount; i++) {
    const input = numberInputs.nth(i);
    if (await input.isVisible() && (await input.inputValue()) === '') {
      await input.fill('3000');
      await page.waitForTimeout(200);
    }
  }
}

/**
 * Override a specific NCDA question to "Yes" by finding its label text
 * and clicking the "Yes" radio in the associated .form-input container.
 * Silently returns if the question is not visible.
 */
async function overrideToYes(page: Page, questionSubstring: string) {
  const questionLabel = page.locator('.ui.form .label', { hasText: questionSubstring }).first();
  if (!(await questionLabel.isVisible({ timeout: 1000 }).catch(() => false))) {
    return;
  }

  // Navigate to the sibling .form-input element via evaluate, same pattern
  // as answerNCDAYesNo in child-scoreboard.ts.
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
    return; // Silently skip if form-input not found.
  }

  await click(page.locator(`#${yesNoId} label`, { hasText: 'Yes' }), page);
  await page.waitForTimeout(300);
}

/**
 * Click "Save" and dismiss any post-save overlay or modal dialog.
 */
async function clickGroupNCDASave(page: Page) {
  const saveBtn = page.locator('button', { hasText: /^Save$/i });
  await saveBtn.scrollIntoViewIfNeeded();
  await saveBtn.click({ force: true });
  await page.waitForTimeout(1000);

  // Dismiss overlay if present.
  const overlay = page.locator('div.overlay');
  if (await overlay.isVisible().catch(() => false)) {
    const skipBtn = overlay.locator('button', { hasText: 'No, skip' });
    const proceedBtn = overlay.locator('button', { hasText: 'Yes, proceed' });
    if (await skipBtn.isVisible().catch(() => false)) {
      await skipBtn.click({ force: true });
    } else if (await proceedBtn.isVisible().catch(() => false)) {
      await proceedBtn.click({ force: true });
    }
    await overlay.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
    await page.waitForTimeout(500);
  }

  // Dismiss modal if present.
  const modal = page.locator('div.ui.tiny.active.modal');
  if (await modal.isVisible().catch(() => false)) {
    const skipModalBtn = modal.locator('button', { hasText: 'No, skip' });
    const proceedModalBtn = modal.locator('button', { hasText: 'Yes, proceed' });
    if (await skipModalBtn.isVisible().catch(() => false)) {
      await skipModalBtn.click({ force: true });
    } else if (await proceedModalBtn.isVisible().catch(() => false)) {
      await proceedModalBtn.click({ force: true });
    }
    await modal.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
    await page.waitForTimeout(500);
  }
}

/**
 * Complete the NCDA (Child Scorecard) activity for a child.
 * The NCDA form has 6 steps. Each step answers all questions "No" first,
 * then overrides specific questions to "Yes" for scoreboard coverage.
 *
 * Steps: AntenatalCare, UniversalInterventions, NutritionBehavior,
 *        NutritionAssessment, TargetedInterventions, InfrastructureEnvironment.
 *
 * The "Yes" answers here are complementary to the child-scoreboard NCDA's
 * "No" answers, ensuring full scoreboard coverage across both encounter types.
 */
export async function completeNCDA(page: Page) {
  await openActivity(page, 'Child Scorecard');

  // The NCDA "Child Scorecard" triggers a skip dialog overlay:
  // "Would you like to proceed?" with "Yes, proceed" / "No, skip".
  // Click "Yes, proceed" to enter the actual NCDA form.
  await page.waitForTimeout(500);
  const overlay = page.locator('div.overlay');
  if (await overlay.isVisible().catch(() => false)) {
    const proceedBtn = overlay.locator('button', { hasText: 'Yes, proceed' });
    if (await proceedBtn.isVisible().catch(() => false)) {
      await proceedBtn.click({ force: true });
      await overlay.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
      await page.waitForTimeout(500);
    }
  }

  // Also check for modal dialog version.
  const modal = page.locator('div.ui.tiny.active.modal');
  if (await modal.isVisible().catch(() => false)) {
    const proceedBtn = modal.locator('button', { hasText: 'Yes, proceed' });
    if (await proceedBtn.isVisible().catch(() => false)) {
      await proceedBtn.click({ force: true });
      await modal.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
      await page.waitForTimeout(500);
    }
  }

  // --- Step 1: Antenatal Care (no overrides) ---
  await answerAllNoAndFillNumbers(page);
  await clickGroupNCDASave(page);

  // --- Step 2: Universal Interventions ---
  await answerAllNoAndFillNumbers(page);
  // ECD → Yes (complementary: child-scoreboard answers No).
  await overrideToYes(page, 'sing lullabies');
  await clickGroupNCDASave(page);

  // --- Step 3: Nutrition Behavior ---
  await answerAllNoAndFillNumbers(page);
  // MealsAtRecommendedTimes → Yes (complementary: child-scoreboard answers No).
  await overrideToYes(page, 'eat at the recommended times');
  await clickGroupNCDASave(page);

  // --- Step 4: Nutrition Assessment (may not be shown for group sessions) ---
  const stillOnForm = await page
    .locator('.link-section.active')
    .isVisible()
    .catch(() => false);

  if (stillOnForm) {
    await answerAllNoAndFillNumbers(page);
    await clickGroupNCDASave(page);
  }

  // --- Step 5: Targeted Interventions ---
  const onStep5 = await page
    .locator('.link-section.active')
    .isVisible()
    .catch(() => false);

  if (onStep5) {
    await answerAllNoAndFillNumbers(page);
    // BeneficiaryCashTransfer → Yes (complementary: child-scoreboard answers No).
    await overrideToYes(page, 'beneficiary of cash transfer');
    await page.waitForTimeout(500);
    // ReceivingCashTransfer → Yes (conditional, appears when above is Yes).
    await overrideToYes(page, 'Are they receiving it');
    // ConditionalFoodItems → Yes (complementary: child-scoreboard answers No).
    await overrideToYes(page, 'other support');
    await clickGroupNCDASave(page);
  }

  // --- Step 6: Infrastructure & Environment ---
  const onStep6 = await page
    .locator('.link-section.active')
    .isVisible()
    .catch(() => false);

  if (onStep6) {
    await answerAllNoAndFillNumbers(page);
    // InsecticideTreatedBednets → Yes (complementary: child-scoreboard answers No).
    await overrideToYes(page, 'insecticide-treated bednets');
    // HasKitchenGarden → Yes (complementary: child-scoreboard answers No).
    await overrideToYes(page, 'kitchen garden');
    await clickGroupNCDASave(page);
  }
}

/**
 * Complete the NutritionSigns activity with abnormal signs (Edema).
 * This triggers NextSteps activities.
 */
export async function completeNutritionSignsAbnormal(page: Page) {
  await openActivity(page, 'Nutrition');
  const edemaLabel = page.getByText('Edema', { exact: true });
  await edemaLabel.scrollIntoViewIfNeeded();
  await edemaLabel.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

// ---------------------------------------------------------------------------
// NextSteps activity helpers (triggered by abnormal nutrition values)
// ---------------------------------------------------------------------------

/**
 * Complete the Contributing Factors activity.
 * Selects "None" (NoContributingFactorsSign).
 */
export async function completeContributingFactors(page: Page) {
  await openActivity(page, 'Contributing Factors');
  const noneLabel = page.getByText('None of these', { exact: true });
  await noneLabel.scrollIntoViewIfNeeded();
  await noneLabel.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Health Education activity.
 * Selects "Yes" for providing health education.
 */
export async function completeHealthEducation(page: Page) {
  await openActivity(page, 'Health Education');
  const yesLabel = page.getByText('Yes', { exact: true }).first();
  await yesLabel.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Send to HC activity.
 * Selects referral reason.
 */
export async function completeSendToHC(page: Page) {
  await openActivity(page, 'Send to Health Center');
  // Answer Yes/No questions — click all "Yes" labels.
  const yesLabels = page.getByText('Yes', { exact: true });
  const count = await yesLabels.count();
  for (let i = 0; i < count; i++) {
    await yesLabels.nth(i).click({ force: true });
    await page.waitForTimeout(200);
  }
  await saveActivity(page);
}

/**
 * Complete the Follow Up activity.
 * Selects a follow-up option.
 */
export async function completeFollowUp(page: Page) {
  await openActivity(page, 'Follow Up');
  // Select "1 Day" follow-up option.
  const option = page.getByText('1 Day', { exact: true });
  await option.scrollIntoViewIfNeeded();
  await option.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

// ---------------------------------------------------------------------------
// Mother activity helpers
// ---------------------------------------------------------------------------

/**
 * Complete the Family Planning activity for a mother.
 * Selects the first available family planning method.
 */
export async function completeFamilyPlanning(page: Page) {
  await openActivity(page, 'Family Planning');
  // Click "Condoms" checkbox.
  const condoms = page.getByText('Condoms', { exact: true });
  await condoms.scrollIntoViewIfNeeded();
  await condoms.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Lactation activity for a mother.
 * Answers "Yes" to breastfeeding question.
 */
export async function completeLactation(page: Page) {
  await openActivity(page, 'Lactation');
  // Click "Yes" for breastfeeding. Use getByText to find the label.
  const yesLabel = page.getByText('Yes', { exact: true });
  await yesLabel.click({ force: true });
  await page.waitForTimeout(300);
  await saveActivity(page);
}

/**
 * Complete the Mother FBF distribution activity.
 * Selects quantity=1 from the dropdown.
 */
export async function completeMotherFbf(page: Page) {
  await openActivity(page, 'FBF Mother');
  // The FBF select uses class .fbf-distirbution (note typo in original code).
  const fbfSelect = page.locator('select').first();
  await fbfSelect.selectOption('1');
  await page.waitForTimeout(300);
  await saveActivity(page);
}

// ---------------------------------------------------------------------------
// End session
// ---------------------------------------------------------------------------

/**
 * End the group session.
 * Navigates to ActivitiesPage, clicks "End Group Encounter", confirms dialog.
 */
export async function endGroupSession(page: Page) {
  await page.waitForTimeout(2000);

  // Click "End Group Encounter" button.
  const endBtn = page.locator('button.ui.fluid.button', {
    hasText: 'End Group Encounter',
  });
  await endBtn.waitFor({ timeout: 10000 });
  await click(endBtn, page);

  // The ActivitiesPage "End Group Encounter" navigates directly to ClinicalPage.
  // The ParticipantsPage version shows a confirmation dialog first.
  // Handle both cases.
  const dialog = page.locator('div.ui.active.modal');
  const dialogVisible = await dialog
    .waitFor({ timeout: 3000 })
    .then(() => true)
    .catch(() => false);

  if (dialogVisible) {
    const confirmBtn = dialog.locator('button.ui.primary.button', {
      hasText: 'Continue',
    });
    await confirmBtn.click({ force: true });
    await dialog.waitFor({ state: 'hidden', timeout: 10000 });
  }

  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Backend verification
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for group session measurement nodes
 * linked to the given mother and child.
 *
 * Inputs are base64-encoded to prevent shell injection.
 */
export function queryGroupSessionNodes(
  motherName: string,
  childName?: string,
): Record<string, boolean> {
  const motherNameB64 = Buffer.from(motherName, 'utf8').toString('base64');
  const childNameB64 = childName
    ? Buffer.from(childName, 'utf8').toString('base64')
    : '';

  const php = `
    \\$mother_name = base64_decode('${motherNameB64}');
    \\$child_name = ${childNameB64 ? `base64_decode('${childNameB64}')` : 'NULL'};

    // Find mother person node.
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$mother_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Mother not found: ' . \\$mother_name]);
      return;
    }
    \\$mother_nid = key(\\$result['node']);

    // Find child person node.
    \\$child_nid = NULL;
    if (\\$child_name) {
      \\$query = new EntityFieldQuery();
      \\$result = \\$query->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'person')
        ->propertyCondition('title', \\$child_name)
        ->execute();
      if (!empty(\\$result['node'])) {
        \\$child_nid = key(\\$result['node']);
      }
    }

    \\$out = array();

    // Check mother measurement types.
    \\$mother_types = array(
      'attendance' => 'attendance',
      'family_planning' => 'familyPlanning',
      'lactation' => 'lactation',
      'mother_fbf' => 'motherFbf',
    );
    foreach (\\$mother_types as \\$node_type => \\$key) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$node_type)
        ->fieldCondition('field_person', 'target_id', \\$mother_nid)
        ->range(0, 1)
        ->execute();
      \\$out[\\$key] = !empty(\\$r['node']);
    }

    // Check child measurement types.
    if (\\$child_nid) {
      \\$child_types = array(
        'height' => 'height',
        'weight' => 'weight',
        'muac' => 'muac',
        'nutrition' => 'nutrition',
        'child_fbf' => 'childFbf',
        'photo' => 'photo',
        'contributing_factors' => 'contributingFactors',
        'follow_up' => 'followUp',
        'group_health_education' => 'groupHealthEducation',
        'group_send_to_hc' => 'groupSendToHC',
        'group_ncda' => 'groupNcda',
      );
      foreach (\\$child_types as \\$node_type => \\$key) {
        \\$q = new EntityFieldQuery();
        \\$r = \\$q->entityCondition('entity_type', 'node')
          ->propertyCondition('type', \\$node_type)
          ->fieldCondition('field_person', 'target_id', \\$child_nid)
          ->range(0, 1)
          ->execute();
        \\$out[\\$key] = !empty(\\$r['node']);
      }
    } else {
      \\$out['height'] = FALSE;
      \\$out['weight'] = FALSE;
      \\$out['muac'] = FALSE;
      \\$out['nutrition'] = FALSE;
      \\$out['childFbf'] = FALSE;
      \\$out['photo'] = FALSE;
      \\$out['contributingFactors'] = FALSE;
      \\$out['followUp'] = FALSE;
      \\$out['groupHealthEducation'] = FALSE;
      \\$out['groupSendToHC'] = FALSE;
      \\$out['groupNcda'] = FALSE;
    }

    echo json_encode(\\$out);
  `;

  const { drushCmd, cwd } = drushEnv();

  // Determine required keys based on whether we have a child.
  const requiredKeys = ['attendance'];
  if (childName) {
    requiredKeys.push('height', 'weight', 'muac', 'nutrition');
  }

  // Retry for eventual consistency after sync.
  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd,
        timeout: 30000,
        encoding: 'utf-8',
      });

      const parsed = JSON.parse(output.trim());
      if (parsed.error) {
        throw new Error(`Backend error: ${parsed.error}`);
      }

      const allFound = requiredKeys.every((k) => parsed[k] === true);
      if (allFound) {
        return parsed;
      }

      const missing = requiredKeys.filter((k) => !parsed[k]);
      console.log(
        `[queryGroupSessionNodes] attempt ${attempt + 1}: missing ${missing.join(', ')}`,
      );
    } catch (e) {
      console.log(
        `[queryGroupSessionNodes] attempt ${attempt + 1}: error — ${e instanceof Error ? e.message : String(e)}`,
      );
    }

    if (attempt < 9) {
      execSync('sleep 5');
    }
  }

  // Final attempt.
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
