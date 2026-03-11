import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Private form helpers
// ---------------------------------------------------------------------------

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

function formInput(page: Page, labelText: string) {
  return page
    .locator('.ui.grid')
    .filter({ hasText: labelText })
    .locator('input')
    .first();
}

async function answerYesNo(
  page: Page,
  fieldClass: string,
  answer: 'Yes' | 'No',
) {
  await click(
    page.locator(`.form-input.yes-no.${fieldClass} label`, {
      hasText: answer,
    }),
    page,
  );
}

async function selectCheckbox(page: Page, optionText: string) {
  await click(
    page.locator('.ui.checkbox label', {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }),
    page,
  );
}

async function selectCheckboxInForm(page: Page, formSelector: string, optionText: string) {
  await click(
    page.locator(`${formSelector} .ui.checkbox`, {
      hasText: new RegExp(`^${optionText}$`, 'i'),
    }).locator('label'),
    page,
  );
}

async function clickSubTaskTab(page: Page, iconClass: string) {
  const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  const isActive = await tab.evaluate(el => el.classList.contains('active')).catch(() => false);
  if (!isActive) {
    await click(tab, page);
    await page.waitForTimeout(500);
  }
}

async function openActivity(page: Page, activityIcon: string) {
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);

  // Dismiss any popup that may overlay the encounter page.
  await dismissPopup(page);

  await click(page.locator(`.icon-task-${activityIcon}`), page);
  await page.locator('div.page-activity.well-child').waitFor({ timeout: 10000 });
}

async function saveActivityAndReturn(page: Page) {
  const saveBtn = page.locator('.actions button.ui.fluid.primary.button.active');
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(500);
}

async function saveSubTaskAndContinue(page: Page) {
  const saveBtn = page.locator('.actions button.ui.fluid.primary.button.active');
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);
  await page.waitForTimeout(500);
}

async function dismissPopup(page: Page) {
  // Nutrition assessment diagnosis popup.
  const diagnosisPopup = page.locator('div.ui.active.modal.diagnosis-popup');
  if (await diagnosisPopup.isVisible({ timeout: 2000 }).catch(() => false)) {
    const btn = diagnosisPopup.locator('button.ui.primary.fluid.button');
    await btn.waitFor({ timeout: 3000 }).catch(() => {});
    if (await btn.isVisible().catch(() => false)) {
      await btn.click({ force: true });
      await page.waitForTimeout(1000);
      // Verify popup is gone.
      await diagnosisPopup.waitFor({ state: 'hidden', timeout: 5000 }).catch(() => {});
      return;
    }
  }
  // Danger signs popup on encounter page.
  const dangerSignsPopup = page.locator('div.ui.active.modal.danger-signs-popup');
  if (await dangerSignsPopup.isVisible({ timeout: 1000 }).catch(() => false)) {
    // Click "Cancel" to dismiss without triggering Acute Illness encounter.
    const cancelBtn = dangerSignsPopup.locator('button.ui.fluid.button', { hasText: 'Cancel' });
    if (await cancelBtn.isVisible({ timeout: 500 }).catch(() => false)) {
      await click(cancelBtn, page);
      await page.waitForTimeout(500);
      return;
    }
    // ECD popup only has "Continue".
    const continueBtn = dangerSignsPopup.locator('button.ui.fluid.button', { hasText: 'Continue' });
    if (await continueBtn.isVisible({ timeout: 500 }).catch(() => false)) {
      await click(continueBtn, page);
      await page.waitForTimeout(500);
      return;
    }
  }
  // Head circumference popup (macro/microcephaly).
  const hcPopup = page.locator('div.ui.active.modal.danger-signs-popup button', { hasText: 'Continue' });
  if (await hcPopup.isVisible({ timeout: 1000 }).catch(() => false)) {
    await click(hcPopup, page);
    await page.waitForTimeout(500);
  }
}

async function fillMeasurement(page: Page, id: string, value: string) {
  await page
    .locator(`.form-input.measurement.${id} input[type="number"]`)
    .fill(value);
}

async function setDate(page: Page, date: Date, triggerSelector = '.form-input.date') {
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

async function setDateOfBirth(page: Page, dob: Date) {
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
// Participant registration
// ---------------------------------------------------------------------------

export async function createChildAndStartWellChildEncounter(
  page: Page,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isChw?: boolean;
  },
) {
  const ageMonths = options?.ageMonths ?? 24;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isChw = options?.isChw ?? false;

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Individual Encounter
  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  // Individual Encounter → Well Child
  const encounterTypeText = isChw ? 'Well Child Visit' : 'Standard Pediatric Visit';
  await click(
    page.locator('button.encounter-type', { hasText: encounterTypeText }),
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
  await setDateOfBirth(page, dob);

  // Select gender = male.
  await page
    .locator('.ui.grid')
    .filter({ hasText: 'Gender:' })
    .locator('input[type="radio"]')
    .first()
    .check();

  // Mode of delivery (required field for children).
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

    const hcSelect = page
      .locator('.ui.grid')
      .filter({ hasText: 'Health Center:' })
      .locator('select');
    await hcSelect.selectOption({ label: 'Nyange Health Center' });
  }

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the participant page.
  await page
    .locator('div.page-participant.individual.well-child')
    .waitFor({ timeout: 30000 });

  // Click the encounter button to start.
  const encounterBtnText = isChw ? 'Well Child Visit' : 'Standard Pediatric Visit Encounter';
  await click(
    page.locator('div.ui.primary.button', { hasText: encounterBtnText }),
    page,
  );
  await page
    .locator('div.page-encounter.well-child')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

// ---------------------------------------------------------------------------
// Danger Signs activity
// ---------------------------------------------------------------------------

export async function completeDangerSigns(
  page: Page,
  options?: {
    symptoms?: string[];
    respiratoryRate?: string;
    bodyTemp?: string;
  },
) {
  const symptoms = options?.symptoms ?? [];
  const respiratoryRate = options?.respiratoryRate ?? '20';
  const bodyTemp = options?.bodyTemp ?? '36.5';

  await openActivity(page, 'danger-signs');

  // --- SymptomsReview tab ---
  await clickSubTaskTab(page, 'symptoms');
  if (symptoms.length === 0) {
    await selectCheckbox(page, 'None of these');
  } else {
    for (const symptom of symptoms) {
      await selectCheckbox(page, symptom);
    }
  }
  await saveSubTaskAndContinue(page);

  // --- Vitals tab ---
  await clickSubTaskTab(page, 'vitals');
  await fillMeasurement(page, 'respiratory-rate', respiratoryRate);
  await fillMeasurement(page, 'body-temperature', bodyTemp);
  await saveSubTaskAndContinue(page);

  // Wait for return to encounter page.
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Nutrition Assessment activity
// ---------------------------------------------------------------------------

export async function completeNutritionAssessment(
  page: Page,
  options?: {
    height?: string;
    headCircumference?: string;
    muac?: string;
    nutritionSigns?: string[];
    weight?: string;
  },
) {
  const nutritionSigns = options?.nutritionSigns ?? [];

  await openActivity(page, 'nutrition-assessment');

  // --- Height tab (if visible) ---
  const heightTab = page.locator('.link-section:has(.icon-activity-task.icon-height)');
  if (options?.height && await heightTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'height');
    await fillMeasurement(page, 'height', options.height);
    await saveSubTaskAndContinue(page);
  }

  // --- Head Circumference tab (if visible) ---
  const hcTab = page.locator('.link-section:has(.icon-activity-task.icon-head-circumference)');
  if (options?.headCircumference && await hcTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'head-circumference');
    await fillMeasurement(page, 'head-circumference', options.headCircumference);
    await saveSubTaskAndContinue(page);

    // Handle macro/microcephaly popup.
    await dismissPopup(page);
  }

  // --- MUAC tab (if visible) ---
  const muacTab = page.locator('.link-section:has(.icon-activity-task.icon-muac)');
  if (options?.muac && await muacTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'muac');
    await fillMeasurement(page, 'muac', options.muac);
    await saveSubTaskAndContinue(page);
  }

  // --- Nutrition tab ---
  const nutritionTab = page.locator('.link-section:has(.icon-activity-task.icon-nutrition)');
  if (await nutritionTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'nutrition');
    if (nutritionSigns.length === 0) {
      await selectCheckbox(page, 'None of These');
    } else {
      for (const sign of nutritionSigns) {
        await selectCheckbox(page, sign);
      }
    }
    await saveSubTaskAndContinue(page);

    // Nutrition assessment may trigger a diagnosis popup.
    await dismissPopup(page);
  }

  // --- Weight tab ---
  const weightTab = page.locator('.link-section:has(.icon-activity-task.icon-weight)');
  if (options?.weight && await weightTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'weight');
    await fillMeasurement(page, 'weight', options.weight);
    await saveSubTaskAndContinue(page);
  }

  // Wait for return to encounter page (or NextSteps if diagnosis triggered).
  // The diagnosis popup may take a moment to appear after the last save.
  await page.waitForTimeout(2000);
  await dismissPopup(page);
  await page.waitForTimeout(500);
  // Double-check — the popup may appear after a re-render.
  await dismissPopup(page);
}

// ---------------------------------------------------------------------------
// ECD activity (PediatricCare nurse only)
// ---------------------------------------------------------------------------

export async function completeECD(page: Page) {
  await openActivity(page, 'ecd');

  // The ECD form shows boolean Yes/No questions for age-appropriate milestones.
  // Answer "Yes" to all by clicking each "Yes" label in the form.
  const ecdForm = page.locator('.ui.form.ecd');
  await ecdForm.waitFor({ timeout: 5000 });

  const boolInputs = ecdForm.locator('.form-input.yes-no');
  const count = await boolInputs.count();
  for (let i = 0; i < count; i++) {
    const yesLabel = boolInputs.nth(i).locator('label', { hasText: 'Yes' });
    if (await yesLabel.isVisible({ timeout: 500 }).catch(() => false)) {
      await click(yesLabel, page);
    }
  }

  await saveActivityAndReturn(page);

  // May trigger ECD warning popup on encounter page.
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
  await dismissPopup(page);
}

// ---------------------------------------------------------------------------
// Medication activity (PediatricCare nurse only)
// ---------------------------------------------------------------------------

export async function completeMedication(page: Page) {
  await openActivity(page, 'medication');

  // Iterate through visible medication sub-task tabs.
  // Tab icons from Elm: albendazole, mebendezole, treatment-review (Vitamin A)
  const tabIcons = ['albendazole', 'mebendezole', 'treatment-review'];
  for (const icon of tabIcons) {
    const tab = page.locator(`.link-section:has(.icon-activity-task.icon-${icon})`);
    if (await tab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, icon);

      // The medication form shows "Administered <medication>?" with Yes/No.
      // Click "Yes" for administered.
      const adminField = page.locator('.form-input.yes-no');
      if (await adminField.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(adminField.locator('label', { hasText: 'Yes' }).first(), page);
      }

      await saveSubTaskAndContinue(page);
    }
  }

  // Wait for return to encounter page.
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Immunisation activity
// ---------------------------------------------------------------------------

export async function completeImmunisation(
  page: Page,
  options?: { isChw?: boolean },
) {
  const isChw = options?.isChw ?? false;

  await openActivity(page, 'immunisation');

  // Get all visible vaccine tabs (non-Overview).
  const allTabs = page.locator('#tasks-bar .link-section');
  const tabCount = await allTabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = allTabs.nth(i);
    const hasOverview = await tab.locator('.icon-vaccination-overview').count();
    if (hasOverview > 0) continue;

    // Check if this is a vaccine tab (has an icon-activity-task).
    const hasVaccineIcon = await tab.locator('.icon-activity-task').count();
    if (hasVaccineIcon === 0) continue;

    // Click the tab.
    await click(tab, page);
    await page.waitForTimeout(500);

    // Wait for the vaccination form.
    await page.locator('.ui.form.vaccination').waitFor({ timeout: 5000 });

    // "Was this vaccine administered previously?" → No
    // This is the first bool input in the form.
    const boolInputs = page.locator('.ui.form.vaccination .form-input.yes-no');
    const firstBoolInput = boolInputs.first();
    await firstBoolInput.waitFor({ timeout: 3000 });
    await click(firstBoolInput.locator('label', { hasText: 'No' }), page);
    await page.waitForTimeout(300);

    if (!isChw) {
      // Nurse: "Will receive vaccine today?" → Yes (second bool input).
      const secondBoolInput = boolInputs.nth(1);
      if (await secondBoolInput.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(secondBoolInput.locator('label', { hasText: 'Yes' }), page);
        await page.waitForTimeout(300);
      }
    }

    // Save this vaccine tab.
    await saveSubTaskAndContinue(page);
  }

  // Click Overview tab and save to return to encounter page.
  const overviewTab = page.locator('.link-section:has(.icon-vaccination-overview)');
  if (await overviewTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await click(overviewTab, page);
    await page.waitForTimeout(500);

    // Overview save returns to encounter page.
    const saveBtn = page.locator('.actions button.ui.fluid.primary.button.active');
    if (await saveBtn.isVisible({ timeout: 3000 }).catch(() => false)) {
      await click(saveBtn, page);
    }
  }

  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Pregnancy Summary activity (NewbornExam only)
// ---------------------------------------------------------------------------

export async function completePregnancySummary(page: Page) {
  await openActivity(page, 'history');

  const form = page.locator('.ui.form.pregnancy-summary');
  await form.waitFor({ timeout: 5000 });

  // Expected Date Concluded — click the date input to open calendar.
  const expectedDate = new Date();
  expectedDate.setMonth(expectedDate.getMonth() - 1);
  await setDate(page, expectedDate);

  // APGAR Scores Available → Yes
  const boolInputs = form.locator('.form-input.yes-no');
  await click(boolInputs.first().locator('label', { hasText: 'Yes' }), page);
  await page.waitForTimeout(300);

  // Fill APGAR 1 minute and 5 minute scores.
  await fillMeasurement(page, 'apgar.one-min', '8');
  await fillMeasurement(page, 'apgar.five-min', '9');

  // Birth Weight — fill value in grams.
  await fillMeasurement(page, 'birth-weight', '3000');

  // Birth Length Available → No (second bool input after APGAR).
  const birthLengthBoolInput = boolInputs.nth(1);
  await click(birthLengthBoolInput.locator('label', { hasText: 'No' }), page);
  await page.waitForTimeout(300);

  // Delivery Complications Present → No
  const complicationsBoolInput = boolInputs.nth(2);
  await click(complicationsBoolInput.locator('label', { hasText: 'No' }), page);
  await page.waitForTimeout(300);

  // Birth Defects Present → No
  const defectsBoolInput = boolInputs.nth(3);
  await click(defectsBoolInput.locator('label', { hasText: 'No' }), page);
  await page.waitForTimeout(300);

  await saveActivityAndReturn(page);

  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Home Visit activity (PediatricCareChw only)
// ---------------------------------------------------------------------------

export async function completeHomeVisit(page: Page) {
  await openActivity(page, 'home-visit');

  // --- Feeding tab ---
  await clickSubTaskTab(page, 'feeding');
  await answerYesNo(page, 'receive-supplement', 'No');
  await answerYesNo(page, 'encouraged-to-eat', 'Yes');
  await answerYesNo(page, 'refusing-to-eat', 'No');
  await answerYesNo(page, 'breastfeeding', 'Yes');
  await answerYesNo(page, 'clean-water-available', 'Yes');
  await saveSubTaskAndContinue(page);

  // --- Caring tab ---
  await clickSubTaskTab(page, 'caring');
  await answerYesNo(page, 'parents-health', 'Yes');
  await selectCheckbox(page, 'Parent');
  await answerYesNo(page, 'child-clean', 'Yes');
  await saveSubTaskAndContinue(page);

  // --- Hygiene tab ---
  await clickSubTaskTab(page, 'hygiene');
  await selectCheckbox(page, 'Piped Water to Home');
  await selectCheckbox(page, 'Boiled');
  await answerYesNo(page, 'soap-in-the-house', 'Yes');
  await answerYesNo(page, 'wash-hands-before-feeding', 'Yes');
  await answerYesNo(page, 'food-covered', 'Yes');
  await saveSubTaskAndContinue(page);

  // --- Food Security tab ---
  await clickSubTaskTab(page, 'food-security');
  await selectCheckbox(page, 'Homebased Agriculture / Livestock');
  // Note: typo in Elm source — class is "household-got-fFood" (capital F).
  await answerYesNo(page, 'household-got-fFood', 'Yes');
  await saveSubTaskAndContinue(page);

  // Wait for return to encounter page.
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Next Steps activity
// ---------------------------------------------------------------------------

export async function completeNextSteps(
  page: Page,
  options?: {
    hasContributingFactors?: boolean;
    hasHealthEducation?: boolean;
    hasSendToHC?: boolean;
    hasFollowUp?: boolean;
  },
) {
  const hasContribFactors = options?.hasContributingFactors ?? true;
  const hasHealthEd = options?.hasHealthEducation ?? true;
  const hasSendToHC = options?.hasSendToHC ?? true;
  const hasFollowUp = options?.hasFollowUp ?? true;

  // Dismiss any popup that may be overlaying the page.
  await dismissPopup(page);

  // May already be on the NextSteps page after diagnosis popup auto-navigation.
  const alreadyOnActivity = await page
    .locator('div.page-activity.well-child')
    .isVisible()
    .catch(() => false);
  if (!alreadyOnActivity) {
    await openActivity(page, 'next-steps');
  }

  // Dismiss any popup that may be overlaying the activity page.
  await dismissPopup(page);

  // --- Contributing Factors ---
  if (hasContribFactors) {
    const cfTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-contributing-factors)');
    if (await cfTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-contributing-factors');
      // Select a contributing factor.
      const factorCheckbox = page.locator('.ui.checkbox.activity').first();
      if (await factorCheckbox.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(factorCheckbox.locator('label'), page);
      }
      await saveSubTaskAndContinue(page);
    }
  }

  // --- Health Education ---
  if (hasHealthEd) {
    const heTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-health-education)');
    if (await heTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-health-education');
      await page.locator('.ui.form.health-education').waitFor({ timeout: 5000 });
      await answerYesNo(page, 'education-for-diagnosis', 'Yes');
      await saveSubTaskAndContinue(page);
    }
  }

  // --- Send to HC ---
  if (hasSendToHC) {
    const stTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-send-to-hc)');
    if (await stTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-send-to-hc');

      // Check if this is the nurse "Refer to Program" form or CHW "Send to HC" form.
      const referToHcField = page.locator('.form-input.yes-no.refer-to-hc');
      if (await referToHcField.isVisible({ timeout: 2000 }).catch(() => false)) {
        await answerYesNo(page, 'refer-to-hc', 'Yes');
        const handForm = page.locator('.form-input.yes-no.hand-referral-form');
        if (await handForm.isVisible({ timeout: 1000 }).catch(() => false)) {
          await answerYesNo(page, 'hand-referral-form', 'Yes');
        }
      } else {
        // Nurse at HC: "Enroll to Nutrition Program" bool inputs.
        const boolInputs = page.locator('.form-input.yes-no');
        const count = await boolInputs.count();
        for (let i = 0; i < count; i++) {
          const noLabel = boolInputs.nth(i).locator('label', { hasText: 'No' });
          if (await noLabel.isVisible({ timeout: 500 }).catch(() => false)) {
            await click(noLabel, page);
          }
        }
      }
      await saveSubTaskAndContinue(page);
    }
  }

  // --- Follow Up ---
  if (hasFollowUp) {
    const fuTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-follow-up)');
    if (await fuTab.isVisible({ timeout: 2000 }).catch(() => false)) {
      await clickSubTaskTab(page, 'next-steps-follow-up');
      await page.locator('.ui.form.follow-up').waitFor({ timeout: 5000 });
      await selectCheckboxInForm(page, '.ui.form.follow-up', '3 Days');
      await saveSubTaskAndContinue(page);
    }
  }

  // --- Next Visit (read-only, just save) ---
  const nvTab = page.locator('.link-section:has(.icon-activity-task.icon-next-steps-next-visit)');
  if (await nvTab.isVisible({ timeout: 2000 }).catch(() => false)) {
    await clickSubTaskTab(page, 'next-steps-next-visit');
    const saveBtn = page.locator('.actions button.ui.fluid.primary.button.active');
    if (await saveBtn.isVisible({ timeout: 3000 }).catch(() => false)) {
      await click(saveBtn, page);
      await page.waitForTimeout(500);
    }
  }

  // After completing all Next Steps tasks, the app auto-navigates to
  // the Progress Report page (not back to the encounter page).
  // Navigate back to the encounter page by clicking back arrows.
  await page.waitForTimeout(1000);
  for (let i = 0; i < 5; i++) {
    const onEncounterPage = await page
      .locator('div.page-encounter.well-child')
      .isVisible()
      .catch(() => false);
    if (onEncounterPage) break;
    await page.goBack();
    await page.waitForTimeout(1000);
  }
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(500);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

export async function endWellChildEncounter(page: Page) {
  await page.locator('div.page-encounter.well-child').waitFor({ timeout: 10000 });
  await page.waitForTimeout(2000);

  // Dismiss any popup first.
  await dismissPopup(page);

  const endBtn = page.locator('.actions button.ui.fluid.primary.button', {
    hasText: 'End Encounter',
  });
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Handle NCDA skip dialog if it appears.
  const skipDialog = page.locator('div.ui.tiny.active.modal');
  if (await skipDialog.isVisible({ timeout: 3000 }).catch(() => false)) {
    // The skip NCDA dialog has a "Skip" button.
    const skipBtn = skipDialog.locator('button', { hasText: /Skip|Continue/i });
    if (await skipBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
      await skipBtn.click({ force: true });
      await page.waitForTimeout(1000);
    }
  }

  // Handle end encounter confirmation dialog.
  const confirmModal = page.locator('div.ui.tiny.active.modal');
  if (await confirmModal.isVisible({ timeout: 3000 }).catch(() => false)) {
    await confirmModal.locator('button', { hasText: 'Continue' }).click({ force: true });
  }

  // Wait for navigation away from encounter page.
  await page
    .locator('div.page-encounter.well-child')
    .waitFor({ state: 'hidden', timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Sync helper
// ---------------------------------------------------------------------------

export async function syncAndWait(page: Page) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const nyange = page.locator('.health-center', {
    has: page.locator('h2', { hasText: 'Nyange Health Center' }),
  });
  await nyange.waitFor({ timeout: 10000 });

  await nyange
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 480000 });

  await page.goBack();
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

export function queryWellChildNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const typesToQuery = expectedTypes ?? [
    'well_child_symptoms_review',
    'well_child_vitals',
    'well_child_height',
    'well_child_weight',
    'well_child_muac',
    'well_child_head_circumference',
    'well_child_nutrition',
    'well_child_ecd',
    'well_child_photo',
    'well_child_albendazole',
    'well_child_mebendezole',
    'well_child_vitamin_a',
    'well_child_bcg_immunisation',
    'well_child_dtp_immunisation',
    'well_child_dtp_sa_immunisation',
    'well_child_hpv_immunisation',
    'well_child_ipv_immunisation',
    'well_child_mr_immunisation',
    'well_child_opv_immunisation',
    'well_child_pcv13_immunisation',
    'well_child_rotarix_immunisation',
    'well_child_contributing_factors',
    'well_child_health_education',
    'well_child_send_to_hc',
    'well_child_follow_up',
    'well_child_next_visit',
    'well_child_feeding',
    'well_child_caring',
    'well_child_hygiene',
    'well_child_food_security',
    'well_child_pregnancy_summary',
    'well_child_ncda',
  ];

  const typesPhp = typesToQuery.map(t => `'${t}'`).join(', ');
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
    \\$types = [${typesPhp}];

    foreach (\\$types as \\$node_type) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$node_type)
        ->fieldCondition('field_person', 'target_id', \\$person_nid)
        ->execute();
      if (!empty(\\$r['node'])) {
        \\$measurements[\\$node_type] = true;
      }
    }

    echo json_encode(\\$measurements);
  `;

  const { drushCmd, cwd } = drushEnv();

  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    }).trim();

    try {
      const parsed = JSON.parse(output);
      if (parsed.error) {
        if (attempt < 4) {
          execSync('sleep 10');
          continue;
        }
        return {};
      }
      return parsed;
    } catch {
      if (attempt < 4) {
        execSync('sleep 10');
      }
    }
  }
  return {};
}
