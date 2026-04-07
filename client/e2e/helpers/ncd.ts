import { Page } from '@playwright/test';
import { click } from './auth';
import {
  WAIT,
  queryMeasurementNodes,
  backdateEncounter,
  answerYesNo,
  selectCheckbox,
  selectCheckboxInForm,
  clickSubTaskTab,
  fillMeasurement,
  openActivity,
  saveActivity,
  saveSubTask,
  registerAdult,
} from './common';

// ---------------------------------------------------------------------------
// Participant registration
// ---------------------------------------------------------------------------

/**
 * Register an adult and start an NCD encounter.
 * Flow: Dashboard → Clinical → Individual Assessment → NCD →
 *       Register → fill form → submit → participant page →
 *       click "NCD Encounter"
 *
 * Returns { firstName, secondName, fullName }.
 */
export async function createAdultAndStartNCDEncounter(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
    isFemale?: boolean;
  },
) {
  const result = await registerAdult(page, 'Noncommunicable Diseases', 'ncd', {
    ageYears: options?.ageYears,
    firstName: options?.firstName ?? `TestNCD${Date.now()}`,
    isFemale: options?.isFemale,
  });

  // Start NCD encounter.
  await click(page.locator('div.ui.primary.button', { hasText: 'NCD Encounter' }), page);
  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 30000 });
  await page.waitForTimeout(WAIT.sectionTransition);

  return result;
}

// ---------------------------------------------------------------------------
// DangerSigns activity
// ---------------------------------------------------------------------------

/**
 * Complete DangerSigns activity.
 * Default: no danger signs selected ("None of the Above").
 *
 * Creates: ncd_danger_signs
 */
export async function completeDangerSigns(
  page: Page,
  options?: { signs?: string[] },
) {
  const signs = options?.signs ?? [];

  await openActivity(page, 'ncd', 'danger-signs');

  if (signs.length === 0) {
    await selectCheckboxInForm(page, '.ui.form.danger-signs', 'None of the Above');
  } else {
    for (const sign of signs) {
      await selectCheckboxInForm(page, '.ui.form.danger-signs', sign);
    }
  }

  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// SymptomReview activity
// ---------------------------------------------------------------------------

/**
 * Complete SymptomReview activity.
 * Default: "None of the Above" for all 3 groups.
 *
 * Creates: ncd_symptom_review
 */
export async function completeSymptomReview(
  page: Page,
  options?: {
    group1?: string[];
    pain?: string[];
    group2?: string[];
  },
) {
  await openActivity(page, 'ncd', 'symptoms');

  const form = page.locator('.ui.form.symptom-review');

  // Group 1 symptoms.
  const group1 = options?.group1 ?? [];
  if (group1.length === 0) {
    // The first "None of the Above" checkbox in the form.
    const noneLabels = form.locator('.ui.checkbox label', {
      hasText: /^None of the Above$/i,
    });
    await click(noneLabels.nth(0), page);
  } else {
    for (const s of group1) {
      await selectCheckboxInForm(page, '.ui.form.symptom-review', s);
    }
  }

  // Pain symptoms.
  const pain = options?.pain ?? [];
  if (pain.length === 0) {
    const noneLabels = form.locator('.ui.checkbox label', {
      hasText: /^None of the Above$/i,
    });
    await click(noneLabels.nth(1), page);
  } else {
    for (const s of pain) {
      await selectCheckboxInForm(page, '.ui.form.symptom-review', s);
    }
  }

  // Group 2 symptoms.
  const group2 = options?.group2 ?? [];
  if (group2.length === 0) {
    const noneLabels = form.locator('.ui.checkbox label', {
      hasText: /^None of the Above$/i,
    });
    await click(noneLabels.nth(2), page);
  } else {
    for (const s of group2) {
      await selectCheckboxInForm(page, '.ui.form.symptom-review', s);
    }
  }

  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// Examination activity (Vitals + CoreExam sub-tasks)
// ---------------------------------------------------------------------------

/**
 * Complete Examination activity: Vitals and CoreExam sub-tasks.
 *
 * Creates: ncd_vitals, ncd_core_exam
 */
export async function completeExamination(
  page: Page,
  options?: {
    sys?: string;
    dia?: string;
    heartRate?: string;
    respiratoryRate?: string;
    bodyTemp?: string;
  },
) {
  await openActivity(page, 'ncd', 'examination');

  // --- Sub-task 1: Vitals (default active) ---
  await clickSubTaskTab(page, 'vitals');
  await fillMeasurement(page, 'sys-blood-pressure', options?.sys ?? '120');
  await fillMeasurement(page, 'dia-blood-pressure', options?.dia ?? '80');
  await fillMeasurement(page, 'heart-rate', options?.heartRate ?? '72');
  await fillMeasurement(page, 'respiratory-rate', options?.respiratoryRate ?? '16');
  await fillMeasurement(page, 'body-temperature', options?.bodyTemp ?? '36.6');
  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 2: CoreExam ---
  await clickSubTaskTab(page, 'core-physical-exam');
  await page.waitForTimeout(WAIT.elmRerender);

  const coreExamForm = page.locator('.ui.form.core-physical-exam');

  // Head/Hair: "Normal" (No brittle hair)
  await answerYesNo(page, 'head-hair', 'No');
  // Eyes: "Normal" (No pale conjunctiva)
  await answerYesNo(page, 'eyes', 'No');

  // Checkbox sections: Neck, Heart, Lungs, Abdomen, Hands, Legs.
  // "Normal" checkboxes appear in order: Neck(0), Lungs(1), Abdomen(2), Hands(3), Legs(4).
  // Heart uses unique text "Normal Rate And Rhythm".
  const normalCheckboxes = coreExamForm.locator('.ui.checkbox label', {
    hasText: /^Normal$/,
  });

  // Neck: Normal (index 0)
  await click(normalCheckboxes.nth(0), page);
  // Heart: Normal Rate And Rhythm (unique text)
  await click(
    coreExamForm.locator('.ui.checkbox label', {
      hasText: /^Normal Rate And Rhythm$/i,
    }),
    page,
  );
  // Heart Murmur: No
  await answerYesNo(page, 'heart-murmur', 'No');
  // Lungs: Normal (index 1)
  await click(normalCheckboxes.nth(1), page);
  // Abdomen: Normal (index 2)
  await click(normalCheckboxes.nth(2), page);
  // Hands: Normal (index 3)
  await click(normalCheckboxes.nth(3), page);
  // Legs: Normal (index 4)
  await click(normalCheckboxes.nth(4), page);

  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// FamilyPlanning activity
// ---------------------------------------------------------------------------

/**
 * Complete FamilyPlanning activity.
 * Default: select "None" (NoFamilyPlanning).
 *
 * Creates: ncd_family_planning
 */
export async function completeFamilyPlanning(page: Page) {
  await openActivity(page, 'ncd', 'planning');

  // Select "None of these" family planning sign.
  await selectCheckboxInForm(page, '.ui.form.family-planning', 'None of these');

  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// MedicalHistory activity (5 sub-tasks)
// ---------------------------------------------------------------------------

/**
 * Complete MedicalHistory activity: CoMorbidities, MedicationHistory,
 * SocialHistory, FamilyHistory, and OutsideCare sub-tasks.
 * Default: all "None" / "No" selections.
 *
 * Creates: ncd_co_morbidities, ncd_medication_history, ncd_social_history,
 *          ncd_family_history, ncd_outside_care
 */
export async function completeMedicalHistory(page: Page) {
  await openActivity(page, 'ncd', 'history');

  // --- Sub-task 1: CoMorbidities ---
  await clickSubTaskTab(page, 'danger-signs');
  await page.waitForTimeout(WAIT.elmRerender);
  await selectCheckboxInForm(page, '.ui.form.co-morbidities', 'None of the Above');
  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 2: MedicationHistory ---
  await clickSubTaskTab(page, 'medical');
  await page.waitForTimeout(WAIT.elmRerender);
  // 3 checkbox groups: medications causing hypertension, treating hypertension, treating diabetes.
  // Select "None of the Above" for each group.
  const medForm = page.locator('.ui.form.medication-history');
  const noneLabels = medForm.locator('.ui.checkbox label', {
    hasText: /^None of the Above$/i,
  });
  const noneCount = await noneLabels.count();
  for (let i = 0; i < noneCount; i++) {
    await click(noneLabels.nth(i), page);
  }
  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 3: SocialHistory ---
  await clickSubTaskTab(page, 'social');
  await page.waitForTimeout(WAIT.elmRerender);
  // Bool inputs: alcohol, cigarettes, salt, difficult4Times, helpAtHome.
  await answerYesNo(page, 'alcohol', 'No');
  await answerYesNo(page, 'cigarettes', 'No');
  await answerYesNo(page, 'salt', 'No');
  await answerYesNo(page, 'difficult-4-times', 'No');
  await answerYesNo(page, 'help-at-home', 'No');
  // Food group: checkbox select (Vegetables, Carbohydrates, Protein).
  // Click the first available option.
  const foodCheckboxes = page.locator('.ui.form.social-history .ui.checkbox label');
  const foodCount = await foodCheckboxes.count();
  if (foodCount > 0) {
    await click(foodCheckboxes.first(), page);
  }
  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 4: FamilyHistory ---
  await clickSubTaskTab(page, 'family');
  await page.waitForTimeout(WAIT.elmRerender);
  // Bool inputs: hypertension in family, heart problem, diabetes.
  await answerYesNo(page, 'hypertension-in-family', 'No');
  await answerYesNo(page, 'heartProblem-in-family', 'No');
  await answerYesNo(page, 'diabetes-in-family', 'No');
  await saveSubTask(page);
  await page.waitForTimeout(WAIT.elmRerender);

  // --- Sub-task 5: OutsideCare ---
  await clickSubTaskTab(page, 'outside-care');
  await page.waitForTimeout(WAIT.elmRerender);
  // Step 1: Diagnoses — answer "No" to "seen at another facility".
  const outsideCareForm = page.locator('.ui.form.history.outside-care');
  if (await outsideCareForm.isVisible({ timeout: 2000 }).catch(() => false)) {
    await answerYesNo(page, 'seen-at-another-facility', 'No');
  }
  // Save — this returns to the encounter page since it's the last sub-task.
  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// OutsideCare activity (standalone, for subsequent encounters)
// ---------------------------------------------------------------------------

/**
 * Complete the standalone OutsideCare activity (subsequent encounters only).
 * Default: "No" to receiving outside care.
 *
 * Creates: ncd_outside_care
 */
export async function completeOutsideCare(page: Page) {
  await openActivity(page, 'ncd', 'outside-care');

  // Step 1: "Have you been seen at another health facility?"
  await answerYesNo(page, 'seen-at-another-facility', 'No');

  await saveActivity(page, 'ncd');
}

// ---------------------------------------------------------------------------
// Laboratory activity
// ---------------------------------------------------------------------------

/**
 * Complete Laboratory activity: iterate visible lab test tabs.
 * For each test, answer "Will the test be performed today?" with the
 * given answer (default: "No" for simplicity — tests not performed).
 *
 * If performTests is true, marks tests as performed today with today's
 * execution date (needed for recurrent encounter lab results to appear).
 *
 * Creates: ncd_hiv_test, ncd_urine_dipstick_test, ncd_random_blood_sugar_test,
 *          ncd_pregnancy_test, ncd_creatinine_test, ncd_liver_function_test,
 *          ncd_lipid_panel_test, ncd_hba1c_test (depending on which appear)
 */
export async function completeLaboratory(
  page: Page,
  options?: { performTests?: boolean },
) {
  const performTests = options?.performTests ?? false;

  await openActivity(page, 'ncd', 'laboratory');

  // Iterate visible lab test tabs.
  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    const tab = tabs.nth(i);
    if (!(await tab.isVisible())) continue;

    const isCompleted = await tab.evaluate(el =>
      el.classList.contains('completed'),
    ).catch(() => false);
    if (isCompleted) continue;

    const isActive = await tab.evaluate(el =>
      el.classList.contains('active'),
    ).catch(() => false);
    if (!isActive) {
      await click(tab, page);
      // Wait for the form to render (every lab tab has at least one yes/no field).
      await page
        .locator('.form-input.yes-no')
        .first()
        .waitFor({ timeout: 5000 });
      await page.waitForTimeout(WAIT.formInteraction);
    }

    // Helper: click a label with force:true (safe for radios/checkboxes,
    // avoids sticky .actions bar interception in headed mode).
    const forceClick = async (locator: import('@playwright/test').Locator) => {
      await locator.click({ force: true });
    };

    // 1. "Known as positive?" (HIV, Pregnancy) → No
    const knownPositive = page.locator('.form-input.yes-no.known-as-positive');
    if (await knownPositive.isVisible().catch(() => false)) {
      await forceClick(knownPositive.locator('label', { hasText: 'No' }));
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 2. "Got results previously?" (HBA1C) → No
    const gotResults = page.locator('.form-input.yes-no.got-results-previously');
    if (await gotResults.isVisible().catch(() => false)) {
      await forceClick(gotResults.locator('label', { hasText: 'No' }));
      await page.waitForTimeout(WAIT.elmRerender);
    }

    // 3. "Were you able to perform the test?" → Yes (perform) or No (skip)
    const testPerformed = page.locator('.form-input.yes-no.test-performed');
    if (await testPerformed.isVisible().catch(() => false)) {
      if (performTests) {
        await forceClick(testPerformed.locator('label', { hasText: 'Yes' }));
        await page.waitForTimeout(WAIT.elmRerender);
      } else {
        await forceClick(testPerformed.locator('label', { hasText: 'No' }));
        // Wait for "Why not?" section and select first reason.
        const whyNot = page.locator('.why-not .ui.checkbox label').first();
        await whyNot.waitFor({ timeout: 5000 });
        await forceClick(whyNot);
        await page.waitForTimeout(WAIT.formInteraction);
      }
    }

    if (performTests) {
      // 4. "Immediate result?" → Lab (custom labels: "Point of Care" / "Lab")
      const immediateResult = page.locator('.form-input.yes-no.immediate-result');
      if (await immediateResult.isVisible().catch(() => false)) {
        await forceClick(immediateResult.locator('label', { hasText: 'Lab' }));
        await page.waitForTimeout(WAIT.elmRerender);
      }

      // 5. "Urine Dipstick variant?" → Short Dip
      const shortDip = page.locator('.ui.checkbox label', { hasText: /^Short Dip$/i });
      if (await shortDip.isVisible().catch(() => false)) {
        await forceClick(shortDip);
        await page.waitForTimeout(WAIT.formInteraction);
      }

      // 6. "Was this test performed before a meal?" → No (Random Blood Sugar)
      const patientFasted = page.locator('.form-input.yes-no.patient-fasted');
      if (await patientFasted.isVisible().catch(() => false)) {
        await forceClick(patientFasted.locator('label', { hasText: 'No' }));
        await page.waitForTimeout(WAIT.formInteraction);
      }

      // 7. "Did you perform this test today?" → Yes
      const testPerformedToday = page.locator('.form-input.yes-no.test-performed-today');
      if (await testPerformedToday.isVisible().catch(() => false)) {
        await forceClick(testPerformedToday.locator('label', { hasText: 'Yes' }));
        await page.waitForTimeout(WAIT.formInteraction);
      }

      // 8. Test result dropdown (HIV, Pregnancy: Positive/Negative/Indeterminate)
      const resultSelects = page.locator('select.form-input');
      const selectCount = await resultSelects.count();
      for (let s = 0; s < selectCount; s++) {
        const sel = resultSelects.nth(s);
        if (await sel.isVisible().catch(() => false)) {
          const currentVal = await sel.inputValue();
          if (!currentVal) {
            // Select "Negative" if available, otherwise second option.
            const negOption = sel.locator('option', { hasText: 'Negative' });
            if (await negOption.count() > 0) {
              const val = await negOption.getAttribute('value');
              if (val) await sel.selectOption(val);
            } else {
              const val = await sel.locator('option').nth(1).getAttribute('value');
              if (val) await sel.selectOption(val);
            }
            await page.waitForTimeout(WAIT.formInteraction);
          }
        }
      }

      // 9. Numeric result inputs (e.g. blood sugar count, creatinine, etc.)
      const numericInputs = page.locator('.form-input.measurement input[type="number"]');
      const numCount = await numericInputs.count();
      for (let n = 0; n < numCount; n++) {
        const input = numericInputs.nth(n);
        if (await input.isVisible().catch(() => false)) {
          const currentVal = await input.inputValue();
          if (!currentVal) {
            await input.fill('5');
            await page.waitForTimeout(WAIT.quickInput);
          }
        }
      }

      // 10. "Is the patient's partner HIV positive?" → No (appears after HIV result)
      const partnerHIV = page.locator('.form-input.yes-no.partner-hiv-positive');
      if (await partnerHIV.isVisible().catch(() => false)) {
        await forceClick(partnerHIV.locator('label', { hasText: 'No' }));
        await page.waitForTimeout(WAIT.formInteraction);
      }
    }

    // Save this test's sub-task.
    // Wait for Save to be enabled (form complete), then force-click
    // (avoids sticky footer / viewport issues in headed mode).
    const saveBtn = page.locator('button.ui.fluid.primary.button:not(.disabled)', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 10000 });
    await saveBtn.click({ force: true });
    await page.waitForTimeout(WAIT.elmRerender);
  }

  // After all tabs, we should be back on the encounter page.
  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

// ---------------------------------------------------------------------------
// NextSteps activity (initial encounter)
// ---------------------------------------------------------------------------

/**
 * Complete NextSteps activity: iterate visible sub-task tabs.
 * Sub-tasks: HealthEducation, MedicationDistribution, Referral.
 *
 * Creates: ncd_health_education, ncd_medication_distribution, ncd_referral
 */
export async function completeNextSteps(page: Page) {
  await openActivity(page, 'ncd', 'next-steps');

  // Iterate visible sub-task tabs.
  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(WAIT.elmRerender);

    // Determine which sub-task we're on by checking the active icon.
    const activeTab = page.locator('.link-section.active .icon-activity-task');
    const classAttr = await activeTab.getAttribute('class').catch(() => '');

    if (classAttr?.includes('next-steps-health-education')) {
      // HealthEducation: answer bool input (hypertension education).
      await answerYesNo(page, 'hypertension', 'Yes');
    } else if (classAttr?.includes('next-steps-treatment')) {
      // MedicationDistribution: select recommended medications.
      // Stage 2 hypertension requires choosing two medications.
      const treatmentCheckboxes = page.locator('.ui.form.medication-distribution .ui.checkbox label');
      const checkboxCount = await treatmentCheckboxes.count();
      for (let c = 0; c < Math.min(checkboxCount, 2); c++) {
        await treatmentCheckboxes.nth(c).click({ force: true });
        await page.waitForTimeout(WAIT.formInteraction);
      }
      // "Guided to return in one month?" → Yes
      const guidedReturn = page.locator('.form-input.yes-no.return-in-one-month label', { hasText: 'Yes' });
      if (await guidedReturn.isVisible({ timeout: 2000 }).catch(() => false)) {
        await guidedReturn.click({ force: true });
      }
    } else if (classAttr?.includes('next-steps-referral')) {
      // Referral: refer to hospital.
      await answerYesNo(page, 'referral', 'Yes');
      await page.waitForTimeout(WAIT.elmRerender);
      // "Hand referral form?" → Yes
      const handForm = page.locator('.form-input.yes-no.hand-referral-form label', { hasText: 'Yes' });
      if (await handForm.isVisible({ timeout: 2000 }).catch(() => false)) {
        await handForm.click({ force: true });
      }
    }

    // Save sub-task (wait for enabled, then force-click for headed mode).
    const saveBtn = page.locator('button.ui.fluid.primary.button:not(.disabled)', { hasText: 'Save' });
    await saveBtn.waitFor({ timeout: 10000 });
    await saveBtn.click({ force: true });
    await page.waitForTimeout(WAIT.sectionTransition);
  }

  // Wait for encounter page.
  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

// ---------------------------------------------------------------------------
// Encounter lifecycle
// ---------------------------------------------------------------------------

/**
 * End the NCD encounter: click "End Encounter", confirm in the dialog.
 */
export async function endNCDEncounter(page: Page) {
  await page.waitForTimeout(WAIT.pageNavigation);

  const endBtn = page.locator('button', { hasText: 'End Encounter' }).first();
  await endBtn.waitFor({ timeout: 10000 });
  await endBtn.click({ force: true });

  // Wait for and confirm the "End Encounter?" dialog.
  const confirmModal = page.locator('div.ui.tiny.active.modal');
  await confirmModal.waitFor({ timeout: 5000 }).catch(() => {});
  if (await confirmModal.isVisible()) {
    await confirmModal.locator('button', { hasText: 'Continue' }).click({ force: true });
  }

  // Wait for navigation away from the encounter page.
  await page
    .locator('div.page-encounter.ncd')
    .waitFor({ state: 'hidden', timeout: 30000 });
}

/**
 * Navigate to the NCD participant page for a given person.
 * Flow: Dashboard → Clinical → Individual Assessment → NCD → search.
 */
export async function navigateToParticipantPage(
  page: Page,
  fullName: string,
) {
  const participantPage = page.locator('div.page-participant.individual.ncd');
  if (await participantPage.isVisible({ timeout: 500 }).catch(() => false)) {
    return;
  }

  const dashboard = page.locator('.wrap-cards');
  if (!await dashboard.isVisible({ timeout: 1000 }).catch(() => false)) {
    await page.goto('/');
    await dashboard.waitFor({ timeout: 30000 });
  }

  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  await click(page.locator('button.individual-assessment'), page);
  await page.locator('div.page-encounter-types').waitFor({ timeout: 10000 });

  await click(
    page.locator('button.encounter-type', { hasText: 'Noncommunicable Diseases' }),
    page,
  );
  await page.locator('div.page-participants').waitFor({ timeout: 10000 });

  // Search for the participant by name.
  const searchInput = page.getByPlaceholder('Enter participant name here');
  await searchInput.waitFor({ timeout: 5000 });
  await searchInput.fill(fullName);

  // Wait for search results, then click the forward-arrow action icon.
  const resultItem = page.locator('.item.participant-view', {
    hasText: fullName,
  });
  await resultItem.first().waitFor({ timeout: 10000 });
  await click(resultItem.first().locator('.action-icon.forward'), page);

  await participantPage.waitFor({ timeout: 15000 });
}

/**
 * Start a new NCD encounter from the participant page.
 */
export async function startNCDEncounter(page: Page) {
  await click(
    page.locator('div.ui.primary.button', { hasText: 'NCD Encounter' }),
    page,
  );
  await page
    .locator('div.page-encounter.ncd')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(WAIT.sectionTransition);
}

/**
 * Backdate the most recent NCD encounter for a person to 7 days ago.
 * Retries up to 5 times with 10s delay for eventual consistency.
 */
export function backdateNCDEncounter(personName: string) {
  backdateEncounter(personName, 'ncd_encounter', 7);
}

// ---------------------------------------------------------------------------
// Recurrent encounter helpers (Case Management)
// ---------------------------------------------------------------------------

/**
 * Navigate to Case Management from the nurse dashboard.
 */
export async function navigateToCaseManagement(page: Page) {
  await click(page.locator('.icon-task-case-management'), page);
  await page.locator('.page-case-management').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Open the NCD recurrent encounter from Case Management's NCD Labs pane.
 * Finds the patient entry by name and clicks the forward icon.
 */
export async function openNCDRecurrentEncounterFromCaseManagement(
  page: Page,
  personName: string,
) {
  const entry = page.locator('.follow-up-entry', {
    has: page.locator('.name', { hasText: personName }),
  });
  await entry.waitFor({ timeout: 10000 });
  await click(entry.locator('.icon-forward'), page);
  // Recurrent encounter page has same CSS: div.page-encounter.ncd.
  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Complete LabResults activity on the recurrent encounter.
 * Iterates visible lab result tabs and fills in result values.
 */
export async function completeLabResults(page: Page) {
  await openActivity(page, 'ncd', 'laboratory');

  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(WAIT.elmRerender);

    // Fill select dropdowns first (e.g., unit of measurement — may reveal more inputs).
    const selects = page.locator('select.form-input');
    const selectCount = await selects.count();
    for (let j = 0; j < selectCount; j++) {
      const sel = selects.nth(j);
      if (await sel.isVisible({ timeout: 500 }).catch(() => false)) {
        const currentVal = await sel.inputValue();
        if (!currentVal) {
          const negOption = sel.locator('option', { hasText: 'Negative' });
          if (await negOption.count() > 0) {
            const val = await negOption.getAttribute('value');
            if (val) await sel.selectOption(val);
          } else {
            const val = await sel.locator('option').nth(1).getAttribute('value');
            if (val) await sel.selectOption(val);
          }
          await page.waitForTimeout(WAIT.elmRerender);
        }
      }
    }

    // Fill numeric result inputs (e.g., blood sugar count, cholesterol levels).
    const numberInputs = page.locator('.form-input.measurement input[type="number"]');
    const inputCount = await numberInputs.count();
    for (let j = 0; j < inputCount; j++) {
      const input = numberInputs.nth(j);
      if (await input.isVisible({ timeout: 500 }).catch(() => false)) {
        const currentVal = await input.inputValue();
        if (!currentVal) {
          await input.fill('5');
        }
      }
    }

    // Save sub-task.
    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
      await click(saveBtn, page);
      await page.waitForTimeout(WAIT.elmRerender);
    }
  }

  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Complete RecurrentNextSteps activity on the recurrent encounter.
 * Sub-tasks: MedicationDistribution and Referral.
 */
export async function completeRecurrentNextSteps(page: Page) {
  await openActivity(page, 'ncd', 'next-steps');

  const tabs = page.locator('.link-section:has(.icon-activity-task)');
  const tabCount = await tabs.count();

  for (let i = 0; i < tabCount; i++) {
    await click(tabs.nth(i), page);
    await page.waitForTimeout(WAIT.elmRerender);

    const activeTab = page.locator('.link-section.active .icon-activity-task');
    const classAttr = await activeTab.getAttribute('class').catch(() => '');

    if (classAttr?.includes('next-steps-treatment')) {
      // MedicationDistribution.
      const treatmentCheckboxes = page.locator('.ui.form.medication-distribution .ui.checkbox label');
      const checkboxCount = await treatmentCheckboxes.count();
      if (checkboxCount > 0) {
        await click(treatmentCheckboxes.first(), page);
        await page.waitForTimeout(WAIT.elmRerender);
      }
      const guidedReturn = page.locator('.form-input.yes-no.return-in-one-month label', { hasText: 'Yes' });
      if (await guidedReturn.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(guidedReturn, page);
      }
    } else if (classAttr?.includes('next-steps-referral')) {
      // Referral: refer to hospital.
      await answerYesNo(page, 'referral', 'Yes');
      await page.waitForTimeout(WAIT.elmRerender);
      const handForm = page.locator('.form-input.yes-no.hand-referral-form label', { hasText: 'Yes' });
      if (await handForm.isVisible({ timeout: 2000 }).catch(() => false)) {
        await click(handForm, page);
      }
    }

    const saveBtn = page.locator('button.ui.fluid.primary.button', { hasText: 'Save' });
    if (await saveBtn.isVisible({ timeout: 2000 }).catch(() => false)) {
      await click(saveBtn, page);
      await page.waitForTimeout(WAIT.sectionTransition);
    }
  }

  await page.locator('div.page-encounter.ncd').waitFor({ timeout: 10000 });
  await page.waitForTimeout(WAIT.elmRerender);
}

/**
 * Leave the recurrent encounter (no confirmation dialog).
 * Clicks "Leave Encounter" → navigates to Case Management.
 */
export async function leaveRecurrentEncounter(page: Page) {
  const leaveBtn = page.locator('button', { hasText: 'Leave Encounter' });
  await leaveBtn.waitFor({ timeout: 10000 });
  await click(leaveBtn, page);
  await page.locator('.page-case-management').waitFor({ timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Backend verification via drush
// ---------------------------------------------------------------------------

/**
 * Query the backend for NCD measurement nodes associated with a person.
 * Returns an object mapping node type → boolean (exists).
 *
 * Retries up to 10 times with 5s delay for eventual consistency.
 */
export function queryNCDNodes(
  personName: string,
  expectedTypes?: string[],
): Record<string, boolean> {
  const ncdTypes = [
    'ncd_danger_signs',
    'ncd_symptom_review',
    'ncd_vitals',
    'ncd_core_exam',
    'ncd_co_morbidities',
    'ncd_medication_history',
    'ncd_social_history',
    'ncd_family_history',
    'ncd_outside_care',
    'ncd_family_planning',
    'ncd_hiv_test',
    'ncd_urine_dipstick_test',
    'ncd_random_blood_sugar_test',
    'ncd_pregnancy_test',
    'ncd_creatinine_test',
    'ncd_liver_function_test',
    'ncd_lipid_panel_test',
    'ncd_hba1c_test',
    'ncd_labs_results',
    'ncd_health_education',
    'ncd_medication_distribution',
    'ncd_referral',
  ];
  return queryMeasurementNodes(personName, ncdTypes, expectedTypes);
}
