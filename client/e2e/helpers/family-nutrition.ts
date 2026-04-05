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

  // Use UTC — Elm date pickers derive dates via Time.utc.
  const year = dob.getUTCFullYear().toString();
  await page
    .locator('div.calendar > div.year > select')
    .selectOption(year);

  const monthValue = (dob.getUTCMonth() + 1).toString();
  await page
    .locator('div.calendar > div.month > select')
    .selectOption(monthValue);

  const day = dob.getUTCDate();
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

/**
 * Register an adult female and navigate to her Person page
 * via the Family Nutrition encounter flow (CHW only).
 *
 * Flow: Dashboard → Clinical → Family Encounter → Nutrition Encounter →
 *       Search participants → Register → fill form → submit → Person page
 *
 * Returns { firstName, secondName, fullName } with page on Person page.
 */
export async function createMotherAndNavigateToPersonPage(
  page: Page,
  options?: {
    ageYears?: number;
    firstName?: string;
  },
) {
  const ageYears = options?.ageYears ?? 25;
  const firstName = options?.firstName ?? `TestMother${Date.now()}`;
  const secondName = 'E2ETest';

  // Navigate: Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Family Encounter
  await click(page.locator('button.family-assessment'), page);
  await page
    .locator('div.page-encounter-types')
    .waitFor({ timeout: 10000 });

  // Family Encounter Types → Nutrition Encounter
  await click(
    page.locator('button.encounter-type', {
      hasText: 'Nutrition Encounter',
    }),
    page,
  );
  await page
    .locator('div.page-participants')
    .waitFor({ timeout: 10000 });

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

  // Fill the registration form (adult female, CHW).
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

  // CHW: address and health center are auto-filled.

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the Person page to load (FamilyEncounterOrigin context).
  await page
    .locator('div.page-person')
    .waitFor({ timeout: 30000 });

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * From the mother's Person page, add a child by registering a new person
 * and creating a relationship.
 *
 * Flow: Person page → click add child → People/Search page →
 *       Register new participant → fill child form → submit →
 *       Relationship page → select "My Child" → Save →
 *       back to Person page
 *
 * If the Relationship page shows a "Not Found" error (REST timing),
 * retries by going back and re-navigating to the child.
 */
export async function addChild(
  page: Page,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isFemale?: boolean;
  },
) {
  const ageMonths = options?.ageMonths ?? 12;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;

  // Click the "Add Child" area on the Person page.
  await click(page.locator('.add-participant-icon'), page);

  // Wait for the People/Search page.
  await page.locator('div.page-people').waitFor({ timeout: 10000 });

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

  // CHW: address and health center are auto-filled.

  // Submit the form.
  await click(page.locator('button[type="submit"]'), page);

  // Wait for the Relationship page.
  await page
    .locator('div.page-relationship')
    .waitFor({ timeout: 30000 });

  // Wait for the relationship form. It may show "Not Found" if the backend
  // hasn't made the child available via GET yet.
  const relationForm = page.locator('.ui.form.relationship-selector');
  let formVisible = await relationForm
    .waitFor({ timeout: 15000 })
    .then(() => true)
    .catch(() => false);

  // If form didn't appear, retry with page reloads.
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

  // Select relationship type: "My Child" (first radio option for adults).
  const relationRadio = page.locator(
    '.ui.radio.checkbox label.relationship-selection',
  ).first();
  await click(relationRadio, page);
  await page.waitForTimeout(500);

  // Click Save.
  await click(
    page.locator('.save-buttons button.ui.button.primary.fluid', {
      hasText: 'Save',
    }),
    page,
  );

  // Wait for navigation back to Person page.
  await page
    .locator('div.page-person')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);

  return { firstName, secondName, fullName: `${secondName} ${firstName}` };
}

/**
 * Create a child person and relationship to the mother via drush,
 * then reload the Person page to pick up the new child.
 *
 * This avoids the REST API timing issue where the Relationship page
 * returns "Not Found" when the child was just created.
 */
export function addChildViaDrush(
  motherName: string,
  options?: {
    ageMonths?: number;
    firstName?: string;
    isFemale?: boolean;
  },
): { firstName: string; secondName: string; fullName: string } {
  const ageMonths = options?.ageMonths ?? 12;
  const firstName = options?.firstName ?? `TestChild${Date.now()}`;
  const secondName = 'E2ETest';
  const isFemale = options?.isFemale ?? false;
  const fullName = `${secondName} ${firstName}`;

  const motherNameB64 = Buffer.from(motherName, 'utf8').toString('base64');
  const childNameB64 = Buffer.from(fullName, 'utf8').toString('base64');

  // Calculate birth date.
  const dob = new Date();
  dob.setMonth(dob.getMonth() - ageMonths);
  const dobStr = `${dob.getFullYear()}-${String(dob.getMonth() + 1).padStart(2, '0')}-${String(dob.getDate()).padStart(2, '0')}`;

  const gender = isFemale ? 'female' : 'male';

  const php = `
    // Find mother person node.
    \\$mother_name = base64_decode('${motherNameB64}');
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$mother_name)
      ->execute();
    if (empty(\\$result['node'])) {
      echo json_encode(['error' => 'Mother not found']);
      return;
    }
    \\$mother_nid = key(\\$result['node']);
    \\$mother_node = node_load(\\$mother_nid);

    // Get mother's health center.
    \\$health_center_id = NULL;
    if (!empty(\\$mother_node->field_health_center[LANGUAGE_NONE][0]['target_id'])) {
      \\$health_center_id = \\$mother_node->field_health_center[LANGUAGE_NONE][0]['target_id'];
    }

    // Create child person node.
    \\$child_name = base64_decode('${childNameB64}');
    \\$child = new stdClass();
    \\$child->type = 'person';
    \\$child->title = \\$child_name;
    \\$child->status = NODE_PUBLISHED;
    \\$child->uid = 1;
    \\$child->field_second_name[LANGUAGE_NONE][0]['value'] = '${secondName}';
    \\$child->field_first_name[LANGUAGE_NONE][0]['value'] = '${firstName}';
    \\$child->field_birth_date[LANGUAGE_NONE][0]['value'] = '${dobStr}';
    \\$child->field_gender[LANGUAGE_NONE][0]['value'] = '${gender}';
    \\$child->field_mode_of_delivery[LANGUAGE_NONE][0]['value'] = 'svd';

    // Copy address fields from mother.
    foreach (['field_province', 'field_district', 'field_sector', 'field_cell', 'field_village'] as \\$field) {
      if (!empty(\\$mother_node->{\\$field}[LANGUAGE_NONE])) {
        \\$child->{\\$field}[LANGUAGE_NONE] = \\$mother_node->{\\$field}[LANGUAGE_NONE];
      }
    }
    if (\\$health_center_id) {
      \\$child->field_health_center[LANGUAGE_NONE][0]['target_id'] = \\$health_center_id;
    }

    // Copy shard from mother.
    if (!empty(\\$mother_node->field_shards[LANGUAGE_NONE])) {
      \\$child->field_shards[LANGUAGE_NONE] = \\$mother_node->field_shards[LANGUAGE_NONE];
    }

    node_save(\\$child);

    // Create relationship: mother -> child.
    \\$relationship = new stdClass();
    \\$relationship->type = 'relationship';
    \\$relationship->title = 'relationship';
    \\$relationship->status = NODE_PUBLISHED;
    \\$relationship->uid = 1;
    \\$relationship->field_person[LANGUAGE_NONE][0]['target_id'] = \\$mother_nid;
    \\$relationship->field_related_to[LANGUAGE_NONE][0]['target_id'] = \\$child->nid;
    \\$relationship->field_related_by[LANGUAGE_NONE][0]['value'] = 'parent';
    if (!empty(\\$mother_node->field_shards[LANGUAGE_NONE])) {
      \\$relationship->field_shards[LANGUAGE_NONE] = \\$mother_node->field_shards[LANGUAGE_NONE];
    }
    node_save(\\$relationship);

    echo json_encode(['child_nid' => \\$child->nid, 'relationship_nid' => \\$relationship->nid]);
  `;

  const { drushCmd, cwd } = drushEnv();
  const output = execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
  });

  const parsed = JSON.parse(output.trim());
  if (parsed.error) {
    throw new Error(`Failed to create child: ${parsed.error}`);
  }

  return { firstName, secondName, fullName };
}

/**
 * From the Person page, click "Continue" to navigate to the
 * Family Nutrition Participant page.
 */
export async function continueToParticipantPage(page: Page) {
  await click(
    page.locator('.register-actions button.ui.primary.button.fluid', {
      hasText: 'Continue',
    }),
    page,
  );
  await page
    .locator('div.page-participant.family.nutrition')
    .waitFor({ timeout: 30000 });
}

/**
 * From the Participant page, start a new Family Nutrition encounter.
 */
export async function startFamilyNutritionEncounter(page: Page) {
  await click(
    page.locator('div.ui.primary.button', {
      hasText: 'Family Nutrition Encounter',
    }),
    page,
  );
  await page
    .locator('div.page-encounter.family-nutrition')
    .waitFor({ timeout: 30000 });
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Family member switching
// ---------------------------------------------------------------------------

/**
 * Select a family member by clicking their icon in the encounter page.
 * @param memberIndex 0 = mother, 1 = child 1, 2 = child 2, etc.
 */
export async function selectFamilyMember(page: Page, memberIndex: number) {
  const links = page.locator('.links-body li');
  const target = links.nth(memberIndex);
  // Scroll into view and force click — the activity form below may
  // intercept pointer events if we don't scroll up first.
  await target.scrollIntoViewIfNeeded();
  await target.click({ force: true });
  await page.waitForTimeout(1000);
}

// ---------------------------------------------------------------------------
// Activity completion helpers
// ---------------------------------------------------------------------------

/**
 * Click an activity icon from the encounter page and wait for the form.
 */
async function openActivity(page: Page, iconClass: string, formSelector: string) {
  // Click the activity icon in the task grid.
  const icon = page.locator(`.link-section:has(.icon-activity-task.icon-${iconClass})`);
  await icon.waitFor({ timeout: 10000 });
  await click(icon, page);
  await page.locator(formSelector).waitFor({ timeout: 10000 });
}

/**
 * Click Save and wait for the encounter page to stabilize.
 */
async function saveActivity(page: Page) {
  await click(
    page.locator('button.ui.fluid.primary.button.active', { hasText: 'Save' }),
    page,
  );
  // Wait for re-render after save.
  await page.waitForTimeout(1000);
}

/**
 * Complete Aheza activity for the mother.
 * Fills distribution reason and distributed amount.
 */
export async function completeAhezaMother(
  page: Page,
  options?: {
    amount?: string;
    reasonIndex?: number;
  },
) {
  const amount = options?.amount ?? '3';
  const reasonIndex = options?.reasonIndex ?? 1;

  await openActivity(page, 'fbf', 'div.ui.full.segment.aheza');

  // Select distribution reason (mother only).
  const reasonSelect = page.locator(
    '.form-input.measurement.distribution-reason select',
  );
  const reasonOptions = reasonSelect.locator('option');
  const count = await reasonOptions.count();
  if (count > reasonIndex) {
    const value = await reasonOptions.nth(reasonIndex).getAttribute('value');
    if (value !== null) {
      await reasonSelect.selectOption(value);
    }
  }

  // Fill distributed amount (must be integer — Elm parses with String.toInt).
  await page
    .locator('.form-input.measurement.aheza input[type="number"]')
    .fill(amount);
  await page.waitForTimeout(500);

  await saveActivity(page);
}

/**
 * Complete Aheza activity for a child.
 * Fills distributed amount only (no distribution reason).
 */
export async function completeAhezaChild(
  page: Page,
  options?: { amount?: string },
) {
  const amount = options?.amount ?? '2';

  await openActivity(page, 'fbf', 'div.ui.full.segment.aheza');

  // Fill distributed amount (must be integer — Elm parses with String.toInt).
  await page
    .locator('.form-input.measurement.aheza input[type="number"]')
    .fill(amount);

  await saveActivity(page);
}

/**
 * Complete MUAC activity for any family member.
 * Fills MUAC measurement value.
 */
export async function completeMuac(
  page: Page,
  options?: { value?: string },
) {
  const value = options?.value ?? '25.0';

  await openActivity(page, 'muac', 'div.ui.full.segment.muac');

  // Fill MUAC value (accepts Float via String.toFloat).
  await page
    .locator('div.ui.full.segment.muac input[type="number"]')
    .fill(value);
  await page.waitForTimeout(500);

  await saveActivity(page);
}

/**
 * Complete Photo activity for a child.
 * Uploads a minimal test image via the dropzone.
 */
export async function completePhoto(page: Page) {
  await openActivity(page, 'photo', 'div.ui.full.segment.photo');

  // The photo form uses Dropzone.js with a hidden file input.
  // Find the file input inside the dropzone and upload a minimal PNG.
  const dropzone = page.locator('div#dropzone');
  const fileInput = dropzone.locator('input[type="file"]');

  // Create a minimal 1x1 PNG in memory.
  const pngBase64 =
    'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==';
  const pngBuffer = Buffer.from(pngBase64, 'base64');

  await fileInput.setInputFiles({
    name: 'test-photo.png',
    mimeType: 'image/png',
    buffer: pngBuffer,
  });

  // Wait for the dropzone to process the upload.
  await page.waitForTimeout(2000);

  await saveActivity(page);
}

// ---------------------------------------------------------------------------
// End encounter
// ---------------------------------------------------------------------------

/**
 * End the Family Nutrition encounter.
 * Clicks "End Encounter" and confirms the dialog.
 */
export async function endFamilyNutritionEncounter(page: Page) {
  await page.waitForTimeout(2000);

  // Click End Encounter button.
  const endBtn = page.locator(
    '.end-encounter-button-wrapper button.ui.fluid.button',
    { hasText: 'End Encounter' },
  );
  await endBtn.waitFor({ timeout: 10000 });
  await click(endBtn, page);

  // Handle the confirmation dialog.
  const dialog = page.locator('div.ui.active.modal');
  await dialog.waitFor({ timeout: 5000 });
  const confirmBtn = dialog.locator('button.ui.primary.button', {
    hasText: 'Continue',
  });
  await confirmBtn.click({ force: true });
  await dialog.waitFor({ state: 'hidden', timeout: 10000 });

  // Wait for navigation away from encounter page.
  await page
    .locator('div.page-encounter.family-nutrition')
    .waitFor({ state: 'hidden', timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Backend verification
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for Family Nutrition nodes linked to given persons.
 * Returns an object with boolean flags for each expected node type.
 *
 * Note: execSync is used here following the established E2E test pattern
 * (see ncd.ts, home-visit.ts). Inputs are base64-encoded to prevent injection.
 */
export function queryFamilyNutritionNodes(
  motherName: string,
  childNames?: string[],
): Record<string, boolean> {
  const allNames = [motherName, ...(childNames ?? [])];
  const namesB64 = allNames.map((n) =>
    Buffer.from(n, 'utf8').toString('base64'),
  );

  // Build PHP that resolves person NIDs and checks for measurement nodes.
  const namesArrayPhp = namesB64
    .map((b) => `base64_decode('${b}')`)
    .join(', ');

  const php = `
    \\$names = array(${namesArrayPhp});
    \\$person_nids = array();
    foreach (\\$names as \\$name) {
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'person')
        ->propertyCondition('title', \\$name)
        ->execute();
      if (!empty(\\$r['node'])) {
        \\$person_nids[\\$name] = key(\\$r['node']);
      }
    }

    if (empty(\\$person_nids)) {
      echo json_encode(['error' => 'No persons found']);
      return;
    }

    \\$mother_name = \\$names[0];
    \\$mother_nid = isset(\\$person_nids[\\$mother_name]) ? \\$person_nids[\\$mother_name] : NULL;
    \\$child_nids = array();
    for (\\$i = 1; \\$i < count(\\$names); \\$i++) {
      if (isset(\\$person_nids[\\$names[\\$i]])) {
        \\$child_nids[] = \\$person_nids[\\$names[\\$i]];
      }
    }

    \\$result = array();

    // Check mother measurement types.
    \\$mother_types = array(
      'aheza_mother' => 'ahezaMother',
      'family_nutrition_muac_mother' => 'muacMother',
    );
    foreach (\\$mother_types as \\$node_type => \\$key) {
      if (\\$mother_nid) {
        \\$q = new EntityFieldQuery();
        \\$r = \\$q->entityCondition('entity_type', 'node')
          ->propertyCondition('type', \\$node_type)
          ->fieldCondition('field_person', 'target_id', \\$mother_nid)
          ->range(0, 1)
          ->execute();
        \\$result[\\$key] = !empty(\\$r['node']);
      } else {
        \\$result[\\$key] = FALSE;
      }
    }

    // Check child measurement types.
    \\$child_types = array(
      'aheza_child' => 'ahezaChild',
      'family_nutrition_muac_child' => 'muacChild',
      'family_nutrition_photo' => 'photo',
    );
    foreach (\\$child_types as \\$node_type => \\$key) {
      \\$found = FALSE;
      foreach (\\$child_nids as \\$child_nid) {
        \\$q = new EntityFieldQuery();
        \\$r = \\$q->entityCondition('entity_type', 'node')
          ->propertyCondition('type', \\$node_type)
          ->fieldCondition('field_person', 'target_id', \\$child_nid)
          ->range(0, 1)
          ->execute();
        if (!empty(\\$r['node'])) {
          \\$found = TRUE;
          break;
        }
      }
      \\$result[\\$key] = \\$found;
    }

    // Check encounter node.
    if (\\$mother_nid) {
      // Find family_participant for this person.
      \\$q = new EntityFieldQuery();
      \\$r = \\$q->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'family_participant')
        ->fieldCondition('field_person', 'target_id', \\$mother_nid)
        ->range(0, 1)
        ->execute();
      if (!empty(\\$r['node'])) {
        \\$participant_nid = key(\\$r['node']);
        \\$q2 = new EntityFieldQuery();
        \\$r2 = \\$q2->entityCondition('entity_type', 'node')
          ->propertyCondition('type', 'family_nutrition_encounter')
          ->fieldCondition('field_family_participant', 'target_id', \\$participant_nid)
          ->range(0, 1)
          ->execute();
        \\$result['encounter'] = !empty(\\$r2['node']);
      } else {
        \\$result['encounter'] = FALSE;
      }
    } else {
      \\$result['encounter'] = FALSE;
    }

    echo json_encode(\\$result);
  `;

  const { drushCmd, cwd } = drushEnv();

  // Determine which keys must be true before we stop retrying.
  const requiredKeys = ['ahezaMother', 'muacMother', 'encounter'];
  if (childNames && childNames.length > 0) {
    requiredKeys.push('ahezaChild', 'muacChild');
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

      // Check if all required keys are found.
      const allFound = requiredKeys.every((k) => parsed[k] === true);
      if (allFound) {
        return parsed;
      }

      // Log which keys are still missing.
      const missing = requiredKeys.filter((k) => !parsed[k]);
      console.log(
        `[queryFamilyNutritionNodes] attempt ${attempt + 1}: missing ${missing.join(', ')}`,
      );
    } catch (e) {
      console.log(
        `[queryFamilyNutritionNodes] attempt ${attempt + 1}: error — ${e instanceof Error ? e.message : String(e)}`,
      );
    }

    if (attempt < 9) {
      execSync('sleep 5');
    }
  }

  // Final attempt without retry.
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
