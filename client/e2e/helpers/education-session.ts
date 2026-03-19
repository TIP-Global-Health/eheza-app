import { Page } from '@playwright/test';
import { execSync } from 'child_process';
import { click } from './auth';
import { drushEnv } from './device';

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

/**
 * Navigate from the dashboard to a new group education session
 * using the CHW flow: Clinical → Group Assessment → GroupEncounterTypesPage
 * → "Health Education".
 *
 * After clicking "Health Education", the app creates an education session
 * and navigates to the EducationSessionPage (topics selection).
 */
export async function navigateToEducationSession(page: Page) {
  // Dashboard → Clinical
  await click(page.locator('.icon-task-clinical'), page);
  await page.locator('div.page-clinical').waitFor({ timeout: 10000 });

  // Clinical → Group Assessment → GroupEncounterTypesPage
  await click(page.locator('button.group-assessment'), page);

  const encounterTypesPage = page.locator('div.page-encounter-types');

  // CHW goes to GroupEncounterTypesPage. Retry with reload if needed
  // (village clinic data may not be loaded yet).
  for (let attempt = 0; attempt < 5; attempt++) {
    const visible = await encounterTypesPage
      .waitFor({ timeout: 5000 })
      .then(() => true)
      .catch(() => false);

    if (visible) break;

    if (attempt === 4) {
      throw new Error(
        'GroupEncounterTypesPage not reached after retries.',
      );
    }

    await page.reload();
    await page.waitForTimeout(1000);
    await page.locator('div.page-clinical').waitFor({ timeout: 10000 });
    await click(page.locator('button.group-assessment'), page);
  }

  // Click "Health Education" button to create session.
  const educationBtn = page.locator('button.encounter-type', {
    hasText: 'Health Education',
  });
  await educationBtn.waitFor({ timeout: 10000 });
  await click(educationBtn, page);

  // Wait for EducationSessionPage (topics selection).
  await page
    .locator('div.page-activity.education-session')
    .waitFor({ timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Topics selection
// ---------------------------------------------------------------------------

/**
 * Select education topics on the ModeTopics page.
 * Each topic is a checkbox rendered as div.ui.checkbox.activity with
 * onClick on the div. The input is CSS-hidden.
 *
 * @param topics - Array of topic label substrings (e.g., "Tuberculosis").
 */
export async function selectTopics(page: Page, topics: string[]) {
  // Verify we're on the topics page (header says "Health Topics").
  await page
    .locator('.ui.header', { hasText: 'Health Topics' })
    .waitFor({ timeout: 5000 });

  for (const topic of topics) {
    const checkbox = page.locator(
      '.checkbox-select-input .ui.checkbox.activity',
      { hasText: topic },
    );
    await checkbox.waitFor({ timeout: 5000 });
    await click(checkbox, page);
    await page.waitForTimeout(300);
  }

  // Click Save button.
  const saveBtn = page.locator('div.actions button.ui.fluid.button', {
    hasText: 'Save',
  });
  await saveBtn.waitFor({ timeout: 5000 });
  await click(saveBtn, page);

  // Wait for transition to attendance page (header says "Attendance").
  await page
    .locator('.ui.header', { hasText: 'Attendance' })
    .waitFor({ timeout: 10000 });
}

// ---------------------------------------------------------------------------
// Participant selection
// ---------------------------------------------------------------------------

/**
 * Click "Display all participants" toggle to show all village adults.
 */
export async function toggleAllParticipants(page: Page) {
  const toggleText = page.locator('.toggle-initial-display .toggle-text', {
    hasText: 'Display all participants',
  });
  await toggleText.waitFor({ timeout: 5000 });
  await click(toggleText, page);
  await page.waitForTimeout(500);
}

/**
 * Click check-in mark on a participant card to select them.
 * Returns the participant's display name.
 *
 * @param index - 0-based index of the participant in the list.
 */
export async function selectParticipant(
  page: Page,
  index: number = 0,
): Promise<string> {
  const participants = page.locator(
    '.participants-list .item',
  );
  const card = participants.nth(index);
  await card.waitFor({ timeout: 5000 });

  // Capture participant name before clicking.
  const name = await card.locator('.content .header').innerText();

  // Click the check-in button.
  const checkInBtn = card.locator('.button-check-in');
  await click(checkInBtn, page);
  await page.waitForTimeout(500);

  return name;
}

// ---------------------------------------------------------------------------
// End session
// ---------------------------------------------------------------------------

/**
 * End the education session by clicking "Record Group Education".
 * After this, the app navigates back to the Dashboard.
 */
export async function endEducationSession(page: Page) {
  await page.waitForTimeout(2000);

  const recordBtn = page.locator('div.actions button.ui.fluid.button', {
    hasText: 'Record Group Education',
  });
  await recordBtn.waitFor({ timeout: 10000 });
  await click(recordBtn, page);

  // Wait for Dashboard.
  await page
    .locator('button.ui.button.logout')
    .waitFor({ timeout: 30000 });
}

// ---------------------------------------------------------------------------
// Sync
// ---------------------------------------------------------------------------

/**
 * Trigger a sync cycle and wait for it to complete.
 */
export async function syncAndWait(page: Page, healthCenter: string) {
  await click(page.locator('span.sync-icon'), page);
  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: healthCenter }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  // Wait for sync to complete.
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 300000 });

  // Verify status is stable.
  await page.waitForTimeout(1000);
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout: 5000 });

  await page.goBack();
}

// ---------------------------------------------------------------------------
// Backend verification
// ---------------------------------------------------------------------------

/**
 * Query the Drupal backend for an education_session node that includes
 * the given participant. Verifies topics and end date are set.
 *
 * Inputs are base64-encoded to prevent shell injection.
 *
 * @param participantName - Drupal title format name (e.g., "Doe Jane").
 * @param expectedTopics - Backend topic strings (e.g., ["tuberculosis", "malaria"]).
 * @returns Object with { found, topics, participantFound, hasEndDate, allTopicsFound }.
 */
export function queryEducationSessionNodes(
  participantName: string,
  expectedTopics: string[],
): { found: boolean; topics: string[]; participantFound: boolean; hasEndDate: boolean; allTopicsFound: boolean } {
  const nameB64 = Buffer.from(participantName, 'utf8').toString('base64');
  const topicsB64 = Buffer.from(JSON.stringify(expectedTopics), 'utf8').toString('base64');

  const php = `
    \\$name = base64_decode('${nameB64}');
    \\$expected = json_decode(base64_decode('${topicsB64}'), TRUE);
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')->propertyCondition('type', 'person')->propertyCondition('title', \\$name)->execute();
    if (empty(\\$r['node'])) { echo json_encode(array('found' => FALSE, 'error' => 'person')); return; }
    \\$pid = key(\\$r['node']);
    \\$q2 = new EntityFieldQuery();
    \\$r2 = \\$q2->entityCondition('entity_type', 'node')->propertyCondition('type', 'education_session')->fieldCondition('field_participating_patients', 'target_id', \\$pid)->execute();
    if (empty(\\$r2['node'])) { echo json_encode(array('found' => FALSE, 'error' => 'session')); return; }
    \\$sid = key(\\$r2['node']);
    \\$s = node_load(\\$sid);
    \\$topics = array();
    foreach (\\$s->field_education_topics['und'] as \\$i) { \\$topics[] = \\$i['value']; }
    \\$end = !empty(\\$s->field_scheduled_date['und'][0]['value2']);
    \\$all = count(array_intersect(\\$expected, \\$topics)) == count(\\$expected);
    echo json_encode(array('found' => TRUE, 'topics' => \\$topics, 'participantFound' => TRUE, 'hasEndDate' => \\$end, 'allTopicsFound' => \\$all));
  `;

  const { drushCmd, cwd } = drushEnv();

  // Retry for eventual consistency after sync.
  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd,
        timeout: 30000,
        encoding: 'utf-8',
      }).trim();

      if (!output) {
        console.log(`[queryEducationSessionNodes] attempt ${attempt + 1}: empty output`);
        if (attempt < 9) { execSync('sleep 5'); }
        continue;
      }

      const parsed = JSON.parse(output);
      if (parsed.found) {
        return parsed;
      }

      console.log(
        `[queryEducationSessionNodes] attempt ${attempt + 1}: session not found yet`,
      );
    } catch (e) {
      console.log(
        `[queryEducationSessionNodes] attempt ${attempt + 1}: error — ${e instanceof Error ? e.message : String(e)}`,
      );
    }

    if (attempt < 9) {
      execSync('sleep 5');
    }
  }

  return { found: false, topics: [], participantFound: false, hasEndDate: false, allTopicsFound: false };
}
