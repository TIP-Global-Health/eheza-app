import { execSync } from 'child_process';

import { Page } from '@playwright/test';
import { click } from './auth';
import { drushEnv } from './device';

/**
 * Click the sync icon, wait for sync to complete on a health center,
 * then navigate back.
 *
 * After the initial "Status: Success" is detected, waits 1 second and
 * re-verifies the status is still "Success" to guard against false
 * positives (e.g., download completes but upload is still in progress).
 *
 * @param page - Playwright page
 * @param healthCenter - Health center name to wait for (default: 'Nyange Health Center')
 * @param timeout - Max time in ms to wait for sync success (default: 300000)
 */
export async function syncAndWait(
  page: Page,
  healthCenter = 'Nyange Health Center',
  timeout = 300000,
) {
  await click(page.locator('span.sync-icon'), page);

  await page.locator('.device-status').waitFor({ timeout: 10000 });

  const hcSection = page.locator('.health-center', {
    has: page.locator('h2', { hasText: healthCenter }),
  });
  await hcSection.waitFor({ timeout: 10000 });

  // Wait for sync to complete.
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout });

  // Verify status is stable (not a transient state between phases).
  await page.waitForTimeout(1000);
  await hcSection
    .locator('.sync-status', { hasText: 'Status: Success' })
    .waitFor({ timeout });

  await page.goBack();
}

/**
 * Finds a person node by Drupal title and returns its nid.
 *
 * Retries up to 10 times (5-second delay between attempts) to handle cases
 * where the node has not yet been synced to the backend.
 *
 * @param personName - The full name of the person as stored in the node title
 * @returns The numeric node ID (nid) of the matching person node
 * @throws If the person is not found after 10 attempts
 */
export function findPersonNid(personName: string): number {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo 'NOT_FOUND'; return; }
    echo key(\\$r['node']);
  `;

  for (let attempt = 0; attempt < 10; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
    }).trim();

    if (output !== 'NOT_FOUND') {
      const nid = parseInt(output, 10);
      if (!isNaN(nid)) return nid;
    }

    console.log(`findPersonNid("${personName}") attempt ${attempt + 1}: ${output}`);
    if (attempt < 9) execSync('sleep 5');
  }

  throw new Error(`findPersonNid: person "${personName}" not found after 10 attempts`);
}

/**
 * Queries which measurement node types exist for a given person.
 *
 * Retries up to 10 times (5-second delay between attempts). If `expectedTypes`
 * is provided, keeps retrying until all expected types are present in the
 * result, making this suitable for post-sync verification.
 *
 * @param personName    - The full name of the person as stored in the node title
 * @param nodeTypes     - List of Drupal node type machine names to check
 * @param expectedTypes - Optional subset of `nodeTypes` that must all be `true`
 *                        before returning early
 * @returns A map of node type → boolean indicating whether at least one node
 *          of that type exists for the person
 */
export function queryMeasurementNodes(
  personName: string,
  nodeTypes: string[],
  expectedTypes?: string[],
): Record<string, boolean> {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');
  const typesStr = nodeTypes.map(t => `'${t}'`).join(', ');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo json_encode(['error' => 'Person not found']); return; }
    \\$nid = key(\\$r['node']);

    \\$types = array(${typesStr});
    \\$found = array();
    foreach (\\$types as \\$type) {
      \\$tq = new EntityFieldQuery();
      \\$tr = \\$tq->entityCondition('entity_type', 'node')
        ->propertyCondition('type', \\$type)
        ->fieldCondition('field_person', 'target_id', \\$nid)
        ->range(0, 1)
        ->execute();
      \\$found[\\$type] = !empty(\\$tr['node']);
    }
    echo json_encode(\\$found);
  `;

  for (let attempt = 0; attempt < 10; attempt++) {
    try {
      const output = execSync(`${drushCmd} eval "${php}"`, {
        cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
      }).trim();

      const parsed = JSON.parse(output);
      if (parsed.error) {
        console.log(`queryMeasurementNodes attempt ${attempt + 1}: ${parsed.error}`);
        if (attempt < 9) { execSync('sleep 5'); continue; }
        return parsed;
      }

      if (expectedTypes) {
        const missing = expectedTypes.filter(t => !parsed[t]);
        if (missing.length === 0) return parsed;
        console.log(`queryMeasurementNodes attempt ${attempt + 1}: missing [${missing.join(', ')}]`);
        if (attempt < 9) { execSync('sleep 5'); continue; }
      }

      return parsed;
    } catch (err) {
      console.log(`queryMeasurementNodes attempt ${attempt + 1}: error`, err);
      if (attempt < 9) execSync('sleep 5');
    }
  }

  return {};
}

/**
 * Backdates the most recent encounter of the given type for a person.
 *
 * Resolves the chain: person → individual_participant → encounter, then sets
 * `field_scheduled_date` to `daysAgo` days in the past. Retries up to 5 times
 * (10-second delay between attempts) and logs each outcome.
 *
 * @param personName    - The full name of the person as stored in the node title
 * @param encounterType - Drupal node type machine name for the encounter
 * @param daysAgo       - How many days back to set the scheduled date (default: 7)
 */
export function backdateEncounter(personName: string, encounterType: string, daysAgo = 7): void {
  const { drushCmd, cwd } = drushEnv();
  const personNameB64 = Buffer.from(personName, 'utf8').toString('base64');

  const php = `
    \\$name = base64_decode('${personNameB64}');
    \\$q = new EntityFieldQuery();
    \\$r = \\$q->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'person')
      ->propertyCondition('title', \\$name)
      ->execute();
    if (empty(\\$r['node'])) { echo 'Person not found'; return; }
    \\$nid = key(\\$r['node']);

    \\$pq = new EntityFieldQuery();
    \\$pr = \\$pq->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'individual_participant')
      ->fieldCondition('field_person', 'target_id', \\$nid)
      ->execute();
    if (empty(\\$pr['node'])) { echo 'No participant found'; return; }

    \\$participant_nids = array_keys(\\$pr['node']);
    \\$eq = new EntityFieldQuery();
    \\$er = \\$eq->entityCondition('entity_type', 'node')
      ->propertyCondition('type', '${encounterType}')
      ->fieldCondition('field_individual_participant', 'target_id', \\$participant_nids, 'IN')
      ->propertyOrderBy('nid', 'DESC')
      ->execute();
    if (empty(\\$er['node'])) { echo 'No encounter found'; return; }

    \\$target_date = date('Y-m-d H:i:s', strtotime('-${daysAgo} days'));
    \\$enc_nid = key(\\$er['node']);
    \\$enc = node_load(\\$enc_nid);
    \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value'] = \\$target_date;
    \\$enc->field_scheduled_date[LANGUAGE_NONE][0]['value2'] = \\$target_date;
    node_save(\\$enc);
    echo 'Backdated encounter ' . \\$enc_nid;
  `;

  for (let attempt = 0; attempt < 5; attempt++) {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd, timeout: 30000, encoding: 'utf-8', stdio: 'pipe',
    }).trim();
    console.log(`backdateEncounter(${encounterType}) attempt ${attempt + 1}: ${output}`);
    if (output.startsWith('Backdated')) return;
    if (attempt < 4) execSync('sleep 10');
  }
  console.error(`backdateEncounter(${encounterType}): failed after 5 attempts`);
}
