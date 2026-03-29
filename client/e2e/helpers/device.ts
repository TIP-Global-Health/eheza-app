import { execSync } from 'child_process';
import { existsSync } from 'fs';

/**
 * Resolve the drush command and working directory for running backend
 * commands.  When E2E_DDEV_PROJECT is set (a path to the DDEV project
 * root), drush commands target that project.  Otherwise, we detect
 * whether we are inside the DDEV container or fall back to the current
 * repo root.
 */
export function drushEnv(): { drushCmd: string; cwd: string } {
  const insideDdev = existsSync('/var/www/html/server/www/sites/default/settings.php');
  if (insideDdev) {
    return { drushCmd: 'drush', cwd: '/var/www/html' };
  }
  const projectDir = process.env.E2E_DDEV_PROJECT
    || process.cwd().replace(/\/client$/, '');
  return { drushCmd: 'ddev drush', cwd: projectDir };
}

/**
 * Delete all E2E test devices and their associated data.
 * user_delete() on the robot user cascade-deletes all owned nodes
 * (encounters, measurements, persons) via node_user_delete hook.
 *
 * Called once from globalSetup before any tests run, to clean up
 * data from previous test runs.
 *
 * Note: execSync with hardcoded commands — no user input involved.
 */
export function cleanupStaleTestData() {
  const { drushCmd, cwd } = drushEnv();
  const php = `
      \\$eq = new EntityFieldQuery();
      \\$old_devices = \\$eq->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'device')
        ->propertyCondition('title', 'E2E Device%', 'LIKE')
        ->execute();
      if (!empty(\\$old_devices['node'])) {
        variable_set('hedley_super_user_mode', 1);
        \\$count = 0;
        foreach (node_load_multiple(array_keys(\\$old_devices['node'])) as \\$old) {
          if (\\$old->uid) {
            user_delete(\\$old->uid);
          }
          node_delete(\\$old->nid);
          \\$count++;
        }
        variable_set('hedley_super_user_mode', 0);
        echo 'Cleaned up ' . \\$count . ' old E2E devices.';
      } else {
        echo 'No stale E2E devices to clean up.';
      }
  `;

  const output = execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 300000,
    encoding: 'utf-8',
  });
  console.log(output.trim());
}

/**
 * Create a fresh E2E test device with a known pairing code and a
 * unique title. Device pairing codes are single-use, so this must
 * be called before each test that needs to pair.
 *
 * Note: execSync with hardcoded commands — no user input involved.
 */
export function resetDevice(pairingCode = '99999999') {
  if (!/^\d+$/.test(pairingCode)) {
    throw new Error(`Invalid pairing code: must be digits only, got "${pairingCode}"`);
  }

  const title = `E2E Device ${Date.now()}`;

  const php = `
      // Clear pairing code on any existing device that holds it,
      // so the uniqueness check passes for the new device.
      variable_set('hedley_super_user_mode', 1);
      \\$query = new EntityFieldQuery();
      \\$result = \\$query->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'device')
        ->fieldCondition('field_pairing_code', 'value', '${pairingCode}')
        ->execute();
      if (!empty(\\$result['node'])) {
        foreach (node_load_multiple(array_keys(\\$result['node'])) as \\$old) {
          \\$old->field_pairing_code[LANGUAGE_NONE][0]['value'] = '';
          node_save(\\$old);
        }
      }
      variable_set('hedley_super_user_mode', 0);

      // 3. Create the new device.
      \\$node = entity_create('node', ['type' => 'device', 'title' => '${title}']);
      \\$wrapper = entity_metadata_wrapper('node', \\$node);
      \\$wrapper->field_pairing_code->set('${pairingCode}');
      node_save(\\$node);

      echo 'E2E device created with pairing code ${pairingCode}';
  `;

  const { drushCmd, cwd } = drushEnv();

  const output = execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
  });
  console.log(output.trim());
}
