import { execSync } from 'child_process';
import { existsSync } from 'fs';

/**
 * Delete any existing E2E test device and create a fresh one with
 * a known pairing code. Device pairing codes are single-use, so
 * this must be called before each test that needs to pair.
 *
 * Enables super_user_mode temporarily to bypass content deletion
 * restrictions on paired devices.
 */
export function resetDevice(pairingCode = '99999999') {
  const php = `
    // Enable super user mode to allow deletion of paired devices.
    variable_set('hedley_super_user_mode', 1);

    // Delete any existing E2E test device and its robot user.
    \\$query = new EntityFieldQuery();
    \\$result = \\$query->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'device')
      ->propertyCondition('title', 'E2E Test Device')
      ->execute();
    if (!empty(\\$result['node'])) {
      foreach (array_keys(\\$result['node']) as \\$nid) {
        // Load the device to find its robot user.
        \\$device = node_load(\\$nid);
        if (\\$device && \\$device->uid) {
          user_delete(\\$device->uid);
        }
        node_delete(\\$nid);
      }
    }

    // Also clean up any orphaned robot user from a previous run.
    \\$robot = user_load_by_name('E2E Test Device Robot');
    if (\\$robot) {
      user_delete(\\$robot->uid);
    }

    // Create a new device with a known pairing code.
    \\$node = entity_create('node', ['type' => 'device', 'title' => 'E2E Test Device']);
    \\$wrapper = entity_metadata_wrapper('node', \\$node);
    \\$wrapper->field_pairing_code->set('${pairingCode}');
    node_save(\\$node);

    // Disable super user mode.
    variable_set('hedley_super_user_mode', 0);

    echo 'E2E device created with pairing code ${pairingCode}';
  `;

  const insideDdev = existsSync('/var/www/html/server/www/sites/default/settings.php');
  const drushCmd = insideDdev ? 'drush' : 'ddev drush';
  const cwd = insideDdev ? '/var/www/html' : process.cwd().replace(/\/client$/, '');

  const output = execSync(`${drushCmd} eval "${php}"`, {
    cwd,
    timeout: 30000,
    encoding: 'utf-8',
  });
  console.log(output.trim());
}
