import { execSync } from 'child_process';
import { existsSync } from 'fs';

/**
 * Playwright global setup: creates a fresh device node with a known
 * pairing code before the E2E tests run.
 *
 * Device pairing codes are single-use (deleted after pairing), so we
 * need a fresh device for each test run.
 */
export default function globalSetup() {
  const php = `
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
    \\$wrapper->field_pairing_code->set('99999999');
    node_save(\\$node);
    echo 'E2E device created with pairing code 99999999';
  `;

  // Detect if running inside the ddev container (drush is available
  // directly) or on the host (need to use `ddev drush`).
  const insideDdev = existsSync('/var/www/html/server/www/sites/default/settings.php');
  const drushCmd = insideDdev ? 'drush' : 'ddev drush';
  const cwd = insideDdev ? '/var/www/html' : process.cwd().replace(/\/client$/, '');

  try {
    const output = execSync(`${drushCmd} eval "${php}"`, {
      cwd,
      timeout: 30000,
      encoding: 'utf-8',
    });
    console.log(output.trim());
  } catch (error) {
    console.error('Failed to create E2E test device:', error);
    throw error;
  }
}
