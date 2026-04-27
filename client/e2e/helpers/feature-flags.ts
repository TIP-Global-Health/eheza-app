import { execSync } from 'child_process';
import { drushEnv } from './device';

/**
 * Toggle a `hedley_admin_feature_<name>_enabled` Drupal variable.
 *
 * After changing a flag, the device must sync (via the sync icon /
 * `syncAndWait`) for the new value to reach the Elm SyncManager
 * `features` set; only then will gated UI re-render.
 *
 * @param name - Short feature name, e.g. 'antenatal', 'gps_coordinates',
 *               'stock_management_hc'. The full variable name is built
 *               by prepending 'hedley_admin_feature_' and appending
 *               '_enabled'.
 * @param enabled - true sets the variable to 1, false sets it to 0.
 *
 * Note: execSync with a hardcoded variable-name pattern — `name` is
 * validated as snake_case to prevent injection. Same pattern as
 * ensureStockManagementHCFeatureEnabled in stock-management.ts.
 */
export function setFeatureFlag(name: string, enabled: boolean) {
  if (!/^[a-z][a-z0-9_]*$/.test(name)) {
    throw new Error(`Invalid feature flag name: "${name}"`);
  }

  const { drushCmd, cwd } = drushEnv();
  const value = enabled ? '1' : '0';
  execSync(`${drushCmd} vset hedley_admin_feature_${name}_enabled ${value}`, {
    cwd,
    timeout: 15000,
    encoding: 'utf-8',
    stdio: 'pipe',
  });
}
