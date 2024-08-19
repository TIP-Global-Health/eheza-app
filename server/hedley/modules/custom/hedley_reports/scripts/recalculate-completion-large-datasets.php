<?php

/**
 * @file
 * Recalculates high level reports completion data for large datasets.
 *
 * Stores results at 'Report Data' nodes.
 * Large datasets are:
 *   - Global.
 *   - Health center.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/recalculate-completion-large-datasets.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

drush_print("Running calculation for Global scope.");
$duration = create_or_update_results_data_node('global');
drush_print("Calculation completed within $duration seconds.");

// Resolving all health centers.
$health_center_ids = hedley_health_center_get_all_health_centers_ids();
foreach ($health_center_ids as $health_center_id) {
  drush_print("Running calculation for Health center scope. Health center ID: $health_center_id.");
  $duration = create_or_update_results_data_node('health-center', $health_center_id);
  drush_print("Calculation completed within $duration seconds.");
}

drush_print('');
drush_print('All calculations completed!');

/**
 * Generates data in scope and creates/updates Report Data node.
 *
 * Calculates the time taken for the operation.
 *
 * @param string $scope
 *   The scope of the report data.
 * @param string $health_center
 *   The health center for the report data.
 *
 * @return int
 *   The time taken for the operation in seconds.
 */
function create_or_update_results_data_node($scope, $health_center = NULL) {
  $before = time();
  drush_print('Loading data...');
  $data = hedley_reports_generate_completion_results_data($health_center);
  hedley_reports_create_or_update_results_data_node($data, 'completion', $scope, NULL, NULL, $health_center);
  $after = time();
  // Free up memory.
  drupal_static_reset();
  // Wait a little to avoid having 2 calculations run at same second
  // which creates naming conflicts.
  sleep(2);
  return $after - $before;
}
