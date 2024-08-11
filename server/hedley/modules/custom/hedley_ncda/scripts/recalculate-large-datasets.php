<?php

/**
 * @file
 * Recalculates high level reports data for large datasets.
 *
 * Stores results at 'Report Data' nodes.
 * Large datasets are:
 *   - District.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_ncda/scripts/recalculate-large-datasets.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Resolving unique provinces as they appear at DB.
$query = db_select('field_data_field_province', 'fp')
  ->fields('fp', array('field_province_value'))
  ->distinct();
$result = $query->execute();
$provinces = [];
foreach ($result as $record) {
  $provinces[] = $record->field_province_value;
}

foreach ($provinces as $province) {
  $province = trim($province);
  if (empty($province)) {
    continue;
  }

  // Resolving unique districts for the province, as they appear at DB.
  $districts = hedley_reports_get_unique_districts_by_province($province);
  foreach ($districts as $district) {
    $district = trim($district);
    if (empty($district)) {
      continue;
    }
    drush_print("Running calculation for District scope. Province: $province, District: $district.");
    $duration = create_or_update_results_data_node('district', $province, $district);
    drush_print("Calculation completed within $duration seconds.");
  }
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
 * @param string $province
 *   The province for the report data.
 * @param string $district
 *   The district for the report data.
 *
 * @return int
 *   The time taken for the operation in seconds.
 */
function create_or_update_results_data_node($scope, $province, $district) {
  $before = time();
  $data = hedley_ncda_generate_results_data($province, $district, NULL, NULL, NULL);
  hedley_reports_create_or_update_results_data_node($data, 'aggregated-ncda', $scope, $province, $district);
  $after = time();
  // Free up memory.
  drupal_static_reset();
  // Wait a little to avoid having 2 calculations run at same second
  // which creates naming conflicts.
  sleep(2);
  return $after - $before;
}
