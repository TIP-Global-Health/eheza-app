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
 *   profiles/hedley/modules/custom/hedley_reports/scripts/completion-recalculate-large-datasets.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Resolving all health centers.
$health_center_ids = hedley_health_center_get_all_health_centers_ids();
foreach ($health_center_ids as $health_center_id) {
  drush_print("Running calculation for Health center scope. Health center ID: $health_center_id.");
  $duration = create_or_update_results_data_node('health-center', $health_center_id);
  drush_print("Calculation completed within $duration seconds.");
}

drush_print("Running calculation for Global scope.");
$duration = create_or_update_results_data_node('global');
drush_print("Calculation completed within $duration seconds.");

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
  $data = generate_completion_results_data($health_center);
  hedley_reports_create_or_update_results_data_node($data, 'completion', $scope, NULL, NULL, $health_center);
  $after = time();
  // Free up memory.
  drupal_static_reset();
  // Wait a little to avoid having 2 calculations run at same second
  // which creates naming conflicts.
  sleep(2);
  return $after - $before;
}

/**
 * Generate Completion report data which is stored on person nodes.
 *
 * @param int|null $health_center
 *   Health center ID.
 *
 * @return array
 *   An array of generated data.
 */
function generate_completion_results_data($health_center) {
  $bundles = [
    // Acute Illness data.
    'acute_illness_encounter',
    // Nutrition Group data.
    'attendance',
    // Child Scoreboard data.
    'child_scoreboard_encounter',
    // HIV Data.
    'hiv_encounter',
    // Home Visit data.
    'home_visit_encounter',
    // NCD Data.
    'ncd_encounter',
    // Nutrition Individual data.
    'nutrition_encounter',
    // Prenatal Data.
    'prenatal_encounter',
    // Tuberculosis Data.
    'tuberculosis_encounter',
    // Well Child data.
    'well_child_encounter',
  ];

  $base_query = new EntityFieldQuery();
  $base_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles, 'IN')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_reports_data', 'value', NULL, 'IS NOT NULL')
    ->propertyOrderBy('nid');

  if (!empty($health_center)) {
    $base_query->fieldCondition('field_shards', 'target_id', $health_center);
  }

  $data = [
    'acute_illness' => [],
    'child_scoreboard' => [],
    'hiv' => [],
    'home_visit' => [],
    'ncd' => [],
    'nutrition_individual' => [],
    'nutrition_group' => [],
    'prenatal' => [],
    'tuberculosis' => [],
    'well_child' => [],
  ];

  $count_query = clone $base_query;
  $total = $count_query->count()->execute();

  $nid = $processed = 0;
  $batch = 400;

  while (TRUE) {
    // Free up memory.
    drupal_static_reset();

    $query = clone $base_query;
    if ($nid) {
      $query->propertyCondition('nid', $nid, '>');
    }

    $result = $query
      ->range(0, $batch)
      ->execute();

    if (empty($result['node'])) {
      // No more items left.
      break;
    }

    $ids = array_keys($result['node']);
    $nodes = node_load_multiple($ids);
    foreach ($nodes as $node) {
      $json_data = $node->field_reports_data[LANGUAGE_NONE][0]['value'];
      if (empty($json_data)) {
        continue;
      }

      switch ($node->type) {
        case 'acute_illness_encounter':
          $data['acute_illness'][] = json_decode($json_data);
          break;

        case 'attendance':
          $data['nutrition_group'][] = json_decode($json_data);
          break;

        case 'child_scoreboard_encounter':
          $data['child_scoreboard'][] = json_decode($json_data);
          break;

        case 'hiv_encounter':
          $data['hiv'][] = json_decode($json_data);
          break;

        case 'home_visit_encounter':
          $data['home_visit'][] = json_decode($json_data);
          break;

        case 'ncd_encounter':
          $data['ncd'][] = json_decode($json_data);
          break;

        case 'nutrition_encounter':
          $data['nutrition_individual'][] = json_decode($json_data);
          break;

        case 'prenatal_encounter':
          $data['prenatal'][] = json_decode($json_data);
          break;

        case 'tuberculosis_encounter':
          $data['tuberculosis'][] = json_decode($json_data);
          break;

        case 'well_child_encounter':
          $data['well_child'][] = json_decode($json_data);
          break;
      }

      // Explicitly unset large variables after use for memory optimization.
      unset($json_data);
    }

    $nid = end($ids);
    $processed += count($nodes);

    // Explicitly unset large variables after use for memory optimization.
    unset($nodes);

    if ($processed % 2000 == 0) {
      drush_print("Processed $processed out of $total.");
    }
  }

  return $data;
}
