<?php

/**
 * @file
 * Generates prevalence monthly report.
 *
 * For FBF and PMTCT groups at Gekenke district, between 2018 and 2021.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-grant-report-groups-monthly.php.
 */

require_once __DIR__ . '/report_common.inc';

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 100);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 250);

$catchment_areas = [
  'ruli' => 7086,
  'nemba' => 228325,
];

$hcs_query = base_query_for_bundle('health_center');

$query = clone $hcs_query;
$query->fieldCondition('field_catchment_area', 'target_id', $catchment_areas['ruli']);
$result = $query->execute();
// Exclude ACHI and Test HCs.
$ruli_hcs_ids = array_diff(array_keys($result['node']), [28589, 555324]);

$query = clone $hcs_query;
$query->fieldCondition('field_catchment_area', 'target_id', $catchment_areas['nemba']);
$result = $query->execute();
$nemba_hcs_ids = array_keys($result['node']);

$gakenke_hcs_ids = array_merge($ruli_hcs_ids, $nemba_hcs_ids);
$base_query = base_query_for_bundle('person');

$six_years_ago = date('Ymd', strtotime('-6 years'));
$base_query->fieldCondition('field_birth_date', 'value', $six_years_ago, '>');
$base_query->fieldCondition('field_health_center', 'target_id', $gakenke_hcs_ids, 'IN');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no patients in DB.");
  exit;
}

drush_print("$total children with age below 6 years located.");

$processed = 0;

$stunting = $underweight = $wasting = [];

while ($processed < $total) {
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
  $children = node_load_multiple($ids);
  foreach ($children as $child) {
    $wrapper = entity_metadata_wrapper('node', $child);
    $deleted = $wrapper->field_deleted->value();
    $birth_date = $wrapper->field_birth_date->value();

    if ($deleted) {
      // Skip deleted patient.
      continue;
    }

    $bundles = array_merge(HEDLEY_ACTIVITY_HEIGHT_BUNDLES, HEDLEY_ACTIVITY_WEIGHT_BUNDLES);
    $measurements_ids = hedley_general_get_person_measurements($child->nid, $bundles);

    // If child got no measurements, move on to next child.
    if (empty($measurements_ids)) {
      continue;
    }

    $measurements = node_load_multiple($measurements_ids);
    foreach ($measurements as $measurement) {
      $wrapper = entity_metadata_wrapper('node', $measurement);
      $measurement_timestamp = $wrapper->field_date_measured->value();
      $grouped_by_month_key = date('Y F', $measurement_timestamp);

      if (in_array($measurement->type, HEDLEY_ACTIVITY_HEIGHT_BUNDLES)) {
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
        $stunting = categorize_data($stunting, $grouped_by_month_key, $stunting_moderate, $stunting_severe, $child->nid);
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
      $underweight = categorize_data($underweight, $grouped_by_month_key, $underweight_moderate, $underweight_severe, $child->nid);

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_length');
      $wasting = categorize_data($wasting, $grouped_by_month_key, $wasting_moderate, $wasting_severe, $child->nid);
    }
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
}

drush_print('Done!');

drush_print("# Nutrition report  - " . date('D/m/Y'));
drush_print('');

$skeleton = [
  [
    'Stunting Moderate',
  ],
  [
    'Stunting Severe',
  ],
  [
    'Underweight Moderate',
  ],
  [
    'Underweight Severe',
  ],
  [
    'Wasting Moderate',
  ],
  [
    'Wasting Severe',
  ],
];

drush_print('## Prevalence by month (period prevalence) for Gakenke district');

print_prevalence_report($skeleton, $stunting, $underweight, $wasting);


/**
 * Classifies measurement by its malnutrition indicator (severe / moderate).
 *
 * If measurement does not indicate malnutrition, 0 values are returned.
 */
function classify_by_malnutrition_type($wrapper, $field): array {
  $z_score = $wrapper->{$field}->value();

  if (empty($z_score)) {
    return [0, 0];
  }

  if ($z_score <= -3) {
    return [0, 1];
  }

  if ($z_score <= -2) {
    return [1, 0];
  }

  return [0, 0];
}

/**
 * Generate base query.
 */
function base_query_for_bundle($bundle): EntityFieldQuery {
  $query = new EntityFieldQuery();
  $query
    ->entityCondition('entity_type', 'node')
    ->propertyCondition('type', $bundle)
    ->propertyCondition('status', NODE_PUBLISHED);

  return $query;
}

/**
 * Formats prevalence data.
 *
 * @param array $cases
 *   Number of cases.
 * @param array $examined
 *   Examined patients.
 */
function format_prevalence(array $cases, array $examined): string {
  return round(((count($cases) / count(array_unique($examined))) * 100), 3) . ' %';
}

/**
 * Categorize the data by keys.
 *
 * @param array $dataset
 *   Dataset for the given malnutrition type.
 * @param string $grouped_by_month_key
 *   Month key.
 * @param int $moderate
 *   Indicated moderate.
 * @param int $severe
 *   Indicated severe.
 * @param int $id
 *   Child node ID.
 *
 * @return array
 *   Updated dataset.
 */
function categorize_data(array $dataset, string $grouped_by_month_key, int $moderate, int $severe, int $id): array {
  $category_ids = [
    'moderate' => [],
    'severe' => [],
    'any' => [],
  ];
  if (!isset($dataset[$grouped_by_month_key])) {
    $dataset[$grouped_by_month_key] = $category_ids;
  }
  if ($moderate) {
    $dataset[$grouped_by_month_key]['moderate'][] = $id;
  }
  elseif ($severe) {
    $dataset[$grouped_by_month_key]['severe'][] = $id;
  }

  $dataset[$grouped_by_month_key]['any'][] = $id;

  return $dataset;
}

/**
 * Prints prevalence report.
 *
 * @param array $skeleton
 *   Column names.
 * @param array $stunting
 *   Stunting data.
 * @param array $underweight
 *   Underweight data.
 * @param array $wasting
 *   Wasting data.
 */
function print_prevalence_report(array $skeleton, array $stunting, array $underweight, array $wasting) {
  // Assemble the table for prevalence.
  for ($j = 0; $j <= 3; $j++) {
    $data[$j] = $skeleton;
    $header = ['Prevalence by Month'];

    for ($i = 1; $i <= 12; $i++) {
      $current_month = strtotime('- ' . ($j * 12 + $i) . 'months');
      $month_key = date('Y F', $current_month);
      $header[] = $month_key;

      if (isset($stunting[$month_key]['moderate'])) {
        $data[$j][0][] = format_prevalence($stunting[$month_key]['moderate'], $stunting[$month_key]['any']);
      } else {
        $data[$j][0][] = '-';
      }
      if (isset($stunting[$month_key]['severe'])) {
        $data[$j][1][] = format_prevalence($stunting[$month_key]['severe'], $stunting[$month_key]['any']);
      } else {
        $data[$j][1][] = '-';
      }

      if (isset($underweight[$month_key]['moderate'])) {
        $data[$j][2][] = format_prevalence($underweight[$month_key]['moderate'], $underweight[$month_key]['any']);
      } else {
        $data[$j][2][] = '-';
      }
      if (isset($underweight[$month_key]['severe'])) {
        $data[$j][3][] = format_prevalence($underweight[$month_key]['severe'], $underweight[$month_key]['any']);
      } else {
        $data[$j][3][] = '-';
      }

      if (isset($wasting[$month_key]['moderate'])) {
        $data[$j][4][] = format_prevalence($wasting[$month_key]['moderate'], $wasting[$month_key]['any']);
      } else {
        $data[$j][4][] = '-';
      }
      if (isset($wasting[$month_key]['severe'])) {
        $data[$j][5][] = format_prevalence($wasting[$month_key]['severe'], $wasting[$month_key]['any']);
      } else {
        $data[$j][5][] = '-';
      }
    }

    $text_table = new HedleyAdminTextTable($header);
    $text_table->addData($data[$j]);
    drush_print($text_table->render());
  }
}
