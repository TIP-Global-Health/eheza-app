<?php

/**
 * @file
 * Generates 'Nutrition' report for past.
 *
 * Counts occurrences of malnutrition indicators (moderate and severe).
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-nutrition-report.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

require_once __DIR__ . '/TextTable.php';

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 100);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

$base_query = base_query_for_bundle('person');

$six_years_ago = date('Ymd', strtotime('-6 years'));
$base_query->fieldCondition('field_birth_date', 'value', $six_years_ago, '>');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no patients in DB.");
  exit;
}


drush_print("$total children with age below 6 years located.");

$processed = 0;
$patients_per_month = [];
$category_counters = [
  'moderate' => 0,
  'severe' => 0,
];
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
      if (!isset($patients_per_month[$grouped_by_month_key])) {
        $patients_per_month[$grouped_by_month_key] = [];
      }
      $patients_per_month[$grouped_by_month_key][] = $child->nid;

      if (in_array($measurement->type, HEDLEY_ACTIVITY_HEIGHT_BUNDLES)) {
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
        if (!isset($stunting[$grouped_by_month_key])) {
          $stunting[$grouped_by_month_key] = $category_counters;
        }

        $stunting[$grouped_by_month_key]['moderate'] += $stunting_moderate;
        $stunting[$grouped_by_month_key]['severe'] += $stunting_severe;
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
      if (!isset($underweight[$grouped_by_month_key])) {
        $underweight[$grouped_by_month_key] = $category_counters;
      }
      $underweight[$grouped_by_month_key]['moderate'] += $underweight_moderate;
      $underweight[$grouped_by_month_key]['severe'] += $underweight_severe;

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_length');
      if (!isset($wasting[$grouped_by_month_key])) {
        $wasting[$grouped_by_month_key] = $category_counters;
      }
      $wasting[$grouped_by_month_key]['moderate'] += $wasting_moderate;
      $wasting[$grouped_by_month_key]['severe'] += $wasting_severe;
    }
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  progress_bar($processed, $total);
}

drush_print('Done!');

drush_print("# Nutrition report  - " . date('D/m/Y'));
drush_print('');

$skeleton = [
  [
    'Stunting Moderate'
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

drush_print('## Prevalence by month (period prevalence)');
drush_print('### One visit or more');

$header = [
  'Prevalence by Month',
];
$data = $skeleton;

// Assemble the table for prevalence.
for ($i = 0; $i < 6; $i++) {
  $current_month = strtotime('- ' . $i . 'months');
  $month_key = date('Y F',  $current_month);
  $header[] = $month_key;

  if (isset($stunting[$month_key]['moderate'])) {
    $data[0][] = format_prevalence($stunting[$month_key]['moderate'], $patients_per_month[$month_key]);
  }
  else {
    $data[0][] = '-';
  }
  if (isset($stunting[$month_key]['severe'])) {
    $data[1][] = format_prevalence($stunting[$month_key]['severe'], $patients_per_month[$month_key]);
  }
  else {
    $data[1][] = '-';
  }

  if (isset($underweight[$month_key]['moderate'])) {
    $data[2][] = format_prevalence($underweight[$month_key]['moderate'], $patients_per_month[$month_key]);
  }
  else {
    $data[2][] = '-';
  }
  if (isset($underweight[$month_key]['severe'])) {
    $data[3][] = format_prevalence($underweight[$month_key]['severe'], $patients_per_month[$month_key]);
  }
  else {
    $data[3][] = '-';
  }

  if (isset($wasting[$month_key]['moderate'])) {
    $data[4][] = format_prevalence($wasting[$month_key]['moderate'], $patients_per_month[$month_key]);
  }
  else {
    $data[4][] = '-';
  }
  if (isset($wasting[$month_key]['severe'])) {
    $data[5][] = format_prevalence($wasting[$month_key]['severe'], $patients_per_month[$month_key]);
  }
  else {
    $data[5][] = '-';
  }

}
$text_table = new TextTable($header);
$text_table->addData($data);

drush_print($text_table->render());

/**
 * Classifies measurement by it's malnutrition indicator (severe / moderate).
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
 * Displays a textual progress bar.
 *
 * @param int $done
 *   Number of completed items.
 * @param int $total
 *   Number of total items.
 */
function progress_bar(int $done, int $total) {
    $percentage = floor(($done / $total) * 100);
    $left = 100 - $percentage;
    $write = sprintf("\033[0G\033[2K[%'={$percentage}s>%-{$left}s] - $percentage%% - $done/$total", "", "");
    fwrite(STDERR, $write);
}

/**
 * Formats prevalence data.
 *
 * @param int $cases
 *   Number of cases.
 * @param array $examined
 *   Examined patients.
 */
function format_prevalence(int $cases, array $examined) {
  return round((($cases / count(array_unique($examined))) * 100), 3) . ' %';
}
