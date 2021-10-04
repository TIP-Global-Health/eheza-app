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

require_once __DIR__ . '/HedleyAdminTextTable.php';

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
$patients_per_quarter = [];
$patients_per_year = [];

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
      $grouped_by_year_key = date('Y', $measurement_timestamp);
      $grouped_by_quarter_key = format_quarter($measurement_timestamp);

      if (!isset($patients_per_month[$grouped_by_month_key])) {
        $patients_per_month[$grouped_by_month_key] = [];
      }
      $patients_per_month[$grouped_by_month_key][] = $child->nid;
      if (!isset($patients_per_year[$grouped_by_year_key])) {
        $patients_per_year[$grouped_by_year_key] = [];
      }
      $patients_per_year[$grouped_by_year_key][] = $child->nid;
      if (!isset($patients_per_quarter[$grouped_by_quarter_key])) {
        $patients_per_quarter[$grouped_by_quarter_key] = [];
      }
      $patients_per_quarter[$grouped_by_quarter_key][] = $child->nid;

      if (in_array($measurement->type, HEDLEY_ACTIVITY_HEIGHT_BUNDLES)) {
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
        $stunting = categorize_data($stunting, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $stunting_moderate, $stunting_severe, $child->nid);
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
      $underweight = categorize_data($underweight, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $underweight_moderate, $underweight_severe, $child->nid);

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_length');
      $wasting = categorize_data($wasting, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $wasting_moderate, $wasting_severe, $child->nid);
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

drush_print('## Prevalence by month (period prevalence)');
drush_print('### One visit or more');

$header = [
  'Prevalence by Month',
];
$data = $skeleton;

// Assemble the table for prevalence.
for ($i = 1; $i <= 12; $i++) {
  $current_month = strtotime('- ' . $i . 'months');
  $month_key = date('Y F', $current_month);
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
$text_table = new HedleyAdminTextTable($header);
$text_table->addData($data);

drush_print($text_table->render());

drush_print('## Incidence by month');
drush_print('### One visit or more');

$header = [
  'Incidence by Month',
];
$data = $skeleton;

for ($i = 1; $i <= 12; $i++) {
  $current_month = strtotime('- ' . $i . 'months');
  $previous_month = strtotime('- ' . ($i - 1) . 'months');
  $month_key = date('Y F', $current_month);
  $previous_month_key = date('Y F', $previous_month);
  $header[] = $month_key;
  $data[0][] = calculate_incidence($stunting, $month_key, $previous_month_key, 'moderate', $patients_per_month[$month_key]);
  $data[1][] = calculate_incidence($stunting, $month_key, $previous_month_key, 'severe', $patients_per_month[$month_key]);
  $data[2][] = calculate_incidence($underweight, $month_key, $previous_month_key, 'moderate', $patients_per_month[$month_key]);
  $data[3][] = calculate_incidence($underweight, $month_key, $previous_month_key, 'severe', $patients_per_month[$month_key]);
  $data[4][] = calculate_incidence($wasting, $month_key, $previous_month_key, 'moderate', $patients_per_month[$month_key]);
  $data[5][] = calculate_incidence($wasting, $month_key, $previous_month_key, 'severe', $patients_per_month[$month_key]);
}

$text_table = new HedleyAdminTextTable($header);
$text_table->addData($data);

drush_print($text_table->render());

drush_print('## Incidence by quarter');
drush_print('### One visit or more');

$header = [
  'Incidence by quarter',
];
$data = $skeleton;

for ($i = 1; $i <= 4; $i++) {
  $current_quarter = strtotime('- ' . ($i * 3) . 'months');
  $previous_quarter = strtotime('- ' . ($i * 6) . 'months');
  $quarter_key = format_quarter($current_quarter);
  $previous_quarter_key = date('Y F', $previous_quarter);
  $header[] = $quarter_key;
  $data[0][] = calculate_incidence($stunting, $quarter_key, $previous_quarter_key, 'moderate', $patients_per_quarter[$quarter_key]);
  $data[1][] = calculate_incidence($stunting, $quarter_key, $previous_quarter_key, 'severe', $patients_per_quarter[$quarter_key]);
  $data[2][] = calculate_incidence($underweight, $quarter_key, $previous_quarter_key, 'moderate', $patients_per_quarter[$quarter_key]);
  $data[3][] = calculate_incidence($underweight, $quarter_key, $previous_quarter_key, 'severe', $patients_per_quarter[$quarter_key]);
  $data[4][] = calculate_incidence($wasting, $quarter_key, $previous_quarter_key, 'moderate', $patients_per_quarter[$quarter_key]);
  $data[5][] = calculate_incidence($wasting, $quarter_key, $previous_quarter_key, 'severe', $patients_per_quarter[$quarter_key]);
}

$text_table = new HedleyAdminTextTable($header);
$text_table->addData($data);

drush_print($text_table->render());

drush_print('## Incidence by year');
drush_print('### One visit or more');

$header = [
  'Incidence by year',
];
$data = $skeleton;

for ($i = 1; $i <= 2; $i++) {
  $current_year = strtotime('- ' . $i . ' years');
  $previous_year = strtotime('- ' . ($i - 1) . ' years');
  $year_key = date('Y', $current_year);
  $previous_year_key = date('Y', $previous_year);
  $header[] = $year_key;
  $data[0][] = calculate_incidence($stunting, $year_key, $previous_year_key, 'moderate', $patients_per_year[$year_key]);
  $data[1][] = calculate_incidence($stunting, $year_key, $previous_year_key, 'severe', $patients_per_year[$year_key]);
  $data[2][] = calculate_incidence($underweight, $year_key, $previous_year_key, 'moderate', $patients_per_year[$year_key]);
  $data[3][] = calculate_incidence($underweight, $year_key, $previous_year_key, 'severe', $patients_per_year[$year_key]);
  $data[4][] = calculate_incidence($wasting, $year_key, $previous_year_key, 'moderate', $patients_per_year[$year_key]);
  $data[5][] = calculate_incidence($wasting, $year_key, $previous_year_key, 'severe', $patients_per_year[$year_key]);
}

$text_table = new HedleyAdminTextTable($header);
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
 * @param array $cases
 *   Number of cases.
 * @param array $examined
 *   Examined patients.
 */
function format_prevalence(array $cases, array $examined) {
  return round(((count($cases) / count(array_unique($examined))) * 100), 3) . ' %';
}

/**
 * Formats a Year quarter string.
 *
 * @param int $timestamp
 *   UNIX timestamp.
 *
 * @return string
 *   For example: 2021 Q1.
 */
function format_quarter(int $timestamp): string {
  $month = date("n", $timestamp);
  $quarter = ceil($month / 3);
  return date('Y', $timestamp) . ' Q' . $quarter;
}

/**
 * Categorize the data by keys.
 *
 * @param array $dataset
 *   Dataset for the given malnutrition type.
 * @param string $grouped_by_month_key
 *   Month key.
 * @param string $grouped_by_quarter_key
 *   Quarter key.
 * @param string $grouped_by_year_key
 *   Year key.
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
function categorize_data(array $dataset, string $grouped_by_month_key, string $grouped_by_quarter_key, string $grouped_by_year_key, int $moderate, int $severe, int $id): array {
  $category_ids = [
    'moderate' => [],
    'severe' => [],
  ];
  if (!isset($dataset[$grouped_by_month_key])) {
    $dataset[$grouped_by_month_key] = $category_ids;
  }
  if (!isset($dataset[$grouped_by_quarter_key])) {
    $dataset[$grouped_by_quarter_key] = $category_ids;
  }
  if (!isset($dataset[$grouped_by_year_key])) {
    $dataset[$grouped_by_year_key] = $category_ids;
  }
  if ($moderate) {
    $dataset[$grouped_by_month_key]['moderate'][] = $id;
    $dataset[$grouped_by_quarter_key]['moderate'][] = $id;
    $dataset[$grouped_by_year_key]['moderate'][] = $id;
  }
  if ($severe) {
    $dataset[$grouped_by_month_key]['severe'][] = $id;
    $dataset[$grouped_by_quarter_key]['severe'][] = $id;
    $dataset[$grouped_by_year_key]['severe'][] = $id;
  }

  return $dataset;
}

/**
 * Calculates incidence score.
 *
 * @param array $dataset
 *   Data about the measurement type.
 * @param string $current_period
 *   Group key, like 2021 May, 2021 or 2020 Q4.
 * @param string $previous_period
 *   Group key, like 2021 May, 2021 or 2020 Q4.
 * @param string $severity
 *   Either 'moderate' or 'severe'.
 * @param mixed $examined
 *   Total examined people in the period.
 */
function calculate_incidence(array $dataset, string $current_period, string $previous_period, string $severity, $examined) {
  $new_cases = 0;
  if (!isset($dataset[$current_period][$severity])) {
    return '-';
  }
  foreach ($dataset[$current_period][$severity] as $id) {
    if (!isset($dataset[$previous_period][$severity])) {
      $new_cases++;
      continue;
    }
    if (!in_array($id, $dataset[$previous_period][$severity])) {
      $new_cases++;
    }
  }

  if (empty($examined)) {
    return '-';
  }

  return round((($new_cases / count(array_unique($examined))) * 100), 3) . ' %';
}
