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

require_once __DIR__ . '/report_common.inc';

// Get the region.
$region = drush_get_option('region', FALSE);

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 100);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// Get optional district option.
$district = drush_get_option('district', NULL);

// Get optional district option.
$textual_date = drush_get_option('date', NULL);
$day_of_reporting = !empty($textual_date) ? strtotime($textual_date) : time();

$impacted = array_flip(db_query("SELECT entity_id FROM {person_impacted}")->fetchCol());
if (empty($impacted)) {
  drush_print('Execute ddev robo report:demographics first to generate data about impacted patients');
  exit(1);
}

$base_query = base_query_for_bundle('person');

$six_years_ago = date('Ymd', strtotime('-6 years', $day_of_reporting));
$base_query->fieldCondition('field_birth_date', 'value', $six_years_ago, '>');

if ($region) {
  $base_query->fieldCondition('field_district', 'value', $region, '=');
}

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no patients in DB.");
  if (!empty($district)) {
    drush_print("District: $district");
  }
  exit(1);
}

$region = ($region) ? $region : 'All Districts';

drush_print("$total children below 6 years located in $region.");

$processed = 0;

$stunting = $underweight = $wasting = [];
$stunting_impacted = $underweight_impacted = $wasting_impacted = [];

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

      if (in_array($measurement->type, HEDLEY_ACTIVITY_HEIGHT_BUNDLES)) {
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
        $stunting = categorize_data($stunting, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $stunting_moderate, $stunting_severe, $child->nid);
        if (isset($impacted[$child->nid])) {
          $stunting_impacted = categorize_data($stunting_impacted, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $stunting_moderate, $stunting_severe, $child->nid);
        }
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_age');
      $underweight = categorize_data($underweight, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $underweight_moderate, $underweight_severe, $child->nid);
      if (isset($impacted[$child->nid])) {
        $underweight_impacted = categorize_data($underweight_impacted, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $underweight_moderate, $underweight_severe, $child->nid);
      }

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($wrapper, 'field_zscore_length');
      $wasting = categorize_data($wasting, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $wasting_moderate, $wasting_severe, $child->nid);
      if (isset($impacted[$child->nid])) {
        $wasting_impacted = categorize_data($wasting_impacted, $grouped_by_month_key, $grouped_by_quarter_key, $grouped_by_year_key, $wasting_moderate, $wasting_severe, $child->nid);
      }
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
drush_print("# Nutrition report - " . $region . " - " . date('D/m/Y'));
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


print_prevalence_report($skeleton, $stunting, $underweight, $wasting);

drush_print('### More than two visits');
print_prevalence_report($skeleton, $stunting_impacted, $underweight_impacted, $wasting_impacted);

drush_print('## Incidence by month');

$month_calculate = function ($i) {
  return [strtotime('- ' . $i . 'months'), strtotime('- ' . ($i + 1) . 'months')];
};

$month_format = function ($val) {
  return date('Y F', $val);
};

drush_print('### One visit or more');
print_incidence_report($skeleton, $stunting, $underweight, $wasting, 'month', 12, $month_calculate, $month_format);
drush_print('### More than two visits');
print_incidence_report($skeleton, $stunting_impacted, $underweight_impacted, $wasting_impacted, 'month', 12, $month_calculate, $month_format);

drush_print('## Incidence by quarter');

$quarter_calculate = function ($i) {
  return [strtotime('- ' . ($i * 3) . ' months'), strtotime('- ' . (($i + 1) * 3) . ' months')];
};

$quarter_format = function ($val) {
  return format_quarter($val);
};

drush_print('### One visit or more');
print_incidence_report($skeleton, $stunting, $underweight, $wasting, 'quarter', 4, $quarter_calculate, $quarter_format);
drush_print('### More than two visits');
print_incidence_report($skeleton, $stunting_impacted, $underweight_impacted, $wasting_impacted, 'quarter', 4, $quarter_calculate, $quarter_format);

drush_print('## Incidence by year');

$year_calculate = function ($i) {
  return [strtotime('- ' . $i . ' years'), strtotime('- ' . ($i + 1) . ' years')];
};

$year_format = function ($val) {
  return date('Y', $val);
};

drush_print('### One visit or more');
print_incidence_report($skeleton, $stunting, $underweight, $wasting, 'year', 2, $year_calculate, $year_format);
drush_print('### More than two visits');
print_incidence_report($skeleton, $stunting_impacted, $underweight_impacted, $wasting_impacted, 'year', 2, $year_calculate, $year_format);

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
  $query = hedley_general_create_entity_field_query_excluding_deleted();
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
    'any' => [],
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
  elseif ($severe) {
    $dataset[$grouped_by_month_key]['severe'][] = $id;
    $dataset[$grouped_by_quarter_key]['severe'][] = $id;
    $dataset[$grouped_by_year_key]['severe'][] = $id;
  }
  $dataset[$grouped_by_month_key]['any'][] = $id;
  $dataset[$grouped_by_quarter_key]['any'][] = $id;
  $dataset[$grouped_by_year_key]['any'][] = $id;

  return $dataset;
}

/**
 * Calculates incidence score.
 *
 * @param array $dataset
 *   Data about the measurement type.
 * @param string $current_period
 *   Group key, like 2021 May, 2021 or 2020 Q4.
 * @param array $previous_periods
 *   Group key, like 2021 May, 2021 or 2020 Q4 or array of group keys.
 * @param string $severity
 *   Either 'moderate' or 'severe'.
 */
function calculate_incidence(array $dataset, string $current_period, array $previous_periods, string $severity) {
  $new_cases = 0;
  if (!isset($dataset[$current_period][$severity])) {
    return '-';
  }
  foreach ($dataset[$current_period][$severity] as $id) {

    $occurs_in_previous_periods = FALSE;
    $tested_in_previous_periods = FALSE;
    foreach ($previous_periods as $period) {
      if (!isset($dataset[$period][$severity])) {
        // We have no data at all, it makes no sense to deal with it.
        continue 2;
      }
      if (in_array($id, $dataset[$period]['any'])) {
        $tested_in_previous_periods = TRUE;
      }

      if (in_array($id, $dataset[$period][$severity])) {
        $occurs_in_previous_periods = TRUE;
        break;
      }
      // If a case became moderate from severe, it's not a new case, as
      // the situation improved, handling this exception.
      if ($severity === 'moderate' && in_array($id, $dataset[$period]['severe'])) {
        $occurs_in_previous_periods = TRUE;
        break;
      }

    }

    if (!$occurs_in_previous_periods && $tested_in_previous_periods) {
      $new_cases++;
    }
  }

  return round((($new_cases / count(array_unique($dataset[$current_period]['any']))) * 100), 3) . ' %';
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
      $data[0][] = format_prevalence($stunting[$month_key]['moderate'], $stunting[$month_key]['any']);
    }
    else {
      $data[0][] = '-';
    }
    if (isset($stunting[$month_key]['severe'])) {
      $data[1][] = format_prevalence($stunting[$month_key]['severe'], $stunting[$month_key]['any']);
    }
    else {
      $data[1][] = '-';
    }

    if (isset($underweight[$month_key]['moderate'])) {
      $data[2][] = format_prevalence($underweight[$month_key]['moderate'], $underweight[$month_key]['any']);
    }
    else {
      $data[2][] = '-';
    }
    if (isset($underweight[$month_key]['severe'])) {
      $data[3][] = format_prevalence($underweight[$month_key]['severe'], $underweight[$month_key]['any']);
    }
    else {
      $data[3][] = '-';
    }

    if (isset($wasting[$month_key]['moderate'])) {
      $data[4][] = format_prevalence($wasting[$month_key]['moderate'], $wasting[$month_key]['any']);
    }
    else {
      $data[4][] = '-';
    }
    if (isset($wasting[$month_key]['severe'])) {
      $data[5][] = format_prevalence($wasting[$month_key]['severe'], $wasting[$month_key]['any']);
    }
    else {
      $data[5][] = '-';
    }

  }
  $text_table = new HedleyAdminTextTable($header);
  $text_table->addData($data);

  drush_print($text_table->render());
}

/**
 * Prints prevalence report.
 *
 * @param array $skeleton
 *   Skeleton of the summary.
 * @param array $stunting
 *   Stunting data.
 * @param array $underweight
 *   Underweight data.
 * @param array $wasting
 *   Wasting data.
 * @param string $frequency_human_readable
 *   Either 'year', 'month', etc.
 * @param int $limit
 *   How many items we have in the date range.
 * @param callable $date_calculate
 *   Calculate the date range item.
 * @param callable $date_format
 *   Print the date in the needed format (2021 Q3 for example).
 */
function print_incidence_report(array $skeleton, array $stunting, array $underweight, array $wasting, string $frequency_human_readable, int $limit, callable $date_calculate, callable $date_format) {
  $header = [
    'Incidence by ' . $frequency_human_readable,
  ];
  $data = $skeleton;

  for ($i = 1; $i <= $limit; $i++) {
    [$current_period, $previous_period] = $date_calculate($i);
    $period_key = $date_format($current_period);
    $previous_period_key = $date_format($previous_period);
    $previous_period_keys = [];

    // The period for determining a new case for the monthly report is 3 months.
    // In the other reports we just compare with the previous period.
    // So we might have multiple items, but not always.
    $previous_period_keys[] = $previous_period_key;
    if ($frequency_human_readable === 'month') {
      // For months, we look 3 months back.
      // That is, if a a child is malnourished in July, but was not in April,
      // May, and June, it is considered a new case.
      [, $previous_period] = $date_calculate($i + 1);
      $previous_period_keys[] = $date_format($previous_period);
      [, $previous_period] = $date_calculate($i + 2);
      $previous_period_keys[] = $date_format($previous_period);
    }
    $header[] = $period_key;
    $data[0][] = calculate_incidence($stunting, $period_key, $previous_period_keys, 'moderate', $frequency_human_readable);
    $data[1][] = calculate_incidence($stunting, $period_key, $previous_period_keys, 'severe', $frequency_human_readable);
    $data[2][] = calculate_incidence($underweight, $period_key, $previous_period_keys, 'moderate', $frequency_human_readable);
    $data[3][] = calculate_incidence($underweight, $period_key, $previous_period_keys, 'severe', $frequency_human_readable);
    $data[4][] = calculate_incidence($wasting, $period_key, $previous_period_keys, 'moderate', $frequency_human_readable);
    $data[5][] = calculate_incidence($wasting, $period_key, $previous_period_keys, 'severe', $frequency_human_readable);
  }

  $text_table = new HedleyAdminTextTable($header);
  $text_table->addData($data);

  drush_print($text_table->render());
}
