<?php

/**
 * @file
 * Generates Malnutrition percentage out of Total encounters annual report.
 *
 * For all nutrition encounters at Gekenke district, between 2018 and 2021.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-grant-report-gekenke-annual.php.
 */

require_once __DIR__ . '/report_common.inc';

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

$catchment_area_hcs_ids = ['Ruli' => $ruli_hcs_ids, 'Nemba' => $nemba_hcs_ids];
$base_query_for_hc = base_query_for_bundle('person');
$six_years_ago = date('Ymd', strtotime('-6 years'));
$base_query_for_hc->fieldCondition('field_birth_date', 'value', $six_years_ago, '>');

foreach ($catchment_area_hcs_ids as $area => $catchment_area_hc_ids) {
  drush_print("Processing $area catchment area...");
  $stunting['fbf'] = $underweight['fbf'] = $wasting['fbf'] = [];
  $stunting['pmtct'] = $underweight['pmtct'] = $wasting['pmtct'] = [];

  foreach ($catchment_area_hc_ids as $catchment_area_hc_id) {
    $base_query = clone $base_query_for_hc;
    $base_query->fieldCondition('field_health_center', 'target_id', $catchment_area_hc_id);

    $count_query = clone $base_query;
    $total = $count_query->count()->execute();

    if ($total == 0) {
      drush_print("There are no patients in DB for health center with ID $catchment_area_hc_id.");
      continue;
    }

    drush_print("$total children with age below 6 years located for health center with ID $catchment_area_hc_id.");

    $fbf_clinics = hedley_health_center_get_clinics_by_health_center($catchment_area_hc_id, 'fbf');
    $pmtct_clinics = hedley_health_center_get_clinics_by_health_center($catchment_area_hc_id, 'pmtct');

    $session_types = [
      'fbf' => hedley_stats_get_sessions_for_clinics($fbf_clinics),
      'pmtct' => hedley_stats_get_sessions_for_clinics($pmtct_clinics),
    ];

    foreach ($session_types as $type => $sessions) {
      $nid = 0;
      $processed = 0;

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

          $bundles = [HEDLEY_ACTIVITY_HEIGHT_CONTENT_TYPE, HEDLEY_ACTIVITY_WEIGHT_CONTENT_TYPE];
          $measurements_ids = hedley_general_get_person_measurements($child->nid, $bundles);

          // If child got no measurements, move on to next child.
          if (empty($measurements_ids)) {
            continue;
          }

          $measurements = node_load_multiple($measurements_ids);
          foreach ($measurements as $measurement) {
            $wrapper = entity_metadata_wrapper('node', $measurement);

            if (!in_array($wrapper->field_session->getIdentifier(), $sessions)) {
              // Measurement sessions is not of session type we're looking for.
              continue;
            }

            $measurement_timestamp = $wrapper->field_date_measured->value();
            $grouped_by_year_key = date('Y', $measurement_timestamp);

            if ($measurement->type == HEDLEY_ACTIVITY_HEIGHT_CONTENT_TYPE) {
              $score = $wrapper->field_zscore_age->value();
              if (empty($score)) {
                continue;
              }

              [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($score);
              $stunting[$type] = categorize_data($stunting[$type], $grouped_by_year_key, $stunting_moderate, $stunting_severe);
              continue;
            }

            // We know it's HEDLEY_ACTIVITY_WEIGHT_CONTENT_TYPE.
            $score = $wrapper->field_zscore_age->value();
            if (!empty($score)) {
              [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($score);
              $underweight[$type] = categorize_data($underweight[$type], $grouped_by_year_key, $underweight_moderate, $underweight_severe);
            }

            $score = $wrapper->field_zscore_length->value();
            if (!empty($score)) {
              [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($score);
              $wasting[$type] = categorize_data($wasting[$type], $grouped_by_year_key, $wasting_moderate, $wasting_severe);
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
        drush_print("$processed children processed.");
      }
    }

    drush_print('Done!');
  }

  drush_print("# Nutrition report for $area - " . date('D/m/Y'));
  drush_print('');

  drush_print('## Malnutrition from total encounters by year, FBF groups');
  print_report($stunting['fbf'], $underweight['fbf'], $wasting['fbf']);
  drush_print('');
  drush_print('');
  drush_print('## Malnutrition from total encounters by year, PMTCT groups');
  print_report($stunting['pmtct'], $underweight['pmtct'], $wasting['pmtct']);
}

/**
 * Classifies measurement by its malnutrition indicator (severe / moderate).
 */
function classify_by_malnutrition_type($z_score): array {
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
 * Formats percentage.
 *
 * @param int $cases
 *   Number of cases - numerator.
 * @param int $examined
 *   Examined patients - denominator.
 */
function format_percentage(int $cases, int $examined): string {
  return round((($cases / $examined) * 100), 3) . ' %';
}

/**
 * Categorize the data by keys.
 *
 * @param array $dataset
 *   Dataset for the given malnutrition type.
 * @param string $grouped_by_year_key
 *   Year key.
 * @param int $moderate
 *   Indicated moderate.
 * @param int $severe
 *   Indicated severe.
 *
 * @return array
 *   Updated dataset.
 */
function categorize_data(array $dataset, string $grouped_by_year_key, int $moderate, int $severe): array {
  $category_ids = [
    'moderate' => 0,
    'severe' => 0,
    'any' => 0,
  ];
  if (!isset($dataset[$grouped_by_year_key])) {
    $dataset[$grouped_by_year_key] = $category_ids;
  }
  if ($moderate) {
    $dataset[$grouped_by_year_key]['moderate'] += $moderate;
  }
  elseif ($severe) {
    $dataset[$grouped_by_year_key]['severe'] += $severe;
  }

  $dataset[$grouped_by_year_key]['any']++;

  return $dataset;
}

/**
 * Prints percentage of malnutrition cases from total encounters.
 *
 * @param array $stunting
 *   Stunting data.
 * @param array $underweight
 *   Underweight data.
 * @param array $wasting
 *   Wasting data.
 */
function print_report(array $stunting, array $underweight, array $wasting) {
  // Assemble the table for prevalence.
  for ($i = 1; $i <= 4; $i++) {
    $header = ['Malnutrition from total encounters by Year'];

    $current_year = strtotime('- ' . $i . 'years');
    $year_key = date('Y', $current_year);
    $header[] = $year_key;

    if (isset($stunting[$year_key]['moderate'])) {
      $data[$i][0] = ['Stunting Moderate', format_percentage($stunting[$year_key]['moderate'], $stunting[$year_key]['any'])];
    } else {
      $data[$i][0] = ['Stunting Moderate', '-'];
    }
    if (isset($stunting[$year_key]['severe'])) {
      $data[$i][1] = ['Stunting Severe', format_percentage($stunting[$year_key]['severe'], $stunting[$year_key]['any'])];
    } else {
      $data[$i][1] = ['Stunting Severe', '-'];
    }

    if (isset($underweight[$year_key]['moderate'])) {
      $data[$i][2] = ['Underweight Moderate', format_percentage($underweight[$year_key]['moderate'], $underweight[$year_key]['any'])];
    } else {
      $data[$i][2] = ['Underweight Moderate', '-'];
    }
    if (isset($underweight[$year_key]['severe'])) {
      $data[$i][3] = ['Underweight Severe', format_percentage($underweight[$year_key]['severe'], $underweight[$year_key]['any'])];
    } else {
      $data[$i][3] = ['Underweight Severe', '-'];
    }

    if (isset($wasting[$year_key]['moderate'])) {
      $data[$i][4] = ['Wasting Moderate', format_percentage($wasting[$year_key]['moderate'], $wasting[$year_key]['any'])];
    } else {
      $data[$i][4] = ['Wasting Moderate', '-'];
    }
    if (isset($wasting[$year_key]['severe'])) {
      $data[$i][5] = ['Wasting Severe', format_percentage($wasting[$year_key]['severe'], $wasting[$year_key]['any'])];
    } else {
      $data[$i][5] = ['Wasting Severe', '-'];
    }

    $text_table = new HedleyAdminTextTable($header);
    $text_table->addData($data[$i]);
    drush_print($text_table->render());
  }
}
