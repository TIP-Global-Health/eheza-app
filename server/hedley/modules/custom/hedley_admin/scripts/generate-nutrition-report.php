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
$total = 600;
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

    $stunting = $underweight = $wasting = $tuple;
    $measurements = node_load_multiple($measurements_ids);
    foreach ($measurements as $measurement) {
      if (in_array($measurement->type, HEDLEY_ACTIVITY_HEIGHT_BUNDLES)) {
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_age');
        $stunting['moderate'] += $stunting_moderate;
        $stunting['severe'] += $stunting_severe;
        $stunting['any']++;
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_age');
      $underweight['moderate'] += $underweight_moderate;
      $underweight['severe'] += $underweight_severe;
      $underweight['any']++;

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_length');
      $wasting['moderate'] += $wasting_moderate;
      $wasting['severe'] += $wasting_severe;
      $wasting['any']++;
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
for ($i = 0; $i < 6; $i++) {
  $current_month = strtotime('- ' . $i . 'months');
  $header[] = date('Y F',  $current_month);
}
$text_table = new TextTable($header);
$text_table->addData($data);

drush_print($text_table->render());

/**
 * Classifies measurement by it's malnutrition indicator (severe / moderate).
 *
 * If measurement does not indicate malnutrition, 0 values are returned.
 */
function classify_by_malnutrition_type($node, $field) {
  $wrapper = entity_metadata_wrapper('node', $node);
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
function base_query_for_bundle($bundle) {
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
function progress_bar($done, $total) {
    $percentage = floor(($done / $total) * 100);
    $left = 100 - $percentage;
    $write = sprintf("\033[0G\033[2K[%'={$percentage}s>%-{$left}s] - $percentage%% - $done/$total", "", "");
    fwrite(STDERR, $write);
}
