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

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

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

drush_print("$total children with age bellow 6 years located.");

$tuple = [
  'moderate' => 0,
  'severe' => 0,
  'any' => 0,
];

$now = time();
$one_year_ago = strtotime('-1 year');

$total_stunting = $total_underweight = $total_wasting = [
  'all' => $tuple,
  'mt1' => $tuple,
  'mt2' => $tuple,
];

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

    $dates_rage = [
      'from' => $one_year_ago,
      'to' => $now,
    ];

    // If child is older than 5 years old.
    if (strtotime('+5 years', $birth_date) < $now) {
      $dates_rage['to'] = strtotime('+5 years', $birth_date);
    }

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
        [$stunting_moderate, $stunting_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_age', $dates_rage);
        $stunting['moderate'] += $stunting_moderate;
        $stunting['severe'] += $stunting_severe;
        $stunting['any']++;
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_age', $dates_rage);
      $underweight['moderate'] += $underweight_moderate;
      $underweight['severe'] += $underweight_severe;
      $underweight['any']++;

      [$wasting_moderate, $wasting_severe] = classify_by_malnutrition_type($measurement, 'field_zscore_length', $dates_rage);
      $wasting['moderate'] += $wasting_moderate;
      $wasting['severe'] += $wasting_severe;
      $wasting['any']++;
    }

    $total_stunting['all']['moderate'] += $stunting['moderate'];
    $total_stunting['all']['severe'] += $stunting['severe'];
    $total_stunting['all']['any'] += $stunting['any'];

    if ($stunting['moderate'] + $stunting['severe'] > 1) {
      $total_stunting['mt1']['moderate'] += $stunting['moderate'];
      $total_stunting['mt1']['severe'] += $stunting['severe'];
      $total_stunting['mt1']['any'] += $stunting['any'];
    }

    if ($stunting['moderate'] + $stunting['severe'] > 2) {
      $total_stunting['mt2']['moderate'] += $stunting['moderate'];
      $total_stunting['mt2']['severe'] += $stunting['severe'];
      $total_stunting['mt2']['any'] += $stunting['any'];
    }

    $total_underweight['all']['moderate'] += $underweight['moderate'];
    $total_underweight['all']['severe'] += $underweight['severe'];
    $total_underweight['all']['any'] += $underweight['any'];

    if ($underweight['moderate'] + $underweight['severe'] > 1) {
      $total_underweight['mt1']['moderate'] += $underweight['moderate'];
      $total_underweight['mt1']['severe'] += $underweight['severe'];
      $total_underweight['mt1']['any'] += $underweight['any'];
    }

    if ($underweight['moderate'] + $underweight['severe'] > 2) {
      $total_underweight['mt2']['moderate'] += $underweight['moderate'];
      $total_underweight['mt2']['severe'] += $underweight['severe'];
      $total_underweight['mt2']['any'] += $underweight['any'];
    }

    $total_wasting['all']['moderate'] += $wasting['moderate'];
    $total_wasting['all']['severe'] += $wasting['severe'];
    $total_wasting['all']['any'] += $wasting['any'];

    if ($wasting['moderate'] + $wasting['severe'] > 1) {
      $total_wasting['mt1']['moderate'] += $wasting['moderate'];
      $total_wasting['mt1']['severe'] += $wasting['severe'];
      $total_wasting['mt1']['any'] += $wasting['any'];
    }

    if ($wasting['moderate'] + $wasting['severe'] > 2) {
      $total_wasting['mt2']['moderate'] += $wasting['moderate'];
      $total_wasting['mt2']['severe'] += $wasting['severe'];
      $total_wasting['mt2']['any'] += $wasting['any'];
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

drush_print('Done!');

drush_print('');
drush_print('Malnutrition report for past year:');
drush_print('');
drush_print('');


drush_print('Stunting Moderate (one visit, or more): ' . $total_stunting['all']['moderate']);
drush_print('Stunting Severe   (one visit, or more): ' . $total_stunting['all']['severe']);
drush_print('Total encounters  (one visit, or more): ' . $total_stunting['all']['any']);

drush_print('');

drush_print('Stunting Moderate (more than one visit): ' . $total_stunting['mt1']['moderate']);
drush_print('Stunting Severe   (more than one visit): ' . $total_stunting['mt1']['severe']);
drush_print('Total encounters  (more than one visit): ' . $total_stunting['mt1']['any']);

drush_print('');

drush_print('Stunting Moderate (more than two visit): ' . $total_stunting['mt2']['moderate']);
drush_print('Stunting Severe   (more than two visit): ' . $total_stunting['mt2']['severe']);
drush_print('Total encounters  (more than two visit): ' . $total_stunting['mt2']['any']);

drush_print('');
drush_print('');

drush_print('Underweight Moderate (one visit, or more): ' . $total_underweight['all']['moderate']);
drush_print('Underweight Severe   (one visit, or more): ' . $total_underweight['all']['severe']);
drush_print('Total encounters     (one visit, or more): ' . $total_underweight['all']['any']);

drush_print('');

drush_print('Underweight Moderate (more than one visit): ' . $total_underweight['mt1']['moderate']);
drush_print('Underweight Severe   (more than one visit): ' . $total_underweight['mt1']['severe']);
drush_print('Total encounters     (more than one visit): ' . $total_underweight['mt1']['any']);

drush_print('');

drush_print('Underweight Moderate (more than two visit): ' . $total_underweight['mt2']['moderate']);
drush_print('Underweight Severe   (more than two visit): ' . $total_underweight['mt2']['severe']);
drush_print('Total encounters     (more than two visit): ' . $total_underweight['mt2']['any']);

drush_print('');
drush_print('');

drush_print('Wasting Moderate (one visit, or more): ' . $total_wasting['all']['moderate']);
drush_print('Wasting Severe   (one visit, or more): ' . $total_wasting['all']['severe']);
drush_print('Total encounters (one visit, or more): ' . $total_wasting['all']['any']);

drush_print('');

drush_print('Wasting Moderate (more than one visit): ' . $total_wasting['mt1']['moderate']);
drush_print('Wasting Severe   (more than one visit): ' . $total_wasting['mt1']['severe']);
drush_print('Total encounters (more than one visit): ' . $total_wasting['mt1']['any']);

drush_print('');

drush_print('Wasting Moderate (more than two visit): ' . $total_wasting['mt2']['moderate']);
drush_print('Wasting Severe   (more than two visit): ' . $total_wasting['mt2']['severe']);
drush_print('Total encounters (more than two visit): ' . $total_wasting['mt2']['any']);

/**
 * Classifies measurement by it's malnutrition indicator (severe / moderate).
 *
 * If measurement does not indicate malnutrition, 0 values are returned.
 */
function classify_by_malnutrition_type($node, $field, array $dates_rage) {
  $wrapper = entity_metadata_wrapper('node', $node);
  $z_score = $wrapper->{$field}->value();

  if (empty($z_score)) {
    return [0, 0];
  }

  $date_measured = $wrapper->field_date_measured->value();
  if ($date_measured < $dates_rage['from'] || $date_measured > $dates_rage['to']) {
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
