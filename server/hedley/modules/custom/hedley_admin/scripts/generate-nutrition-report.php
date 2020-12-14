<?php

/**
 * @file
 * Generates 'Nutrition' report.
 *
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
  wlog("There are no patients in DB.");
  exit;
}

wlog("$total children with age bellow 6 years located.");

$tuple = [
  'moderate' => 0,
  'severe' => 0,
];

$now = time();
$one_year_ago = strtotime('-1 year');

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
      drush_print("CHILD $child->nid is over 5 years");
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
        [$stunting_moderate, $stunting_severe] = resolve_indicator_tuple($measurement, 'field_zscore_age', $dates_rage);
        $stunting['moderate'] += $stunting_moderate;
        $stunting['severe'] += $stunting_severe;
        continue;
      }

      // We know it's one of HEDLEY_ACTIVITY_WEIGHT_BUNDLES.
      [$underweight_moderate, $underweight_severe] = resolve_indicator_tuple($measurement, 'field_zscore_age', $dates_rage);
      $underweight['moderate'] += $underweight_moderate;
      $underweight['severe'] += $underweight_severe;

      [$wasting_moderate, $wasting_severe] = resolve_indicator_tuple($measurement, 'field_zscore_length', $dates_rage);
      $wasting['wasting'] += $wasting_moderate;
      $wasting['wasting'] += $wasting_severe;
    }

//    drush_print("Person $child->nid:");
//    drush_print('sm: ' . $stunting['moderate']);
//    drush_print('ss: ' . $stunting['severe']);
//    drush_print('um: ' . $underweight['moderate']);
//    drush_print('us: ' . $underweight['severe']);
//    drush_print('wm: ' . $wasting['moderate']);
//    drush_print('ws: ' . $wasting['severe']);
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  wlog("$processed children processed.");
}

wlog('Done!');

function resolve_indicator_tuple($node, $field, array $dates_rage) {
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

//drush_print('');
//drush_print('Groups report:');
//drush_print('PMTCT encounters:    ' . $total_encounters['pmtct']);
//drush_print('FBF encounters:      ' . $total_encounters['fbf']);
//drush_print('SORWATHE encounters: ' . $total_encounters['sorwathe']);
//drush_print('CHW encounters:      ' . $total_encounters['chw']);
//drush_print('ACHI encounters:     ' . $total_encounters['achi']);
//drush_print('');
//drush_print('Total encounters:    ' . $total_encounters['all']);
//drush_print('');
//
//drush_print('Registered patients report:');
//$male = $all_patients['lt1m']['male'];
//$female = $all_patients['lt1m']['female'];
//drush_print("* 0 - 1 month: Male - $male, Female - $female");
//$male = $all_patients['lt2y']['male'];
//$female = $all_patients['lt2y']['female'];
//drush_print("* 1 month - 2 years: Male - $male, Female - $female");
//$male = $all_patients['lt5y']['male'];
//$female = $all_patients['lt5y']['female'];
//drush_print("* 2 years - 5 years: Male - $male, Female - $female");
//$male = $all_patients['lt10y']['male'];
//$female = $all_patients['lt10y']['female'];
//drush_print("* 5 years - 10 years: Male - $male, Female - $female");
//$male = $all_patients['lt20y']['male'];
//$female = $all_patients['lt20y']['female'];
//drush_print("* 10 years - 20 years: Male - $male, Female - $female");
//$male = $all_patients['lt50y']['male'];
//$female = $all_patients['lt50y']['female'];
//drush_print("* 20 years - 50 years: Male - $male, Female - $female");
//$male = $all_patients['mt50y']['male'];
//$female = $all_patients['mt50y']['female'];
//drush_print("* Older than 50 years: Male - $male, Female - $female");
//
//drush_print('');
//
//drush_print('Impacted patients report:');
//$male = $impacted_patients['lt1m']['male'];
//$female = $impacted_patients['lt1m']['female'];
//drush_print("* 0 - 1 month: Male - $male, Female - $female");
//$male = $impacted_patients['lt2y']['male'];
//$female = $impacted_patients['lt2y']['female'];
//drush_print("* 1 month - 2 years: Male - $male, Female - $female");
//$male = $impacted_patients['lt5y']['male'];
//$female = $impacted_patients['lt5y']['female'];
//drush_print("* 2 years - 5 years: Male - $male, Female - $female");
//$male = $impacted_patients['lt10y']['male'];
//$female = $impacted_patients['lt10y']['female'];
//drush_print("* 5 years - 10 years: Male - $male, Female - $female");
//$male = $impacted_patients['lt20y']['male'];
//$female = $impacted_patients['lt20y']['female'];
//drush_print("* 10 years - 20 years: Male - $male, Female - $female");
//$male = $impacted_patients['lt50y']['male'];
//$female = $impacted_patients['lt50y']['female'];
//drush_print("* 20 years - 50 years: Male - $male, Female - $female");
//$male = $impacted_patients['mt50y']['male'];
//$female = $impacted_patients['mt50y']['female'];
//drush_print("* Older than 50 years: Male - $male, Female - $female");

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
 * Prints log based on verbosity option.
 */
function wlog($message) {
  // Get the option that will determine if output should be verbose or not.
  $verbose = drush_get_option('verbose', FALSE);

  if ($verbose !== FALSE) {
    drush_print($message);
  }
}
