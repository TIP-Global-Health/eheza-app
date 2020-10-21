<?php

/**
 * @file
 * Generate 'Impacted Patients' report.
 * Patient is considered impacted if attended at least 2 encounters.
 * Code criteria for considering patient as imported:
 * A person that has at least 2 measurements, taken with interval of one week, or more.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-impacted-patients.php.
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

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'person')
  ->propertyCondition('status', NODE_PUBLISHED);

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no patients in DB.");
  exit;
}

drush_print("$total patients located.");

$impacted_patients = [
  'lt1m' => [
    'male' => 0,
    'female' => 0
  ],
  'lt2y' => [
    'male' => 0,
    'female' => 0
  ],
  'lt5y' => [
    'male' => 0,
    'female' => 0
  ],
  'lt10y' => [
    'male' => 0,
    'female' => 0
  ],
  'lt20y' => [
    'male' => 0,
    'female' => 0
  ],
  'lt50y' => [
    'male' => 0,
    'female' => 0
  ],
  'mt50y' => [
    'male' => 0,
    'female' => 0
  ],
];
$processed = 0;
$measurement_types = hedley_admin_get_measurement_types();

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
  foreach ($ids as $id) {
    $measurements_ids = hedley_admin_get_person_measurements($id, $measurement_types);
    // If patient got no measurements, move on to next patient.
    if (empty($measurements_ids)) {
      continue;
    }

    // When there are more than 50 measurements, we classify patient as
    // affected, without further checks, as this amount of measurements
    // can't belong to single encounter.
    if (count($measurements_ids) > 50) {
      list($age, $gender) = classify_by_age_and_gender($id);
      $impacted_patients[$age][$gender]++;
      continue;
    }

    $measurements = node_load_multiple($measurements_ids);
    // Creation timestamp of first measurement.
    $first_timestamp = array_shift($measurements)->created;
    foreach ($measurements as $measurement) {
      // When we find 2 measurements that are at least taken week apart,
      // we classify the patient as impacted, as these 2 measurements
      // were taken in different encounters.
      if (abs($first_timestamp - $measurement->created) > 7*24*3600) {
        list($age, $gender) = classify_by_age_and_gender($id);
        $impacted_patients[$age][$gender]++;
        break;
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
  drush_print("$processed persons processed.");
}

drush_print('Done!');
drush_print('Impacted patients report:');
$male = $impacted_patients['lt1m']['male'];
$female = $impacted_patients['lt1m']['female'];
drush_print("* 0 - 1 month: Male - $male, Female - $female");
$male = $impacted_patients['lt2y']['male'];
$female = $impacted_patients['lt2y']['female'];
drush_print("* 1 month - 2 years: Male - $male, Female - $female");
$male = $impacted_patients['lt5y']['male'];
$female = $impacted_patients['lt5y']['female'];
drush_print("* 2 years - 5 years: Male - $male, Female - $female");
$male = $impacted_patients['lt10y']['male'];
$female = $impacted_patients['lt10y']['female'];
drush_print("* 5 years - 10 years: Male - $male, Female - $female");
$male = $impacted_patients['lt20y']['male'];
$female = $impacted_patients['lt20y']['female'];
drush_print("* 10 years - 20 years: Male - $male, Female - $female");
$male = $impacted_patients['lt50y']['male'];
$female = $impacted_patients['lt50y']['female'];
drush_print("* 20 years - 50 years: Male - $male, Female - $female");
$male = $impacted_patients['mt50y']['male'];
$female = $impacted_patients['mt50y']['female'];
drush_print("* Older than 50 years: Male - $male, Female - $female");

/**
 * Resolve age indication, according to person's birth date.
 */
function classify_by_age_and_gender($person_id) {
  $wrapper = entity_metadata_wrapper('node', $person_id);
  $gender = $wrapper->field_gender->value();

  $birth_date = $wrapper->field_birth_date->value();

  if ($birth_date > strtotime('-1 month')) {
    $age = 'lt1m';
  }
  elseif ($birth_date > strtotime('-2 year')) {
    $age = 'lt2y';
  }
  elseif ($birth_date > strtotime('-5 year')) {
    $age = 'lt5y';
  }
  elseif ($birth_date > strtotime('-10 year')) {
    $age = 'lt10y';
  }
  elseif ($birth_date > strtotime('-20 year')) {
    $age = 'lt20y';
  }
  elseif ($birth_date > strtotime('-50 year')) {
    $age = 'lt50y';
  }
  else {
    $age = 'mt50y';
  }

  return [$age, $gender];
}
