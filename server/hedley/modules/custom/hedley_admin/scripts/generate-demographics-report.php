<?php

/**
 * @file
 * Generate 'Demographics' report.
 *
 * Data for each individual encounter type:
 *   - Number of completed encounters.
 *   - Number of unique patients.
 *
 * Patients data:
 *
 * - Registered patients by age and gender.
 * - Impacted patients by age and gender.
 *
 * Note: Patient is considered impacted if attended at least 2 encounters.
 * Code criteria for considering patient as imported:
 * A person that has at least 2 measurements, taken with interval of one week,
 * or more.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
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

$total_participation = count_total_group_attendances();
drush_print("Total group participation: $total_participation");

generate_individual_encounter_report('prenatal', $batch, $memory_limit);
generate_individual_encounter_report('nutrition', $batch, $memory_limit);
generate_individual_encounter_report('acute_illness', $batch, $memory_limit);

$base_query = base_query_for_bundle('person');
$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  wlog("There are no patients in DB.");
  exit;
}

wlog("$total patients located.");

$all_patients = $impacted_patients = [
  'lt1m' => [
    'male' => 0,
    'female' => 0,
  ],
  'lt2y' => [
    'male' => 0,
    'female' => 0,
  ],
  'lt5y' => [
    'male' => 0,
    'female' => 0,
  ],
  'lt10y' => [
    'male' => 0,
    'female' => 0,
  ],
  'lt20y' => [
    'male' => 0,
    'female' => 0,
  ],
  'lt50y' => [
    'male' => 0,
    'female' => 0,
  ],
  'mt50y' => [
    'male' => 0,
    'female' => 0,
  ],
];
$skipped = $skipped_with_measurements = [
  'age' => [],
  'gender' => [],
];

$processed = 0;
$total_encounters = [
  'all' => 0,
  'pmtct' => 0,
  'fbf' => 0,
  'sorwathe' => 0,
  'chw' => 0,
];
$measurement_types = hedley_general_get_measurement_types();

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
    list($age, $gender) = classify_by_age_and_gender($id);
    $measurements_ids = hedley_general_get_person_measurements($id, $measurement_types);

    if (!$age) {
      if (count($measurements_ids) > 0) {
        $skipped_with_measurements['age'][] = $id;
      }
      else {
        $skipped['age'][] = $id;
      }
      // Person classification failed. Skipping.
      continue;
    }

    if (!$gender) {
      if (count($measurements_ids) > 0) {
        $skipped_with_measurements['gender'][] = $id;
      }
      else {
        $skipped['gender'][] = $id;
      }
      // Person classification failed. Skipping.
      continue;
    }

    $all_patients[$age][$gender]++;

    // If patient got no measurements, move on to next patient.
    if (empty($measurements_ids)) {
      continue;
    }

    $measurements = node_load_multiple($measurements_ids);
    list($pmtct, $fbf, $sorwathe, $chw, $nutrition, $prenatal, $acute_illness) = count_encounters($measurements);
    $person_total_encounters = $pmtct + $fbf + $sorwathe + $chw + $nutrition + $prenatal + $acute_illness;

    if ($person_total_encounters > 1) {
      $impacted_patients[$age][$gender]++;
    }

    $total_encounters['all'] += $person_total_encounters;
    $total_encounters['pmtct'] += $pmtct;
    $total_encounters['fbf'] += $fbf;
    $total_encounters['sorwathe'] += $sorwathe;
    $total_encounters['chw'] += $chw;
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  wlog("$processed persons processed.");
}

wlog('Done!');

drush_print('');
drush_print('PMTCT encounters:    ' . $total_encounters['pmtct']);
drush_print('FBF encounters:      ' . $total_encounters['fbf']);
drush_print('SORWATHE encounters: ' . $total_encounters['sorwathe']);
drush_print('CHW encounters:      ' . $total_encounters['chw']);
drush_print('');
drush_print('Total encounters:    ' . $total_encounters['all']);
drush_print('');

drush_print('Registered patients report:');
$male = $all_patients['lt1m']['male'];
$female = $all_patients['lt1m']['female'];
drush_print("* 0 - 1 month: Male - $male, Female - $female");
$male = $all_patients['lt2y']['male'];
$female = $all_patients['lt2y']['female'];
drush_print("* 1 month - 2 years: Male - $male, Female - $female");
$male = $all_patients['lt5y']['male'];
$female = $all_patients['lt5y']['female'];
drush_print("* 2 years - 5 years: Male - $male, Female - $female");
$male = $all_patients['lt10y']['male'];
$female = $all_patients['lt10y']['female'];
drush_print("* 5 years - 10 years: Male - $male, Female - $female");
$male = $all_patients['lt20y']['male'];
$female = $all_patients['lt20y']['female'];
drush_print("* 10 years - 20 years: Male - $male, Female - $female");
$male = $all_patients['lt50y']['male'];
$female = $all_patients['lt50y']['female'];
drush_print("* 20 years - 50 years: Male - $male, Female - $female");
$male = $all_patients['mt50y']['male'];
$female = $all_patients['mt50y']['female'];
drush_print("* Older than 50 years: Male - $male, Female - $female");

drush_print('');

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

drush_print('');
$count = count($skipped['age']);
drush_print("Skipped due to missing AGE: $count");
$count = count($skipped_with_measurements['age']);
drush_print("Skipped due to missing AGE with measurements: $count");
$count = count($skipped['gender']);
drush_print("Skipped due to missing GENDER: $count");
$count = count($skipped_with_measurements['gender']);
drush_print("Skipped due to missing GENDER with measurements: $count");

/**
 * Resolve age indication, according to person's birth date.
 */
function classify_by_age_and_gender($person_id) {
  $wrapper = entity_metadata_wrapper('node', $person_id);

  $gender = $wrapper->field_gender->value();
  if (empty($gender)) {
    $gender = FALSE;
  }

  $birth_date = $wrapper->field_birth_date->value();
  if (empty($birth_date)) {
    $age = FALSE;
  }
  elseif ($birth_date > strtotime('-1 month')) {
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

function count_encounters($measurements) {
  $group_encounters = $nutrition_encounters = $prenatal_encounters = $acute_illness_encounters = [];

  foreach ($measurements as $measurement) {
    $wrapper = entity_metadata_wrapper('node', $measurement);

    if ($wrapper->__isset('field_session')) {
      $encounter = $wrapper->field_session->getIdentifier();
      if (!empty($encounter) && !in_array($encounter, $group_encounters)) {
        $group_encounters[] = $encounter;
      }
      continue;
    }

    if ($wrapper->__isset('field_nutrition_encounter')) {
      $encounter = $wrapper->field_nutrition_encounter->getIdentifier();
      if (!empty($encounter) && !in_array($encounter, $nutrition_encounters)) {
        $nutrition_encounters[] = $encounter;
      }
      continue;
    }

    if ($wrapper->__isset('field_prenatal_encounter')) {
      $encounter = $wrapper->field_prenatal_encounter->getIdentifier();
      if (!empty($encounter) && !in_array($encounter, $prenatal_encounters)) {
        $prenatal_encounters[] = $encounter;
      }
      continue;
    }

    if ($wrapper->__isset('field_acute_illness_encounter')) {
      $encounter = $wrapper->field_acute_illness_encounter->getIdentifier();
      if (!empty($encounter) && !in_array($encounter, $acute_illness_encounters)) {
        $acute_illness_encounters[] = $encounter;
      }
    }
  }

  $pmtct_encounters = $fbf_encounters = $sorwathe_encounters = $chw_encounters = 0;
  foreach ($group_encounters as $encounter) {
    $wrapper = entity_metadata_wrapper('node', $encounter);
    $clinic = $wrapper->field_clinic->getIdentifier();

    if (empty($clinic)) {
      continue;
    }

    $wrapper = entity_metadata_wrapper('node', $clinic);
    $clinic_type = $wrapper->field_group_type->value();

    switch ($clinic_type) {
      case 'pmtct':
        $pmtct_encounters++;
        break;

      case 'fbf':
        $fbf_encounters++;
        break;

      case 'sorwathe':
        $sorwathe_encounters++;
        break;

      case 'chw':
        $chw_encounters++;
    }
  }

  return [
    $pmtct_encounters,
    $fbf_encounters,
    $sorwathe_encounters,
    $chw_encounters,
    count($nutrition_encounters),
    count($prenatal_encounters),
    count($acute_illness_encounters),
  ];
}

/**
 * Generates a report for certain type of individual encounter.
 *
 * Reported info:
 *   - Number of completed encounters.
 *   - Number of unique patients.
 */
function generate_individual_encounter_report($encounter_type, $batch, $memory_limit) {
  $types = [
    'acute_illness',
    'nutrition',
    'prenatal',
  ];

  if (!in_array($encounter_type, $types)) {
    drush_print("Invalid encounter type - $encounter_type");
    return;
  }

  $encounter_bundle = "{$encounter_type}_encounter";
  $nid = 0;

  $base_query = base_query_for_bundle($encounter_bundle);
  $count_query = clone $base_query;
  $count_query->propertyCondition('nid', $nid, '>');
  $total = $count_query->count()->execute();

  if ($total == 0) {
    wlog("There are no encounters of type $encounter_type in DB.");
    return;
  }

  wlog("$total encounters of type $encounter_type located.");

  $processed = 0;
  $encounters_with_measurements = 0;
  $encounter_participants = [];
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
      $encounter_measurements = hedley_general_get_individual_encounter_measurements($id, $encounter_type);
      if (count($encounter_measurements) == 0) {
        // No measurements were taken - disregard the encounter.
        continue;
      }
      $encounters_with_measurements++;

      // Get patient ID from first measurement.
      $wrapper = entity_metadata_wrapper('node', array_shift($encounter_measurements));
      $patient_id = $wrapper->field_person->getIdentifier();

      if (!in_array($patient_id, $encounter_participants)) {
        $encounter_participants[] = $patient_id;
      }
    }

    $nid = end($ids);

    if (round(memory_get_usage() / 1048576) >= $memory_limit) {
      drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
      return;
    }

    $count = count($ids);
    $processed += $count;
    wlog("$processed encounters processed.");
  }

  $encounter_type = ucfirst($encounter_type);
  $unique_patients = count($encounter_participants);

  drush_print('');
  drush_print("$encounter_type report:");
  drush_print("Completed encounters: $encounters_with_measurements.");
  drush_print("Unique patients:      $unique_patients.");
  drush_print('');
}

/**
 * Counts total number of participation for all types of groups.
 *
 * In other words, number of times mother with child attended group encounter.
 */
function count_total_group_attendances() {
  $query = base_query_for_bundle('attendance');
  $query->fieldCondition('field_attended', 'value', 1);

  return $query->count()->execute();
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
 * Prints log based on verbosity option.
 */
function wlog($message) {
  // Get the option that will determine if output should be verbose or not.
  $verbose = drush_get_option('verbose', FALSE);

  if ($verbose !== FALSE) {
    drush_print($message);
  }
}
