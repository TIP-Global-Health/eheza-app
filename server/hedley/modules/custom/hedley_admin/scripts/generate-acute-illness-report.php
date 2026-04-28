<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
 */

require_once __DIR__ . '/report_common.inc';

$start_date = drush_get_option('start_date', FALSE);
$end_date = drush_get_option('end_date', FALSE);
$region = drush_get_option('region', FALSE);
$region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";
$region_name = ($region) ? $region : 'All Districts';

if (!$start_date) {
  drush_print('Please specify --start_date option');
  exit;
}

if (!$end_date) {
  drush_print('Please specify --end_date option');
  exit;
}

drush_print("# Acute Illness report - $region_name - $start_date - $end_date");

// Get all of the encounters and the related participations.
$result = db_query("
SELECT
    ip.entity_id as encounter, ip.field_individual_participant_target_id as participant
  FROM
    field_data_field_individual_participant ip
  LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  LEFT JOIN
    field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN
    field_data_field_district district ON person.field_person_target_id=district.entity_id
  WHERE
    ip.bundle = 'acute_illness_encounter'
     AND field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
     {$region_clause}
  ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchAll(PDO::FETCH_ASSOC);

// Check each paticipation to see that we have the first encounter.
$first_encounters = [];
foreach ($result as $item) {
  $query = 'SELECT entity_id FROM field_data_field_individual_participant WHERE field_individual_participant_target_id = ' . $item['participant'];
  $data = db_query($query)->fetchCol();

  // If this enounter is equal to the smallest ID, it's the first.
  if ($item['encounter'] == min($data)) {
    $first_encounters[] = $item['encounter'];
  }
}

$first_encounters = array_unique($first_encounters);

$diagnoses = [];
$malaria_test_count = 0;

// Now get the diagnosis for each first encounter.
foreach ($first_encounters as $first_encounter) {

  $query = 'SELECT field_acute_illness_diagnosis_value FROM field_data_field_acute_illness_diagnosis di WHERE entity_id = ' . $first_encounter;
  $result = db_query($query)->fetchField();
  if ($result) {
    $diagnoses[] = $result;
    if ($result == 'fever-of-unknown-origin') {
      $check_malaria = db_query('SELECT field_malaria_rapid_test_value
        FROM field_data_field_malaria_rapid_test mrt
        LEFT JOIN field_data_field_acute_illness_encounter aie ON aie.entity_id  = mrt.entity_id
        LEFT JOIN field_data_field_acute_illness_diagnosis di ON aie.field_acute_illness_encounter_target_id  = di.entity_id
        WHERE di.entity_id = ' . $first_encounter)->fetchField();

      if ($check_malaria == 'unable-to-run') {
        $malaria_test_count++;
      }
    }
  }
}

// Return an array of the diagnoses and their counts.
$diagnosis_count = array_count_values($diagnoses);

$data = [];
foreach ($diagnosis_count as $label => $value) {
  $data[] = [
    $label,
    $value,
  ];
}

// Put the list of disgnoses in alpha order.
sort($data);

// Add the total diagnoses.
$data[] = [
  'Total',
  count($diagnoses),
];

$table = new HedleyAdminTextTable(['Initial Diagnosis', 'Count']);
drush_print($table->render($data));

drush_print ('Malaria tests: ' . $malaria_test_count);


// ANC Diagnoses.
drush_print("# ANC Diagnoses report - $region_name - $start_date - $end_date");

// Get all of the encounters and the related participations.
$result = db_query("
SELECT
    ip.entity_id as encounter, ip.field_individual_participant_target_id as participant
  FROM
    field_data_field_individual_participant ip
  LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  LEFT JOIN
    field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN
    field_data_field_district district ON person.field_person_target_id=district.entity_id
  WHERE
    ip.bundle = 'prenatal_encounter'
     AND field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
     {$region_clause}
  ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchAll(PDO::FETCH_ASSOC);


$diagnoses = [];
foreach ($result as $item) {
  $query = 'SELECT field_prenatal_diagnoses_value FROM field_data_field_prenatal_diagnoses di WHERE entity_id = ' . $item['encounter'];
  $diagnosis = db_query($query)->fetchField();
  if ($diagnosis) {
    $diagnoses[] = $diagnosis;
  }
}

// Return an array of the diagnoses and their counts.
$diagnosis_count = array_count_values($diagnoses);

$data = [];
foreach ($diagnosis_count as $label => $value) {
  $data[] = [
    $label,
    $value,
  ];
}

// Put the list of disgnoses in alpha order.
sort($data);

// Add the total diagnoses.
$data[] = [
  'Total',
  count($diagnoses),
];
