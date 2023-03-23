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

if (!$start_date) {
  drush_print('Please specify --start_date option');
  exit;
}

if (!$end_date) {
  drush_print('Please specify --end_date option');
  exit;
}

drush_print("# Acute Illness report - $start_date  - $end_date");

// Get all of the encounters and the related participations.
$result = db_query("
SELECT 
    ip.entity_id as encounter, ip.field_individual_participant_target_id as participant
  FROM 
    field_data_field_individual_participant ip
  LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  WHERE
    ip.bundle = 'acute_illness_encounter'
     AND field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
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

$$first_encounters = array_unique($first_encounters);

// Now get the diagnosis for each first encounter.
foreach ($first_encounters as $first_encounter) {

  $query = 'SELECT field_acute_illness_diagnosis_value FROM field_data_field_acute_illness_diagnosis di WHERE entity_id = ' . $first_encounter;
  $result = db_query($query)->fetchField();
  if ($result) {
    $diagnoses[] = $result;
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

drush_print("# ANC Acute Illness report - $start_date  - $end_date");
$result = db_query("
  SELECT 
    field_prenatal_diagnoses_value
  FROM 
    field_data_field_prenatal_diagnoses pd
  LEFT JOIN node on pd.entity_id = node.nid 
  WHERE 
    FROM_UNIXTIME(node.created) > '$start_date'
    AND FROM_UNIXTIME(node.created) < '$end_date'")->fetchCol();

$diagnosis_count = array_count_values($result);

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
