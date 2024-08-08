<?php

/**
 * @file
 * Genrates MUAC report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/muac-report.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 500);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

// Resolve all Health Centers (associate ID to name).
$query = db_select('node', 'n');
$query->fields('n', ['nid', 'title']);
$query->condition('type', 'health_center');
$result = $query->execute()->fetchAll();
$health_centers = [];
foreach ($result as $row) {
  $health_centers[$row->nid] = $row->title;
}

// Resolve all Nurses. We need to determine if measurement was taken
// by nurse or CHW.
$query = db_select('field_data_field_role', 'fr')
  ->fields('fr', ['entity_id']);
$query->condition('fr.field_role_value', 'nurse');
$nurses = $query->execute()->fetchCol();

$encounter_types = [
  'acute_illness_muac' => 'Acute Illness',
  'child_scoreboard_ncda' => 'Child Scorecard',
  'muac' => 'Nutrition Group',
  'nutrition_muac' => 'Nutrition Group',
  'well_child_muac' => 'SPV',
];

$query = db_select('node', 'node');
$query->fields('node', ['nid', 'type']);
$query->condition('type', array_keys($encounter_types), 'IN');
$query->condition('status', NODE_PUBLISHED);

$field_to_join = [
  'field_person',
  'field_muac',
  'field_nurse',
  'field_date_measured',
  'field_shards',
];

foreach ($field_to_join as $field) {
  hedley_general_join_field_to_query($query, 'node', $field);
}

$query->leftJoin('field_data_field_district', 'field_district', 'field_person.field_person_target_id = field_district.entity_id');
$query->addField('field_district', 'field_district_value', 'field_district');

$query->leftJoin('field_data_field_birth_date', 'field_birth_date', 'field_person.field_person_target_id = field_birth_date.entity_id');
$query->addField('field_birth_date', 'field_birth_date_value', 'field_birth_date');
$two_years_ago = date('Y-m-d', strtotime('-2 years'));
$query->condition('field_date_measured.field_date_measured_value', $two_years_ago, '>=');
$query->range(0, $batch);
$query->orderBy('node.nid');

$nid = 0;
$rows[0] = [
  'Patient ID',
  'MUAC Value',
  'Date Measured (YYYY-MM-DD)',
  'Nurse/CHW',
  'Encounter Type',
  'Health Center',
  'District',
];

while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $batch_query = clone $query;
  if ($nid) {
    $batch_query->condition('node.nid', $nid, '>');
  }

  $result = $batch_query->execute()->fetchAll();

  if (empty($result)) {
    // No more items left.
    break;
  }
  $nid = end($result)->nid;

  foreach ($result as $row) {
    $measured_date = explode(' ', $row->field_date_measured)[0];
    $birth_date = explode(' ', $row->field_birth_date)[0];

    $measured_date_obj = new DateTime($measured_date);
    $birth_date_obj = new DateTime($birth_date);

    // Calculate the age at the time of measurement.
    $age_interval = $birth_date_obj->diff($measured_date_obj);
    // If age is above 5 years, skip the measurement.
    if ($age_interval->y >= 5) {
      continue;
    }

    // If currently recorded  measurement for patient is more
    // recent that than the one being processed, skip processing.
    if ($rows[$row->field_person]) {
      $current_measured_date_obj = new DateTime($rows[$row->nid][2]);
      if ($current_measured_date_obj >= $measured_date_obj) {
        continue;
      }
    }

    $nurse_type = in_array($row->field_nurse, $nurses) ? 'nurse' : 'chw';

    $rows[$row->field_person] = [
      $row->field_person,
      $row->field_muac,
      $measured_date,
      $nurse_type,
      $encounter_types[$row->type],
      $health_centers[$row->field_shards],
      $row->field_district,
    ];
  }

  $total = count($result);
}

foreach ($rows as $row) {
  $row_as_string = implode(',', $row);
  drush_print($row_as_string);
}
