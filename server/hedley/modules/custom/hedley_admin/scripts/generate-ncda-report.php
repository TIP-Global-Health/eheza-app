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

drush_print("# NCDA report - $region_name - $start_date - $end_date");

$total_encounters = 0;
$has_toilets_wc = 0;
$has_clean_water_wc = 0;
$has_toilets_n = 0;
$has_clean_water_n = 0;
$has_toilets_sc = 0;
$has_clean_water_sc = 0;
$has_toilets_group = 0;
$has_clean_water_group = 0;


// Get all of the well child encounters.
 $result = db_query("
 SELECT DISTINCT signs.entity_id
 FROM field_data_field_ncda_signs signs
LEFT JOIN
    field_data_field_well_child_encounter encounter ON signs.entity_id = encounter.entity_id
LEFT JOIN
    field_data_field_individual_participant ip ON encounter.field_well_child_encounter_target_id = ip.entity_id
LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  LEFT JOIN
    field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN
    field_data_field_district district ON person.field_person_target_id=district.entity_id
  WHERE
     field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
     {$region_clause}
  ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchCol();

  $well_child_encounters = count($result);
  $total_encounters = $total_encounters + $well_child_encounters;

  $sign_map = [];

  foreach ($result as $key => $value) {
    $query = 'SELECT signs.field_ncda_signs_value FROM field_data_field_ncda_signs signs WHERE signs.entity_id =' . $value;
    $signs = db_query($query)->fetchCol();
    $sign_map[$value] = $signs;
    if (in_array('has-toilets', $signs)) {
        $has_toilets_wc ++;
    }
    if (in_array('has-clean-water', $signs)) {
        $has_clean_water_wc ++;
    }
}

// Get all of the nutrition encounters.
$result = db_query("
SELECT DISTINCT signs.entity_id
FROM field_data_field_ncda_signs signs
LEFT JOIN
    field_data_field_nutrition_encounter encounter ON signs.entity_id = encounter.entity_id
LEFT JOIN
    field_data_field_individual_participant ip ON encounter.field_nutrition_encounter_target_id = ip.entity_id
LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  LEFT JOIN
    field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN
    field_data_field_district district ON person.field_person_target_id=district.entity_id
  WHERE
     field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
     {$region_clause}
  ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchCol();

  $nutrition_encounters = count($result);
  $total_encounters = $total_encounters + $nutrition_encounters;

  $sign_map = [];

  foreach ($result as $key => $value) {
    $query = 'SELECT signs.field_ncda_signs_value FROM field_data_field_ncda_signs signs WHERE signs.entity_id =' . $value;
    $signs = db_query($query)->fetchCol();
    $sign_map[$value] = $signs;
    if (in_array('has-toilets', $signs)) {
        $has_toilets_n ++;
    }
    if (in_array('has-clean-water', $signs)) {
        $has_clean_water_n ++;
    }
}

  // Get all of the child scoreboard .
$result = db_query("
SELECT DISTINCT signs.entity_id
FROM field_data_field_ncda_signs signs
LEFT JOIN
    field_data_field_child_scoreboard_encounter encounter ON signs.entity_id = encounter.entity_id
LEFT JOIN
    field_data_field_individual_participant ip ON encounter.field_child_scoreboard_encounter_target_id = ip.entity_id
LEFT JOIN
    field_data_field_scheduled_date sd ON ip.entity_id = sd.entity_id
  LEFT JOIN
    field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN
    field_data_field_district district ON person.field_person_target_id=district.entity_id
  WHERE
     field_scheduled_date_value >= :start_date
     AND field_scheduled_date_value <= :end_date
     {$region_clause}
  ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchCol();

  $scorecard_encounters = count($result);
  $total_encounters = $total_encounters + $scorecard_encounters;

  $sign_map = [];

foreach ($result as $key => $value) {
    $query = 'SELECT signs.field_ncda_signs_value FROM field_data_field_ncda_signs signs WHERE signs.entity_id =' . $value;
    $signs = db_query($query)->fetchCol();
    $sign_map[$value] = $signs;
    if (in_array('has-toilets', $signs)) {
        $has_toilets_sc ++;
    }
    if (in_array('has-clean-water', $signs)) {
        $has_clean_water_sc ++;
    }
}

  // Get all of the group encounters.
  $result = db_query("
  SELECT DISTINCT signs.entity_id
  FROM field_data_field_ncda_signs signs
  LEFT JOIN
      field_data_field_session session ON signs.entity_id = session.entity_id
  LEFT JOIN
      field_data_field_person person ON person.entity_id = session.entity_id
  LEFT JOIN
      node ON session.entity_id = node.nid
  LEFT JOIN
      field_data_field_district district ON person.field_person_target_id=district.entity_id
    WHERE
       FROM_UNIXTIME(node.created) >= :start_date
       AND FROM_UNIXTIME(node.created) <= :end_date
       {$region_clause}
    ", [':start_date' => $start_date, 'end_date' => $end_date])->fetchCol();

    $group_encounters = count($result);
    $total_encounters = $total_encounters + $group_encounters;
    $sign_map = [];

  foreach ($result as $key => $value) {
      $query = 'SELECT signs.field_ncda_signs_value FROM field_data_field_ncda_signs signs WHERE signs.entity_id =' . $value;
      $signs = db_query($query)->fetchCol();
      $sign_map[$value] = $signs;
      if (in_array('has-toilets', $signs)) {
          $has_toilets_group ++;
      }
      if (in_array('has-clean-water', $signs)) {
          $has_clean_water_group ++;
      }
  }

$has_clean_water_total = $has_clean_water_sc + $has_clean_water_group + $has_clean_water_wc + $has_clean_water_n;
$has_toilets_total = $has_toilets_sc + $has_toilets_group + $has_toilets_wc + $has_toilets_n;


$data = [
    [
        'Has Toilets',
        $has_toilets_total,
        $total_encounters,
        $has_toilets_sc,
        $scorecard_encounters,
        $has_toilets_group,
        $group_encounters,
        $has_toilets_wc,
        $well_child_encounters,
        $has_clean_water_n,
        $nutrition_encounters
    ],
    [
        'Has Clean Water',
        $has_clean_water_total,
        $total_encounters,
        $has_clean_water_sc,
        $scorecard_encounters,
        $has_clean_water_group,
        $group_encounters,
        $has_clean_water_wc,
        $well_child_encounters,
        $has_clean_water_n,
        $nutrition_encounters
    ],
];

$table = new HedleyAdminTextTable(['Question', 'Total', 'Encounters', 'Scorecard', 'Encounters', 'Group Nutr', 'Encounters', 'SPV', 'Encounters', 'Indiv Nutr', 'Encounters']);
drush_print($table->render($data));

