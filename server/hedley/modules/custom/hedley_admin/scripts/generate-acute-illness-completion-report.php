<?php

/**
 * @file
 * Generates 'acute illness completion' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-acute-illness-completion-report.php.
 */

require_once __DIR__ . '/report_common.inc';
$start_date = drush_get_option('start_date', FALSE);
$end_date = drush_get_option('end_date', FALSE);
$name = drush_get_option('name', FALSE);
$type = drush_get_option('mode', FALSE);


if (!$start_date) {
  drush_print('Please specify --start_date option');
  exit;
}

if (!$end_date) {
  drush_print('Please specify --end_date option');
  exit;
}

if (!$type) {
  $type = FALSE;
}
elseif (strtolower($type) != 'h' &&  strtolower($type) != 'd') {
  $type = FALSE;
}

$name_clause = "";
if ($type == 'H') {
  $name = get_health_center_id($name);
  drush_print("# Acute Illness Completion Report - Mode: Health Center - " . get_health_center($name) . " - " . $start_date . " - " . $end_date);
  $name_clause = "AND (field_ai_encounter_type_value='nurse-encounter' OR field_ai_encounter_type_value is NULL) AND field_health_center_target_id='$name'";
}
elseif ($type == 'D') {
  $name = get_district_name($name);
  drush_print("# Acute Illness Completion Report - Mode: Districts - " . $name . " - " . $start_date . " - " . $end_date);
  $name_clause = "AND field_district_value='$name'";
}
else {
  drush_print("# Acute Illness Completion Report - Mode: ALL - " . $start_date . " - " . $end_date);
}

$encounters = [
  [
    'Symptom Review (total)',
    symptom_total($start_date, $end_date, $name_clause) . ' / ' . symptom_review($start_date, $end_date, $name_clause),
    round(symptom_total($start_date, $end_date, $name_clause) / symptom_review($start_date, $end_date, $name_clause) * 100, 3) . '%',

  ],
  [
    '  General',
    symptom_review($start_date, $end_date, $name_clause, 'general_complete') . ' / ' . symptom_review($start_date, $end_date, $name_clause),
    round(symptom_review($start_date, $end_date, $name_clause, 'general_complete') / symptom_review($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '   General days = 0/1',
    (symptom_general_zero_one($start_date, $end_date, $name_clause) - symptom_general_zero_one($start_date, $end_date, $name_clause, TRUE)) . ' / ' . (symptom_review($start_date, $end_date, $name_clause, 'general_complete') - symptom_general_zero_one($start_date, $end_date, $name_clause, TRUE)),
    round((symptom_general_zero_one($start_date, $end_date, $name_clause) - symptom_general_zero_one($start_date, $end_date, $name_clause, TRUE)) / (symptom_review($start_date, $end_date, $name_clause, 'general_complete') - symptom_general_zero_one($start_date, $end_date, $name_clause, TRUE)) * 100, 3) . '%',
  ],
  [
    '  Respiratory',
    symptom_review($start_date, $end_date, $name_clause, 'respiratory_complete') . ' / ' . symptom_review($start_date, $end_date, $name_clause),
    round(symptom_review($start_date, $end_date, $name_clause, 'respiratory_complete') / symptom_review($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    Respiratory = 0/1',
    (symptom_review($start_date, $end_date, $name_clause, 'respiratory_one') - symptom_review($start_date, $end_date, $name_clause, 'respiratory_zero')) . ' / ' . (symptom_review($start_date, $end_date, $name_clause, 'respiratory_complete') - symptom_review($start_date, $end_date, $name_clause, 'respiratory_zero')),
    round((symptom_review($start_date, $end_date, $name_clause, 'respiratory_one') - symptom_review($start_date, $end_date, $name_clause, 'respiratory_zero')) / (symptom_review($start_date, $end_date, $name_clause, 'respiratory_complete') - symptom_review($start_date, $end_date, $name_clause, 'respiratory_zero')) * 100, 3) . '%',
  ],
  [
    '  GI',
    symptom_review($start_date, $end_date, $name_clause, 'gi_complete') . ' / ' . symptom_review($start_date, $end_date, $name_clause),
    round(symptom_review($start_date, $end_date, $name_clause, 'gi_complete') / symptom_review($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    GI days = 0/1',
    (symptom_review($start_date, $end_date, $name_clause, 'gi_one') - symptom_review($start_date, $end_date, $name_clause, 'gi_zero')) . ' / ' . (symptom_review($start_date, $end_date, $name_clause, 'gi_complete') - symptom_review($start_date, $end_date, $name_clause, 'gi_zero')),
    round((symptom_review($start_date, $end_date, $name_clause, 'gi_one') - symptom_review($start_date, $end_date, $name_clause, 'gi_zero')) / (symptom_review($start_date, $end_date, $name_clause, 'gi_complete') - symptom_review($start_date, $end_date, $name_clause, 'gi_zero')) * 100, 3) . '%',
  ],
  ["", "", ""],
  [
    'Exposure/Travel History',
    exposure_travel_history($start_date, $end_date, $name_clause, 'all_complete') . ' / ' . exposure_travel_history($start_date, $end_date, $name_clause),
    round(exposure_travel_history($start_date, $end_date, $name_clause, 'all_complete') / exposure_travel_history($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  Travel History',
    exposure_travel_history($start_date, $end_date, $name_clause, 'travel_complete') . ' / ' . exposure_travel_history($start_date, $end_date, $name_clause),
    round(exposure_travel_history($start_date, $end_date, $name_clause, 'travel_complete') / exposure_travel_history($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  Contact Exposure',
    exposure_travel_history($start_date, $end_date, $name_clause, 'exposure_complete') . ' / ' . exposure_travel_history($start_date, $end_date, $name_clause),
    round(exposure_travel_history($start_date, $end_date, $name_clause, 'exposure_complete') / exposure_travel_history($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  ["", "", ""],
  [
    'Prior Treatment History',
    treatment_history($start_date, $end_date, $name_clause, 'complete') . ' / ' . treatment_history($start_date, $end_date, $name_clause),
    round(treatment_history($start_date, $end_date, $name_clause, 'complete') / treatment_history($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  ["", "", ""],
  [
    'Physical Exam',
    physical_exam_total($start_date, $end_date, $name_clause) . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam_total($start_date, $end_date, $name_clause) / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  Blood Pressure',
    physical_exam($start_date, $end_date, $name_clause, 'BP') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'BP') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  Heart Rate',
    physical_exam($start_date, $end_date, $name_clause, 'heart_rate') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'heart_rate') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    Heart Rate Outside Normal',
    physical_exam_abnormal($start_date, $end_date, $name_clause, 'heart_abnormal') . ' / ' . physical_exam_abnormal($start_date, $end_date, $name_clause, 'heart'),
    round(physical_exam_abnormal($start_date, $end_date, $name_clause, 'heart_abnormal') / physical_exam_abnormal($start_date, $end_date, $name_clause, 'heart') * 100, 3) . '%',
  ],
  [
    '  Respiratory Rate',
    physical_exam($start_date, $end_date, $name_clause, 'resp_rate') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'resp_rate') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    Respiratory Rate Normal',
    physical_exam_abnormal($start_date, $end_date, $name_clause, 'respiratory_abnormal') . ' / ' . physical_exam_abnormal($start_date, $end_date, $name_clause, 'respiratory'),
    round(physical_exam_abnormal($start_date, $end_date, $name_clause, 'respiratory_abnormal') / physical_exam_abnormal($start_date, $end_date, $name_clause, 'respiratory') * 100, 3) . '%',
  ],
  [
    '  Body Temp',
    physical_exam($start_date, $end_date, $name_clause, 'body') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'body') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    Body Temp Outside Normal',
    physical_exam_abnormal($start_date, $end_date, $name_clause, 'temp_abnormal') . ' / ' . physical_exam_abnormal($start_date, $end_date, $name_clause, 'temp'),
    round(physical_exam_abnormal($start_date, $end_date, $name_clause, 'temp_abnormal') / physical_exam_abnormal($start_date, $end_date, $name_clause, 'temp') * 100, 3) . '%',
  ],
  [
    '  Core Exam',
    physical_exam($start_date, $end_date, $name_clause, 'core') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'core') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  MUAC',
    physical_exam($start_date, $end_date, $name_clause, 'muac') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'muac') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '    MUAC Outside Normal',
    physical_exam_abnormal($start_date, $end_date, $name_clause, 'muac_abnormal') . ' / ' . physical_exam_abnormal($start_date, $end_date, $name_clause, 'muac'),
    round(physical_exam_abnormal($start_date, $end_date, $name_clause, 'muac_abnormal') / physical_exam_abnormal($start_date, $end_date, $name_clause, 'muac') * 100, 3) . '%',
  ],
  [
    '  Nutrition',
    physical_exam($start_date, $end_date, $name_clause, 'nutrition') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'nutrition') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  [
    '  Acute Findings',
    physical_exam($start_date, $end_date, $name_clause, 'acute') . ' / ' . physical_exam($start_date, $end_date, $name_clause),
    round(physical_exam($start_date, $end_date, $name_clause, 'acute') / physical_exam($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
  ["", "", ""],
  [
    'Total Encounter Complete',
    enconter_complete($start_date, $end_date, $name_clause, TRUE) . ' / ' . enconter_complete($start_date, $end_date, $name_clause),
    round(enconter_complete($start_date, $end_date, $name_clause, TRUE) / enconter_complete($start_date, $end_date, $name_clause) * 100, 3) . '%',
  ],
];


$text_table = new HedleyAdminTextTable([
  'Encounter section',
  'Complete / All',
  'Percent Complete',
]);

drush_print($text_table->render($encounters));

/**
 * Gets the node ID of the health center.
 *
 * @param string $health_center_name
 *   The name of a healthcenter.
 *
 * @return string
 *   The node ID of the health center.
 */
function get_health_center_id($health_center_name = NULL) {
  if (!$health_center_name) {
    return NULL;
  }
  else {
    $health_center_name = str_replace(['_', '-', '.'], ' ', $health_center_name);
    $results = db_query("SELECT nid FROM node WHERE title = :title AND type = :type", array(
      ':title' => $health_center_name,
      ':type' => 'health_center',
    ));
    if (!$results->fetchField()) {
      $results = db_query("SELECT nid FROM node WHERE title LIKE :title AND type = :type", array(
        ':title' => db_like($health_center_name) . '%',
        ':type' => 'health_center',
      ));
      if (!$results->fetchField()) {
        drush_print('No health centers match the name provided');
        exit(1);
      }
      elseif (!$results->fetchField()) {
        return db_query("SELECT nid FROM node WHERE title LIKE :title AND type = :type LIMIT 1", array(
          ':title' => db_like($health_center_name) . '%',
          ':type' => 'health_center',
        ))->fetchField();
      }
      else {
        $results = db_query("SELECT nid FROM node WHERE title LIKE :title AND type = :type", array(
          ':title' => db_like($health_center_name) . '%',
          ':type' => 'health_center',
        ));
        drush_print('Multiple health centers match the name provided including ' .
          get_health_center($results->fetchField()) . ', ' . get_health_center($results->fetchField()) .
          ", etc. \r\nPlease use a more specific name");
        exit(1);
      }
    }
    else {
      return db_query("SELECT nid FROM node WHERE title = :title AND type = :type", array(
        ':title' => $health_center_name,
        ':type' => 'health_center',
      ))->fetchField();
    }
  }
}

/**
 * Gets the full name of the district.
 *
 * @param string $district
 *   The user input name of the district.
 *
 * @return string
 *   The name of the district as stored in SQL.
 */
function get_district_name($district = NULL) {
  if (!$district) {
    return NULL;
  }
  else {
    $district = str_replace(['_', '-', '.'], ' ', $district);
    $results = db_query("SELECT DISTINCT field_district_value FROM field_data_field_district WHERE field_district_value = :district", array(
      ':district' => $district,
    ));
    if (!$results->fetchField()) {
      $results = db_query("SELECT DISTINCT field_district_value FROM field_data_field_district WHERE field_district_value LIKE :district", array(
        ':district' => db_like($district) . '%',
      ));
      if (!$results->fetchField()) {
        drush_print('No districts match the name provided');
        exit(1);
      }
      elseif (!$results->fetchField()) {
        return db_query("SELECT DISTINCT field_district_value FROM field_data_field_district WHERE field_district_value LIKE :district LIMIT 1", array(
          ':district' => db_like($district) . '%',
        ))->fetchField();
      }
      else {
        $results = db_query("SELECT DISTINCT field_district_value FROM field_data_field_district WHERE field_district_value LIKE :district", array(
          ':district' => db_like($district) . '%',
        ));
        drush_print('Multiple districts match the name provided including ' .
          $results->fetchField() . ', ' . $results->fetchField() .
          ", etc. \r\nPlease use a more specific name");
        exit(1);
      }
    }
    else {
      return db_query("SELECT DISTINCT field_district_value FROM field_data_field_district WHERE field_district_value = :district", array(
        ':district' => $district,
      ))->fetchField();
    }
  }
}

/**
 * Gets the name of the health center.
 *
 * @param string $health_center_id
 *   The node ID of teh healthcenter.
 *
 * @return string
 *   The name for the health center.
 */
function get_health_center($health_center_id = NULL) {
  if ($health_center_id) {
    return db_query("SELECT title FROM node WHERE nid='$health_center_id' AND type='health_center' LIMIT 1")->fetchField();
  }
  else {
    return NULL;
  }
}

/**
 * Gets distinct AI encounter IDs.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 *
 * @return array[int]
 *   List of distinct AI encounter IDs.
 */
function get_id_list($start_date, $end_date, $name_clause) {
  return db_query("SELECT DISTINCT field_acute_illness_encounter_target_id AS id
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
    [
      ':start_date' => $start_date,
      ':end_date' => $end_date,
    ])->fetchAll(PDO::FETCH_COLUMN);
}

/**
 * Gets various number of encouters related to the symptom review.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param string $mode
 *   Resperatory_zero: all resp encounters with days = 0
 *   respiratory_one: all resp encounters with days = 0/1
 *   respiratory_complete: all completed resp encounters
 *   gi_zero: all resp encounters with days = 0
 *   gi_one: all resp encounters with days = 0/1
 *   gi_complete: all completed resp encounters
 *   general_complete: all completed resp encounter.
 *
 * @return int
 *   Number of encounters based on mode.
 */
function symptom_review($start_date, $end_date, $name_clause, $mode = FALSE) {
  $mode_clause = "";

  if ($mode == "respiratory_one") {
    $mode_clause = "AND (field_cough_period_value = 0 OR field_cough_period_value = 1)
    AND (field_shortness_of_breath_period_value = 0 OR field_shortness_of_breath_period_value = 1)
    AND (field_nasal_congestion_period_value = 0 OR field_nasal_congestion_period_value = 1)
    AND (field_blood_in_sputum_period_value = 0 OR field_blood_in_sputum_period_value = 1)
    AND (field_sore_throat_period_value = 0 OR field_sore_throat_period_value = 1)
    AND (field_loss_of_smell_period_value = 0 OR field_loss_of_smell_period_value = 1)
    AND (field_stabbing_chest_pain_period_value = 0 OR field_stabbing_chest_pain_period_value = 1)";
  }
  elseif ($mode == "respiratory_zero" || $mode == 'Resperatory_zero') {
    $mode_clause = "AND field_cough_period_value = 0
    AND field_shortness_of_breath_period_value = 0
    AND field_nasal_congestion_period_value = 0
    AND field_blood_in_sputum_period_value = 0
    AND field_sore_throat_period_value = 0
    AND field_loss_of_smell_period_value = 0
    AND field_stabbing_chest_pain_period_value = 0";
  }
  elseif ($mode == "gi_one") {
    $mode_clause = "AND (field_bloody_diarrhea_period_value = 0 OR field_bloody_diarrhea_period_value = 1)
    AND (field_nausea_period_value = 0 OR field_nausea_period_value = 1)
    AND (field_abdominal_pain_period_value = 0 OR field_abdominal_pain_period_value = 1)
    AND (field_non_bloody_diarrhea_period_value = 0 OR field_non_bloody_diarrhea_period_value = 1)
    AND (field_vomiting_period_value = 0 OR field_vomiting_period_value = 1)";
  }
  elseif ($mode == "gi_zero") {
    $mode_clause = "AND field_bloody_diarrhea_period_value = 0
    AND field_nausea_period_value = 0
    AND field_abdominal_pain_period_value = 0
    AND field_non_bloody_diarrhea_period_value = 0
    AND field_vomiting_period_value = 0";
  }
  elseif ($mode == 'general_complete') {
    $mode_clause = "AND field_fever_period_value IS NOT NULL";
  }
  elseif ($mode == 'respiratory_complete') {
    $mode_clause = "AND field_cough_period_value IS NOT NULL";
  }
  elseif ($mode == 'gi_complete') {
    $mode_clause = "AND field_nausea_period_value IS NOT NULL";
  }

  return db_query("SELECT COUNT (DISTINCT field_acute_illness_encounter_target_id)
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_fever_period fev ON node.nid=fev.entity_id
    LEFT JOIN field_data_field_cough_period cou ON node.nid=cou.entity_id
    LEFT JOIN field_data_field_shortness_of_breath_period sho ON node.nid=sho.entity_id
    LEFT JOIN field_data_field_nasal_congestion_period nas ON node.nid=nas.entity_id
    LEFT JOIN field_data_field_blood_in_sputum_period blo ON node.nid=blo.entity_id
    LEFT JOIN field_data_field_sore_throat_period sor ON node.nid=sor.entity_id
    LEFT JOIN field_data_field_loss_of_smell_period los ON node.nid=los.entity_id
    LEFT JOIN field_data_field_stabbing_chest_pain_period sta ON node.nid=sta.entity_id
    LEFT JOIN field_data_field_abdominal_pain_period abd ON node.nid=abd.entity_id
    LEFT JOIN field_data_field_bloody_diarrhea_period blod ON node.nid=blod.entity_id
    LEFT JOIN field_data_field_nausea_period nau ON node.nid=nau.entity_id
    LEFT JOIN field_data_field_non_bloody_diarrhea_period non ON node.nid=non.entity_id
    LEFT JOIN field_data_field_vomiting_period vom ON node.nid=vom.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$mode_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
    [
      ':start_date' => $start_date,
      ':end_date' => $end_date,
    ])->fetchField();
}

/**
 * Gets number of encouters with completed symptom reviews.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 *
 * @return int
 *   Number of encouters with completed symptom reviews.
 */
function symptom_total($start_date, $end_date, $name_clause) {
  $ids = array_fill_keys(get_id_list($start_date, $end_date, $name_clause), 0);

  $constraints = [
    "AND field_fever_period_value IS NOT NULL",
    "AND field_cough_period_value IS NOT NULL",
    "AND field_nausea_period_value IS NOT NULL",
  ];

  foreach ($constraints as $constraint_clause) {
    $results = db_query("SELECT DISTINCT field_acute_illness_encounter_target_id
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_fever_period fev ON node.nid=fev.entity_id
    LEFT JOIN field_data_field_cough_period cou ON node.nid=cou.entity_id
    LEFT JOIN field_data_field_nausea_period nau ON node.nid=nau.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$constraint_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      $ids[$result] += 1;
    }
  }

  // Count number of encounter IDs that meet all constraints.
  $count = 0;
  foreach ($ids as $id) {
    if ($id == count($constraints)) {
      ++$count;
    }
  }

  return $count;
}

/**
 * Gets the number of encouters where symptom-general values = 0/1.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param bool $zero_only
 *   Counts only all 0 value answers when true.
 *
 * @return int
 *   Number of encouters encouters where symptom-general values = 0/1.
 */
function symptom_general_zero_one($start_date, $end_date, $name_clause, $zero_only = FALSE) {

  $ids = array_fill_keys(get_id_list($start_date, $end_date, $name_clause), 0);

  if ($zero_only) {
    $constraints = [
      "AND field_yellow_eyes_period_value = 0",
      "AND field_fever_period_value = 0",
      "AND field_chills_period_value = 0",
      "AND field_night_sweats_period_value = 0",
      "AND field_body_aches_period_value = 0",
      "AND field_headache_period_value = 0",
      "AND field_lethargy_period_value = 0",
      "AND field_poor_suck_period_value = 0",
      "AND field_unable_to_drink_period_value = 0",
      "AND field_unable_to_eat_period_value = 0",
      "AND field_increased_thirst_period_value = 0",
      "AND field_dry_mouth_period_value = 0",
      "AND field_coke_colored_urine_period_value = 0",
      "AND field_convulsions_period_value = 0",
      "AND field_severe_weakness_period_value = 0",
      "AND field_spontaneos_bleeding_period_value = 0",
    ];
  }
  else {
    $constraints = [
      "AND (field_yellow_eyes_period_value = 0 OR field_yellow_eyes_period_value = 1)",
      "AND (field_fever_period_value = 0 OR field_fever_period_value = 1)",
      "AND (field_chills_period_value = 0 OR field_chills_period_value = 1)",
      "AND (field_night_sweats_period_value = 0 OR field_night_sweats_period_value = 1)",
      "AND (field_body_aches_period_value = 0 OR field_body_aches_period_value = 1)",
      "AND (field_headache_period_value = 0 OR field_headache_period_value = 1)",
      "AND (field_lethargy_period_value = 0 OR field_lethargy_period_value = 1)",
      "AND (field_poor_suck_period_value = 0 OR field_poor_suck_period_value = 1)",
      "AND (field_unable_to_drink_period_value = 0 OR field_unable_to_drink_period_value = 1)",
      "AND (field_unable_to_eat_period_value = 0 OR field_unable_to_eat_period_value = 1)",
      "AND (field_increased_thirst_period_value = 0 OR field_increased_thirst_period_value = 1)",
      "AND (field_dry_mouth_period_value = 0 OR field_dry_mouth_period_value = 1)",
      "AND (field_coke_colored_urine_period_value = 0 OR field_coke_colored_urine_period_value  = 1)",
      "AND (field_convulsions_period_value = 0 OR field_convulsions_period_value = 1)",
      "AND (field_severe_weakness_period_value = 0 OR field_severe_weakness_period_value = 1)",
      "AND (field_spontaneos_bleeding_period_value = 0 OR field_spontaneos_bleeding_period_value = 1)",
    ];
  }

  foreach ($constraints as $constraint_clause) {
    $results = db_query("SELECT DISTINCT field_acute_illness_encounter_target_id
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_fever_period fev ON node.nid=fev.entity_id
    LEFT JOIN field_data_field_chills_period chi ON node.nid=chi.entity_id
    LEFT JOIN field_data_field_night_sweats_period nig ON node.nid=nig.entity_id
    LEFT JOIN field_data_field_body_aches_period bod ON node.nid=bod.entity_id
    LEFT JOIN field_data_field_headache_period hea ON node.nid=hea.entity_id
    LEFT JOIN field_data_field_lethargy_period let ON node.nid=let.entity_id
    LEFT JOIN field_data_field_poor_suck_period poo ON node.nid=poo.entity_id
    LEFT JOIN field_data_field_unable_to_drink_period und ON node.nid=und.entity_id
    LEFT JOIN field_data_field_unable_to_eat_period une ON node.nid=une.entity_id
    LEFT JOIN field_data_field_increased_thirst_period inc ON node.nid=inc.entity_id
    LEFT JOIN field_data_field_dry_mouth_period dry ON node.nid=dry.entity_id
    LEFT JOIN field_data_field_yellow_eyes_period yel ON node.nid=yel.entity_id
    LEFT JOIN field_data_field_coke_colored_urine_period cok ON node.nid=cok.entity_id
    LEFT JOIN field_data_field_spontaneos_bleeding_period spo ON node.nid=spo.entity_id
    LEFT JOIN field_data_field_severe_weakness_period weak ON node.nid=weak.entity_id
    LEFT JOIN field_data_field_convulsions_period conv ON node.nid=conv.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$constraint_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      $ids[$result] += 1;
    }
  }

  // Count number of encounter IDs that meet all the constraints.
  $count = 0;
  foreach ($ids as $id) {
    if ($id == count($constraints)) {
      ++$count;
    }
  }

  return $count;
}

/**
 * Gets number of encouters related to the exposure travel history review.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param string $mode
 *   Exposure_complete: completed contact exposures
 *   travel_complete: completed travel histories
 *   all_complete: completed both contact exposure and travel history.
 *
 * @return int
 *   Number of encounters based on mode.
 */
function exposure_travel_history($start_date, $end_date, $name_clause, $mode = FALSE) {
  if ($mode != "all_complete") {
    $mode_clause = "";

    if ($mode == "exposure_complete" || $mode == "Exposure_complete") {
      $mode_clause = "AND field_exposure_value IS NOT NULL";
    }
    elseif ($mode == "travel_complete") {
      $mode_clause = "AND field_travel_history_value IS NOT NULL";
    }

    return db_query("SELECT COUNT (DISTINCT field_acute_illness_encounter_target_id)
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id=node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
      LEFT JOIN field_data_field_travel_history tra ON node.nid=tra.entity_id
      LEFT JOIN field_data_field_exposure exp ON node.nid=exp.entity_id
      WHERE FROM_UNIXTIME(node.created) >= :start_date
        {$name_clause}
        {$mode_clause}
        AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchField();
  }
  else {
    $ids = array_fill_keys(get_id_list($start_date, $end_date, $name_clause), 0);

    $constraints = [
      "AND field_exposure_value IS NOT NULL",
      "AND field_travel_history_value IS NOT NULL",
    ];
    foreach ($constraints as $constraint_clause) {
      $results = db_query("SELECT DISTINCT field_acute_illness_encounter_target_id
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id=node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
      LEFT JOIN field_data_field_travel_history tra ON node.nid=tra.entity_id
      LEFT JOIN field_data_field_exposure exp ON node.nid=exp.entity_id
      WHERE FROM_UNIXTIME(node.created) >= :start_date
        {$name_clause}
        {$constraint_clause}
        AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchAll(PDO::FETCH_COLUMN);
      foreach ($results as $result) {
        $ids[$result] += 1;
      }
    }

    // Count number of encounter IDs that meet all the constraints.
    $count = 0;
    foreach ($ids as $id) {
      if ($id == count($constraints)) {
        ++$count;
      }
    }
    return $count;
  }
}

/**
 * Gets various number of encouters related to the treatment history review.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param string $mode
 *   Complete: completed treatment history.
 *
 * @return int
 *   Number of encounters based on mode.
 */
function treatment_history($start_date, $end_date, $name_clause, $mode = FALSE) {
  $mode_clause = "";

  if ($mode == 'complete' || $mode == 'Complete') {
    $mode_clause = "AND field_treatment_history_value IS NOT NULL";
  }
  return db_query("SELECT COUNT (DISTINCT field_acute_illness_encounter_target_id)
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_treatment_history tre ON e.entity_id=tre.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$mode_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
    [
      ':start_date' => $start_date,
      ':end_date' => $end_date,
    ])->fetchField();
}

/**
 * Gets various number of encouters related to the physical examination.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param string $mode
 *   BP: completed systolic and diastolic measurments
 *   heart_rate: completed heart rate measurments
 *   resp_rate: completed respiratory rate measurments
 *   body: completed body temp measurments
 *   core: completed core exam section
 *   muac: completed MUAC measurments
 *   nutrition: completed nutrition measurments
 *   acute: completed acute findings section.
 *
 * @return int
 *   Number of encounters based on mode.
 */
function physical_exam($start_date, $end_date, $name_clause, $mode = FALSE) {
  $mode_clause = "";

  if ($mode == 'BP') {
    $mode_clause = "AND field_sys_value IS NOT NULL AND field_dia_value IS NOT NULL";
  }
  elseif ($mode == 'heart_rate') {
    $mode_clause = "AND field_heart_rate_value IS NOT NULL";
  }
  elseif ($mode == 'resp_rate') {
    $mode_clause = "AND field_respiratory_rate_value IS NOT NULL";
  }
  elseif ($mode == 'body') {
    $mode_clause = "AND field_body_temperature_value IS NOT NULL";
  }
  elseif ($mode == 'core') {
    $mode_clause = "AND  field_heart_value IS NOT NULL AND field_lungs_value IS NOT NULL";
  }
  elseif ($mode == 'muac') {
    $mode_clause = "AND field_muac_value IS NOT NULL";
  }
  elseif ($mode == 'nutrition') {
    $mode_clause = "AND field_nutrition_signs_value IS NOT NULL";
  }
  elseif ($mode == 'acute') {
    $mode_clause = "AND field_findings_signs_general_value IS NOT NULL AND field_findings_signs_respiratory_value IS NOT NULL";
  }

  return db_query("SELECT COUNT (DISTINCT field_acute_illness_encounter_target_id)
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_heart hea ON node.nid=hea.entity_id
    LEFT JOIN field_data_field_lungs lun ON node.nid=lun.entity_id
    LEFT JOIN field_data_field_findings_signs_general gen ON node.nid=gen.entity_id
    LEFT JOIN field_data_field_findings_signs_respiratory res ON node.nid=res.entity_id
    LEFT JOIN field_data_field_heart_rate her ON node.nid=her.entity_id
    LEFT JOIN field_data_field_respiratory_rate rer ON node.nid=rer.entity_id
    LEFT JOIN field_data_field_body_temperature bod ON node.nid=bod.entity_id
    LEFT JOIN field_data_field_dia dia ON node.nid=dia.entity_id
    LEFT JOIN field_data_field_sys sys ON node.nid=sys.entity_id
    LEFT JOIN field_data_field_muac muac ON node.nid=muac.entity_id
    LEFT JOIN field_data_field_nutrition_signs nutr ON node.nid=nutr.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$mode_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
    [
      ':start_date' => $start_date,
      ':end_date' => $end_date,
    ])->fetchField();
}

/**
 * Gets number of encouters with completed physical exams.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 *
 * @return int
 *   Number of encouters with completed physical exams.
 */
function physical_exam_total($start_date, $end_date, $name_clause) {
  $ids = array_fill_keys(get_id_list($start_date, $end_date, $name_clause), 0);

  $constraints = [
    "AND field_sys_value IS NOT NULL AND field_dia_value IS NOT NULL",
    "AND field_heart_rate_value IS NOT NULL",
    "AND field_respiratory_rate_value IS NOT NULL",
    "AND field_body_temperature_value IS NOT NULL",
    "AND  field_heart_value IS NOT NULL AND field_lungs_value IS NOT NULL",
    "AND field_muac_value IS NOT NULL",
    "AND field_nutrition_signs_value IS NOT NULL",
    "AND field_findings_signs_general_value IS NOT NULL AND field_findings_signs_respiratory_value IS NOT NULL",
  ];

  $modifier = 0;

  foreach ($constraints as $constraint_clause) {
    $at_least_one = FALSE;

    $results = db_query("SELECT DISTINCT field_acute_illness_encounter_target_id
    FROM field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
    LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
    LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_heart hea ON node.nid=hea.entity_id
    LEFT JOIN field_data_field_lungs lun ON node.nid=lun.entity_id
    LEFT JOIN field_data_field_findings_signs_general gen ON node.nid=gen.entity_id
    LEFT JOIN field_data_field_findings_signs_respiratory res ON node.nid=res.entity_id
    LEFT JOIN field_data_field_heart_rate her ON node.nid=her.entity_id
    LEFT JOIN field_data_field_respiratory_rate rer ON node.nid=rer.entity_id
    LEFT JOIN field_data_field_body_temperature bod ON node.nid=bod.entity_id
    LEFT JOIN field_data_field_dia dia ON node.nid=dia.entity_id
    LEFT JOIN field_data_field_sys sys ON node.nid=sys.entity_id
    LEFT JOIN field_data_field_muac muac ON node.nid=muac.entity_id
    LEFT JOIN field_data_field_nutrition_signs nutr ON node.nid=nutr.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$constraint_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      $ids[$result] += 1;
      $at_least_one = TRUE;
    }
    // Counts how many constraint aren't true for any IDs.
    if (!$at_least_one) {
      $modifier += 1;
    }
  }

  // Counts IDs that meet all constraints minus ones who arn't true for any IDs.
  $count = 0;
  foreach ($ids as $id) {
    if ($id == count($constraints) - $modifier) {
      ++$count;
    }
  }

  return $count;
}

/**
 * Gets various number of encouters related to the physical examination.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param string $mode
 *   Heart: completed heart rate measurments with an age
 *   heart_abnormal: abnormal heart rate measurments for age
 *   respiration: completed respiratory rate measurments with an age
 *   respiration_abnormal: completed respiratory rate measurments for age
 *   temp: completed body temp measurments with an age
 *   temp_abnormal: completed body temp measurments with an age
 *   muac: completed MUAC measurments with an age
 *   muac_abnormal: completed MUAC measurments with an age.
 *
 * @return int
 *   Number of encounters based on mode.
 */
function physical_exam_abnormal($start_date, $end_date, $name_clause, $mode) {
  $ids = array_fill_keys(get_id_list($start_date, $end_date, $name_clause), 0);

  if ($mode == "heart" || $mode == "Heart") {
    $constraints = ["AND age IS NOT NULL AND field_heart_rate_value IS NOT NULL"];
  }
  elseif ($mode == "heart_abnormal") {
    $constraints = [
      "AND age = 'mt50y' AND (field_heart_rate_value > 160 OR field_heart_rate_value < 110)",
      "AND age = 'lt2y' AND (field_heart_rate_value > 150 OR field_heart_rate_value < 100)",
      "AND age = 'lt5y' AND (field_heart_rate_value > 140 OR field_heart_rate_value < 95)",
      "AND age = 'lt10y' AND (field_heart_rate_value > 120 OR field_heart_rate_value < 80)",
      "AND (age = 'lt20y' OR age = 'lt50y') AND (field_heart_rate_value > 100 OR field_heart_rate_value < 60)",
    ];
  }
  elseif ($mode == "respiratory") {
    $constraints = ["AND age IS NOT NULL AND field_respiratory_rate_value IS NOT NULL"];
  }
  elseif ($mode == "respiratory_abnormal") {
    $constraints = [
      "AND age = 'mt50y' AND (field_respiratory_rate_value > 49 OR field_respiratory_rate_value < 30)",
      "AND (age = 'lt2y' OR age = 'lt5y') AND (field_respiratory_rate_value > 39 OR field_respiratory_rate_value < 24)",
      "AND (age = 'lt10y' OR age = 'lt20y' OR age = 'lt50y') AND (field_respiratory_rate_value > 100 OR field_respiratory_rate_value < 60)",
    ];
  }
  elseif ($mode == "temp") {
    $constraints = ["AND age IS NOT NULL AND field_body_temperature_value IS NOT NULL"];
  }
  elseif ($mode == "temp_abnormal") {
    $constraints = ["AND age IS NOT NULL AND (field_body_temperature_value > 37.5 OR field_body_temperature_value < 35)"];
  }
  elseif ($mode == "muac") {
    $constraints = ["AND age IS NOT NULL AND field_muac_value IS NOT NULL"];
  }
  elseif ($mode == "muac_abnormal") {
    $constraints = ["AND age IS NOT NULL AND field_muac_value < 14.5"];
  }

  foreach ($constraints as $constraint_clause) {
    $results = db_query("SELECT DISTINCT field_acute_illness_encounter_target_id from field_data_field_acute_illness_encounter e
    LEFT JOIN node ON e.entity_id=node.nid
	  LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
  	LEFT JOIN field_data_field_person per on ip.field_individual_participant_target_id = per.entity_id
    LEFT JOIN person_classified cla on per.field_person_target_id = cla.entity_id
    LEFT JOIN field_data_field_health_center hc ON per.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_district district ON per.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
    LEFT JOIN field_data_field_heart_rate her ON node.nid=her.entity_id
    LEFT JOIN field_data_field_respiratory_rate rer ON node.nid=rer.entity_id
    LEFT JOIN field_data_field_body_temperature bod ON node.nid=bod.entity_id
    LEFT JOIN field_data_field_muac muac ON node.nid=muac.entity_id
    WHERE FROM_UNIXTIME(node.created) >= :start_date
      {$name_clause}
      {$constraint_clause}
      AND FROM_UNIXTIME(node.created) <= :end_date",
      [
        ':start_date' => $start_date,
        ':end_date' => $end_date,
      ])->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      $ids[$result] += 1;
    }
  }
  // Count number of encounter IDs that meet at least one constraint.
  $count = 0;
  foreach ($ids as $id) {
    if ($id == 1) {
      ++$count;
    }
  }

  return $count;
}

/**
 * Gets number of completed acute illness encounters.
 *
 * @param string $start_date
 *   The starting date.
 * @param string $end_date
 *   The ending date.
 * @param string $name_clause
 *   The district/health-center SQL clause.
 * @param bool $complete
 *   When true, counts only complete encounters.
 *
 * @return int
 *   Number of completed acute illness encounters.
 */
function enconter_complete($start_date, $end_date, $name_clause, $complete = FALSE) {

  $constraint_clause = ($complete) ? "AND dt.field_scheduled_date_value2 is not null" : "";

  return db_query("SELECT COUNT(DISTINCT field_acute_illness_encounter_target_id)
  FROM field_data_field_acute_illness_encounter e
  LEFT JOIN node ON e.entity_id=node.nid
  LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
  LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
  LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
  LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
  LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
  LEFT JOIN field_data_field_scheduled_date dt ON dt.entity_id=e.field_acute_illness_encounter_target_id
  WHERE FROM_UNIXTIME(node.created) >= :start_date
    {$name_clause}
    {$constraint_clause}
    AND FROM_UNIXTIME(node.created) <= :end_date",
    [
      ':start_date' => $start_date,
      ':end_date' => $end_date,
    ])->fetchField();
}
