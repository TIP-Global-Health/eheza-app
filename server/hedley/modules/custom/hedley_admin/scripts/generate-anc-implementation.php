<?php

/**
 * @file
 * Generates 'anc implementation' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-anc-implementation.php.
 */

 /*
require_once __DIR__ . '/report_common.inc';
$start_date = drush_get_option('start_date', FALSE);
$end_date = drush_get_option('end_date', FALSE);
$name = drush_get_option('name', FALSE);
$type = drush_get_option('mode', FALSE);
$print = drush_get_option('print', FALSE);


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
  drush_print("# Child Nutrition Completion Report - Mode: Health Center - " . get_health_center($name) . " - " . $start_date . " - " . $end_date);
  $name_clause = "AND (field_ai_encounter_type_value='nurse-encounter' OR field_ai_encounter_type_value is NULL) AND field_health_center_target_id='$name'";
}
elseif ($type == 'D') {
  $name = get_district_name($name);
  drush_print("# Child Nutrition Completion Report - Mode: Districts - " . $name . " - " . $start_date . " - " . $end_date);
  $name_clause = "AND field_district_value='$name'";
}
else {
  drush_print("# Child Nutrition Completion Report - Mode: ALL - " . $start_date . " - " . $end_date);
}

$encounters = [];


$text_table = new HedleyAdminTextTable([
  'Encounter section',
  'Complete / All',
  'Percent Complete',
]);
*/

$conditions = [
  ["", "All Districts"],
  ["AND district.field_district_value = 'Gakenke'", "Gakenke District"],
  ["AND district.field_district_value = 'Rulindo'", "Rulindo District"],
  ["AND district.field_district_value = 'BUGESERA'", "Bugesera District"],
  ["AND district.field_district_value = 'Gasabo'", "Gasabl District"],
  ["AND district.field_district_value = 'Nyamata'", "Nyamata District"],
  ["AND district.field_district_value = 'Nyarugenge'", "Nyarugenge District"],
];

$ids = first_and_subsequent_encounter();
echo( create_json($ids, $conditions));


function ids_size($ids, $condition) {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    WHERE et.field_prenatal_encounter_type_value = 'nurse'
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);;

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }

  return $count;
}




//drush_print($text_table->render($encounters));

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

function first_and_subsequent_encounter() {
  $ids = db_query("SELECT DISTINCT ip.field_individual_participant_target_id, e.field_prenatal_encounter_target_id, sd.field_scheduled_date_value FROM field_data_field_prenatal_encounter e
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_scheduled_date sd ON sd.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    WHERE et.field_prenatal_encounter_type_value = 'nurse'")->fetchAll();

  $ip_ids = [];

  foreach ($ids as $id) {
    if(array_key_exists($id->field_individual_participant_target_id, $ip_ids)) {
      if($ip_ids[$id->field_individual_participant_target_id][1] > $id->field_scheduled_date_value) {
        $ip_ids[$id->field_individual_participant_target_id][0] = $id->field_prenatal_encounter_target_id;
        $ip_ids[$id->field_individual_participant_target_id][1] = $id->field_scheduled_date_value;
      }
      else {
        //put in subsequent visits array
      }
    }
    else {
      $ip_ids[$id->field_individual_participant_target_id] = [$id->field_prenatal_encounter_target_id, $id->field_scheduled_date_value];
    }
  }
  
  $encounter_ids = [];

  foreach ($ip_ids as $ip_id) {
    $encounter_ids[$ip_id[0]] = 0;
  }

  return $encounter_ids;
}

function pregnancy_dating($ids, $constraint = "") {
  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_last_menstrual_period lmp ON lmp.entity_id=n.nid
    LEFT JOIN field_data_field_confident c ON c.entity_id=n.nid
    LEFT JOIN field_data_field_not_confident_reason ncr ON ncr.entity_id=n.nid
    WHERE (c.field_confident_value = 1 
    OR (c.field_confident_value = 0 AND ncr.field_not_confident_reason_value is not null))
    {$constraint}")->fetchAll(PDO::FETCH_COLUMN);

  $count_true = 0;

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? ++$count_true : '';
  }
  return $count_true;
}

function history($ids, $condition = "") {
  
  $counts = [
    "medical" => 0,
    "partner" => 0,
    "history" => 0,
  ];
  $constraints = ["WHERE oh.field_obstetric_history_value is not null",
    "WHERE cp.field_currently_pregnant_value is not null",
  ]; 

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_medical_history mh ON mh.entity_id=n.nid
    WHERE mh.field_medical_history_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    if (array_key_exists($result, $ids)) {
      $counts["medical"]++;
      $ids[$result]+= 1;
    }
  }

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_social_history sh ON sh.entity_id=n.nid
    WHERE sh.field_social_history_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    if (array_key_exists($result, $ids)) {
      $counts["partner"]++;
      $ids[$result]+= 1;
    }
  }

  foreach ($constraints as $constraint) {
    $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
      LEFT JOIN node n on n.nid=e.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
      LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
      LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_currently_pregnant cp ON cp.entity_id=n.nid
      LEFT JOIN field_data_field_obstetric_history oh ON oh.entity_id=n.nid
      {$constraint}
      {$condition}")->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      array_key_exists($result, $ids) ? $ids[$result]+=1 : '';
    }

}
  foreach ($ids as $id) {
    ($id == 4) ? $counts["history"]++ : '';
  }

  return $counts;
}

function family_panning($ids, $condition = "") {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_family_planning_signs fp ON fp.entity_id=n.nid
    WHERE fp.field_family_planning_signs_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }
  
  return $count;
}

function medication($ids, $condition = "") {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_medication s ON s.entity_id=n.nid
    WHERE s.field_medication_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function danger_sings($ids, $condition = "") {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_danger_signs ds ON ds.entity_id=n.nid
    WHERE ds.field_danger_signs_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}


function immunizations($ids, $condition = "") {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_execution_date ed ON ed.entity_id=n.nid
    LEFT JOIN field_data_field_administered_doses ad ON ad.entity_id=n.nid
    WHERE ad.field_administered_doses_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }

  return $count;
}

function symptom_review($ids, $condition) {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_prenatal_symptoms ps ON ps.entity_id=n.nid
    WHERE ps.field_prenatal_symptoms_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function mental_health($ids, $condition = "") {
  $count = 0;

  $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_mental_health_signs mhs ON mhs.entity_id=n.nid
    WHERE mhs.field_mental_health_signs_value is not null
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function labratory($ids, $condition = "") {
  $counts = [0,0,0,0,0,0,0,0,0,0];

  $constraints = [
    "WHERE ten.bundle = 'prenatal_partner_hiv_test'",
    "WHERE ten.bundle = 'prenatal_hiv_pcr_test' OR ten.bundle = 'prenatal_hiv_test'",
    "WHERE ten.bundle = 'prenatal_syphilis_test'",
    "WHERE ten.bundle = 'prenatal_hepatitis_b_test'",
    "WHERE ten.bundle = 'prenatal_malaria_test'",
    "WHERE ten.bundle = 'prenatal_blood_gprs_test'",
    "WHERE ten.bundle = 'prenatal_urine_dipstick_test'",
    "WHERE ten.bundle = 'prenatal_hemoglobin_test'",
    "WHERE ten.bundle = 'prenatal_random_blood_sugar_test'",
  ];

  for($i = 0; $i < sizeof($constraints); $i++) {
    $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
      LEFT JOIN node n on n.nid=e.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
      LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
      LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_test_execution_note ten ON ten.entity_id=n.nid
      {$constraints[$i]}
      {$condition}")->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
     if (array_key_exists($result, $ids)) {
        ++$ids[$result];
        ++$counts[$i];
      }
    }
  }
  
  $total = 0;
  foreach ($ids as $id) {
    ($id == sizeof($constraints)) ? ++$total : '';
  }

  $counts[sizeof($constraints)] = $total;

  return $counts;
}

function malaria_prevention($ids, $condition = "") {
  $count = 0;

    $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
      LEFT JOIN node n on n.nid=e.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
      LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
      LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_medication s ON s.entity_id=n.nid
      WHERE e.bundle = 'resource'
      {$condition}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }

  return $count;
}

function examination($ids, $condition) {
  $counts = [
      "vitals" => 0,
      "hr" => 0,
      "resp" => 0,
      "temp" => 0,
      "bp" => 0,
      "nutrition" => 0,
      "weight" => 0,
      "muac" => 0,
      "core" => 0,
      "obstetrical" => 0,
      "brest" => 0,
      "total" => 0,
  ];

  $constraints = [
    "vitals" => "WHERE sys.field_sys_value is not null AND hr.field_heart_rate_value is not null",
    "hr" => "WHERE hr.field_heart_rate_value < 60 OR hr.field_heart_rate_value > 100",
    "resp" => "WHERE rr.field_respiratory_rate_value < 14 OR rr.field_respiratory_rate_value > 28",
    "temp" => "WHERE bt.field_body_temperature_value < 35 OR bt.field_body_temperature_value > 37.5",
    "bp" => "WHERE sys.field_sys_value < 100 OR  sys.field_sys_value > 140 OR 
      dia.field_dia_value < 60 OR dia.field_dia_value > 90",
    "nutrition" => "WHERE w.field_weight_value is not null",
    "muac" => "WHERE m.field_muac_value < 11.5",
    "core" => "WHERE a.field_abdomen_value is not null",
    "obstetrical" => "WHERE fm.field_fetal_movement_value is not null",
    "brest" => "WHERE b.field_breast_value is not null",
  ];


  foreach ($constraints as $key => $constraint) {
    $results = 0;
    $results = db_query("SELECT DISTINCT e.field_prenatal_encounter_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    LEFT JOIN field_data_field_dia dia ON dia.entity_id=n.nid
    LEFT JOIN field_data_field_sys sys ON sys.entity_id=n.nid
    LEFT JOIN field_data_field_heart_rate hr ON hr.entity_id=n.nid
    LEFT JOIN field_data_field_respiratory_rate rr ON rr.entity_id=n.nid
    LEFT JOIN field_data_field_body_temperature bt ON bt.entity_id=n.nid
    LEFT JOIN field_data_field_height h ON h.entity_id=n.nid
    LEFT JOIN field_data_field_weight w ON w.entity_id=n.nid
    LEFT JOIN field_data_field_muac m ON m.entity_id=n.nid
    LEFT JOIN field_data_field_abdomen a ON a.entity_id=n.nid
    LEFT JOIN field_data_field_fetal_movement fm ON fm.entity_id=n.nid
    LEFT JOIN field_data_field_breast b ON b.entity_id=n.nid
    {$constraint}
    {$condition}")->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      if(array_key_exists($result, $ids)) {
        $counts[$key]++;
        ($key == 'vitals' || $key == "nutrition" || $key == "core" || $key == "obstetrical" || $key == "brest") ? ++$ids[$result] : '';
      }
    }
  }

  foreach($ids as $id) {
    ($id == 5) ? $counts['total']++ : '';
  }  

  return $counts;
}


function create_json($ids = NULL, $constraints){
  $data = [];
  $instance_template = [
    "Name" => "",
    "Pregnancy Dating" => 0,
    "History" => 0,
    "Medical History" => 0,
    "Partner Informaiton" => 0,
    "Examination" => 0,
    "Vitals" => 0,
    "HR" => 0,
    "Respiratory Rate" => 0,
    "Body Temp" => 0,
    "BP" => 0,
    "Nutrition Assessment" => 0,
    "Weight" => 0,
    "BMI" => 0,
    "MUAC" => 0,
    "Core Physical Exam" => 0,
    "Obstetrical Exam" => 0,
    "Brest Exam" => 0,
    "Family Planning" => 0,
    "Medication" => 0,
    "Malaria Prevention" => 0,
    "Danger Signs" => 0,
    "Symptom Review" => 0,
    "Laboratory" => 0,
    "Partner HIV" => 0,
    "HIV RDT" => 0,
    "Syphilis" => 0,
    "Hep B" => 0,
    "Malaria" => 0,
    "Blood Group" => 0,
    "Urine Dipstick" => 0,
    "Hemoglobin" => 0,
    "Random Blood Sugar" => 0,
    "Immunizations" => 0,
    "Maternal Mental Health" => 0,
  ];

  foreach ($constraints as $constraint) {
    $instance = $instance_template;
    $size = ids_size($ids, $constraint[0]);
    $instance["Name"] = $constraint[1];
    $instance["Pregnancy Dating"] = pregnancy_dating($ids, $constraint[0]) . "  " . $size;
    $temp = history($ids, $constraint[0]);
    $instance["History"] = $temp["history"] . "  " . $size;
    $instance["Medical History"] = $temp["medical"] . "  " . $size;
    $instance["Partner Informaiton"] = $temp["partner"] . "  " . $size;
    $temp = examination($ids, $constraint[0]);
    $instance["Examination"] = $temp["total"] . "  " . $size;
    $instance["Vitals"] = $temp["vitals"] . "  " . $size;
    $instance["HR"] = $temp["hr"] . "  " . $temp["vitals"];
    $instance["Respiratory Rate"] = $temp["resp"] . "  " . $temp["vitals"];
    $instance["Body Temp"] = $temp["temp"] . "  " . $temp["vitals"];
    $instance["BP"] = $temp["bp"] . "  " . $temp["vitals"];
    $instance["Nutrition Assessment"] = $temp["nutrition"] . "  " . $size;
    $instance["MUAC"] = $temp["muac"] . "  " . $temp["nutrition"];
    $instance["Core Physical Exam"] = $temp["core"] . "  " . $size;
    $instance["Obstetrical Exam"] = $temp["obstetrical"] . "  " . $size;
    $instance["Brest Exam"] = $temp["brest"] . "  " . $size;
    $instance["Family Planning"] = family_panning($ids, $constraint[0]) . "  " . $size;
    $instance["Medication"] = medication($ids, $constraint[0]) . "  " . $size;
    $instance["Malaria Prevention"] = malaria_prevention($ids, $constraint[0]) . "  " . $size;
    $instance["Danger Signs"] = danger_sings($ids, $constraint[0]) . "  " . $size;
    $instance["Symptom Review"] = symptom_review($ids, $constraint[0]) . "  " . $size;
    $temp = labratory($ids, $constraint[0]);
    $instance["Laboratory"] = $temp[9] . "  " . $size;
    $instance["Partner HIV"] = $temp[0] . "  " . $size;
    $instance["HIV RDT"] = $temp[1] . "  " . $size;
    $instance["Syphilis"] = $temp[2] . "  " . $size;
    $instance["Hep B"] = $temp[3] . "  " . $size;
    $instance["Malaria"] = $temp[4] . "  " . $size;
    $instance["Blood Group"] = $temp[5] . "  " . $size;
    $instance["Urine Dipstick"] = $temp[6] . "  " . $size;
    $instance["Hemoglobin"] = $temp[7] . "  " . $size;
    $instance["Random Blood Sugar"] = $temp[8] . "  " . $size;
    $instance["Immunizations"] = immunizations($ids, $constraint[0]) . "  " . $size;
    $instance["Maternal Mental Health"] = mental_health($ids, $constraint[0]) . "  " . $size;
  
    array_push($data, $instance);
  }
  
  $json = json_encode($data);
  file_put_contents("server/hedley/modules/custom/hedley_admin/scripts/anc-implementation.json", $json, FILE_USE_INCLUDE_PATH);
  return $json;
}






/*

// POST PARTUM

//





//IMPLEMENTED
//VITALS (heart rate, Blood Pressure, resp rate, body temp)
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_dia dia ON dia.entity_id=n.nid
LEFT JOIN field_data_field_sys sys ON sys.entity_id=n.nid
LEFT JOIN field_data_field_heart_rate hr ON hr.entity_id=n.nid
LEFT JOIN field_data_field_respiratory_rate rr ON rr.entity_id=n.nid
LEFT JOIN field_data_field_body_temperature bt ON bt.entity_id=n.nid

//nutrition assesment NOTE: neet to manually calculate BMI as not linked with bmi table
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_height h ON h.entity_id=n.nid
LEFT JOIN field_data_field_weight w ON w.entity_id=n.nid
LEFT JOIN field_data_field_muac m ON m.entity_id=n.nid

//core physical exam
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_head_hair hh ON hh.entity_id=n.nid
LEFT JOIN field_data_field_eyes eye ON eye.entity_id=n.nid
LEFT JOIN field_data_field_neck nec ON nec.entity_id=n.nid
LEFT JOIN field_data_field_heart h ON h.entity_id=n.nid
LEFT JOIN field_data_field_heart_murmur hm ON hm.entity_id=n.nid
LEFT JOIN field_data_field_lungs l ON l.entity_id=n.nid
LEFT JOIN field_data_field_abdomen a ON a.entity_id=n.nid
LEFT JOIN field_data_field_hands hand ON hand.entity_id=n.nid
LEFT JOIN field_data_field_legs leg ON leg.entity_id=n.nid

//Obstetrical Exam
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_fundal_height fh ON fh.entity_id=n.nid
LEFT JOIN field_data_field_fetal_presentation fp ON fp.entity_id=n.nid
LEFT JOIN field_data_field_fetal_movement fm ON fm.entity_id=n.nid
LEFT JOIN field_data_field_fetal_heart_rate fhr ON fhr.entity_id=n.nid
LEFT JOIN field_data_field_c_section_scar css ON css.entity_id=n.nid

//Brest Exam
SELECT DISTINCT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_breast_self_exam bse ON bse.entity_id=n.nid
LEFT JOIN field_data_field_breast b ON b.entity_id=n.nid

//mental health signs
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_mental_health_signs mhs ON mhs.entity_id=n.nid

//medicine (possible results include deworming-pill, folic-acid, iron-and-folic-acid-supplement, mebendezole, none, vitamin-a)
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_medication s ON s.entity_id=n.nid

//pregnancy Dating
SELECT DISTINCT FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_last_menstrual_period lmp ON lmp.entity_id=n.nid
LEFT JOIN field_data_field_confident c ON c.entity_id=n.nid
LEFT JOIN field_data_field_not_confident_reason ncr ON ncr.entity_id=n.nid

//Obstetric History page 1
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_currently_pregnant cp ON cp.entity_id=n.nid
LEFT JOIN field_revision_field_preterm_pregnancy ptp ON ptp.entity_id=n.nid
LEFT JOIN field_data_field_term_pregnancy tp ON tp.entity_id=n.nid
LEFT JOIN field_data_field_stillbirths_at_term sat ON sat.entity_id=n.nid
LEFT JOIN field_data_field_stillbirths_preterm spt ON spt.entity_id=n.nid
LEFT JOIN field_data_field_abortions ab ON ab.entity_id=n.nid
LEFT JOIN field_data_field_live_children lc ON lc.entity_id=n.nid

//Obstetric History page 2
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_obstetric_history oh ON oh.entity_id=n.nid

//Medical History NOTE!!!! multiple data points when 2 or more boxes checked
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_medical_history mh ON mh.entity_id=n.nid

//PARTNER INFO 
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_social_history sh ON sh.entity_id=n.nid

//family planning
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_family_planning_signs fp ON fp.entity_id=n.nid

//danger signs
SELECT DISTINCT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_danger_signs ds ON ds.entity_id=n.nid

//symptom review
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_prenatal_symptoms ps ON ps.entity_id=n.nid

//LABROTORY
//partner hiv testing? ONLY GOOD IF ACTUALLY TOOK TEST. 
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_execution_date ed ON ed.entity_id=n.nid
LEFT JOIN field_data_field_test_execution_note ten ON ten.entity_id=n.nid
WHERE e.bundle="prenatal_partner_hiv_test"

//labretory for All tests completed except malaria and partner hiv    -- also includes "vitals-recheck"
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
//LEFT JOIN field_data_field_completed_tests mhs ON mhs.entity_id=n.nid  -- dont think needed
LEFT JOIN field_data_field_test_execution_note ten ON ten.entity_id=n.nid

//Labretory Malaria tests
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_test_execution_note ten ON ten.entity_id=n.nid
WHERE e.bundle="prenatal_malaria_test"

//immunizations  --  results can be different number of doses only
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_execution_date ed ON ed.entity_id=n.nid
LEFT JOIN field_data_field_administered_doses ad ON ad.entity_id=n.nid

///HC first antenatal encounter
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_mental_health_signs mhs ON mhs.entity_id=n.nid

*/