<?php

/**
 * @file
 * Generates 'anc implementation' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-anc-implementation.php.
 */

//TODO:
//implement start and end date
//modes: HCs, Districts, 
//separate by pregnancy to account for multiple pregnancies from the same patient
//add follow up visits and pregnancy outcomes
//add readable output option


require_once __DIR__ . '/report_common.inc';
$mode = drush_get_option('mode', FALSE);
$start_date = drush_get_option('start_date', FALSE);
$end_date = drush_get_option('end_date', FALSE);
$readable = drush_get_option('readable', FALSE);


$date_phrase = date_condition($start_date, $end_date);
$conditions = mode_set($mode, $date_phrase = "");
$ids = encounter_parsing($date_phrase);
if(!$readable) {
  echo(create_json($ids, $conditions, $date_phrase = ""));
}
else {
  readable_output($ids, $conditions, $date_phrase = "");
}


function date_condition($start_date, $end_date) {
  $condition = "";
  if($start_date) {
    $condition .= "AND FROM_UNIXTIME(n.created) >= " . $start_date . " ";
  }
  if($end_date) {
    $condition .= "AND FROM_UNIXTIME(n.created) < " . $end_date . " ";
  }
  return $condition;
}

function mode_set($mode, $date_phrase = "") {
  if(!$mode) {
    drush_print('Please specify --mode option');
    exit(1);
  }
  elseif($mode != 'H' && $mode != 'D' && $mode != 'A') {
    drush_print('Invalad mode option, correct options H: health centers, D: districts');
    exit(1);
  }
  elseif($mode == 'A') {
    $constraints = [["", "All Districts"],];
    return $constraints;
  }
  elseif($mode == 'D') {
    $results = db_query("SELECT DISTINCT district.field_district_value FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    WHERE district.field_district_value is not null
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

    $constraints = [["", "All Districts"],];
    foreach ($results as $result){
      $condition = "AND district.field_district_value = '" . $result . "' ";
      $name = strtolower($result);
      $name = ucwords($result);
      array_push($constraints, [$condition, $name . " District"]);
    }
    return $constraints;
  } elseif ($mode == 'H') {
    $results = db_query("SELECT DISTINCT hc.field_health_center_target_id FROM field_data_field_prenatal_encounter e
    LEFT JOIN node n on n.nid=e.entity_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
    LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
    LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
    LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
    WHERE hc.field_health_center_target_id is not null
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

    $constraints = [["", "All Health Centers"],];
    foreach ($results as $result){
      $condition = "AND hc.field_health_center_target_id = " . $result . " ";
      $name = strtolower(get_health_center($result));
      $name = ucwords($name);
      array_push($constraints, [$condition, $name]);
    }
    return $constraints;
  }
}

function ids_size($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);;

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

function encounter_parsing($date_phrase = "") {
  $ids = db_query("SELECT DISTINCT ip.field_individual_participant_target_id, e.field_prenatal_encounter_target_id, sd.field_scheduled_date_value FROM field_data_field_prenatal_encounter e
    LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
    LEFT JOIN field_data_field_scheduled_date sd ON sd.entity_id=e.field_prenatal_encounter_target_id
    LEFT JOIN field_data_field_prenatal_encounter_type et ON et.entity_id=e.field_prenatal_encounter_target_id
    WHERE et.field_prenatal_encounter_type_value = 'nurse'
    {$date_phrase}")->fetchAll();


//keys is ip id -> array where 0 is encounter target id and 1 is scheduled date
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


function pregnancy_dating($ids, $constraint = "", $date_phrase = "") {
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
    {$constraint}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

  $count_true = 0;

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? ++$count_true : '';
  }
  return $count_true;
}

function history($ids, $condition = "", $date_phrase = "") {
  
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

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
      {$condition}
      {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

    foreach ($results as $result) {
      array_key_exists($result, $ids) ? $ids[$result]+=1 : '';
    }

}
  foreach ($ids as $id) {
    ($id == 4) ? $counts["history"]++ : '';
  }

  return $counts;
}

function family_panning($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }
  
  return $count;
}

function medication($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function danger_sings($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}


function immunizations($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }

  return $count;
}

function symptom_review($ids, $condition, $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function mental_health($ids, $condition = "", $date_phrase = "") {
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

foreach ($results as $result) {
  array_key_exists($result, $ids) ? $count++ : '';
}

return $count;
}

function labratory($ids, $condition = "", $date_phrase = "") {
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
      {$condition}
      {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

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

function malaria_prevention($ids, $condition = "", $date_phrase = "") {
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
      {$condition}
      {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

  foreach ($results as $result) {
    array_key_exists($result, $ids) ? $count++ : '';
  }

  return $count;
}

function examination($ids, $condition = "", $date_phrase = "") {
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
    "hr" => "WHERE (hr.field_heart_rate_value < 60 OR hr.field_heart_rate_value > 100)",
    "resp" => "WHERE (rr.field_respiratory_rate_value < 14 OR rr.field_respiratory_rate_value > 28)",
    "temp" => "WHERE (bt.field_body_temperature_value < 35 OR bt.field_body_temperature_value > 37.5)",
    "bp" => "WHERE (sys.field_sys_value < 100 OR  sys.field_sys_value > 140 OR 
      dia.field_dia_value < 60 OR dia.field_dia_value > 90)",
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
    {$condition}
    {$date_phrase}")->fetchAll(PDO::FETCH_COLUMN);

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


function create_json($ids = NULL, $constraints, $date_phrase = ""){
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
    $size = ids_size($ids, $constraint[0], $date_phrase);
    $instance["Name"] = $constraint[1];
    $instance["Pregnancy Dating"] = pregnancy_dating($ids, $constraint[0], $date_phrase) . "  " . $size;
    $temp = history($ids, $constraint[0], $date_phrase);
    $instance["History"] = $temp["history"] . "  " . $size;
    $instance["Medical History"] = $temp["medical"] . "  " . $size;
    $instance["Partner Informaiton"] = $temp["partner"] . "  " . $size;
    $temp = examination($ids, $constraint[0], $date_phrase);
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
    $instance["Breast Exam"] = $temp["breast"] . "  " . $size;
    $instance["Family Planning"] = family_panning($ids, $constraint[0], $date_phrase) . "  " . $size;
    $instance["Medication"] = medication($ids, $constraint[0], $date_phrase) . "  " . $size;
    $instance["Malaria Prevention"] = malaria_prevention($ids, $constraint[0], $date_phrase) . "  " . $size;
    $instance["Danger Signs"] = danger_sings($ids, $constraint[0], $date_phrase) . "  " . $size;
    $instance["Symptom Review"] = symptom_review($ids, $constraint[0], $date_phrase) . "  " . $size;
    $temp = labratory($ids, $constraint[0], $date_phrase);
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
    $instance["Immunizations"] = immunizations($ids, $constraint[0], $date_phrase) . "  " . $size;
    $instance["Maternal Mental Health"] = mental_health($ids, $constraint[0], $date_phrase) . "  " . $size;
  
    array_push($data, $instance);
  }
  
  $json = json_encode($data);
  //file_put_contents("server/hedley/modules/custom/hedley_admin/scripts/anc-implementation.json", $json, FILE_USE_INCLUDE_PATH);
  return $json;
}

function readable_output($ids = NULL, $constraints = "") {
  $tables = ['Encounter section'];
  foreach ($constraints as $constraint) {
    array_push($tables, $constraint[1]);
  }
  $text_table = new HedleyAdminTextTable($tables);

  $encounters = [
    ["Pregnancy Dating",], ["History",], ["   Medical History",], ["   Partner Information",], ["Examination",], ["   Vitals",], ["   HR Outside Normal Values"], ["      Respiratory Rate Outside Normal Values"], ["      Body Temp Outside Normal Values"], ["      BP Outside Normal Values"], ["   Nutrition Assessment"],
    ["      MUAC Outside Normal Values"], ["    Core Physical Exam"], ["    Obstetrical Exam"], ["    Breast Exam"], ["Family Planning"], ["Medication"], ["Malaria Prevention"], ["Danger Signs"], ["Symptom Review"], ["Laboratory"], ["     Partner HIV"],
    ["     HIV RDT"], ["    Syphilis"], ["     Hep B"], ["     Malaria"], ["     Blood Group"], ["     Urine Dipstick"], ["     Hemoglobin"], ["     Random Blood Sugar"], ["     Immunizations"], ["Maternal Mental Health"],
  ];

  foreach ($constraints as $constraint) {
    $size = ids_size($ids, $constraint[0], $date_phrase);
    array_push($encounters[0], pregnancy_dating($ids, $constraint[0], $date_phrase) . "  " . $size);
    $temp = history($ids, $constraint[0], $date_phrase);
    array_push($encounters[1], $temp["history"] . " / " . $size);
    array_push($encounters[2], $temp["medical"] . " / " . $size);
    array_push($encounters[3], $temp["partner"] . " / " . $size);
    $temp = examination($ids, $constraint[0], $date_phrase);
    array_push($encounters[4], $temp["total"] . " / " . $size);
    array_push($encounters[5], $temp["vitals"] . " / " . $size);
    array_push($encounters[6], $temp["hr"] . " / " . $temp["vitals"]);
    array_push($encounters[7], $temp["resp"] . " / " . $temp["vitals"]);
    array_push($encounters[8], $temp["temp"] . " / " . $temp["vitals"]);
    array_push($encounters[9], $temp["bp"] . " / " . $temp["vitals"]);
    array_push($encounters[10], $temp["nutrition"] . " / " . $size);
    array_push($encounters[11], $temp["muac"] . " / " . $temp["nutrition"]);
    array_push($encounters[12], $temp["core"] . " / " . $size);
    array_push($encounters[13], $temp["obstetrical"] . " / " . $size);
    array_push($encounters[14], $temp["brest"] . " / " . $size);
    array_push($encounters[15], family_panning($ids, $constraint[0], $date_phrase) . " / " . $size);
    array_push($encounters[16], medication($ids, $constraint[0], $date_phrase) . " / " . $size);
    array_push($encounters[17], malaria_prevention($ids, $constraint[0], $date_phrase) . " / " . $size);
    array_push($encounters[18], danger_sings($ids, $constraint[0], $date_phrase) . " / " . $size);
    array_push($encounters[19], symptom_review($ids, $constraint[0], $date_phrase) . " / " . $size);
    $temp = labratory($ids, $constraint[0], $date_phrase);
    array_push($encounters[20], $temp[9] . " / " . $size);
    array_push($encounters[21], $temp[0] . " / " . $size);
    array_push($encounters[22], $temp[1] . " / " . $size);
    array_push($encounters[23], $temp[2] . " / " . $size);
    array_push($encounters[24], $temp[3] . " / " . $size);
    array_push($encounters[25], $temp[4] . " / " . $size);
    array_push($encounters[26], $temp[5] . " / " . $size);
    array_push($encounters[27], $temp[6] . " / " . $size);
    array_push($encounters[28], $temp[7] . " / " . $size);
    array_push($encounters[29], $temp[8] . " / " . $size);
    array_push($encounters[30], immunizations($ids, $constraint[0], $date_phrase) . " / " . $size);
    array_push($encounters[31], mental_health($ids, $constraint[0], $date_phrase) . " / " . $size);
  }

  drush_print($text_table->render($encounters));
}