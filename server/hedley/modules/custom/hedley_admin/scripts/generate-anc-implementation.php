<?php

/**
 * @file
 * Generates 'anc implementation' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-anc-implementation.php.
 */

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

function build_table () {

}






///HC first antenatal encounter

//pregnancy Dating
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
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
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_obstetric_history oh ON oh.entity_id=n.nid

//Medical History NOTE!!!! multiple data points when 2 or more boxes checked
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_medical_history mh ON mh.entity_id=n.nid

//PARTNER INFO 
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_social_history sh ON sh.entity_id=n.nid

//VITALS (heart rate, Blood Pressure, resp rate, body temp)
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
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

//nutrition assesment NOTE: neet ot manually calculate BMI as not linked with bmi table
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
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
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_breast_self_exam bse ON bse.entity_id=n.nid
LEFT JOIN field_data_field_breast b ON b.entity_id=n.nid


//family planning
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_family_planning_signs fp ON fp.entity_id=n.nid

//medicine (possible results include deworming-pill, folic-acid, iron-and-folic-acid-supplement, mebendezole, none, vitamin-a)
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_medication s ON s.entity_id=n.nid

//danger signs
SELECT DISTINCT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_danger_signs ds ON ds.entity_id=n.nid

//symptom review
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_prenatal_symptoms ps ON ps.entity_id=n.nid

//mental health signs
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_mental_health_signs mhs ON mhs.entity_id=n.nid

//LABROTORY
//partner hiv testing? ONLY GOOD IF ACTUALLY TOOK TEST. CANT FIND ROUT FOR NO TEST
SELECT * FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_partner_hiv_testing ht ON ht.entity_id=n.nid

//labretory for All tests completed except malaria and partner hiv    -- also includes "vitals-recheck"
SELECT DISTINCT mhs.field_completed_tests_value FROM field_data_field_prenatal_encounter e
LEFT JOIN node n on n.nid=e.entity_id
LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
LEFT JOIN field_data_field_person pr ON pr.entity_id=ip.field_individual_participant_target_id
LEFT JOIN field_data_field_health_center hc ON pr.field_person_target_id=hc.entity_id
LEFT JOIN field_data_field_birth_date bd ON bd.entity_id=pr.field_person_target_id
LEFT JOIN field_data_field_district district ON pr.field_person_target_id=district.entity_id
LEFT JOIN field_data_field_completed_tests mhs ON mhs.entity_id=n.nid

//