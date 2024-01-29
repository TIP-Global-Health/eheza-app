<?php

/**
 * @file
 * Generates 'acute illness completion' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-acute-illness-completion-report.php.
 */

class Patient {
  $person_id;
  $birth_date;
  $bcg_dose = 0;
  $dtp_dose = 0;
  $mr_dose = 0;
  $opv_dose = 0;
  $pcv13_dose = 0;
  $rotarix_dose = 0;
  $hpv_dose = 0;
  $albendazol_date = null;
  $vitamin_a_dose = null;

}







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



function 