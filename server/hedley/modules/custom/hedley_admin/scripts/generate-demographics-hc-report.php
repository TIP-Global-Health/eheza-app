<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
 */

require_once __DIR__ . '/report_common.inc';

$limit_date = drush_get_option('limit_date', FALSE);
$center_name = drush_get_option('region', FALSE);
if (!$limit_date) {
  drush_print('Please specify --limit_date option');
  exit;
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
  if ($health_center_name) {
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
  else {
    return NULL;
  }
}

$center_id = get_health_center_id($center_name);

// We need to filter for all the measurements at several places,
// but it's a bad idea to hardcode the list, so we generate a piece of SQL
// here in advance.
$types = hedley_general_get_measurement_types();
array_walk($types, function (&$val) {
  $val = "'$val'";
});
$measurement_types_sql_list = implode(', ', $types);

/**
 * Fetches registered / classified count from the temporary helper table.
 *
 * @param string $age
 *   Age classifier string.
 * @param string $gender
 *   Male or female.
 * @param string $center_id
 *   The administrative district.
 *
 * @return int
 *   Amount of patients.
 */
function classified_count($age, $gender, $center_id = NULL) {
  if ($center_id) {
    if ($age === 'all' && $gender === 'all') {
      return db_query("SELECT COUNT(*)
      FROM person_classified
      LEFT JOIN field_data_field_health_center hc ON person_classified.entity_id=hc.entity_id
      WHERE field_health_center_target_id = '$center_id'")->fetchField();
    }
    else {
      return db_query("SELECT
      COUNT(*)
      FROM
      person_classified
      LEFT JOIN field_data_field_health_center hc ON person_classified.entity_id=hc.entity_id
      WHERE field_health_center_target_id = '$center_id'
      AND age = :age AND
      gender = :gender
      ", [
        ':age' => $age,
        ':gender' => $gender,
      ])->fetchField();
    }
  }
  else {
    if ($age === 'all' && $gender === 'all') {
      return db_query("SELECT COUNT(*)
      FROM person_classified")->fetchField();
    }
    else {
      return db_query("SELECT
      COUNT(*)
      FROM
      person_classified
      WHERE age = :age AND
      gender = :gender
      ", [
        ':age' => $age,
        ':gender' => $gender,
      ])->fetchField();
    }
  }
}

/**
 * Fetches impacted count from the temporary helper table.
 *
 * @param string $age
 *   Age classifier string.
 * @param string $gender
 *   Male or female.
 * @param string $center_id
 *   The administrative district.
 *
 * @return int
 *   Amount of patients.
 */
function impacted_count($age, $gender, $center_id = NULL) {

  if ($center_id) {
    if ($age === 'all' && $gender === 'all') {
      return (int) db_query("SELECT COUNT(*)
        FROM person_classified cl
        INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id
        LEFT JOIN field_data_field_health_center hc ON cl.entity_id=hc.entity_id
        WHERE field_health_center_target_id = '$center_id'")->fetchField();
    }
    else {
      return (int) db_query("
      SELECT
          COUNT(*)
        FROM
          person_classified cl
          INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id
          LEFT JOIN field_data_field_health_center hc ON cl.entity_id=hc.entity_id
          WHERE field_health_center_target_id = '$center_id'
          AND age = :age
          AND gender = :gender
        ", [
          ':age' => $age,
          ':gender' => $gender,
        ])->fetchField();
    }
  }
  else {
    if ($age === 'all' && $gender === 'all') {
      return (int) db_query("SELECT COUNT(*)
        FROM person_classified cl
        INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id")->fetchField();
    }
    else {
      return (int) db_query("
      SELECT
          COUNT(*)
        FROM
          person_classified cl
          INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id
          AND age = :age
          AND gender = :gender
        ", [
          ':age' => $age,
          ':gender' => $gender,
        ])->fetchField();
    }
  }
}

/**
 * Counts encounter types.
 *
 * @param string $type
 *   Encounter type.
 * @param mixed $filter
 *   Filter type 'hc' or NULL.
 * @param string $limit
 *   The date limit.
 * @param string $center_id
 *   The administrative district.
 *
 * @return int
 *   Amount of encounters.
 */
function encounter_all_count($type, $filter = NULL, $limit = NULL, $center_id = NULL) {
  $center_clause = ($center_id) ? "AND field_health_center_target_id ='$center_id'" : "";

  if ($filter === 'hc' && $type == 'prenatal') {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT field_prenatal_encounter_target_id)
      FROM field_data_field_prenatal_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type t ON e.field_prenatal_encounter_target_id=t.entity_id
      WHERE (field_prenatal_encounter_type_value='nurse'
        OR field_prenatal_encounter_type_value is NULL)
        {$center_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }
  elseif ($filter === 'hc' && $type == 'acute_illness') {
    // Health center AI.
    return db_query("SELECT COUNT(DISTINCT field_acute_illness_encounter_target_id)
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
      WHERE (field_ai_encounter_type_value='nurse-encounter'
        OR field_ai_encounter_type_value is NULL)
        {$center_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();

  }
  else {
    return db_query("SELECT COUNT(DISTINCT field_{$type}_encounter_target_id)
      FROM field_data_field_{$type}_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_{$type}_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      WHERE 
      FROM_UNIXTIME(node.created) < '$limit'
      {$center_clause}")->fetchField();
  }
}

/**
 * Counts encounter types among unique patients.
 *
 * @param string $type
 *   Encounter type.
 * @param mixed $filter
 *   Filter type 'hc' or NULL.
 * @param string $limit
 *   The date limit.
 * @param string $center_id
 *   The administrative district.
 *
 * @return int
 *   Amount of encounters.
 */
function encounter_unique_count($type, $filter = NULL, $limit = NULL, $center_id = NULL) {
  $center_clause = ($center_id) ? "AND field_health_center_target_id = '$center_id'" : "";

  if ($filter === 'hc' && $type == 'prenatal') {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_prenatal_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type t on e.field_prenatal_encounter_target_id=t.entity_id
        WHERE (field_prenatal_encounter_type_value='nurse'
          OR field_prenatal_encounter_type_value is NULL)
          {$center_clause}
          AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }

  elseif ($filter === 'hc' && $type == 'acute_illness') {
    // Health center AI.
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t on e.field_acute_illness_encounter_target_id=t.entity_id
        WHERE (field_ai_encounter_type_value='nurse-encounter'
          OR field_ai_encounter_type_value is NULL)
          {$center_clause}
          AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }
  else {
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_{$type}_encounter e
      LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_{$type}_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_health_center hc ON person.field_person_target_id=hc.entity_id
      WHERE 
        FROM_UNIXTIME(node.created) < '$limit'
        {$center_clause}")->fetchField();
  }
}

$bootstrap_data_structures = file_get_contents(__DIR__ . '/bootstrap-demographics-report.SQL');
$commands = explode(';', $bootstrap_data_structures);
$k = 0;
foreach ($commands as $command) {
  if (empty($command)) {
    continue;
  }
  $command = str_replace('__MEASUREMENT_TYPES_LIST__', $measurement_types_sql_list, $command);
  db_query($command, [':limit' => $limit_date]);
}
$group_encounter_all = group_encounter_all($measurement_types_sql_list, $limit_date, $center_id);
$group_encounter_unique = group_encounter_unique($measurement_types_sql_list, $limit_date, $center_id);

$health_center_name = get_health_center($center_id);

drush_print("# Demographics report - " . $health_center_name . " - " . $limit_date);

drush_print("## REGISTERED PATIENTS");

$registered = [
  [
    '0 - 1M',
    classified_count('lt1m', 'male', $center_id),
    classified_count('lt1m', 'female', $center_id),
  ],
  [
    '1M - 2Y',
    classified_count('lt2y', 'male', $center_id),
    classified_count('lt2y', 'female', $center_id),
  ],
  [
    '2Y - 5Y',
    classified_count('lt5y', 'male', $center_id),
    classified_count('lt5y', 'female', $center_id),
  ],
  [
    '5Y - 10Y',
    classified_count('lt10y', 'male', $center_id),
    classified_count('lt10y', 'female', $center_id),
  ],
  [
    '10Y - 20Y',
    classified_count('lt20y', 'male', $center_id),
    classified_count('lt20y', 'female', $center_id),
  ],
  [
    '20Y - 50Y',
    classified_count('lt50y', 'male', $center_id),
    classified_count('lt50y', 'female', $center_id),
  ],
  [
    '50Y +',
    classified_count('mt50y', 'male', $center_id),
    classified_count('mt50y', 'female', $center_id),
  ],
  [
    'TOTAL',
    '',
    classified_count('all', 'all', $center_id),
  ],
];
$text_table = new HedleyAdminTextTable(['Registered', 'Male', 'Female']);
$text_table->addData($registered);

drush_print($text_table->render());

$impacted = [
  [
    '0 - 1M',
    impacted_count('lt1m', 'male', $center_id),
    impacted_count('lt1m', 'female', $center_id),
  ],
  [
    '1M - 2Y',
    impacted_count('lt2y', 'male', $center_id),
    impacted_count('lt2y', 'female', $center_id),
  ],
  [
    '2Y - 5Y',
    impacted_count('lt5y', 'male', $center_id),
    impacted_count('lt5y', 'female', $center_id),
  ],
  [
    '5Y - 10Y',
    impacted_count('lt10y', 'male', $center_id),
    impacted_count('lt10y', 'female', $center_id),
  ],
  [
    '10Y - 20Y',
    impacted_count('lt20y', 'male', $center_id),
    impacted_count('lt20y', 'female', $center_id),
  ],
  [
    '20Y - 50Y',
    impacted_count('lt50y', 'male', $center_id),
    impacted_count('lt50y', 'female', $center_id),
  ],
  [
    '50Y +',
    impacted_count('mt50y', 'male', $center_id),
    impacted_count('mt50y', 'female', $center_id),
  ],
  [
    'TOTAL',
    '',
    impacted_count('all', 'all', $center_id),
  ],
];
$text_table = new HedleyAdminTextTable([
  'Impacted (2+ visits)',
  'Male',
  'Female',
]);
$text_table->addData($impacted);

drush_print($text_table->render());

drush_print("## ENCOUNTERS");

/**
 * Gets the name of the health center.
 *
 * @param string $health_center_id
 *   The node ID of the healthcenter.
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
 * Gathers group encounter visits by type.
 *
 * @return array
 *   Associative array, keyed by type.
 */
function group_encounter_all($measurement_types_list, $limit = NULL, $center_id = NULL) {
  $center_clause = ($center_id) ? " AND field_health_center_target_id = '$center_id'" : "";

  return db_query("
  SELECT
  field_group_type_value as type, COUNT(*) as counter
FROM
  (
    SELECT
      field_group_type_value,
      p.field_person_target_id,
      sess_rel.field_session_target_id
    FROM
      field_data_field_session sess_rel
        LEFT JOIN field_data_field_clinic c ON sess_rel.field_session_target_id = c.entity_id
        LEFT JOIN field_data_field_group_type gt ON field_clinic_target_id = gt.entity_id
        LEFT JOIN field_data_field_person p ON p.entity_id = sess_rel.entity_id
        LEFT JOIN person_classified class ON p.field_person_target_id = class.entity_id
        LEFT JOIN field_data_field_health_center hc ON p.field_person_target_id=hc.entity_id
        LEFT JOIN node ON sess_rel.entity_id = node.nid
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
        AND field_group_type_value IS NOT NULL
        AND class.entity_id IS NOT NULL
        {$center_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'
    GROUP BY
      field_group_type_value, field_person_target_id, field_session_target_id
  ) b
GROUP BY
  field_group_type_value;
  ")->fetchAllAssoc('type');
}

/**
 * Gathers group encounter patients by type.
 *
 * @return array
 *   Amount of patients by type.
 */
function group_encounter_unique($measurement_types_list, $limit = NULL, $center_id = NULL) {
  $center_clause = ($center_id) ? "AND field_health_center_target_id = '$center_id'" : "";

  return db_query("
  SELECT
  field_group_type_value as type, COUNT(*) as counter
FROM
(
  SELECT
      field_group_type_value,
      p.field_person_target_id,
      sess_rel.field_session_target_id
    FROM
      field_data_field_session sess_rel
        LEFT JOIN field_data_field_clinic c ON sess_rel.field_session_target_id = c.entity_id
        LEFT JOIN field_data_field_group_type gt ON field_clinic_target_id = gt.entity_id
        LEFT JOIN field_data_field_person p ON p.entity_id = sess_rel.entity_id
        LEFT JOIN person_classified class ON p.field_person_target_id = class.entity_id
        LEFT JOIN field_data_field_health_center hc ON p.field_person_target_id=hc.entity_id
        LEFT JOIN node ON sess_rel.entity_id = node.nid
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
        AND field_group_type_value IS NOT NULL
        AND class.entity_id IS NOT NULL
        {$center_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'
    GROUP BY
      field_group_type_value, field_person_target_id
  ) b
GROUP BY
  field_group_type_value;
  ")->fetchAllAssoc('type');
}

$encounters = [
  [
    'ANC (total)',
    encounter_all_count('prenatal', 'all', $limit_date, $center_id),
    encounter_unique_count('prenatal', 'all', $limit_date, $center_id),
  ],
  [
    '   Health Center',
    encounter_all_count('prenatal', 'hc', $limit_date, $center_id),
    encounter_unique_count('prenatal', 'hc', $limit_date, $center_id),
  ],
  [
    '   CHW',
    encounter_all_count('prenatal', 'all', $limit_date, $center_id) - encounter_all_count('prenatal', 'hc', $limit_date, $center_id),
    encounter_unique_count('prenatal', 'all', $limit_date, $center_id) - encounter_unique_count('prenatal', 'hc', $limit_date, $center_id),
  ],
  [
    'Acute Illness',
    encounter_all_count('acute_illness', 'all', $limit_date, $center_id),
    encounter_unique_count('acute_illness', 'all', $limit_date, $center_id),
  ],
  [
    '   Health Center',
    encounter_all_count('acute_illness', 'hc', $limit_date, $center_id),
    encounter_unique_count('acute_illness', 'hc', $limit_date, $center_id),
  ],
  [
    '   CHW',
    encounter_all_count('acute_illness', 'all', $limit_date, $center_id) - encounter_all_count('acute_illness', 'hc', $limit_date, $center_id),
    encounter_unique_count('acute_illness', 'all', $limit_date, $center_id) - encounter_unique_count('acute_illness', 'hc', $limit_date, $center_id),
  ],
  [
    'Standard Pediatric Visit',
    encounter_all_count('well_child', 'hc', $limit_date, $center_id),
    encounter_unique_count('well_child', 'hc', $limit_date, $center_id),
  ],
  [
    '  Home Visit',
    encounter_all_count('home_visit', 'chw', $limit_date, $center_id),
    encounter_unique_count('home_visit', 'chw', $limit_date, $center_id),
  ],
  [
    'Nutrition (total)',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition', 'chw', $limit_date, $center_id) + encounter_all_count('home_visit', 'chw', $limit_date, $center_id),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition', 'chw', $limit_date, $center_id) + encounter_unique_count('home_visit', 'chw', $limit_date, $center_id),
  ],
  [
    '  PMTCT',
    $group_encounter_all['pmtct']->counter,
    $group_encounter_unique['pmtct']->counter,
  ],
  [
    '  FBF',
    $group_encounter_all['fbf']->counter,
    $group_encounter_unique['fbf']->counter,
  ],
  [
    '  Sorwhate',
    $group_encounter_all['sorwathe']->counter,
    $group_encounter_unique['sorwathe']->counter,
  ],
  [
    '  CBNP',
    $group_encounter_all['chw']->counter,
    $group_encounter_unique['chw']->counter,
  ],
  [
    '  ACHI',
    $group_encounter_all['achi']->counter,
    $group_encounter_unique['achi']->counter,
  ],
  [
    '  Individual',
    encounter_all_count('nutrition', 'chw', $limit_date, $center_id),
    encounter_unique_count('nutrition', 'chw', $limit_date, $center_id),
  ],
  [
    'TOTAL',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition', 'chw', $limit_date, $center_id) + encounter_all_count('prenatal', 'all', $limit_date, $center_id) + encounter_all_count('acute_illness', 'all', $limit_date) + encounter_all_count('well_child', 'chw', $limit_date),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition', 'chw', $limit_date) + encounter_unique_count('prenatal', 'all', $limit_date, $center_id) + encounter_unique_count('acute_illness', 'all', $limit_date, $center_id) + encounter_unique_count('well_child', 'hc', $limit_date, $center_id),
  ],
];

$text_table = new HedleyAdminTextTable(['Encounter type', 'All', 'Unique']);
drush_print($text_table->render($encounters));
