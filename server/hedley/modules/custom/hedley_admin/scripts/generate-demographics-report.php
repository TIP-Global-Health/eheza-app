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
$region = drush_get_option('region', FALSE);

if (!$limit_date) {
  drush_print('Please specify --limit_date option');
  exit;
}

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
 * @param string $region
 *   The administrative district.
 *
 * @return int
 *   Amount of patients.
 */
function classified_count($age, $gender, $region = NULL) {
  if ($region) {
    if ($age === 'all' && $gender === 'all') {
      return db_query("SELECT COUNT(*)
      FROM person_classified
      LEFT JOIN field_data_field_district district ON person_classified.entity_id=district.entity_id
      WHERE field_district_value LIKE '%$region%'")->fetchField();
    }
    else {
      return db_query("SELECT
      COUNT(*)
      FROM
      person_classified
      LEFT JOIN field_data_field_district district ON person_classified.entity_id=district.entity_id
      WHERE field_district_value LIKE '%$region%'
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
 * @param string $region
 *   The administrative district.
 *
 * @return int
 *   Amount of patients.
 */
function impacted_count($age, $gender, $region = NULL) {

  if ($region) {
    if ($age === 'all' && $gender === 'all') {
      return (int) db_query("SELECT COUNT(*)
        FROM person_classified cl
        INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id
        LEFT JOIN field_data_field_district district ON cl.entity_id=district.entity_id
        WHERE field_district_value LIKE '%$region%'")->fetchField();
    }
    else {
      return (int) db_query("
      SELECT
          COUNT(*)
        FROM
          person_classified cl
          INNER JOIN person_impacted pi ON cl.entity_id = pi.entity_id
          LEFT JOIN field_data_field_district district ON cl.entity_id=district.entity_id
          WHERE field_district_value LIKE '%$region%'
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
 * @param string $region
 *   The administrative district.
 *
 * @return int
 *   Amount of encounters.
 */
function encounter_all_count($type, $filter = NULL, $limit = NULL, $region = NULL) {
  $region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";

  if ($filter === 'hc' && $type == 'prenatal') {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT field_prenatal_encounter_target_id)
      FROM field_data_field_prenatal_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type t ON e.field_prenatal_encounter_target_id=t.entity_id
      WHERE (field_prenatal_encounter_type_value='nurse'
        OR field_prenatal_encounter_type_value is NULL)
        {$region_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }
  elseif ($filter === 'hc' && $type == 'acute_illness') {
    // Health center AI.
    return db_query("SELECT COUNT(DISTINCT field_acute_illness_encounter_target_id)
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t ON e.field_acute_illness_encounter_target_id=t.entity_id
      WHERE (field_ai_encounter_type_value='nurse-encounter'
        OR field_ai_encounter_type_value is NULL)
        {$region_clause}
        AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();

  }
  else {
    return db_query("SELECT COUNT(DISTINCT field_{$type}_encounter_target_id)
      FROM field_data_field_{$type}_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_{$type}_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      WHERE
      FROM_UNIXTIME(node.created) < '$limit'
      {$region_clause}")->fetchField();
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
 * @param string $region
 *   The administrative district.
 *
 * @return int
 *   Amount of encounters.
 */
function encounter_unique_count($type, $filter = NULL, $limit = NULL, $region = NULL) {
  $region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";

  if ($filter === 'hc' && $type == 'prenatal') {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_prenatal_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_prenatal_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_prenatal_encounter_type t on e.field_prenatal_encounter_target_id=t.entity_id
        WHERE (field_prenatal_encounter_type_value='nurse'
          OR field_prenatal_encounter_type_value is NULL)
          {$region_clause}
          AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }

  elseif ($filter === 'hc' && $type == 'acute_illness') {
    // Health center AI.
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_acute_illness_encounter e
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_acute_illness_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      LEFT JOIN field_data_field_ai_encounter_type t on e.field_acute_illness_encounter_target_id=t.entity_id
        WHERE (field_ai_encounter_type_value='nurse-encounter'
          OR field_ai_encounter_type_value is NULL)
          {$region_clause}
          AND FROM_UNIXTIME(node.created) < '$limit'")->fetchField();
  }
  else {
    return db_query("SELECT COUNT(DISTINCT person.field_person_target_id)
      FROM field_data_field_{$type}_encounter e
      LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id
      LEFT JOIN node ON e.entity_id = node.nid
      LEFT JOIN field_data_field_individual_participant ip ON e.field_{$type}_encounter_target_id=ip.entity_id
      LEFT JOIN field_data_field_person person ON ip.field_individual_participant_target_id=person.entity_id
      LEFT JOIN field_data_field_district district ON person.field_person_target_id=district.entity_id
      WHERE
        FROM_UNIXTIME(node.created) < '$limit'
        {$region_clause}")->fetchField();
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
$group_encounter_all = group_encounter_all($measurement_types_sql_list, $limit_date, $region);
$group_encounter_unique = group_encounter_unique($measurement_types_sql_list, $limit_date, $region);

$print_region = ($region) ? $region : "All Districts";
drush_print("# Demographics report - " . $print_region . " - " . $limit_date);

drush_print("## REGISTERED PATIENTS");

$registered = [
  [
    '0 - 1M',
    classified_count('lt1m', 'male', $region),
    classified_count('lt1m', 'female', $region),
  ],
  [
    '1M - 2Y',
    classified_count('lt2y', 'male', $region),
    classified_count('lt2y', 'female', $region),
  ],
  [
    '2Y - 5Y',
    classified_count('lt5y', 'male', $region),
    classified_count('lt5y', 'female', $region),
  ],
  [
    '5Y - 10Y',
    classified_count('lt10y', 'male', $region),
    classified_count('lt10y', 'female', $region),
  ],
  [
    '10Y - 20Y',
    classified_count('lt20y', 'male', $region),
    classified_count('lt20y', 'female', $region),
  ],
  [
    '20Y - 50Y',
    classified_count('lt50y', 'male', $region),
    classified_count('lt50y', 'female', $region),
  ],
  [
    '50Y +',
    classified_count('mt50y', 'male', $region),
    classified_count('mt50y', 'female', $region),
  ],
  [
    'TOTAL',
    '',
    classified_count('all', 'all', $region),
  ],
];
$text_table = new HedleyAdminTextTable(['Registered', 'Male', 'Female']);
$text_table->addData($registered);

drush_print($text_table->render());

$impacted = [
  [
    '0 - 1M',
    impacted_count('lt1m', 'male', $region),
    impacted_count('lt1m', 'female', $region),
  ],
  [
    '1M - 2Y',
    impacted_count('lt2y', 'male', $region),
    impacted_count('lt2y', 'female', $region),
  ],
  [
    '2Y - 5Y',
    impacted_count('lt5y', 'male', $region),
    impacted_count('lt5y', 'female', $region),
  ],
  [
    '5Y - 10Y',
    impacted_count('lt10y', 'male', $region),
    impacted_count('lt10y', 'female', $region),
  ],
  [
    '10Y - 20Y',
    impacted_count('lt20y', 'male', $region),
    impacted_count('lt20y', 'female', $region),
  ],
  [
    '20Y - 50Y',
    impacted_count('lt50y', 'male', $region),
    impacted_count('lt50y', 'female', $region),
  ],
  [
    '50Y +',
    impacted_count('mt50y', 'male', $region),
    impacted_count('mt50y', 'female', $region),
  ],
  [
    'TOTAL',
    '',
    impacted_count('all', 'all', $region),
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
 * Gathers group encounter visits by type.
 *
 * @return array
 *   Associative array, keyed by type.
 */
function group_encounter_all($measurement_types_list, $limit = NULL, $region = NULL) {
  $region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";

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
        LEFT JOIN field_data_field_district district ON p.field_person_target_id=district.entity_id
        LEFT JOIN node ON sess_rel.entity_id = node.nid
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
        AND field_group_type_value IS NOT NULL
        AND class.entity_id IS NOT NULL
        {$region_clause}
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
function group_encounter_unique($measurement_types_list, $limit = NULL, $region = NULL) {
  $region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";

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
        LEFT JOIN field_data_field_district district ON p.field_person_target_id=district.entity_id
        LEFT JOIN node ON sess_rel.entity_id = node.nid
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
        AND field_group_type_value IS NOT NULL
        AND class.entity_id IS NOT NULL
        {$region_clause}
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
    encounter_all_count('prenatal', 'all', $limit_date, $region),
    encounter_unique_count('prenatal', 'all', $limit_date, $region),
  ],
  [
    '   Health Center',
    encounter_all_count('prenatal', 'hc', $limit_date, $region),
    encounter_unique_count('prenatal', 'hc', $limit_date, $region),
  ],
  [
    '   CHW',
    encounter_all_count('prenatal', 'all', $limit_date, $region) - encounter_all_count('prenatal', 'hc', $limit_date, $region),
    encounter_unique_count('prenatal', 'all', $limit_date, $region) - encounter_unique_count('prenatal', 'hc', $limit_date, $region),
  ],
  [
    'Acute Illness',
    encounter_all_count('acute_illness', 'all', $limit_date, $region),
    encounter_unique_count('acute_illness', 'all', $limit_date, $region),
  ],
  [
    '   Health Center',
    encounter_all_count('acute_illness', 'hc', $limit_date, $region),
    encounter_unique_count('acute_illness', 'hc', $limit_date, $region),
  ],
  [
    '   CHW',
    encounter_all_count('acute_illness', 'all', $limit_date, $region) - encounter_all_count('acute_illness', 'hc', $limit_date, $region),
    encounter_unique_count('acute_illness', 'all', $limit_date, $region) - encounter_unique_count('acute_illness', 'hc', $limit_date, $region),
  ],
  [
    'Standard Pediatric Visit',
    encounter_all_count('well_child', 'hc', $limit_date, $region),
    encounter_unique_count('well_child', 'hc', $limit_date, $region),
  ],
  [
    'Home Visit',
    encounter_all_count('home_visit', 'chw', $limit_date, $region),
    encounter_unique_count('home_visit', 'chw', $limit_date, $region),
  ],
  [
    'Child Scorecard',
    encounter_all_count('child_scoreboard', 'chw', $limit_date, $region),
    encounter_unique_count('child_scoreboard', 'chw', $limit_date, $region),
  ],
  [
    'NCD',
    encounter_all_count('ncd', 'nc', $limit_date, $region),
    encounter_unique_count('ncd', 'hc', $limit_date, $region),
  ],
  [
    'Nutrition (total)',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition', 'chw', $limit_date, $region),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition', 'chw', $limit_date, $region),
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
    encounter_all_count('nutrition', 'chw', $limit_date, $region),
    encounter_unique_count('nutrition', 'chw', $limit_date, $region),
  ],
  [
    'TOTAL',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition', 'chw', $limit_date, $region) + encounter_all_count('prenatal', 'all', $limit_date, $region) + encounter_all_count('acute_illness', 'all', $limit_date, $region) + encounter_all_count('well_child', 'hc', $limit_date, $region) + encounter_all_count('home_visit', 'chw', $limit_date, $region) + encounter_all_count('child_scoreboard', 'chw', $limit_date, $region) + encounter_all_count('ncd', 'nc', $limit_date, $region),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition', 'chw', $limit_date, $region) + encounter_unique_count('prenatal', 'all', $limit_date, $region) + encounter_unique_count('acute_illness', 'all', $limit_date, $region) + encounter_unique_count('well_child', 'hc', $limit_date, $region) + encounter_unique_count('home_visit', 'chw', $limit_date, $region) + encounter_unique_count('child_scoreboard', 'chw', $limit_date, $region) + encounter_unique_count('ncd', 'nc', $limit_date, $region),
  ],
];

$text_table = new HedleyAdminTextTable(['Encounter type', 'All', 'Unique']);
drush_print($text_table->render($encounters));
