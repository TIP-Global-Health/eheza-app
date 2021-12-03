<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-demographics-report.php.
 */

require_once __DIR__ . '/report_common.inc';

// We need to filter for all the measurements at several places,
// but it's a bad idea to hardcode the list, so we generate a piece of SQL
// here in advance.
$types = hedley_general_get_measurement_types();
array_walk($types, function (&$val) {
  $val = "'$val'";
});
$measurement_types_sql_list = join(', ', $types);

/**
 * Fetches registered / classified count from the temporary helper table.
 *
 * @param string $age
 *   Age classifier string.
 * @param string $gender
 *   Male or female.
 *
 * @return int
 */
function classified_count($age, $gender) {
  return db_query("SELECT
    COUNT(*)
FROM
  person_classified
WHERE
  age = :age AND
  gender = :gender
  ", [
    ':age' => $age,
    ':gender' => $gender,
  ])->fetchField();
}

/**
 * Fetches impacted count from the temporary helper table.
 *
 * @param string $age
 *   Age classifier string.
 * @param string $gender
 *   Male or female.
 *
 * @return int
 */
function impacted_count($age, $gender) {
  return (int) db_query("
SELECT
    COUNT(*)
  FROM
    person_classified cl INNER JOIN
    person_impacted pi ON cl.entity_id = pi.entity_id
  WHERE
      age = :age AND
      gender = :gender
  ", [
    ':age' => $age,
    ':gender' => $gender,
  ])->fetchField();
}

/**
 * Counts encounter types.
 *
 * @param $type
 *   Encounter type.
 * @param $filter
 *   Filter type 'hc' or NULL.
 *
 * @return int
 */
function encounter_all_count($type, $filter = NULL) {
  if ($filter === 'hc' && $type == 'prenatal')  {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT field_prenatal_encounter_target_id) FROM field_data_field_prenatal_encounter e left join field_data_field_prenatal_encounter_type t on e.field_prenatal_encounter_target_id=t.entity_id where field_prenatal_encounter_type_value='nurse'")->fetchField();
  }
  else {
    return db_query("SELECT COUNT(DISTINCT field_{$type}_encounter_target_id) FROM field_data_field_{$type}_encounter;")->fetchField();
  }
}

/**
 * Counts encounter types - unique patients.
 *
 * @param $type
 *   Encounter type.
 * @param $filter
 *   Filter type 'hc' or NULL.*
 *
 * @return int
 */
function encounter_unique_count($type, $filter = NULL) {
  if ($filter === 'hc' && $type == 'prenatal') {
    // Health center ANC.
    return db_query("SELECT COUNT(DISTINCT field_person_target_id) FROM field_data_field_prenatal_encounter e LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id LEFT JOIN field_data_field_prenatal_encounter_type t on e.field_prenatal_encounter_target_id=t.entity_id where field_prenatal_encounter_type_value='nurse'")->fetchField();
  }
  return db_query("SELECT COUNT(DISTINCT field_person_target_id) FROM field_data_field_{$type}_encounter e LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id;")->fetchField();
}

$bootstrap_data_structures = file_get_contents(__DIR__ . '/bootstrap-demographics-report.SQL');
$commands = explode(';', $bootstrap_data_structures);
$k = 0;
foreach ($commands as $command) {
  if (empty($command)) {
    continue;
  }
  $command = str_replace('__MEASUREMENT_TYPES_LIST__', $measurement_types_sql_list, $command);
  db_query($command);
}
$group_encounter_all = group_encounter_all($measurement_types_sql_list);
$group_encounter_unique = group_encounter_unique($measurement_types_sql_list);

drush_print("# Demographics report  - " . date('D/m/Y'));

drush_print("## REGISTERED PATIENTS");

$registered = [
  [
    '0 - 1M',
    classified_count('lt1m', 'male'),
    classified_count('lt1m', 'female'),
  ],
  [
    '1M - 2Y',
    classified_count('lt2y', 'male'),
    classified_count('lt2y', 'female'),
  ],
  [
    '2Y - 5Y',
    classified_count('lt5y', 'male'),
    classified_count('lt5y', 'female'),
  ],
  [
    '5Y - 10Y',
    classified_count('lt10y', 'male'),
    classified_count('lt10y', 'female'),
  ],
  [
    '10Y - 20Y',
    classified_count('lt20y', 'male'),
    classified_count('lt20y', 'female'),
  ],
  [
    '20Y - 50Y',
    classified_count('lt50y', 'male'),
    classified_count('lt50y', 'female'),
  ],
  [
    '50Y +',
    classified_count('mt50y', 'male'),
    classified_count('mt50y', 'female'),
  ],
];
$text_table = new HedleyAdminTextTable(['Registered', 'Male', 'Female']);
$text_table->addData($registered);

drush_print($text_table->render());

$impacted = [
  [
    '0 - 1M',
    impacted_count('lt1m', 'male'),
    impacted_count('lt1m', 'female'),
  ],
  [
    '1M - 2Y',
    impacted_count('lt2y', 'male'),
    impacted_count('lt2y', 'female'),
  ],
  [
    '2Y - 5Y',
    impacted_count('lt5y', 'male'),
    impacted_count('lt5y', 'female'),
  ],
  [
    '5Y - 10Y',
    impacted_count('lt10y', 'male'),
    impacted_count('lt10y', 'female'),
  ],
  [
    '10Y - 20Y',
    impacted_count('lt20y', 'male'),
    impacted_count('lt20y', 'female'),
  ],
  [
    '20Y - 50Y',
    impacted_count('lt50y', 'male'),
    impacted_count('lt50y', 'female'),
  ],
  [
    '50Y +',
    impacted_count('mt50y', 'male'),
    impacted_count('mt50y', 'female'),
  ],
];
$text_table = new HedleyAdminTextTable(['Impacted (2+ visits)', 'Male', 'Female']);
$text_table->addData($impacted);

drush_print($text_table->render());

drush_print("## ENCOUNTERS");

/**
 * Gathers group encounter visits by type.
 *
 * @return array
 */
function group_encounter_all($measurement_types_list) {
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
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
    AND field_group_type_value IS NOT NULL
    AND class.entity_id IS NOT NULL
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
 */
function group_encounter_unique($measurement_types_list) {
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
    WHERE
        sess_rel.bundle IN ($measurement_types_list)
  AND field_group_type_value IS NOT NULL
  AND class.entity_id IS NOT NULL
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
    encounter_all_count('prenatal'),
    encounter_unique_count('prenatal'),
  ],
  [
    '   Health Center',
    encounter_all_count('prenatal', 'hc'),
    encounter_unique_count('prenatal', 'hc'),
  ],
  [
    '   CHW',
    encounter_all_count('prenatal') - encounter_all_count('prenatal', 'hc'),
    encounter_unique_count('prenatal') - encounter_unique_count('prenatal', 'hc'),
  ],
  [
    'Acute Illness',
    encounter_all_count('acute_illness'),
    encounter_unique_count('acute_illness'),
  ],
  [
    'Nutrition (total)',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition'),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition'),
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
    encounter_all_count('nutrition'),
    encounter_unique_count('nutrition'),
  ],
  [
    'TOTAL',
    $group_encounter_all['pmtct']->counter + $group_encounter_all['fbf']->counter + $group_encounter_all['sorwathe']->counter + $group_encounter_all['chw']->counter + $group_encounter_all['achi']->counter + encounter_all_count('nutrition')  + encounter_all_count('prenatal') + encounter_all_count('acute_illness'),
    $group_encounter_unique['pmtct']->counter + $group_encounter_unique['fbf']->counter + $group_encounter_unique['sorwathe']->counter + $group_encounter_unique['chw']->counter + $group_encounter_unique['achi']->counter + encounter_unique_count('nutrition') + encounter_unique_count('prenatal') + encounter_unique_count('acute_illness'),
  ]
];

$text_table = new HedleyAdminTextTable(['Encounter type', 'All', 'Unique']);
drush_print($text_table->render($encounters));
