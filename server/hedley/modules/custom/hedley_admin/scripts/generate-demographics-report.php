<?php

/**
 * @file
 * Generates 'Demographics' report.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-nutrition-report.php.
 */

require_once __DIR__ . '/report_common.inc';

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
 *
 * @return int
 */
function encounter_all_count($type) {
  return db_query("SELECT COUNT(DISTINCT field_{$type}_encounter_target_id) FROM field_data_field_{$type}_encounter;")->fetchField();
}

/**
 * Counts encounter types - unique patients.
 *
 * @param $type
 *   Encounter type.
 *
 * @return int
 */
function encounter_unique_count($type) {
  return db_query("SELECT COUNT(DISTINCT field_person_target_id) FROM field_data_field_{$type}_encounter e LEFT JOIN field_data_field_person p ON e.entity_id = p.entity_id;")->fetchField();
}



drush_print("Bootstrapping demographics data calculation");
$bootstrap_data_structures = file_get_contents(__DIR__ . '/bootstrap-demographics-report.SQL');
$commands = explode(';', $bootstrap_data_structures);
$k = 0;
foreach ($commands as $command) {
  if (empty($command)) {
    continue;
  }
  db_query($command);
}
$group_encounter_all = group_encounter_all();
$group_encounter_unique = group_encounter_unique();
drush_print("Ready to print the report.\n\n");

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

function group_encounter_all() {
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
        sess_rel.bundle IN ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors',
                            'core_physical_exam',
                            'danger_signs', 'family_planning', 'follow_up', 'group_health_education',
                            'group_send_to_hc', 'height',
                            'lactation', 'last_menstrual_period', 'medical_history', 'medication',
                            'mother_fbf', 'muac', 'nutrition',
                            'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo',
                            'nutrition_weight',
                            'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo',
                            'pregnancy_testing',
                            'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition',
                            'prenatal_photo',
                            'resource', 'social_history', 'vitals', 'weight', 'acute_findings',
                            'acute_illness_danger_signs',
                            'acute_illness_follow_up', 'acute_illness_muac', 'acute_illness_nutrition',
                            'acute_illness_vitals',
                            'call_114', 'exposure', 'hc_contact', 'health_education', 'isolation',
                            'malaria_testing',
                            'medication_distribution', 'send_to_hc', 'symptoms_general', 'symptoms_gi',
                            'symptoms_respiratory',
                            'travel_history', 'treatment_history', 'treatment_ongoing',
                            'participant_consent', 'nutrition_caring',
                            'nutrition_contributing_factors', 'nutrition_feeding', 'nutrition_follow_up',
                            'nutrition_food_security',
                            'nutrition_health_education', 'nutrition_hygiene', 'nutrition_send_to_hc',
                            'appointment_confirmation',
                            'prenatal_follow_up', 'prenatal_send_to_hc', 'counseling_session')
    AND field_group_type_value IS NOT NULL
    AND class.entity_id IS NOT NULL
    GROUP BY
      field_group_type_value, field_person_target_id, field_session_target_id
  ) b
GROUP BY
  field_group_type_value;
  ")->fetchAllAssoc('type');
}

function group_encounter_unique() {
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
        sess_rel.bundle IN ('attendance', 'birth_plan', 'breast_exam', 'child_fbf', 'contributing_factors',
    'core_physical_exam',
    'danger_signs', 'family_planning', 'follow_up', 'group_health_education',
    'group_send_to_hc', 'height',
    'lactation', 'last_menstrual_period', 'medical_history', 'medication',
    'mother_fbf', 'muac', 'nutrition',
    'nutrition_height', 'nutrition_muac', 'nutrition_nutrition', 'nutrition_photo',
    'nutrition_weight',
    'obstetric_history', 'obstetric_history_step2', 'obstetrical_exam', 'photo',
    'pregnancy_testing',
    'prenatal_family_planning', 'prenatal_health_education', 'prenatal_nutrition',
    'prenatal_photo',
    'resource', 'social_history', 'vitals', 'weight', 'acute_findings',
    'acute_illness_danger_signs',
    'acute_illness_follow_up', 'acute_illness_muac', 'acute_illness_nutrition',
    'acute_illness_vitals',
    'call_114', 'exposure', 'hc_contact', 'health_education', 'isolation',
    'malaria_testing',
    'medication_distribution', 'send_to_hc', 'symptoms_general', 'symptoms_gi',
    'symptoms_respiratory',
    'travel_history', 'treatment_history', 'treatment_ongoing',
    'participant_consent', 'nutrition_caring',
    'nutrition_contributing_factors', 'nutrition_feeding', 'nutrition_follow_up',
    'nutrition_food_security',
    'nutrition_health_education', 'nutrition_hygiene', 'nutrition_send_to_hc',
    'appointment_confirmation',
    'prenatal_follow_up', 'prenatal_send_to_hc', 'counseling_session')
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
    'ANC',
    encounter_all_count('prenatal'),
    encounter_unique_count('prenatal'),

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
