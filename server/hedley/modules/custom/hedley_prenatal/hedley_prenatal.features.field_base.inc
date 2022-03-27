<?php

/**
 * @file
 * hedley_prenatal.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_prenatal_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_appointment_confirmation'.
  $field_bases['field_appointment_confirmation'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_appointment_confirmation',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_bilirubin'.
  $field_bases['field_bilirubin'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_bilirubin',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        'small' => 'Small (+)',
        'medium' => 'Medium (++)',
        'large' => 'Large (+++)',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_blood_group'.
  $field_bases['field_blood_group'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_blood_group',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'a' => 'A',
        'b' => 'B',
        'ab' => 'AB',
        'o' => 'O',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_completed_tests'.
  $field_bases['field_completed_tests'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_completed_tests',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'syphilis' => 'Syphilis',
        'hepatitis-b' => 'Hepatitis B',
        'blood-group' => 'Blood Group and Rhesus',
        'urine-dipstick' => 'Urine Dipstick',
        'hemoglobin' => 'Hemoglobin',
        'random-blood-sugar' => 'Random Blood Sugar',
        'vitals-recheck' => 'Vitals Recheck',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_dia_repeated'.
  $field_bases['field_dia_repeated'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_dia_repeated',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_execution_date'.
  $field_bases['field_execution_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_execution_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
      'cache_count' => 4,
      'cache_enabled' => 0,
      'granularity' => array(
        'day' => 'day',
        'hour' => 0,
        'minute' => 0,
        'month' => 'month',
        'second' => 0,
        'year' => 'year',
      ),
      'timezone_db' => '',
      'todate' => '',
      'tz_handling' => 'none',
    ),
    'translatable' => 0,
    'type' => 'datetime',
  );

  // Exported field_base: 'field_glucose'.
  $field_bases['field_glucose'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_glucose',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        0 => 0,
        '+1' => '+1',
        '+2' => '+2',
        '+3' => '+3',
        '+4' => '+4',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_haemoglobin'.
  $field_bases['field_haemoglobin'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_haemoglobin',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        'non-hemolyzed-trace' => 'Non-Hemolyzed Trace',
        'non-hemolyzed-moderate' => 'Non-Hemolyzed Moderate',
        'hemolyzed-trace' => 'Hemolyzed Trace',
        'small' => 'Small',
        'moderate' => 'Moderate',
        'large' => 'Large',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hemoglobin_count'.
  $field_bases['field_hemoglobin_count'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hemoglobin_count',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_hiv_signs'.
  $field_bases['field_hiv_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hiv_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hiv-program-hc' => 'HIV program at Health Center',
        'partner-hiv-positive' => 'Partner HIV posiitve',
        'partner-taking-arv' => 'Partner taking ARVs',
        'partner-surpressed-viral-load' => 'Partner got surpressed viral load',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_illness_symptoms'.
  $field_bases['field_illness_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_illness_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'headache' => 'Headache',
        'vision-changes' => 'Vision Changes',
        'rash' => 'Rash',
        'painless-ulcer-mouth' => 'Painless ulcer in Mouth',
        'painless-ulcer-genitals' => 'Painless ulcer in genital area',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ketone'.
  $field_bases['field_ketone'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ketone',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        5 => 5,
        10 => 10,
        15 => 15,
        40 => 40,
        80 => 80,
        100 => 100,
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_leukocytes'.
  $field_bases['field_leukocytes'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_leukocytes',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        'small' => 'Small (+)',
        'medium' => 'Medium (++)',
        'large' => 'Large (+++)',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_nitrite'.
  $field_bases['field_nitrite'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_nitrite',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        '+' => '+',
        '++' => '++',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_performed_tests'.
  $field_bases['field_performed_tests'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_performed_tests',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'syphilis' => 'Syphilis',
        'hepatitis-b' => 'Hepatitis B',
        'blood-group' => 'Blood Group and Rhesus',
        'urine-dipstick' => 'Urine Dipstick',
        'hemoglobin' => 'Hemoglobin',
        'random-blood-sugar' => 'Random Blood Sugar',
        'vitals-recheck' => 'Vitals Recheck',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_ph'.
  $field_bases['field_ph'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_ph',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        '5.0' => '5.0',
        '6.0' => '6.0',
        '6.5' => 6.5,
        '7.0' => '7.0',
        '7.5' => 7.5,
        '8.0' => '8.0',
        '8.5' => 8.5,
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prenatal_assesment'.
  $field_bases['field_prenatal_assesment'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prenatal_assesment',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'normal' => 'Normal',
        'high-risk' => 'High risk',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prenatal_diagnoses'.
  $field_bases['field_prenatal_diagnoses'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prenatal_diagnoses',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'prescribe-mebendezole' => 'Prescribe Mebendazole',
        'chronic-hypertension-immediate' => 'Chronic Hypertension',
        'chronic-hypertension-recheck' => 'Chronic Hypertension',
        'gestational-hypertension-immediate' => 'Gestational Hypertension',
        'gestational-hypertension-recheck' => 'Gestational Hypertension',
        'hiv' => 'HIV',
        'partner-hiv' => 'HIV positive partner',
        'syphilis' => 'Syphilis',
        'syphilis-complications' => 'Syphilis with Complications',
        'neurosyphilis' => 'Neurosyphilis',
        'hepatitis-b' => 'Hepatitis B',
        'malaria' => 'Malaria',
        'malaria-anemia' => 'Malaria with Anemia',
        'malaria-severe-anemia' => 'Malaria with Severe Anemia',
        'anemia' => 'Mild to Moderate Anemia',
        'severe-anemia' => 'Severe Anemia',
        'severe-anemia-complications' => 'Severe Anemia with Complications',
        'eclampsia' => 'Eclampsia',
        'miscarriage' => 'Miscarriage',
        'molar-pregnancy' => 'Molar Pregnancy',
        'placenta-previa' => 'Placenta Previa',
        'placental-abruption' => 'Placental Abruption',
        'uterine-rupture' => 'Uterine Rupture',
        'obstructed-labor' => 'Obstructed Labor',
        'post-abortion-sepsis' => 'Post Abortion Sepsis',
        'ectopic-pregnancy' => 'Ectopic Pregnancy',
        'prom' => 'PROM',
        'pprom' => 'PPROM',
        'hyperemesis-gravidum' => 'Hyperemesis Gravidum',
        'maternal-complications' => 'Maternal Complications',
        'infection' => 'Infection',
        'imminent-delivery' => 'Imminent delivery',
        'labor' => 'Labor + Delivery',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_protein'.
  $field_bases['field_protein'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_protein',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'negative' => 'Negative',
        30 => 30,
        100 => 100,
        300 => 300,
        2000 => 2000,
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_recommended_treatment'.
  $field_bases['field_recommended_treatment'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_recommended_treatment',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'quinine-sulphate' => 'Quinine Sulphate',
        'coartem' => 'Coartem',
        'written-protocols' => 'Written Protocols',
        'refer-to-hospital' => 'Refer to Hospital',
        'penecilin-1' => 'Penecilin 1',
        'penecilin-3' => 'Penecilin 3',
        'erythromycin' => 'Erythromycin',
        'azithromycin' => 'Azithromycin',
        'ceftriaxon' =>'Ceftriaxon',
        'methyldopa-2' => 'Methyldopa twice a day',
        'methyldopa-3' => 'Methyldopa three times a day',
        'methyldopa-4' => 'Methyldopa four times a day',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_rhesus'.
  $field_bases['field_rhesus'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_rhesus',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Positive',
        'negative' => 'Negative',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_specific_gravity'.
  $field_bases['field_specific_gravity'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_specific_gravity',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        '1.000' => '1.000',
        '1.005' => 1.005,
        '1.010' => '1.010',
        '1.015' => 1.015,
        '1.020' => '1.020',
        '1.025' => 1.025,
        '1.030' => '1.030',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_sugar_count'.
  $field_bases['field_sugar_count'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_sugar_count',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_sys_repeated'.
  $field_bases['field_sys_repeated'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_sys_repeated',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_test_execution_note'.
  $field_bases['field_test_execution_note'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_test_execution_note',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'run-today' => 'Run today',
        'run-previously' => 'Run Previously',
        'lack-of-reagents' => 'Lack of Reagents',
        'lack-of-other-supplies' => 'Lack of Other Supplies',
        'no-equipment' => 'No Equipment',
        'broken-equipment' => 'Broken Equipment',
        'not-indicated' => 'Not Indicated',
        'known-as-positive' => 'Known as Positive',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_test_result'.
  $field_bases['field_test_result'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_test_result',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Positive',
        'negative' => 'Negative',
        'indeterminate' => 'Indeterminate',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_test_variant'.
  $field_bases['field_test_variant'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_test_variant',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'short' => 'Short',
        'long' => 'Long',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_urobilinogen'.
  $field_bases['field_urobilinogen'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_urobilinogen',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        '0.2' => 0.2,
        1 => 1,
        2 => 2,
        4 => 4,
        8 => 8,
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
