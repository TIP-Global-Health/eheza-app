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

  // Exported field_base: 'field_avoiding_guidance_reason'.
  $field_bases['field_avoiding_guidance_reason'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_avoiding_guidance_reason',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hypertension-lack-of-stock' => 'Hypertension - Lack of stock',
        'hypertension-known-allergy' => 'Hypertension - Known allergy',
        'hypertension-patient-declined' => 'Hypertension - Patient declined',
        'hypertension-patient-unable-to-afford' => 'Hypertension - Patient unable to afford',
        'hypertension-reinforce-adherence' => 'Hypertension - Reinforce adherence',
        'hypertension-other' => 'Hypertension - Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
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
        'hiv-pcr' => 'HIV PCR',
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

  // Exported field_base: 'field_flank_pain_sign'.
  $field_bases['field_flank_pain_sign'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_flank_pain_sign',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'left' => 'Left side',
        'right' => 'Right side',
        'both' => 'Both sides',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
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

  // Exported field_base: 'field_hiv_viral_load'.
  $field_bases['field_hiv_viral_load'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hiv_viral_load',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
  );

  // Exported field_base: 'field_hiv_viral_load_status'.
  $field_bases['field_hiv_viral_load_status'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_hiv_viral_load_status',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'detectable' => 'Detectable',
        'undetectable' => 'Undetectable',
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

  // Exported field_base: 'field_mental_health_signs'.
  $field_bases['field_mental_health_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_mental_health_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'q1-0' => 'Question 1 - 0',
        'q1-1' => 'Question 1 - 1',
        'q1-2' => 'Question 1 - 2',
        'q1-3' => 'Question 1 - 3',
        'q2-0' => 'Question 2 - 0',
        'q2-1' => 'Question 2 - 1',
        'q2-2' => 'Question 2 - 2',
        'q2-3' => 'Question 2 - 3',
        'q3-0' => 'Question 3 - 0',
        'q3-1' => 'Question 3 - 1',
        'q3-2' => 'Question 3 - 2',
        'q3-3' => 'Question 3 - 3',
        'q4-0' => 'Question 4 - 0',
        'q4-1' => 'Question 4 - 1',
        'q4-2' => 'Question 4 - 2',
        'q4-3' => 'Question 4 - 3',
        'q5-0' => 'Question 5 - 0',
        'q5-1' => 'Question 5 - 1',
        'q5-2' => 'Question 5 - 2',
        'q5-3' => 'Question 5 - 3',
        'q6-0' => 'Question 6 - 0',
        'q6-1' => 'Question 6 - 1',
        'q6-2' => 'Question 6 - 2',
        'q6-3' => 'Question 6 - 3',
        'q7-0' => 'Question 7 - 0',
        'q7-1' => 'Question 7 - 1',
        'q7-2' => 'Question 7 - 2',
        'q7-3' => 'Question 7 - 3',
        'q8-0' => 'Question 8 - 0',
        'q8-1' => 'Question 8 - 1',
        'q8-2' => 'Question 8 - 2',
        'q8-3' => 'Question 8 - 3',
        'q9-0' => 'Question 9 - 0',
        'q9-1' => 'Question 9 - 1',
        'q9-2' => 'Question 9 - 2',
        'q9-3' => 'Question 9 - 3',
        'q10-0' => 'Question 10 - 0',
        'q10-1' => 'Question 10 - 1',
        'q10-2' => 'Question 10 - 2',
        'q10-3' => 'Question 10 - 3',
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

  // Exported field_base: 'field_originating_encounter'.
  $field_bases['field_originating_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_originating_encounter',
    'indexes' => array(
      'target_id' => array(
        0 => 'target_id',
      ),
    ),
    'locked' => 0,
    'module' => 'entityreference',
    'settings' => array(
      'handler' => 'base',
      'handler_settings' => array(
        'behaviors' => array(
          'views-select-list' => array(
            'status' => 0,
          ),
        ),
        'sort' => array(
          'type' => 'none',
        ),
        'target_bundles' => array(
          'prenatal_encounter' => 'prenatal_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_outside_care_medications'.
  $field_bases['field_outside_care_medications'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_outside_care_medications',
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
        'no-treatment-malaria' => 'No Treatment for Malaria',
        'penecilin-1' => 'Penecilin 1',
        'penecilin-3' => 'Penecilin 3',
        'erythromycin' => 'Erythromycin',
        'azithromycin' => 'Azithromycin',
        'ceftriaxon' => 'Ceftriaxon',
        'no-treatment-syphilis' => 'No Treatment for Syphilis',
        'methyldopa-2' => 'Methyldopa twice a day',
        'methyldopa-3' => 'Methyldopa three times a day',
        'methyldopa-4' => 'Methyldopa four times a day',
        'carvedilol' => 'Carvedilol',
        'amlodipine' => 'Amlodipine',
        'no-treatment-hypertension' => 'No Treatment for Hypertension',
        'tdf3tc' => 'TDF + 3TC',
        'dolutegravir' => 'Dolutegravir',
        'no-treatment-hiv' => 'No treatment for HIV',
        'iron1' => 'Iron 1x a day',
        'iron2' => 'Iron 2x a day',
        'folic-acid' => 'Folic Acid',
        'no-treatment-anemia' => 'No treatment for Anemia',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_outside_care_signs'.
  $field_bases['field_outside_care_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_outside_care_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'seen-at-another-facility' => 'Seen at another facility',
        'given-new-diagnoses' => 'Given new diagnoses',
        'given-medicine' => 'Given medicine',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_past_prenatal_diagnoses'.
  $field_bases['field_past_prenatal_diagnoses'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_past_prenatal_diagnoses',
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
        'syphilis-complications' => 'Syphilis with Complications',
        'neurosyphilis' => 'Neurosyphilis',
        'hepatitis-b' => 'Hepatitis B',
        'diabetes' => 'Diabetes',
        'gestational-diabetes' => 'Gestational diabetes',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_patient_notified'.
  $field_bases['field_patient_notified'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_patient_notified',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        0 => '',
        1 => '',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_boolean',
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
        'hiv-pcr' => 'HIV PCR',
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
        '4.0' => '4.0',
        '4.5' => 4.5,
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
        'chronic-hypertension-immediate' => 'Chronic Hypertension Immediate',
        'chronic-hypertension-recheck' => 'Chronic Hypertension After Recheck',
        'gestational-hypertension-immediate' => 'Gestational Hypertension Immediate',
        'gestational-hypertension-recheck' => 'Gestational Hypertension After Recheck',
        'moderate-preeclampsia-immediate' => 'Moderate Preeclampsia Immediate',
        'moderate-preeclampsia-immediate-ega-37+' => 'Moderate Preeclampsia Immediate EGA 37 or more',
        'moderate-preeclampsia-recheck' => 'Moderate Preeclampsia After Recheck',
        'moderate-preeclampsia-recheck-ega-37+' => 'Moderate Preeclampsia After Recheck EGA 37 or more',
        'severe-preeclampsia-immediate' => 'Severe Preeclampsia Immediate',
        'severe-preeclampsia-immediate-ega-37+' => 'Severe Preeclampsia Immediate EGA 37 or more',
        'severe-preeclampsia-recheck' => 'Severe Preeclampsia After Recheck',
        'severe-preeclampsia-recheck-ega-37+' => 'Severe Preeclampsia After Recheck EGA 37 or more',
        'eclampsia' => 'Eclampsia',
        'hiv' => 'HIV',
        'hiv-detectable-viral-load' => 'HIV - Detectable Viral Load',
        'partner-hiv' => 'HIV positive partner',
        'syphilis' => 'Syphilis',
        'syphilis-complications' => 'Syphilis with Complications',
        'neurosyphilis' => 'Neurosyphilis',
        'hepatitis-b' => 'Hepatitis B',
        'malaria' => 'Malaria',
        'malaria-continued' => 'Malaria Continued',
        'malaria-anemia' => 'Malaria with Anemia',
        'malaria-severe-anemia' => 'Malaria with Severe Anemia',
        'anemia' => 'Mild to Moderate Anemia',
        'severe-anemia' => 'Severe Anemia',
        'severe-anemia-complications' => 'Severe Anemia with Complications',
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
        'hyperemesis-gravidum-by-symptoms' => 'Hyperemesis Gravidum by Symptoms',
        'severe-vomiting' => 'Severe Vomiting',
        'severe-vomiting-by-symptoms' => 'Severe Vomiting by Symptoms',
        'maternal-complications' => 'Maternal Complications',
        'infection' => 'Infection',
        'imminent-delivery' => 'Imminent delivery',
        'labor' => 'Labor + Delivery',
        'heartburn' => 'Heartburn',
        'heartburn-persistent' => 'Persistent Heartburn',
        'dvt' => 'Deep Vein Thrombosis',
        'pelvic-pain-intense' => 'Intense Pelvic Pain',
        'pelvic-pain-continued' => 'Intense Pelvic Continued',
        'urinary-tract-infection' => 'Urinary Tract Infection',
        'urinary-tract-infection-continued' => 'Urinary Tract Infection Continued',
        'pyelonephritis' => 'Pyelonephritis',
        'candidiasis' => 'Candidiasis',
        'candidiasis-continued' => 'Candidiasis Continued',
        'gonorrhea' => 'Gonorrhea',
        'gonorrhea-continued' => 'Gonorrhea Continued',
        'trichomonas-or-bv' => 'Trichomonas or Bacterial Vaginosis',
        'trichomonas-or-bv-continued' => 'Trichomonas or Bacterial Vaginosis Continued',
        'diabetes' => 'Diabetes',
        'gestational-diabetes' => 'Gestational diabetes',
        'tuberculosis' => 'Tuberculosis',
        'depression-not-likely' => 'Depression Not Likely',
        'depression-possible' => 'Depression Possible',
        'depression-highly-possible' => 'Depression Highly Possible',
        'depression-probable' => 'Depression Probable',
        'suicide-risk' => 'Suicide Risk',
        'other' => 'Other',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prenatal_indicators'.
  $field_bases['field_prenatal_indicators'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prenatal_indicators',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'past-labs-completed' => 'Past labs completed',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prenatal_symptom_questions'.
  $field_bases['field_prenatal_symptom_questions'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prenatal_symptom_questions',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'dizziness' => 'Dizziness',
        'low-urine-output' => 'Low Urine Output',
        'dark-urine' => 'Dark Urine',
        'pelvic-pain-hospitalization' => 'Pelvic Pain Hospitalization',
        'leg-pain-redness-left' => 'Leg Pain or Redness - Left',
        'leg-painful' => 'Leg Painful',
        'leg-swollen' => 'Leg Pain or Redness - Swollen',
        'leg-warm' => 'Leg Pain or Redness - Red Or Warm',
        'night-sweats' => 'Night Sweats',
        'blood-in-sputum' => 'Blood In Sputum',
        'weight-loss' => 'Weight Loss',
        'severe-fatigue' => 'Severe Fatigue',
        'vaginal-itching' => 'Vaginal Itching',
        'partner-urethral-discharge' => 'Partner Urethral Discharge',
        'vaginal-discharge' => 'Vaginal Discharge',
        'frequent-urination' => 'Frequent Urination',
        'flank-pain' => 'Flank Pain',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prenatal_symptoms'.
  $field_bases['field_prenatal_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_prenatal_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'burning-with-urination' => 'Burning with Urination',
        'abnormal-vaginal-discharge' => 'Abnormal Vaginal Discharge',
        'nausea-and-vomiting' => 'Nausea and Vomiting',
        'heartburn' => 'Heartburn',
        'leg-cramps' => 'Leg Cramps',
        'low-back-pain' => 'Lower Back Pain',
        'cough-continuous' => 'Cough for >2 weeks',
        'pelvic-pain' => 'Pelvic Pain',
        'constipation' => 'Constipation',
        'varicose-veins' => 'Varicose Veins',
        'leg-pain-redness' => 'Leg Pain or Redness (One Leg)',
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
        'no-treatment-malaria' => 'No Treatment for Malaria',
        'penecilin-1' => 'Penecilin 1',
        'penecilin-3' => 'Penecilin 3',
        'erythromycin' => 'Erythromycin',
        'azithromycin' => 'Azithromycin',
        'ceftriaxon' => 'Ceftriaxon',
        'no-treatment-syphilis' => 'No Treatment for Syphilis',
        'methyldopa-2' => 'Methyldopa twice a day',
        'methyldopa-3' => 'Methyldopa three times a day',
        'methyldopa-4' => 'Methyldopa four times a day',
        'add-carvedilol' => 'Add Carvedilol',
        'add-amlodipine' => 'Add Amlodipine',
        'no-treatment-hypertension' => 'No Treatment for Hypertension',
        'aluminium-hydroxide' => 'Aluminium Hydroxide',
        'education-heartburn' => 'Health Education For Heartburn',
        'nitrofurantoin' => 'Nitrofurantoin',
        'amoxicillin' => 'Amoxicillin',
        'clotrimaxazole-200' => 'Clotrimaxazole 200mg',
        'clotrimaxazole-500' => 'Clotrimaxazole 500mg',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_referral_facility'.
  $field_bases['field_referral_facility'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_referral_facility',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hiv-program' => 'HIV program',
        'mental-health-specialist' => 'Mental health specialist',
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

  // Exported field_base: 'field_specialist_at_hc'.
  $field_bases['field_specialist_at_hc'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_specialist_at_hc',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        0 => '',
        1 => '',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_boolean',
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
    'settings' => array(
      'decimal_separator' => '.',
    ),
    'translatable' => 0,
    'type' => 'number_float',
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

  // Exported field_base: 'field_test_prerequisites'.
  $field_bases['field_test_prerequisites'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_test_prerequisites',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'fasting-12h' => 'Fasting for 12 hours',
        'none' => 'None',
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
        '0-0.2' => '0-0.2',
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
