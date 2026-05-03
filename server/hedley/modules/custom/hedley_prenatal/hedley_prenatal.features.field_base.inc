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

  // Exported field_base: 'field_blood_smear_result'.
  $field_bases['field_blood_smear_result'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_blood_smear_result',
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
        '+++' => '+++',
        'not-taken' => 'Not taken',
        'pending-input' => 'Pending input',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_breastfeeding_signs'.
  $field_bases['field_breastfeeding_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_breastfeeding_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'breastfeeding' => 'Breastfeeding',
        'not-breastfeeding-breast-pain' => 'Not breastfeeding - breast pain',
        'not-breastfeeding-breast-redness' => 'Not breastfeeding - breast redness',
        'not-breastfeeding-low-milk-production' => 'Not breastfeeding - low milk production',
        'not-breastfeeding-problems-latching' => 'Not breastfeeding - problems latching',
        'not-breastfeeding-medical-problems' => 'Not breastfeeding - medical problems',
        'not-breastfeeding-personal-choice' => 'Not breastfeeding - personal choice',
        'not-breastfeeding-other' => 'Not breastfeeding - other',
        'breast-pain' => 'Breast pain',
        'breast-redness' => 'Breast redness',
        'enough-milk' => 'Enough milk for 8 feeds',
        'latching-well' => 'Baby latching well',
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
        'hiv' => 'HIV',
        'partner-hiv' => 'Partner HIV',
        'malaria' => 'Malaria',
        'syphilis' => 'Syphilis',
        'hepatitis-b' => 'Hepatitis B',
        'blood-group' => 'Blood Group and Rhesus',
        'urine-dipstick' => 'Urine Dipstick',
        'hemoglobin' => 'Hemoglobin',
        'random-blood-sugar' => 'Random Blood Sugar',
        'vitals-recheck' => 'Vitals Recheck',
        'hiv-pcr' => 'HIV PCR',
        'creatinine' => 'Creatinine',
        'liver-function' => 'Liver Function',
        'lipid-panel' => 'Lipid Panel',
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

  // Exported field_base: 'field_edd_days'.
  $field_bases['field_edd_days'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_edd_days',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
  );

  // Exported field_base: 'field_edd_weeks'.
  $field_bases['field_edd_weeks'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_edd_weeks',
    'indexes' => array(),
    'locked' => 0,
    'module' => 'number',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'number_integer',
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

  // Exported field_base: 'field_gu_exam_signs'.
  $field_bases['field_gu_exam_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_gu_exam_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'episiotomy-perineal-tear' => 'Episiotomy or a perineal tear',
        'rectal-hemorrhoids' => 'Rectal Hemorrhoids',
        'none' => 'None',
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
        'pending-input' => 'Pending input',
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
        'pending-input' => 'Pending input',
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
        'follow-up-with-specialist' => 'Planned follow up care with specialist',
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
        'rhesus-negative' => 'Rhesus negative',
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
        'hiv' => 'HIV',
        'partner-hiv' => 'Partner HIV',
        'malaria' => 'Malaria',
        'syphilis' => 'Syphilis',
        'hepatitis-b' => 'Hepatitis B',
        'blood-group' => 'Blood Group and Rhesus',
        'urine-dipstick' => 'Urine Dipstick',
        'hemoglobin' => 'Hemoglobin',
        'random-blood-sugar' => 'Random Blood Sugar',
        'vitals-recheck' => 'Vitals Recheck',
        'hiv-pcr' => 'HIV PCR',
        'creatinine' => 'Creatinine',
        'liver-function' => 'Liver Function',
        'lipid-panel' => 'Lipid Panel',
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

  // Exported field_base: 'field_postpartum_healing_problem'.
  $field_bases['field_postpartum_healing_problem'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_postpartum_healing_problem',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'normal-healing' => 'Normal Healing',
        'swelling' => 'Swelling',
        'discharge' => 'Discharge',
        'release-of-sutures' => 'Release (lâchage) of sutures',
        'hematoma' => 'Hematoma',
        'bruising' => 'Bruising',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_pregnancy_signs'.
  $field_bases['field_pregnancy_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_pregnancy_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'not-viable' => 'Not Viable',
        'ectopic' => 'Ectopic',
        'multiple-fetuses' => 'Multiple fetuses',
        'none' => 'None',
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
        'gestational-hypertension-immediate' => 'Pregnancy-Induced Hypertension Immediate',
        'gestational-hypertension-recheck' => 'Pregnancy-Induced Hypertension After Recheck',
        'moderate-preeclampsia-initial' => 'Moderate Preeclampsia Initial',
        'moderate-preeclampsia-initial-ega-37+' => 'Moderate Preeclampsia Initial EGA 37 or more',
        'moderate-preeclampsia-recurrent' => 'Moderate Preeclampsia Recurrent',
        'moderate-preeclampsia-recurrent-ega-37+' => 'Moderate Preeclampsia Recurrent EGA 37 or more',
        'severe-preeclampsia-initial' => 'Severe Preeclampsia Initial',
        'severe-preeclampsia-initial-ega-37+' => 'Severe Preeclampsia Initial EGA 37 or more',
        'severe-preeclampsia-recurrent' => 'Severe Preeclampsia Recurrent',
        'severe-preeclampsia-recurrent-ega-37+' => 'Severe Preeclampsia Recurrent EGA 37 or more',
        'eclampsia' => 'Eclampsia',
        'hiv' => 'HIV - Initial Phase',
        'hiv-recurrent' => 'HIV - Recurrent Phase',
        'hiv-detectable-viral-load-initial' => 'HIV - Detectable Viral Load - Initial Phase',
        'hiv-detectable-viral-load' => 'HIV - Detectable Viral Load - Recurrent Phase',
        'partner-hiv' => 'HIV positive partner - Initial Phase',
        'partner-hiv-recurrent' => 'HIV positive partner - Recurrent Phase',
        'syphilis-initial' => 'Syphilis - Initial Phase',
        'syphilis' => 'Syphilis - Recurrent Phase',
        'syphilis-complications-initial' => 'Syphilis with Complications - Initial Phase',
        'syphilis-complications' => 'Syphilis with Complications - Recurrent Phase',
        'neurosyphilis-initial' => 'Neurosyphilis - Initial Phase',
        'neurosyphilis' => 'Neurosyphilis - Recurrent Phase',
        'hepatitis-b-initial' => 'Hepatitis B - Initial Phase',
        'hepatitis-b' => 'Hepatitis B - Recurrent Phase',
        'malaria' => 'Malaria - Initial Phase',
        'malaria-recurrent' => 'Malaria - Recurrent Phase',
        'malaria-continued' => 'Malaria Continued - Initial Phase',
        'malaria-continued-recurrent' => 'Malaria Continued - Recurrent Phase',
        'malaria-anemia' => 'Malaria with Anemia - Initial Phase',
        'malaria-anemia-recurrent' => 'Malaria with Anemia - Recurrent Phase',
        'malaria-anemia-continued' => 'Malaria with Anemia Continued - Initial Phase',
        'malaria-anemia-continued-recurrent' => 'Malaria with Anemia Continued - Recurrent Phase',
        'malaria-severe-anemia' => 'Malaria with Severe Anemia - Initial Phase',
        'malaria-severe-anemia-recurrent' => 'Malaria with Severe Anemia - Recurrent Phase',
        'anemia-initial' => 'Mild to Moderate Anemia - Initial Phase',
        'anemia' => 'Mild to Moderate Anemia - Recurrent Phase',
        'severe-anemia-initial' => 'Severe Anemia - Initial Phase',
        'severe-anemia' => 'Severe Anemia - Recurrent Phase',
        'severe-anemia-complications-initial' => 'Severe Anemia with Complications - Initial Phase',
        'severe-anemia-complications' => 'Severe Anemia with Complications - Recurrent Phase',
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
        'tuberculosis' => 'Tuberculosis',
        'diabetes-initial' => 'Diabetes - Initial Phase',
        'diabetes' => 'Diabetes - Recurrent Phase',
        'gestational-diabetes-initial' => 'Gestational diabetes - Initial Phase',
        'gestational-diabetes' => 'Gestational diabetes - Recurrent Phase',
        'rhesus-negative-initial' => 'Rhesus negative - Initial Phase',
        'rhesus-negative' => 'Rhesus negative - Recurrent Phase',
        'depression-not-likely' => 'Depression Not Likely',
        'depression-possible' => 'Depression Possible',
        'depression-highly-possible' => 'Depression Highly Possible',
        'depression-probable' => 'Depression Probable',
        'suicide-risk' => 'Suicide Risk',
        'high-risk-of-preeclampsia-initial' => 'High Risk of Preeclampsia - Initial Phase',
        'high-risk-of-preeclampsia-recurrent' => 'High Risk of Preeclampsia - Recurrent Phase',
        'moderate-risk-of-preeclampsia' => 'Moderate Risk of Preeclampsia',
        'other' => 'Other',
        'postpartum-abdominal-pain' => 'Postpartum Abdominal Pain',
        'postpartum-urinary-incontinence' => 'Postpartum Urinary Incontinence',
        'postpartum-headache' => 'Postpartum Headache',
        'postpartum-fatigue' => 'Postpartum Fatigue',
        'postpartum-fever' => 'Postpartum Fever',
        'postpartum-perineal-pain-discharge' => 'Postpartum Perineal Pain Or Discharge',
        'postpartum-infection' => 'Postpartum Infection',
        'postpartum-excessive-bleeding' => 'Postpartum Excessive Bleeding',
        'postpartum-early-mastitis-engorgment' => 'Postpartum Early Mastitis or Engorgment',
        'postpartum-mastitis' => 'Postpartum Mastitis',
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
        'abdominal-pain' => 'Abdominal Pain',
        'urinary-incontinence' => 'Urinary Incontinence',
        'headache' => 'Headache',
        'fatigue' => 'Fatigue',
        'fever' => 'Fever',
        'perineal-pain-discharge' => 'Perineal Pain or Discharge',
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

  // Exported field_base: 'field_reasons_for_non_referrals'.
  $field_bases['field_reasons_for_non_referrals'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_reasons_for_non_referrals',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hospital-client-refused' => 'Hospital - Client refused',
        'hospital-no-ambulance' => 'Hospital - ambulance available',
        'hospital-unable-to-afford-fee' => 'Hospital - client unable to afford fees',
        'hospital-not-indicated' => 'Hospital - Not indicated',
        'hospital-other' => 'Hospital - Other',
        'mhs-client-refused' => 'Mental health specialist - Client refused',
        'mhs-already-in-care' => 'Mental health specialist - Client already in care',
        'mhs-not-indicated' => 'Mental health specialist - Not indicated',
        'mhs-other' => 'Mental health specialist - Other',
        'arv-client-refused' => 'ARV program - Client refused',
        'arv-already-in-care' => 'ARV program - Client already in care',
        'arv-not-indicated' => 'ARV program - Not indicated',
        'arv-other' => 'ARV program - Other',
        'ncd-client-refused' => 'NCD program - Client refused',
        'ncd-already-in-care' => 'NCD program - Client already in care',
        'ncd-not-indicated' => 'NCD program - Not indicated',
        'ncd-other' => 'NCD program - Other',
        'anc-client-refused' => 'ANC services - Client refused',
        'anc-already-in-care' => 'ANC services - Client already in care',
        'anc-not-indicated' => 'ANC services - Not indicated',
        'anc-other' => 'ANC services - Other',
        'us-client-refused' => 'Ultrasound - Client refused',
        'us-already-in-care' => 'Ultrasound - Client already in care',
        'us-not-indicated' => 'Ultrasound - Not indicated',
        'us-other' => 'Ultrasound - Other',
        'none' => 'None',
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
        'hydrochlorothiazide' => 'Hydrochlorothiazide',
        'amlodipine' => 'Amlodipine',
        'nifedipine' => 'Nifedipine',
        'captopril' => 'Captopril',
        'lisinopril' => 'Lisinopril',
        'atenlol' => 'Atenlol',
        'no-treatment-hypertension' => 'No Treatment for Hypertension',
        'aluminium-hydroxide' => 'Aluminium Hydroxide',
        'education-heartburn' => 'Health Education For Heartburn',
        'nitrofurantoin' => 'Nitrofurantoin',
        'amoxicillin' => 'Amoxicillin',
        'clotrimaxazole-200' => 'Clotrimaxazole 200mg',
        'clotrimaxazole-500' => 'Clotrimaxazole 500mg',
        'cloxacillin' => 'Cloxacillin',
        'mastitis-amoxicillin' => 'Amoxicillin for Mastitis',
        'penecilin-v' => 'Penecilin V',
        'paracetamol' => 'Paracetamol',
        'ibuprofen' => 'Ibuprofen',
        'no-treatment-mastitis' => 'No treatment for Mastitis',
        'metformin-1m1e' => 'Metformin1m1e',
        'glipenclamide-1m1e' => 'Glipenclamide1m1e',
        'metformin-2m1e' => 'Metformin2m1e',
        'glipenclamide-2m1e' => 'Glipenclamide2m1e',
        'metformin-2m2e' => 'Metformin2m2e',
        'glipenclamide-2m2e' => 'Glipenclamide2m2e',
        'metformin-2m2e-glipenclamide-1m1e' => 'Metformin2m2eGlipenclamide1m1e',
        'glipenclamide-2m2e-metformin-1m1e' => 'Glipenclamide2m2eMetformin1m1e',
        'no-treatment-diabetes' => 'NoTreatmentForDiabetes',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_referrals'.
  $field_bases['field_referrals'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_referrals',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hospital' => 'Hospital',
        'hospital-referral-form' => 'Hospital - referral form',
        'mhs' => 'Mental health specialist',
        'mhs-referral-form' => 'Mental health specialist - referral form',
        'mhs-accompany' => 'Mental health specialist - accompany',
        'arv' => 'ARV program',
        'arv-referral-form' => 'ARV program - referral form',
        'arv-accompany' => 'ARV program - accompany',
        'ncd' => 'NCD program',
        'ncd-referral-form' => 'NCD program - referral form',
        'ncd-accompany' => 'NCD program - accompany',
        'anc' => 'ANC services',
        'anc-referral-form' => 'ANC services - referral form',
        'anc-accompany' => 'ANC services - accompany',
        'us' => 'Ultrasound',
        'us-referral-form' => 'Ultrasound - referral form',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_reinforce_treatment_signs'.
  $field_bases['field_reinforce_treatment_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_reinforce_treatment_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'anemia-fefol' => 'Anemia Fefol',
        'anemia-mms' => 'Anemia MMS',
        'anemia-repeat-test' => 'Anemia Repeat test',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_review_state'.
  $field_bases['field_review_state'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_review_state',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'requested' => 'Requested',
        'completed' => 'Completed',
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

  // Exported field_base: 'field_speciality_care_signs'.
  $field_bases['field_speciality_care_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_speciality_care_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'arv' => 'Enrolled to ARV program',
        'ncd' => 'Enrolled to NCD program',
        'none' => 'None',
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
        'to-be-done-at-hospital' => 'To be done at hospital',
        'run-confirmed-by-lab-tech' => 'Run confirmed by lab tech',
        'not-present' => 'Not present',
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
        'immediate-result' => 'Immediate result (performed at point of care)',
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

  // Exported field_base: 'field_tests_with_follow_up'.
  $field_bases['field_tests_with_follow_up'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_tests_with_follow_up',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'hiv' => 'HIV',
        'syphilis' => 'Syphilis',
        'partner-hiv' => 'Partner HIV',
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

  // Exported field_base: 'field_vaginal_exam_signs'.
  $field_bases['field_vaginal_exam_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_vaginal_exam_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'foul-smelling-lochia' => 'Foul Smelling Lochia',
        'bleeding' => 'Bleeding',
        'normal' => 'Normal',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
