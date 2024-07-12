<?php

/**
 * @file
 * hedley_hiv.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_hiv_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_hiv_diagnosis_signs'.
  $field_bases['field_hiv_diagnosis_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_hiv_diagnosis_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'result-positive-reported' => 'Positive result reported',
        'result-positive-known' => 'Positive result known',
        'result-date-estimated' => 'Positive result date estimated',
        'test-run' => 'HIV test run',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hiv_encounter'.
  $field_bases['field_hiv_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_hiv_encounter',
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
          'hiv_encounter' => 'hiv_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_hiv_health_education_signs'.
  $field_bases['field_hiv_health_education_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_hiv_health_education_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive-result' => 'Positive Result',
        'safer-sex-practices' => 'Safer Sex Practices',
        'encouraged-partner-testing' => 'Encouraged Partner Testing',
        'family-planning-options' => 'Family Planning Options',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_hiv_symptoms'.
  $field_bases['field_hiv_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_hiv_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'fever' => 'Fever',
        'fatigue' => 'Fatigue',
        'swollen-lymph-nodes' => 'Swollen lymph nodes',
        'sore-throat' => 'Sore throat',
        'rash' => 'Rash',
        'muscle-joint-pain' => 'Muscle and joint pain',
        'headache' => 'Headache',
        'severe-abdominal-pain' => 'Severe abdominal pain',
        'night-sweats' => 'Night sweats',
        'diarrhea' => 'Diarrhea',
        'wight-loss' => 'Weight loss',
        'coughing-up-blood' => 'Coughing up blood',
        'hair-loss' => 'Hair loss',
        'mouth-ulcers' => 'Mouth ulcers',
        'difficulty-breathing' => 'Difficulty breathing',
        'vomiting' => 'Vomiting',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_positive_result_date'.
  $field_bases['field_positive_result_date'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_positive_result_date',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'date',
    'settings' => array(
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

  // Exported field_base: 'field_prescribed_hiv_medications'.
  $field_bases['field_prescribed_hiv_medications'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_prescribed_hiv_medications',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'dtg-3tc-tdf' => 'Dolutegravir Lamivudine Tenofovir',
        'atz-r' => 'Atazanavir Ritonavir',
        'dtg' => 'Dolutegravir',
        'abc-3tc' => 'Abacavir Lamivudine',
        '3tc-tdf' => 'Lamivudine Tenofovir',
        'azt' => 'Zidovudine',
        '3tc-azt-nvp' => 'Lamivudine Zidovudine Nevirapine',
        'efv-3tc-tdf' => 'Efavirenz Lamivudine Tenofovir',
        '3tc-azt' => 'Lamivudine Zidovudine',
        'lvp-r' => 'Lopinavir Ritonavir',
        'drv-r' => 'Darunavir Ritonavir',
        'drv-c' => 'Darunavir Cobicistat',
        'ral' => 'Raltegravir',
        'efv' => 'Efavirenz',
        'nvp' => 'Nevirapine',
        'etr' => 'Etravirine',
        'tdf' => 'Tenofovir',
        '3tc' => 'Lamivudine',
        'abc' => 'Abacavir',
        'bactrim' => 'Bactrim',
        'trimethoprim-sulfamethoxazole' => 'Trimethoprim Sulfamethoxazole',
        'co-trimoxazole-tablets' => 'Co-Trimoxazole Tablets',
        'co-trimoxazole-oral-suspension' => 'Co-Trimoxazole Oral Suspension',
        'not-changed' => 'Medications Not Changed',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
