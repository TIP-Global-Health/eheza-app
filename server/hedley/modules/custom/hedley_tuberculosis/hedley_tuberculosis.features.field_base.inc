<?php

/**
 * @file
 * hedley_tuberculosis.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_tuberculosis_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_dot_meds_distribution_sign'.
  $field_bases['field_dot_meds_distribution_sign'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_dot_meds_distribution_sign',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Provide DOT today',
        'negative-taken-today' => 'No, meds already taken today',
        'negative-unavailable' => 'No, meds unavailable',
        'negative-side-effects' => 'No, meds cause side effects',
        'negative-patient-refused' => 'No, patient refused',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_dot_signs'.
  $field_bases['field_dot_signs'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_dot_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'positive' => 'Provide DOT today',
        'negative-taken-today' => 'No, meds already taken today',
        'negative-unavailable' => 'No, meds unavailable',
        'negative-side-effects' => 'No, meds cause side effects',
        'negative-patient-refused' => 'No, patient refused',
        'negative-not-indicated' => 'No, reason not indicated',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_prescribed_tb_medications'.
  $field_bases['field_prescribed_tb_medications'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_prescribed_tb_medications',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'rhze' => 'RHZE',
        'rh' => 'RH',
        'other' => 'Other',
        'not-changed' => 'Medications Not Changed',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_tb_health_education_signs'.
  $field_bases['field_tb_health_education_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_tb_health_education_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'followup-testing' => 'Followup testing',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_tuberculosis_diagnosis'.
  $field_bases['field_tuberculosis_diagnosis'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_tuberculosis_diagnosis',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'pulmonary' => 'Pulmonary (in the lungs)',
        'extrapulmonary' => 'Extrapulmonary (outside the lungs)',
        'none' => 'Illness not diagnosed',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_tuberculosis_encounter'.
  $field_bases['field_tuberculosis_encounter'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_tuberculosis_encounter',
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
          'tuberculosis_encounter' => 'tuberculosis_encounter',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  // Exported field_base: 'field_tuberculosis_symptoms'.
  $field_bases['field_tuberculosis_symptoms'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_tuberculosis_symptoms',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'night-sweats' => 'Night sweats',
        'blood-in-sputum' => 'Blood in sputum',
        'wight-loss' => 'Weight loss',
        'severe-fatigue' => 'Severe fatigue',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
