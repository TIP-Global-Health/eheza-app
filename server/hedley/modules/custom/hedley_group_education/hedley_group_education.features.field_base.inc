<?php

/**
 * @file
 * hedley_group_education.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_group_education_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_education_topics'.
  $field_bases['field_education_topics'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_education_topics',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'tuberculosis' => 'Tuberculosis',
        'std' => 'STD',
        'mental-health' => 'Mental Health',
        'malaria' => 'Malaria',
        'childhood-illnesses' => 'Childhood Illnesses',
        'anc-postpartum' => 'ANC & Postpartum',
        'malnutrition' => 'Malnutrition',
        'family-planning' => 'Family Planning',
        'gender' => 'Gender',
        'ncd' => 'NCD',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_participating_patients'.
  $field_bases['field_participating_patients'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_participating_patients',
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
          'person' => 'person',
        ),
      ),
      'target_type' => 'node',
    ),
    'translatable' => 0,
    'type' => 'entityreference',
  );

  return $field_bases;
}
