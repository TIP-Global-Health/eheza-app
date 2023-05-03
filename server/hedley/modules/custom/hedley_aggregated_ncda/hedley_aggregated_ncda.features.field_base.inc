<?php

/**
 * @file
 * hedley_aggregated_ncda.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_aggregated_ncda_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_json_data'.
  $field_bases['field_json_data'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_json_data',
    'indexes' => array(
      'format' => array(
        0 => 'format',
      ),
    ),
    'locked' => 0,
    'module' => 'text',
    'settings' => array(),
    'translatable' => 0,
    'type' => 'text_long',
  );

  return $field_bases;
}
