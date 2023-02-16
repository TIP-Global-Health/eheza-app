<?php

/**
 * @file
 * hedley_stock_management.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_stock_management_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_signature'.
  $field_bases['field_signature'] = array(
    'active' => 1,
    'cardinality' => 1,
    'deleted' => 0,
    'entity_id_type' => NULL,
    'entity_types' => array(),
    'field_name' => 'field_signature',
    'indexes' => array(
      'fid' => array(
        0 => 'fid',
      ),
    ),
    'locked' => 0,
    'module' => 'image',
    'settings' => array(
      'default_image' => 0,
      'uri_scheme' => 'private',
    ),
    'translatable' => 0,
    'type' => 'image',
  );

  return $field_bases;
}
