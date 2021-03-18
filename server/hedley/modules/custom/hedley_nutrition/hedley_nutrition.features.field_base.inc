<?php

/**
 * @file
 * hedley_nutrition.features.field_base.inc
 */

/**
 * Implements hook_field_default_field_bases().
 */
function hedley_nutrition_field_default_field_bases() {
  $field_bases = array();

  // Exported field_base: 'field_contributing_factors_signs'.
  $field_bases['field_contributing_factors_signs'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_contributing_factors_signs',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        'lack-of-breast-milk' => 'Lack of breast milk',
        'maternal-mastitis' => 'Maternal mastitis',
        'poor-suck' => 'Poor suck',
        'diarrhea-or-vomiting' => 'Diarrhea or vomiting',
        'none' => 'None',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  // Exported field_base: 'field_follow_up_options'.
  $field_bases['field_follow_up_options'] = array(
    'active' => 1,
    'cardinality' => -1,
    'deleted' => 0,
    'entity_types' => array(),
    'field_name' => 'field_follow_up_options',
    'indexes' => array(
      'value' => array(
        0 => 'value',
      ),
    ),
    'locked' => 0,
    'module' => 'list',
    'settings' => array(
      'allowed_values' => array(
        '1-d' => '1 Day',
        '3-d' => '3 Days',
        '1-w' => '1 Week',
        '2-w' => '2 Weeks',
      ),
      'allowed_values_function' => '',
    ),
    'translatable' => 0,
    'type' => 'list_text',
  );

  return $field_bases;
}
