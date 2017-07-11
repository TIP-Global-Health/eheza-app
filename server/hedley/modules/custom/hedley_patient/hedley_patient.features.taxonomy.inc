<?php
/**
 * @file
 * hedley_patient.features.taxonomy.inc
 */

/**
 * Implements hook_taxonomy_default_vocabularies().
 */
function hedley_patient_taxonomy_default_vocabularies() {
  return array(
    'nutrition_health_signs' => array(
      'name' => 'Nutrition/Health Signs',
      'machine_name' => 'nutrition_health_signs',
      'description' => '',
      'hierarchy' => 0,
      'module' => 'taxonomy',
      'weight' => 0,
    ),
  );
}
