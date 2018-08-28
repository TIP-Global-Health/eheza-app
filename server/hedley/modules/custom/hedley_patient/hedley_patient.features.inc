<?php
/**
 * @file
 * hedley_patient.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_patient_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_image_default_styles().
 */
function hedley_patient_image_default_styles() {
  $styles = array();

  // Exported image style: patient-photo.
  $styles['patient-photo'] = array(
    'label' => 'Patient photo',
    'effects' => array(
      2 => array(
        'name' => 'image_scale_and_crop',
        'data' => array(
          'width' => 200,
          'height' => 200,
        ),
        'weight' => 1,
      ),
    ),
  );

  return $styles;
}

/**
 * Implements hook_node_info().
 */
function hedley_patient_node_info() {
  $items = array(
    'child' => array(
      'name' => t('Child'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Name'),
      'help' => '',
    ),
    'mother' => array(
      'name' => t('Mother/Caregiver'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Name'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
