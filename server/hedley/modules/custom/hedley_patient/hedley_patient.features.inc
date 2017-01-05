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
 * Implements hook_node_info().
 */
function hedley_patient_node_info() {
  $items = array(
    'child' => array(
      'name' => t('Child'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'mother' => array(
      'name' => t('Mother'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
