<?php

/**
 * @file
 * hedley_healthy_start.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_healthy_start_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_healthy_start_node_info() {
  $items = array(
    'healthy_start_encounter' => array(
      'name' => t('Healthy Start Encounter'),
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
