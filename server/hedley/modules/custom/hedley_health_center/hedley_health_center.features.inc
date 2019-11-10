<?php

/**
 * @file
 * hedley_health_center.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_health_center_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_health_center_node_info() {
  $items = array(
    'catchment_area' => array(
      'name' => t('Catchment Area'),
      'base' => 'node_content',
      'description' => t('A catchment area represents a group of health centers.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'health_center' => array(
      'name' => t('Health Center'),
      'base' => 'node_content',
      'description' => t('A location at which health services are provided. Each health centre has multiple groups. Each health centre falls within a "catchment area" (a group of health centers).'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
