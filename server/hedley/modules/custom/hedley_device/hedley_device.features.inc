<?php

/**
 * @file
 * hedley_device.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_device_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_device_node_info() {
  $items = array(
    'device' => array(
      'name' => t('Device'),
      'base' => 'node_content',
      'description' => t('Represents a device which we are willing to sync data with.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'sync_incident' => array(
      'name' => t('Sync incident'),
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
