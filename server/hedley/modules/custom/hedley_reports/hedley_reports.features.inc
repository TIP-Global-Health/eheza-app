<?php

/**
 * @file
 * hedley_reports.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_reports_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_reports_node_info() {
  $items = array(
    'report_data' => array(
      'name' => t('Report Data'),
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
