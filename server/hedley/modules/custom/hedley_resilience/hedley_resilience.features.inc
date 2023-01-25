<?php

/**
 * @file
 * hedley_resilience.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_resilience_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_resilience_node_info() {
  $items = array(
    'resilience_survey' => array(
      'name' => t('Resilience Survey'),
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
