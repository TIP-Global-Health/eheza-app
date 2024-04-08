<?php

/**
 * @file
 * hedley_hiv.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_hiv_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_hiv_node_info() {
  $items = array(
    'hiv_encounter' => array(
      'name' => t('HIV Encounter'),
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
