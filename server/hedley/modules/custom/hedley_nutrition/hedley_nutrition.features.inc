<?php

/**
 * @file
 * hedley_nutrition.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_nutrition_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_nutrition_node_info() {
  $items = array(
    'contributing_factors' => array(
      'name' => t('Contributing Factors'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'nutrition_health_education' => array(
      'name' => t('Nutrition Health Education'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'nutrition_send_to_hc' => array(
      'name' => t('Nutrition Send to HC'),
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
