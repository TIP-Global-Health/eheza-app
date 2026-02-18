<?php

/**
 * @file
 * hedley_family_nutrition.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_family_nutrition_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_family_nutrition_node_info() {
  $items = array(
    'family_nutrition_encounter' => array(
      'name' => t('Family Nutrition Encounter'),
      'base' => 'node_content',
      'description' => t('Tracks a visit by adult and their children, in which nutrition activities are performed.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
