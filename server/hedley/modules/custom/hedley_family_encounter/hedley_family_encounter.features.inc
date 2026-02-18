<?php

/**
 * @file
 * hedley_family_encounter.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_family_encounter_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_family_encounter_node_info() {
  $items = array(
    'family_encounter' => array(
      'name' => t('Family Encounter'),
      'base' => 'node_content',
      'description' => t('Tracks a visit by a family (mother + children) in which family activities are performed.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'family_fbf_mother' => array(
      'name' => t('Family FBF Mother'),
      'base' => 'node_content',
      'description' => t('Mother FBF measurement within a family encounter.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'family_fbf_child' => array(
      'name' => t('Family FBF Child'),
      'base' => 'node_content',
      'description' => t('Child FBF measurement within a family encounter.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
