<?php

/**
 * @file
 * hedley_activity.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_activity_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_activity_node_info() {
  $items = array(
    'attendance' => array(
      'name' => t('Attendance'),
      'base' => 'node_content',
      'description' => t('Records whether a mother/caregiver has attended at an encounter.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_fbf' => array(
      'name' => t('Child FBF'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'family_planning' => array(
      'name' => t('Family Planning'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'height' => array(
      'name' => t('Height'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'lactation' => array(
      'name' => t('Lactation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'mother_fbf' => array(
      'name' => t('Mother FBF'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'muac' => array(
      'name' => t('MUAC'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'nutrition' => array(
      'name' => t('Nutrition'),
      'base' => 'node_content',
      'description' => t('Backend storage for Health signs/nutrition measurements.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'photo' => array(
      'name' => t('Photo'),
      'base' => 'node_content',
      'description' => t('Backend storage for Child progression photos.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'weight' => array(
      'name' => t('Weight'),
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
