<?php

/**
 * @file
 * hedley_aggregated_ncda.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_aggregated_ncda_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_aggregated_ncda_node_info() {
  $items = array(
    'child_scoreboard_encounter' => array(
      'name' => t('Child Scoreboard Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_ncda' => array(
      'name' => t('Child Scoreboard NCDA'),
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
