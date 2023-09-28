<?php

/**
 * @file
 * hedley_ncda.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_ncda_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_ncda_node_info() {
  $items = array(
    'child_scoreboard_bcg_iz' => array(
      'name' => t('Child Scoreboard BCG Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_dtp_iz' => array(
      'name' => t('Child Scoreboard DTP Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_encounter' => array(
      'name' => t('Child Scoreboard Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_height' => array(
      'name' => t('Child Scoreboard Height'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_ipv_iz' => array(
      'name' => t('Child Scoreboard IPV Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_mr_iz' => array(
      'name' => t('Child Scoreboard MR Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_muac' => array(
      'name' => t('Child Scoreboard MUAC'),
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
    'child_scoreboard_nutrition' => array(
      'name' => t('Child Scoreboard Nutrition'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_opv_iz' => array(
      'name' => t('Child Scoreboard OPV Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_pcv13_iz' => array(
      'name' => t('Child Scoreboard PCV13 Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_rotarix_iz' => array(
      'name' => t('Child Scoreboard Rotarix Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'child_scoreboard_weight' => array(
      'name' => t('Child Scoreboard Weight'),
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
