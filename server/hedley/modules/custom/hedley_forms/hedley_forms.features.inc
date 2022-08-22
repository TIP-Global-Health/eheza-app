<?php

/**
 * @file
 * hedley_forms.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_forms_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_forms_node_info() {
  $items = array(
    'participant_consent' => array(
      'name' => t('Participant consent'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'participant_form' => array(
      'name' => t('Participant form'),
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
