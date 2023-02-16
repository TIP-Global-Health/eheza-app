<?php

/**
 * @file
 * hedley_stock_management.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_stock_management_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_stock_management_node_info() {
  $items = array(
    'nurse_signature' => array(
      'name' => t('Nurse Signature'),
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
