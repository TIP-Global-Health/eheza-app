<?php
/**
 * @file
 * hedley_user.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_user_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_user_node_info() {
  $items = array(
    'user' => array(
      'name' => t('User'),
      'base' => 'node_content',
      'description' => t('A user who has a PIN code to identify themselves on a device.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
