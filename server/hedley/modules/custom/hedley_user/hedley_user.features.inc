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
    'nurse' => array(
      'name' => t('Nurse'),
      'base' => 'node_content',
      'description' => t('A person who has a PIN code to identify themselves on a device.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
  );
  drupal_alter('node_info', $items);
  return $items;
}
