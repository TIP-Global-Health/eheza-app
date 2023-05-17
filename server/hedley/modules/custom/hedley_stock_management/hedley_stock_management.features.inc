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
 * Implements hook_image_default_styles().
 */
function hedley_stock_management_image_default_styles() {
  $styles = array();

  // Exported image style: signature.
  $styles['signature'] = array(
    'label' => 'Signature',
    'effects' => array(
      1 => array(
        'name' => 'image_scale_and_crop',
        'data' => array(
          'width' => 600,
          'height' => 187,
        ),
        'weight' => 1,
      ),
    ),
  );

  return $styles;
}

/**
 * Implements hook_node_info().
 */
function hedley_stock_management_node_info() {
  $items = array(
    'stock_update' => array(
      'name' => t('Stock Update'),
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
