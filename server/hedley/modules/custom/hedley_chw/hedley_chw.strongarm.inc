<?php

/**
 * @file
 * hedley_chw.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_chw_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_village';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_village';
  $strongarm->value = '[node:field-village] at [node:field-cell]';
  $export['auto_entitylabel_pattern_node_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_village';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__village';
  $strongarm->value = array(
    'extra_fields' => array(
      'display' => array(),
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
    ),
    'view_modes' => array(),
  );
  $export['field_bundle_settings_node__village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_village';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_village';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_village';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_village';
  $strongarm->value = '1';
  $export['node_preview_village'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_village';
  $strongarm->value = 1;
  $export['node_submitted_village'] = $strongarm;

  return $export;
}
