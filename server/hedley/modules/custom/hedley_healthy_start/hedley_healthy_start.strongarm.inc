<?php

/**
 * @file
 * hedley_healthy_start.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_healthy_start_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_healthy_start_encounter';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_healthy_start_encounter';
  $strongarm->value = '[node:content-type] - [node:field-individual-participant:title] - [node:field-scheduled-date:value:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_healthy_start_encounter';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__healthy_start_encounter';
  $strongarm->value = array(
    'extra_fields' => array(
      'display' => array(),
      'form' => array(
        'title' => array(
          'weight' => '0',
        ),
      ),
    ),
    'view_modes' => array(),
  );
  $export['field_bundle_settings_node__healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_healthy_start_encounter';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_healthy_start_encounter';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_healthy_start_encounter';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_healthy_start_encounter';
  $strongarm->value = '1';
  $export['node_preview_healthy_start_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_healthy_start_encounter';
  $strongarm->value = 1;
  $export['node_submitted_healthy_start_encounter'] = $strongarm;

  return $export;
}
