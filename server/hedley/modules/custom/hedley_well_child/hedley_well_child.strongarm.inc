<?php

/**
 * @file
 * hedley_well_child.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_well_child_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_contributing_factors';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_ecd';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_encounter';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_follow_up';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_health_education';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_height';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_muac';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_nutrition';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_photo';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_send_to_hc';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_well_child_weight';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_contributing_factors';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_ecd';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_encounter';
  $strongarm->value = '[node:content-type] - [node:field-individual-participant:title] - [node:field-scheduled-date:value:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_follow_up';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_health_education';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_height';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_muac';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_nutrition';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_photo';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_send_to_hc';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_well_child_weight';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_contributing_factors';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_ecd';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_encounter';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_follow_up';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_health_education';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_height';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_muac';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_nutrition';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_photo';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_send_to_hc';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_well_child_weight';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_contributing_factors';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_ecd';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_encounter';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '0',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_follow_up';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_health_education';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_height';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_muac';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_nutrition';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_photo';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_send_to_hc';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__well_child_weight';
  $strongarm->value = array(
    'view_modes' => array(),
    'extra_fields' => array(
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
      'display' => array(),
    ),
  );
  $export['field_bundle_settings_node__well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_contributing_factors';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_ecd';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_encounter';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_follow_up';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_health_education';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_height';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_muac';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_nutrition';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_photo';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_send_to_hc';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_well_child_weight';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_contributing_factors';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_ecd';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_encounter';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_follow_up';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_health_education';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_height';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_muac';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_nutrition';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_photo';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_send_to_hc';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_well_child_weight';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_contributing_factors';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_ecd';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_encounter';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_follow_up';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_health_education';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_height';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_muac';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_nutrition';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_photo';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_send_to_hc';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_well_child_weight';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_contributing_factors';
  $strongarm->value = '1';
  $export['node_preview_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_ecd';
  $strongarm->value = '1';
  $export['node_preview_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_encounter';
  $strongarm->value = '1';
  $export['node_preview_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_follow_up';
  $strongarm->value = '1';
  $export['node_preview_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_health_education';
  $strongarm->value = '1';
  $export['node_preview_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_height';
  $strongarm->value = '1';
  $export['node_preview_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_muac';
  $strongarm->value = '1';
  $export['node_preview_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_nutrition';
  $strongarm->value = '1';
  $export['node_preview_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_photo';
  $strongarm->value = '1';
  $export['node_preview_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_send_to_hc';
  $strongarm->value = '1';
  $export['node_preview_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_well_child_weight';
  $strongarm->value = '1';
  $export['node_preview_well_child_weight'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_contributing_factors';
  $strongarm->value = 1;
  $export['node_submitted_well_child_contributing_factors'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_ecd';
  $strongarm->value = 1;
  $export['node_submitted_well_child_ecd'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_encounter';
  $strongarm->value = 1;
  $export['node_submitted_well_child_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_follow_up';
  $strongarm->value = 1;
  $export['node_submitted_well_child_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_health_education';
  $strongarm->value = 1;
  $export['node_submitted_well_child_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_height';
  $strongarm->value = 1;
  $export['node_submitted_well_child_height'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_muac';
  $strongarm->value = 1;
  $export['node_submitted_well_child_muac'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_nutrition';
  $strongarm->value = 1;
  $export['node_submitted_well_child_nutrition'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_photo';
  $strongarm->value = 1;
  $export['node_submitted_well_child_photo'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_send_to_hc';
  $strongarm->value = 1;
  $export['node_submitted_well_child_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_well_child_weight';
  $strongarm->value = 1;
  $export['node_submitted_well_child_weight'] = $strongarm;

  return $export;
}
