<?php

/**
 * @file
 * hedley_whatsapp.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_whatsapp_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_whatsapp_record';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_whatsapp_record';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_whatsapp_record';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__whatsapp_record';
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
  $export['field_bundle_settings_node__whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_whatsapp_record';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_whatsapp_record';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_whatsapp_record';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_whatsapp_record';
  $strongarm->value = '1';
  $export['node_preview_whatsapp_record'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_whatsapp_record';
  $strongarm->value = 1;
  $export['node_submitted_whatsapp_record'] = $strongarm;

  return $export;
}
