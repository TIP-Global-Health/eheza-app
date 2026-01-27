<?php

/**
 * @file
 * hedley_forms.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_forms_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_participant_consent';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_participant_form';
  $strongarm->value = '0';
  $export['auto_entitylabel_node_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_participant_consent';
  $strongarm->value = '[node:content-type] - [node:field-mother:title] - [node:field-date-measured:custom:Y-m-d] - [node:field-session:nid]';
  $export['auto_entitylabel_pattern_node_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_participant_form';
  $strongarm->value = '[node:content-type] - [node:nid]';
  $export['auto_entitylabel_pattern_node_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_participant_consent';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_participant_form';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__participant_consent';
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
  $export['field_bundle_settings_node__participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__participant_form';
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
  $export['field_bundle_settings_node__participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_participant_consent';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_participant_form';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_participant_consent';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_participant_form';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_participant_consent';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_participant_form';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_participant_consent';
  $strongarm->value = '1';
  $export['node_preview_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_participant_form';
  $strongarm->value = '1';
  $export['node_preview_participant_form'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_participant_consent';
  $strongarm->value = 1;
  $export['node_submitted_participant_consent'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_participant_form';
  $strongarm->value = 1;
  $export['node_submitted_participant_form'] = $strongarm;

  return $export;
}
