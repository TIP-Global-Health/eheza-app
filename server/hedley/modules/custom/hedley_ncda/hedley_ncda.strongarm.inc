<?php

/**
 * @file
 * hedley_ncda.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_ncda_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_bcg_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_dtp_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_dtp_sa_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_encounter';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_ipv_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_mr_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_ncda';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_opv_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_pcv13_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_child_scoreboard_rotarix_iz';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_bcg_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_dtp_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_dtp_sa_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_encounter';
  $strongarm->value = '[node:content-type] - [node:field-individual-participant:title] - [node:field-scheduled-date:value:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_ipv_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_mr_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_ncda';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_opv_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_pcv13_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_child_scoreboard_rotarix_iz';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_bcg_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_dtp_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_dtp_sa_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_encounter';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_ipv_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_mr_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_ncda';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_opv_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_pcv13_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_child_scoreboard_rotarix_iz';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_bcg_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_dtp_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_dtp_sa_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_encounter';
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
  $export['field_bundle_settings_node__child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_ipv_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_mr_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_ncda';
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
  $export['field_bundle_settings_node__child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_opv_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_pcv13_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__child_scoreboard_rotarix_iz';
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
  $export['field_bundle_settings_node__child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_bcg_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_dtp_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_dtp_sa_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_encounter';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_ipv_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_mr_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_ncda';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_opv_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_pcv13_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_child_scoreboard_rotarix_iz';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_bcg_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_dtp_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_dtp_sa_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_encounter';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_ipv_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_mr_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_ncda';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_opv_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_pcv13_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_child_scoreboard_rotarix_iz';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_bcg_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_dtp_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_dtp_sa_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_encounter';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_ipv_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_mr_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_ncda';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_opv_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_pcv13_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_child_scoreboard_rotarix_iz';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_bcg_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_dtp_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_dtp_sa_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_encounter';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_ipv_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_mr_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_ncda';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_opv_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_pcv13_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_child_scoreboard_rotarix_iz';
  $strongarm->value = '1';
  $export['node_preview_child_scoreboard_rotarix_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_bcg_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_bcg_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_dtp_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_dtp_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_dtp_sa_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_dtp_sa_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_encounter';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_encounter'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_ipv_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_ipv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_mr_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_mr_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_ncda';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_ncda'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_opv_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_opv_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_pcv13_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_pcv13_iz'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_child_scoreboard_rotarix_iz';
  $strongarm->value = 1;
  $export['node_submitted_child_scoreboard_rotarix_iz'] = $strongarm;

  return $export;
}
