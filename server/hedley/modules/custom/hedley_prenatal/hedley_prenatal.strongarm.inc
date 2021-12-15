<?php

/**
 * @file
 * hedley_prenatal.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_prenatal_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_appointment_confirmation';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_blood_gprs_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_follow_up';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_hemoglobin_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_hepatitis_b_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_hiv_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_labs_results';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_malaria_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_random_blood_sugar_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_send_to_hc';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_syphilis_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_prenatal_urine_dipstick_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_appointment_confirmation';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_blood_gprs_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_follow_up';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_hemoglobin_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_hepatitis_b_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_hiv_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_labs_results';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_malaria_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_random_blood_sugar_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_send_to_hc';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d]';
  $export['auto_entitylabel_pattern_node_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_syphilis_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_prenatal_urine_dipstick_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_appointment_confirmation';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_blood_gprs_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_follow_up';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_hemoglobin_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_hepatitis_b_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_hiv_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_labs_results';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_malaria_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_random_blood_sugar_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_send_to_hc';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_syphilis_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_prenatal_urine_dipstick_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__appointment_confirmation';
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
  $export['field_bundle_settings_node__appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_blood_gprs_test';
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
  $export['field_bundle_settings_node__prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_follow_up';
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
  $export['field_bundle_settings_node__prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_hemoglobin_test';
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
  $export['field_bundle_settings_node__prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_hepatitis_b_test';
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
  $export['field_bundle_settings_node__prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_hiv_test';
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
  $export['field_bundle_settings_node__prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_labs_results';
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
  $export['field_bundle_settings_node__prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_malaria_test';
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
  $export['field_bundle_settings_node__prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_random_blood_sugar_test';
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
  $export['field_bundle_settings_node__prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_send_to_hc';
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
  $export['field_bundle_settings_node__prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_syphilis_test';
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
  $export['field_bundle_settings_node__prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__prenatal_urine_dipstick_test';
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
  $export['field_bundle_settings_node__prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_appointment_confirmation';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_blood_gprs_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_follow_up';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_hemoglobin_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_hepatitis_b_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_hiv_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_labs_results';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_malaria_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_random_blood_sugar_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_send_to_hc';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_syphilis_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_prenatal_urine_dipstick_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_appointment_confirmation';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_blood_gprs_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_follow_up';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_hemoglobin_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_hepatitis_b_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_hiv_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_labs_results';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_malaria_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_random_blood_sugar_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_send_to_hc';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_syphilis_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_prenatal_urine_dipstick_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_appointment_confirmation';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_blood_gprs_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_follow_up';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_hemoglobin_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_hepatitis_b_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_hiv_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_labs_results';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_malaria_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_random_blood_sugar_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_send_to_hc';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_syphilis_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_prenatal_urine_dipstick_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_appointment_confirmation';
  $strongarm->value = '1';
  $export['node_preview_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_blood_gprs_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_follow_up';
  $strongarm->value = '1';
  $export['node_preview_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_hemoglobin_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_hepatitis_b_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_hiv_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_labs_results';
  $strongarm->value = '1';
  $export['node_preview_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_malaria_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_random_blood_sugar_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_send_to_hc';
  $strongarm->value = '1';
  $export['node_preview_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_syphilis_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_prenatal_urine_dipstick_test';
  $strongarm->value = '1';
  $export['node_preview_prenatal_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_appointment_confirmation';
  $strongarm->value = 1;
  $export['node_submitted_appointment_confirmation'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_blood_gprs_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_blood_gprs_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_follow_up';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_follow_up'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_hemoglobin_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_hemoglobin_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_hepatitis_b_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_hepatitis_b_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_hiv_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_labs_results';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_malaria_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_malaria_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_random_blood_sugar_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_send_to_hc';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_send_to_hc'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_syphilis_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_syphilis_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_prenatal_urine_dipstick_test';
  $strongarm->value = 1;
  $export['node_submitted_prenatal_urine_dipstick_test'] = $strongarm;

  return $export;
}
