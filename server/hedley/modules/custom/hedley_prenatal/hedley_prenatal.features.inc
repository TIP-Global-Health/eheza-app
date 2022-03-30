<?php

/**
 * @file
 * hedley_prenatal.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_prenatal_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_prenatal_node_info() {
  $items = array(
    'appointment_confirmation' => array(
      'name' => t('Appointment Confirmation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_blood_gprs_test' => array(
      'name' => t('Prenatal Blood GpRs Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_follow_up' => array(
      'name' => t('Prenatal Follow Up'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_hemoglobin_test' => array(
      'name' => t('Prenatal Hemoglobin Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_hepatitis_b_test' => array(
      'name' => t('Prenatal Hepatitis B Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_hiv_test' => array(
      'name' => t('Prenatal HIV Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_labs_results' => array(
      'name' => t('Prenatal Labs Results'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_malaria_test' => array(
      'name' => t('Prenatal Malaria Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_random_blood_sugar_test' => array(
      'name' => t('Prenatal Random Blood Sugar Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_send_to_hc' => array(
      'name' => t('Prenatal Send To HC'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_syphilis_test' => array(
      'name' => t('Prenatal Syphilis Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_urine_dipstick_test' => array(
      'name' => t('Prenatal Urine Dipstick Test'),
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
