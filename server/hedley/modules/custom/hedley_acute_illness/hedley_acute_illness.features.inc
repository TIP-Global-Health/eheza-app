<?php

/**
 * @file
 * hedley_acute_illness.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_acute_illness_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_acute_illness_node_info() {
  $items = array(
    'acute_findings' => array(
      'name' => t('Acute Findings'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_contacts_tracing' => array(
      'name' => t('Acute Illness Contacts Tracing'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_core_exam' => array(
      'name' => t('Acute Illness Core Exam'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_danger_signs' => array(
      'name' => t('Acute Illness Danger Signs'),
      'base' => 'node_content',
      'description' => t('This activity appears at subsequent encounters of Acute Illness'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_follow_up' => array(
      'name' => t('Acute Illness Follow Up'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_muac' => array(
      'name' => t('Acute Illness Muac'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_nutrition' => array(
      'name' => t('Acute Illness Nutrition'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_trace_contact' => array(
      'name' => t('Acute Illness Trace Contact'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'acute_illness_vitals' => array(
      'name' => t('Acute Illness Vitals'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'call_114' => array(
      'name' => t('Call 114'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'covid_testing' => array(
      'name' => t('COVID Testing'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'exposure' => array(
      'name' => t('Exposure'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'hc_contact' => array(
      'name' => t('HC Contact'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'health_education' => array(
      'name' => t('Health Education'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'isolation' => array(
      'name' => t('Isolation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'malaria_testing' => array(
      'name' => t('Malaria Testing'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'medication_distribution' => array(
      'name' => t('Medication Distribution'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'send_to_hc' => array(
      'name' => t('Send to HC'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'symptoms_general' => array(
      'name' => t('Symptoms General'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'symptoms_gi' => array(
      'name' => t('Symptoms GI'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'symptoms_respiratory' => array(
      'name' => t('Symptoms Respiratory'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'travel_history' => array(
      'name' => t('Travel History'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'treatment_history' => array(
      'name' => t('Treatment History'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'treatment_ongoing' => array(
      'name' => t('Treatment Ongoing'),
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
