<?php

/**
 * @file
 * hedley_tuberculosis.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_tuberculosis_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_tuberculosis_node_info() {
  $items = array(
    'tuberculosis_diagnostics' => array(
      'name' => t('Tuberculosis Diagnostics'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_dot' => array(
      'name' => t('Tuberculosis DOT'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_encounter' => array(
      'name' => t('Tuberculosis Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_follow_up' => array(
      'name' => t('Tuberculosis Follow Up'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_health_education' => array(
      'name' => t('Tuberculosis Health Education'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_medication' => array(
      'name' => t('Tuberculosis Medication'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_referral' => array(
      'name' => t('Tuberculosis Referral'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_symptom_review' => array(
      'name' => t('Tuberculosis Symptom Review'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'tuberculosis_treatment_review' => array(
      'name' => t('Tuberculosis Treatment Review'),
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
