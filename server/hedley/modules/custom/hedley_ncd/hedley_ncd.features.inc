<?php

/**
 * @file
 * hedley_ncd.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_ncd_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_ncd_node_info() {
  $items = array(
    'ncd_co_morbidities' => array(
      'name' => t('NCD Co-Morbidities'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_core_exam' => array(
      'name' => t('NCD Core Exam'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_creatinine_test' => array(
      'name' => t('NCD Creatinine Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_danger_signs' => array(
      'name' => t('NCD Danger Signs'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_family_history' => array(
      'name' => t('NCD Family History'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_family_planning' => array(
      'name' => t('NCD Family Planning'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_hba1c_test' => array(
      'name' => t('NCD HbA1c Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_health_education' => array(
      'name' => t('NCD Health Education'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_hiv_test' => array(
      'name' => t('NCD HIV Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_labs_results' => array(
      'name' => t('NCD Labs Results'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_lipid_panel_test' => array(
      'name' => t('NCD Lipid Panel Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_liver_function_test' => array(
      'name' => t('NCD Liver Function Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_medication_distribution' => array(
      'name' => t('NCD Medication Distribution'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_medication_history' => array(
      'name' => t('NCD Medication History'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_outside_care' => array(
      'name' => t('NCD Outside Care'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_pregnancy_test' => array(
      'name' => t('NCD Pregnancy Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_random_blood_sugar_test' => array(
      'name' => t('NCD Random Blood Sugar Test '),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_referral' => array(
      'name' => t('NCD Referral'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_social_history' => array(
      'name' => t('NCD Social History'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_symptom_review' => array(
      'name' => t('NCD Symptom Review'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_urine_dipstick_test' => array(
      'name' => t('NCD Urine Dipstick Test'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'ncd_vitals' => array(
      'name' => t('NCD Vitals'),
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
