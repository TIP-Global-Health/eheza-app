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
    'prenatal_breastfeeding' => array(
      'name' => t('Prenatal Breastfeeding'),
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
    'prenatal_gu_exam' => array(
      'name' => t('Prenatal GU Exam'),
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
    'prenatal_hiv_pcr_test' => array(
      'name' => t('Prenatal HIV PCR Test'),
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
    'prenatal_medication_distribution' => array(
      'name' => t('Prenatal Medication Distribution'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_mental_health' => array(
      'name' => t('Prenatal Mental Health'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_outside_care' => array(
      'name' => t('Prenatal Outside Care'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_partner_hiv_test' => array(
      'name' => t('Prenatal Partner HIV Test'),
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
      'name' => t('Prenatal Referrals'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_speciality_care' => array(
      'name' => t('Prenatal Specialty Care'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'prenatal_symptom_review' => array(
      'name' => t('Prenatal Symptom Review'),
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
    'prenatal_tetanus_immunisation' => array(
      'name' => t('Prenatal Tetanus Immunisation'),
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
