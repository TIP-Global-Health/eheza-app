<?php

/**
 * @file
 * hedley_well_child.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_well_child_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}

/**
 * Implements hook_node_info().
 */
function hedley_well_child_node_info() {
  $items = array(
    'well_child_albendazole' => array(
      'name' => t('Well Child Albendazole'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_bcg_immunisation' => array(
      'name' => t('Well Child BCG Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_caring' => array(
      'name' => t('Well Child Caring'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_contributing_factors' => array(
      'name' => t('Well Child Contributing Factors'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_dtp_immunisation' => array(
      'name' => t('Well Child DTP Immunisation'),
      'base' => 'node_content',
      'description' => t('Stands for PTP-HepB-Hib immunisation, also called Pentavalent.
Protects against five potential killers – Diptheria, Tetanus, Pertusis, Hib, and Hepatitis B.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_dtp_sa_immunisation' => array(
      'name' => t('Well Child DTP (standalone) Immunisation'),
      'base' => 'node_content',
      'description' => t('Protects against three potential killers – Diptheria, Tetanus and Pertusis.'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_ecd' => array(
      'name' => t('Well Child ECD'),
      'base' => 'node_content',
      'description' => t('Early Childhood Development'),
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_encounter' => array(
      'name' => t('Well Child Encounter'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_feeding' => array(
      'name' => t('Well Child Feeding'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_follow_up' => array(
      'name' => t('Well Child Follow Up'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_food_security' => array(
      'name' => t('Well Child Food Security'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_head_circumference' => array(
      'name' => t('Well Child Head Circumference'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_health_education' => array(
      'name' => t('Well Child Health Education'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_height' => array(
      'name' => t('Well Child Height'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_hpv_immunisation' => array(
      'name' => t('Well Child HPV Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_hygiene' => array(
      'name' => t('Well Child Hygiene'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_ipv_immunisation' => array(
      'name' => t('Well Child IPV Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_mebendezole' => array(
      'name' => t('Well Child Mebendezole'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_mr_immunisation' => array(
      'name' => t('Well Child MR Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_muac' => array(
      'name' => t('Well Child MUAC'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_ncda' => array(
      'name' => t('Well Child NCDA'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_next_visit' => array(
      'name' => t('Well Child Next Visit'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_nutrition' => array(
      'name' => t('Well Child Nutrition'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_opv_immunisation' => array(
      'name' => t('Well Child OPV Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_pcv13_immunisation' => array(
      'name' => t('Well Child PCV13 Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_photo' => array(
      'name' => t('Well Child Photo'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_pregnancy_summary' => array(
      'name' => t('Well Child Pregnancy Summary'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_rotarix_immunisation' => array(
      'name' => t('Well Child Rotarix Immunisation'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_send_to_hc' => array(
      'name' => t('Well Child Send to HC'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_symptoms_review' => array(
      'name' => t('Well Child Symptoms Review'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_vitals' => array(
      'name' => t('Well Child Vitals'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_vitamin_a' => array(
      'name' => t('Well Child Vitamin A'),
      'base' => 'node_content',
      'description' => '',
      'has_title' => '1',
      'title_label' => t('Title'),
      'help' => '',
    ),
    'well_child_weight' => array(
      'name' => t('Well Child Weight'),
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
