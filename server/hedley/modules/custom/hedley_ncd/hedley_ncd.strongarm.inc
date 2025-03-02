<?php

/**
 * @file
 * hedley_ncd.strongarm.inc
 */

/**
 * Implements hook_strongarm().
 */
function hedley_ncd_strongarm() {
  $export = array();

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_core_exam';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_co_morbidities';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_creatinine_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_danger_signs';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_family_history';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_family_planning';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_hba1c_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_health_education';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_hiv_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_labs_results';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_lipid_panel_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_liver_function_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_medication_distribution';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_medication_history';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_outside_care';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_pregnancy_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_random_blood_sugar_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_referral';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_social_history';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_symptom_review';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_urine_dipstick_test';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_node_ncd_vitals';
  $strongarm->value = '1';
  $export['auto_entitylabel_node_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_core_exam';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_co_morbidities';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_creatinine_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_danger_signs';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_family_history';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_family_planning';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_hba1c_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_health_education';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_hiv_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_labs_results';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_lipid_panel_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_liver_function_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_medication_distribution';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_medication_history';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_outside_care';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_pregnancy_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_random_blood_sugar_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_referral';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_social_history';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_symptom_review';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_urine_dipstick_test';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_pattern_node_ncd_vitals';
  $strongarm->value = '[node:content-type] - [node:field-person:title] - [node:field-date-measured:custom:Y-m-d] ';
  $export['auto_entitylabel_pattern_node_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_core_exam';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_co_morbidities';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_creatinine_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_danger_signs';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_family_history';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_family_planning';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_hba1c_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_health_education';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_hiv_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_labs_results';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_lipid_panel_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_liver_function_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_medication_distribution';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_medication_history';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_outside_care';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_pregnancy_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_random_blood_sugar_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_referral';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_social_history';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_symptom_review';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_urine_dipstick_test';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'auto_entitylabel_php_node_ncd_vitals';
  $strongarm->value = 0;
  $export['auto_entitylabel_php_node_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_core_exam';
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
  $export['field_bundle_settings_node__ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_co_morbidities';
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
  $export['field_bundle_settings_node__ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_creatinine_test';
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
  $export['field_bundle_settings_node__ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_danger_signs';
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
  $export['field_bundle_settings_node__ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_family_history';
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
  $export['field_bundle_settings_node__ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_family_planning';
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
  $export['field_bundle_settings_node__ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_hba1c_test';
  $strongarm->value = array(
    'extra_fields' => array(
      'display' => array(),
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
    ),
    'view_modes' => array(),
  );
  $export['field_bundle_settings_node__ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_health_education';
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
  $export['field_bundle_settings_node__ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_hiv_test';
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
  $export['field_bundle_settings_node__ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_labs_results';
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
  $export['field_bundle_settings_node__ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_lipid_panel_test';
  $strongarm->value = array(
    'extra_fields' => array(
      'display' => array(),
      'form' => array(
        'title' => array(
          'weight' => '-5',
        ),
      ),
    ),
    'view_modes' => array(),
  );
  $export['field_bundle_settings_node__ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_liver_function_test';
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
  $export['field_bundle_settings_node__ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_medication_distribution';
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
  $export['field_bundle_settings_node__ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_medication_history';
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
  $export['field_bundle_settings_node__ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_outside_care';
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
  $export['field_bundle_settings_node__ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_pregnancy_test';
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
  $export['field_bundle_settings_node__ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_random_blood_sugar_test';
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
  $export['field_bundle_settings_node__ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_referral';
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
  $export['field_bundle_settings_node__ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_social_history';
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
  $export['field_bundle_settings_node__ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_symptom_review';
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
  $export['field_bundle_settings_node__ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_urine_dipstick_test';
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
  $export['field_bundle_settings_node__ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'field_bundle_settings_node__ncd_vitals';
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
  $export['field_bundle_settings_node__ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_core_exam';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_co_morbidities';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_creatinine_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_danger_signs';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_family_history';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_family_planning';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_hba1c_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_health_education';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_hiv_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_labs_results';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_lipid_panel_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_liver_function_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_medication_distribution';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_medication_history';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_outside_care';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_pregnancy_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_random_blood_sugar_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_referral';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_social_history';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_symptom_review';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_urine_dipstick_test';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_options_ncd_vitals';
  $strongarm->value = array(
    0 => 'main-menu',
  );
  $export['menu_options_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_core_exam';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_co_morbidities';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_creatinine_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_danger_signs';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_family_history';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_family_planning';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_hba1c_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_health_education';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_hiv_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_labs_results';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_lipid_panel_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_liver_function_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_medication_distribution';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_medication_history';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_outside_care';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_pregnancy_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_random_blood_sugar_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_referral';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_social_history';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_symptom_review';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_urine_dipstick_test';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'menu_parent_ncd_vitals';
  $strongarm->value = 'main-menu:0';
  $export['menu_parent_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_core_exam';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_co_morbidities';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_creatinine_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_danger_signs';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_family_history';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_family_planning';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_hba1c_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_health_education';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_hiv_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_labs_results';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_lipid_panel_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_liver_function_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_medication_distribution';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_medication_history';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_outside_care';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_pregnancy_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_random_blood_sugar_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_referral';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_social_history';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_symptom_review';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_urine_dipstick_test';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_options_ncd_vitals';
  $strongarm->value = array(
    0 => 'status',
    1 => 'promote',
  );
  $export['node_options_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_core_exam';
  $strongarm->value = '1';
  $export['node_preview_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_co_morbidities';
  $strongarm->value = '1';
  $export['node_preview_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_creatinine_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_danger_signs';
  $strongarm->value = '1';
  $export['node_preview_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_family_history';
  $strongarm->value = '1';
  $export['node_preview_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_family_planning';
  $strongarm->value = '1';
  $export['node_preview_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_hba1c_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_health_education';
  $strongarm->value = '1';
  $export['node_preview_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_hiv_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_labs_results';
  $strongarm->value = '1';
  $export['node_preview_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_lipid_panel_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_liver_function_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_medication_distribution';
  $strongarm->value = '1';
  $export['node_preview_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_medication_history';
  $strongarm->value = '1';
  $export['node_preview_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_outside_care';
  $strongarm->value = '1';
  $export['node_preview_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_pregnancy_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_random_blood_sugar_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_referral';
  $strongarm->value = '1';
  $export['node_preview_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_social_history';
  $strongarm->value = '1';
  $export['node_preview_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_symptom_review';
  $strongarm->value = '1';
  $export['node_preview_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_urine_dipstick_test';
  $strongarm->value = '1';
  $export['node_preview_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_preview_ncd_vitals';
  $strongarm->value = '1';
  $export['node_preview_ncd_vitals'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_core_exam';
  $strongarm->value = 1;
  $export['node_submitted_ncd_core_exam'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_co_morbidities';
  $strongarm->value = 1;
  $export['node_submitted_ncd_co_morbidities'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_creatinine_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_creatinine_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_danger_signs';
  $strongarm->value = 1;
  $export['node_submitted_ncd_danger_signs'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_family_history';
  $strongarm->value = 1;
  $export['node_submitted_ncd_family_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_family_planning';
  $strongarm->value = 1;
  $export['node_submitted_ncd_family_planning'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_hba1c_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_hba1c_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_health_education';
  $strongarm->value = 1;
  $export['node_submitted_ncd_health_education'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_hiv_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_hiv_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_labs_results';
  $strongarm->value = 1;
  $export['node_submitted_ncd_labs_results'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_lipid_panel_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_lipid_panel_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_liver_function_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_liver_function_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_medication_distribution';
  $strongarm->value = 1;
  $export['node_submitted_ncd_medication_distribution'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_medication_history';
  $strongarm->value = 1;
  $export['node_submitted_ncd_medication_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_outside_care';
  $strongarm->value = 1;
  $export['node_submitted_ncd_outside_care'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_pregnancy_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_pregnancy_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_random_blood_sugar_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_random_blood_sugar_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_referral';
  $strongarm->value = 1;
  $export['node_submitted_ncd_referral'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_social_history';
  $strongarm->value = 1;
  $export['node_submitted_ncd_social_history'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_symptom_review';
  $strongarm->value = 1;
  $export['node_submitted_ncd_symptom_review'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_urine_dipstick_test';
  $strongarm->value = 1;
  $export['node_submitted_ncd_urine_dipstick_test'] = $strongarm;

  $strongarm = new stdClass();
  $strongarm->disabled = FALSE; /* Edit this to true to make a default strongarm disabled initially */
  $strongarm->api_version = 1;
  $strongarm->name = 'node_submitted_ncd_vitals';
  $strongarm->value = 1;
  $export['node_submitted_ncd_vitals'] = $strongarm;

  return $export;
}
