<?php

/**
 * @file
 * hedley_prenatal.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_prenatal_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create appointment_confirmation content'.
  $permissions['create appointment_confirmation content'] = array(
    'name' => 'create appointment_confirmation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_blood_gprs_test content'.
  $permissions['create prenatal_blood_gprs_test content'] = array(
    'name' => 'create prenatal_blood_gprs_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_breastfeeding content'.
  $permissions['create prenatal_breastfeeding content'] = array(
    'name' => 'create prenatal_breastfeeding content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_calcium content'.
  $permissions['create prenatal_calcium content'] = array(
    'name' => 'create prenatal_calcium content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_folate content'.
  $permissions['create prenatal_folate content'] = array(
    'name' => 'create prenatal_folate content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_follow_up content'.
  $permissions['create prenatal_follow_up content'] = array(
    'name' => 'create prenatal_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_gu_exam content'.
  $permissions['create prenatal_gu_exam content'] = array(
    'name' => 'create prenatal_gu_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_hemoglobin_test content'.
  $permissions['create prenatal_hemoglobin_test content'] = array(
    'name' => 'create prenatal_hemoglobin_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_hepatitis_b_test content'.
  $permissions['create prenatal_hepatitis_b_test content'] = array(
    'name' => 'create prenatal_hepatitis_b_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_hiv_pcr_test content'.
  $permissions['create prenatal_hiv_pcr_test content'] = array(
    'name' => 'create prenatal_hiv_pcr_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_hiv_test content'.
  $permissions['create prenatal_hiv_test content'] = array(
    'name' => 'create prenatal_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_iron content'.
  $permissions['create prenatal_iron content'] = array(
    'name' => 'create prenatal_iron content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_labs_results content'.
  $permissions['create prenatal_labs_results content'] = array(
    'name' => 'create prenatal_labs_results content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_malaria_test content'.
  $permissions['create prenatal_malaria_test content'] = array(
    'name' => 'create prenatal_malaria_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_mebendazole content'.
  $permissions['create prenatal_mebendazole content'] = array(
    'name' => 'create prenatal_mebendazole content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_medication_distribution content'.
  $permissions['create prenatal_medication_distribution content'] = array(
    'name' => 'create prenatal_medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_mental_health content'.
  $permissions['create prenatal_mental_health content'] = array(
    'name' => 'create prenatal_mental_health content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_mms content'.
  $permissions['create prenatal_mms content'] = array(
    'name' => 'create prenatal_mms content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_outside_care content'.
  $permissions['create prenatal_outside_care content'] = array(
    'name' => 'create prenatal_outside_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_partner_hiv_test content'.
  $permissions['create prenatal_partner_hiv_test content'] = array(
    'name' => 'create prenatal_partner_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_random_blood_sugar_test content'.
  $permissions['create prenatal_random_blood_sugar_test content'] = array(
    'name' => 'create prenatal_random_blood_sugar_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_send_to_hc content'.
  $permissions['create prenatal_send_to_hc content'] = array(
    'name' => 'create prenatal_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_speciality_care content'.
  $permissions['create prenatal_speciality_care content'] = array(
    'name' => 'create prenatal_speciality_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_symptom_review content'.
  $permissions['create prenatal_symptom_review content'] = array(
    'name' => 'create prenatal_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_syphilis_test content'.
  $permissions['create prenatal_syphilis_test content'] = array(
    'name' => 'create prenatal_syphilis_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_tetanus_immunisation content'.
  $permissions['create prenatal_tetanus_immunisation content'] = array(
    'name' => 'create prenatal_tetanus_immunisation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_urine_dipstick_test content'.
  $permissions['create prenatal_urine_dipstick_test content'] = array(
    'name' => 'create prenatal_urine_dipstick_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any appointment_confirmation content'.
  $permissions['delete any appointment_confirmation content'] = array(
    'name' => 'delete any appointment_confirmation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_blood_gprs_test content'.
  $permissions['delete any prenatal_blood_gprs_test content'] = array(
    'name' => 'delete any prenatal_blood_gprs_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_breastfeeding content'.
  $permissions['delete any prenatal_breastfeeding content'] = array(
    'name' => 'delete any prenatal_breastfeeding content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_calcium content'.
  $permissions['delete any prenatal_calcium content'] = array(
    'name' => 'delete any prenatal_calcium content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_folate content'.
  $permissions['delete any prenatal_folate content'] = array(
    'name' => 'delete any prenatal_folate content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_follow_up content'.
  $permissions['delete any prenatal_follow_up content'] = array(
    'name' => 'delete any prenatal_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_gu_exam content'.
  $permissions['delete any prenatal_gu_exam content'] = array(
    'name' => 'delete any prenatal_gu_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_hemoglobin_test content'.
  $permissions['delete any prenatal_hemoglobin_test content'] = array(
    'name' => 'delete any prenatal_hemoglobin_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_hepatitis_b_test content'.
  $permissions['delete any prenatal_hepatitis_b_test content'] = array(
    'name' => 'delete any prenatal_hepatitis_b_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_hiv_pcr_test content'.
  $permissions['delete any prenatal_hiv_pcr_test content'] = array(
    'name' => 'delete any prenatal_hiv_pcr_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_hiv_test content'.
  $permissions['delete any prenatal_hiv_test content'] = array(
    'name' => 'delete any prenatal_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_iron content'.
  $permissions['delete any prenatal_iron content'] = array(
    'name' => 'delete any prenatal_iron content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_labs_results content'.
  $permissions['delete any prenatal_labs_results content'] = array(
    'name' => 'delete any prenatal_labs_results content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_malaria_test content'.
  $permissions['delete any prenatal_malaria_test content'] = array(
    'name' => 'delete any prenatal_malaria_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_mebendazole content'.
  $permissions['delete any prenatal_mebendazole content'] = array(
    'name' => 'delete any prenatal_mebendazole content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_medication_distribution content'.
  $permissions['delete any prenatal_medication_distribution content'] = array(
    'name' => 'delete any prenatal_medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_mental_health content'.
  $permissions['delete any prenatal_mental_health content'] = array(
    'name' => 'delete any prenatal_mental_health content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_mms content'.
  $permissions['delete any prenatal_mms content'] = array(
    'name' => 'delete any prenatal_mms content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_outside_care content'.
  $permissions['delete any prenatal_outside_care content'] = array(
    'name' => 'delete any prenatal_outside_care content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_partner_hiv_test content'.
  $permissions['delete any prenatal_partner_hiv_test content'] = array(
    'name' => 'delete any prenatal_partner_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_random_blood_sugar_test content'.
  $permissions['delete any prenatal_random_blood_sugar_test content'] = array(
    'name' => 'delete any prenatal_random_blood_sugar_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_send_to_hc content'.
  $permissions['delete any prenatal_send_to_hc content'] = array(
    'name' => 'delete any prenatal_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_speciality_care content'.
  $permissions['delete any prenatal_speciality_care content'] = array(
    'name' => 'delete any prenatal_speciality_care content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_symptom_review content'.
  $permissions['delete any prenatal_symptom_review content'] = array(
    'name' => 'delete any prenatal_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_syphilis_test content'.
  $permissions['delete any prenatal_syphilis_test content'] = array(
    'name' => 'delete any prenatal_syphilis_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_tetanus_immunisation content'.
  $permissions['delete any prenatal_tetanus_immunisation content'] = array(
    'name' => 'delete any prenatal_tetanus_immunisation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_urine_dipstick_test content'.
  $permissions['delete any prenatal_urine_dipstick_test content'] = array(
    'name' => 'delete any prenatal_urine_dipstick_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own appointment_confirmation content'.
  $permissions['delete own appointment_confirmation content'] = array(
    'name' => 'delete own appointment_confirmation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_blood_gprs_test content'.
  $permissions['delete own prenatal_blood_gprs_test content'] = array(
    'name' => 'delete own prenatal_blood_gprs_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_breastfeeding content'.
  $permissions['delete own prenatal_breastfeeding content'] = array(
    'name' => 'delete own prenatal_breastfeeding content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_calcium content'.
  $permissions['delete own prenatal_calcium content'] = array(
    'name' => 'delete own prenatal_calcium content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_folate content'.
  $permissions['delete own prenatal_folate content'] = array(
    'name' => 'delete own prenatal_folate content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_follow_up content'.
  $permissions['delete own prenatal_follow_up content'] = array(
    'name' => 'delete own prenatal_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_gu_exam content'.
  $permissions['delete own prenatal_gu_exam content'] = array(
    'name' => 'delete own prenatal_gu_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_hemoglobin_test content'.
  $permissions['delete own prenatal_hemoglobin_test content'] = array(
    'name' => 'delete own prenatal_hemoglobin_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_hepatitis_b_test content'.
  $permissions['delete own prenatal_hepatitis_b_test content'] = array(
    'name' => 'delete own prenatal_hepatitis_b_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_hiv_pcr_test content'.
  $permissions['delete own prenatal_hiv_pcr_test content'] = array(
    'name' => 'delete own prenatal_hiv_pcr_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_hiv_test content'.
  $permissions['delete own prenatal_hiv_test content'] = array(
    'name' => 'delete own prenatal_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_iron content'.
  $permissions['delete own prenatal_iron content'] = array(
    'name' => 'delete own prenatal_iron content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_labs_results content'.
  $permissions['delete own prenatal_labs_results content'] = array(
    'name' => 'delete own prenatal_labs_results content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_malaria_test content'.
  $permissions['delete own prenatal_malaria_test content'] = array(
    'name' => 'delete own prenatal_malaria_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_mebendazole content'.
  $permissions['delete own prenatal_mebendazole content'] = array(
    'name' => 'delete own prenatal_mebendazole content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_medication_distribution content'.
  $permissions['delete own prenatal_medication_distribution content'] = array(
    'name' => 'delete own prenatal_medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_mental_health content'.
  $permissions['delete own prenatal_mental_health content'] = array(
    'name' => 'delete own prenatal_mental_health content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_mms content'.
  $permissions['delete own prenatal_mms content'] = array(
    'name' => 'delete own prenatal_mms content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_outside_care content'.
  $permissions['delete own prenatal_outside_care content'] = array(
    'name' => 'delete own prenatal_outside_care content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_partner_hiv_test content'.
  $permissions['delete own prenatal_partner_hiv_test content'] = array(
    'name' => 'delete own prenatal_partner_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_random_blood_sugar_test content'.
  $permissions['delete own prenatal_random_blood_sugar_test content'] = array(
    'name' => 'delete own prenatal_random_blood_sugar_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_send_to_hc content'.
  $permissions['delete own prenatal_send_to_hc content'] = array(
    'name' => 'delete own prenatal_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_speciality_care content'.
  $permissions['delete own prenatal_speciality_care content'] = array(
    'name' => 'delete own prenatal_speciality_care content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_symptom_review content'.
  $permissions['delete own prenatal_symptom_review content'] = array(
    'name' => 'delete own prenatal_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_syphilis_test content'.
  $permissions['delete own prenatal_syphilis_test content'] = array(
    'name' => 'delete own prenatal_syphilis_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_tetanus_immunisation content'.
  $permissions['delete own prenatal_tetanus_immunisation content'] = array(
    'name' => 'delete own prenatal_tetanus_immunisation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_urine_dipstick_test content'.
  $permissions['delete own prenatal_urine_dipstick_test content'] = array(
    'name' => 'delete own prenatal_urine_dipstick_test content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any appointment_confirmation content'.
  $permissions['edit any appointment_confirmation content'] = array(
    'name' => 'edit any appointment_confirmation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_blood_gprs_test content'.
  $permissions['edit any prenatal_blood_gprs_test content'] = array(
    'name' => 'edit any prenatal_blood_gprs_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_breastfeeding content'.
  $permissions['edit any prenatal_breastfeeding content'] = array(
    'name' => 'edit any prenatal_breastfeeding content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_calcium content'.
  $permissions['edit any prenatal_calcium content'] = array(
    'name' => 'edit any prenatal_calcium content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_folate content'.
  $permissions['edit any prenatal_folate content'] = array(
    'name' => 'edit any prenatal_folate content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_follow_up content'.
  $permissions['edit any prenatal_follow_up content'] = array(
    'name' => 'edit any prenatal_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_gu_exam content'.
  $permissions['edit any prenatal_gu_exam content'] = array(
    'name' => 'edit any prenatal_gu_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_hemoglobin_test content'.
  $permissions['edit any prenatal_hemoglobin_test content'] = array(
    'name' => 'edit any prenatal_hemoglobin_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_hepatitis_b_test content'.
  $permissions['edit any prenatal_hepatitis_b_test content'] = array(
    'name' => 'edit any prenatal_hepatitis_b_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_hiv_pcr_test content'.
  $permissions['edit any prenatal_hiv_pcr_test content'] = array(
    'name' => 'edit any prenatal_hiv_pcr_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_hiv_test content'.
  $permissions['edit any prenatal_hiv_test content'] = array(
    'name' => 'edit any prenatal_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_iron content'.
  $permissions['edit any prenatal_iron content'] = array(
    'name' => 'edit any prenatal_iron content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_labs_results content'.
  $permissions['edit any prenatal_labs_results content'] = array(
    'name' => 'edit any prenatal_labs_results content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_malaria_test content'.
  $permissions['edit any prenatal_malaria_test content'] = array(
    'name' => 'edit any prenatal_malaria_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_mebendazole content'.
  $permissions['edit any prenatal_mebendazole content'] = array(
    'name' => 'edit any prenatal_mebendazole content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_medication_distribution content'.
  $permissions['edit any prenatal_medication_distribution content'] = array(
    'name' => 'edit any prenatal_medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_mental_health content'.
  $permissions['edit any prenatal_mental_health content'] = array(
    'name' => 'edit any prenatal_mental_health content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_mms content'.
  $permissions['edit any prenatal_mms content'] = array(
    'name' => 'edit any prenatal_mms content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_outside_care content'.
  $permissions['edit any prenatal_outside_care content'] = array(
    'name' => 'edit any prenatal_outside_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_partner_hiv_test content'.
  $permissions['edit any prenatal_partner_hiv_test content'] = array(
    'name' => 'edit any prenatal_partner_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_random_blood_sugar_test content'.
  $permissions['edit any prenatal_random_blood_sugar_test content'] = array(
    'name' => 'edit any prenatal_random_blood_sugar_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_send_to_hc content'.
  $permissions['edit any prenatal_send_to_hc content'] = array(
    'name' => 'edit any prenatal_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_speciality_care content'.
  $permissions['edit any prenatal_speciality_care content'] = array(
    'name' => 'edit any prenatal_speciality_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_symptom_review content'.
  $permissions['edit any prenatal_symptom_review content'] = array(
    'name' => 'edit any prenatal_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_syphilis_test content'.
  $permissions['edit any prenatal_syphilis_test content'] = array(
    'name' => 'edit any prenatal_syphilis_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_tetanus_immunisation content'.
  $permissions['edit any prenatal_tetanus_immunisation content'] = array(
    'name' => 'edit any prenatal_tetanus_immunisation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_urine_dipstick_test content'.
  $permissions['edit any prenatal_urine_dipstick_test content'] = array(
    'name' => 'edit any prenatal_urine_dipstick_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own appointment_confirmation content'.
  $permissions['edit own appointment_confirmation content'] = array(
    'name' => 'edit own appointment_confirmation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_blood_gprs_test content'.
  $permissions['edit own prenatal_blood_gprs_test content'] = array(
    'name' => 'edit own prenatal_blood_gprs_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_breastfeeding content'.
  $permissions['edit own prenatal_breastfeeding content'] = array(
    'name' => 'edit own prenatal_breastfeeding content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_calcium content'.
  $permissions['edit own prenatal_calcium content'] = array(
    'name' => 'edit own prenatal_calcium content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_folate content'.
  $permissions['edit own prenatal_folate content'] = array(
    'name' => 'edit own prenatal_folate content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_follow_up content'.
  $permissions['edit own prenatal_follow_up content'] = array(
    'name' => 'edit own prenatal_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_gu_exam content'.
  $permissions['edit own prenatal_gu_exam content'] = array(
    'name' => 'edit own prenatal_gu_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_hemoglobin_test content'.
  $permissions['edit own prenatal_hemoglobin_test content'] = array(
    'name' => 'edit own prenatal_hemoglobin_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_hepatitis_b_test content'.
  $permissions['edit own prenatal_hepatitis_b_test content'] = array(
    'name' => 'edit own prenatal_hepatitis_b_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_hiv_pcr_test content'.
  $permissions['edit own prenatal_hiv_pcr_test content'] = array(
    'name' => 'edit own prenatal_hiv_pcr_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_hiv_test content'.
  $permissions['edit own prenatal_hiv_test content'] = array(
    'name' => 'edit own prenatal_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_iron content'.
  $permissions['edit own prenatal_iron content'] = array(
    'name' => 'edit own prenatal_iron content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_labs_results content'.
  $permissions['edit own prenatal_labs_results content'] = array(
    'name' => 'edit own prenatal_labs_results content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_malaria_test content'.
  $permissions['edit own prenatal_malaria_test content'] = array(
    'name' => 'edit own prenatal_malaria_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_mebendazole content'.
  $permissions['edit own prenatal_mebendazole content'] = array(
    'name' => 'edit own prenatal_mebendazole content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_medication_distribution content'.
  $permissions['edit own prenatal_medication_distribution content'] = array(
    'name' => 'edit own prenatal_medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_mental_health content'.
  $permissions['edit own prenatal_mental_health content'] = array(
    'name' => 'edit own prenatal_mental_health content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_mms content'.
  $permissions['edit own prenatal_mms content'] = array(
    'name' => 'edit own prenatal_mms content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_outside_care content'.
  $permissions['edit own prenatal_outside_care content'] = array(
    'name' => 'edit own prenatal_outside_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_partner_hiv_test content'.
  $permissions['edit own prenatal_partner_hiv_test content'] = array(
    'name' => 'edit own prenatal_partner_hiv_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_random_blood_sugar_test content'.
  $permissions['edit own prenatal_random_blood_sugar_test content'] = array(
    'name' => 'edit own prenatal_random_blood_sugar_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_send_to_hc content'.
  $permissions['edit own prenatal_send_to_hc content'] = array(
    'name' => 'edit own prenatal_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_speciality_care content'.
  $permissions['edit own prenatal_speciality_care content'] = array(
    'name' => 'edit own prenatal_speciality_care content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_symptom_review content'.
  $permissions['edit own prenatal_symptom_review content'] = array(
    'name' => 'edit own prenatal_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_syphilis_test content'.
  $permissions['edit own prenatal_syphilis_test content'] = array(
    'name' => 'edit own prenatal_syphilis_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_tetanus_immunisation content'.
  $permissions['edit own prenatal_tetanus_immunisation content'] = array(
    'name' => 'edit own prenatal_tetanus_immunisation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_urine_dipstick_test content'.
  $permissions['edit own prenatal_urine_dipstick_test content'] = array(
    'name' => 'edit own prenatal_urine_dipstick_test content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
