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

  // Exported permission: 'create prenatal_follow_up content'.
  $permissions['create prenatal_follow_up content'] = array(
    'name' => 'create prenatal_follow_up content',
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

  // Exported permission: 'create prenatal_hiv_test content'.
  $permissions['create prenatal_hiv_test content'] = array(
    'name' => 'create prenatal_hiv_test content',
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

  // Exported permission: 'create prenatal_syphilis_test content'.
  $permissions['create prenatal_syphilis_test content'] = array(
    'name' => 'create prenatal_syphilis_test content',
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

  // Exported permission: 'delete any prenatal_follow_up content'.
  $permissions['delete any prenatal_follow_up content'] = array(
    'name' => 'delete any prenatal_follow_up content',
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

  // Exported permission: 'delete any prenatal_hiv_test content'.
  $permissions['delete any prenatal_hiv_test content'] = array(
    'name' => 'delete any prenatal_hiv_test content',
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

  // Exported permission: 'delete any prenatal_syphilis_test content'.
  $permissions['delete any prenatal_syphilis_test content'] = array(
    'name' => 'delete any prenatal_syphilis_test content',
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

  // Exported permission: 'delete own prenatal_follow_up content'.
  $permissions['delete own prenatal_follow_up content'] = array(
    'name' => 'delete own prenatal_follow_up content',
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

  // Exported permission: 'delete own prenatal_hiv_test content'.
  $permissions['delete own prenatal_hiv_test content'] = array(
    'name' => 'delete own prenatal_hiv_test content',
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

  // Exported permission: 'delete own prenatal_syphilis_test content'.
  $permissions['delete own prenatal_syphilis_test content'] = array(
    'name' => 'delete own prenatal_syphilis_test content',
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

  // Exported permission: 'edit any prenatal_follow_up content'.
  $permissions['edit any prenatal_follow_up content'] = array(
    'name' => 'edit any prenatal_follow_up content',
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

  // Exported permission: 'edit any prenatal_hiv_test content'.
  $permissions['edit any prenatal_hiv_test content'] = array(
    'name' => 'edit any prenatal_hiv_test content',
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

  // Exported permission: 'edit any prenatal_syphilis_test content'.
  $permissions['edit any prenatal_syphilis_test content'] = array(
    'name' => 'edit any prenatal_syphilis_test content',
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

  // Exported permission: 'edit own prenatal_follow_up content'.
  $permissions['edit own prenatal_follow_up content'] = array(
    'name' => 'edit own prenatal_follow_up content',
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

  // Exported permission: 'edit own prenatal_hiv_test content'.
  $permissions['edit own prenatal_hiv_test content'] = array(
    'name' => 'edit own prenatal_hiv_test content',
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

  // Exported permission: 'edit own prenatal_syphilis_test content'.
  $permissions['edit own prenatal_syphilis_test content'] = array(
    'name' => 'edit own prenatal_syphilis_test content',
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
