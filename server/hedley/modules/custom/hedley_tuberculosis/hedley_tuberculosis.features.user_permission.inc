<?php

/**
 * @file
 * hedley_tuberculosis.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_tuberculosis_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create tuberculosis_diagnostics content'.
  $permissions['create tuberculosis_diagnostics content'] = array(
    'name' => 'create tuberculosis_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_dot content'.
  $permissions['create tuberculosis_dot content'] = array(
    'name' => 'create tuberculosis_dot content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_encounter content'.
  $permissions['create tuberculosis_encounter content'] = array(
    'name' => 'create tuberculosis_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_follow_up content'.
  $permissions['create tuberculosis_follow_up content'] = array(
    'name' => 'create tuberculosis_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_health_education content'.
  $permissions['create tuberculosis_health_education content'] = array(
    'name' => 'create tuberculosis_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_medication content'.
  $permissions['create tuberculosis_medication content'] = array(
    'name' => 'create tuberculosis_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_referral content'.
  $permissions['create tuberculosis_referral content'] = array(
    'name' => 'create tuberculosis_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_symptom_review content'.
  $permissions['create tuberculosis_symptom_review content'] = array(
    'name' => 'create tuberculosis_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create tuberculosis_treatment_review content'.
  $permissions['create tuberculosis_treatment_review content'] = array(
    'name' => 'create tuberculosis_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_diagnostics content'.
  $permissions['delete any tuberculosis_diagnostics content'] = array(
    'name' => 'delete any tuberculosis_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_dot content'.
  $permissions['delete any tuberculosis_dot content'] = array(
    'name' => 'delete any tuberculosis_dot content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_encounter content'.
  $permissions['delete any tuberculosis_encounter content'] = array(
    'name' => 'delete any tuberculosis_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_follow_up content'.
  $permissions['delete any tuberculosis_follow_up content'] = array(
    'name' => 'delete any tuberculosis_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_health_education content'.
  $permissions['delete any tuberculosis_health_education content'] = array(
    'name' => 'delete any tuberculosis_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_medication content'.
  $permissions['delete any tuberculosis_medication content'] = array(
    'name' => 'delete any tuberculosis_medication content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_referral content'.
  $permissions['delete any tuberculosis_referral content'] = array(
    'name' => 'delete any tuberculosis_referral content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_symptom_review content'.
  $permissions['delete any tuberculosis_symptom_review content'] = array(
    'name' => 'delete any tuberculosis_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_treatment_review content'.
  $permissions['delete any tuberculosis_treatment_review content'] = array(
    'name' => 'delete any tuberculosis_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_diagnostics content'.
  $permissions['delete own tuberculosis_diagnostics content'] = array(
    'name' => 'delete own tuberculosis_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_dot content'.
  $permissions['delete own tuberculosis_dot content'] = array(
    'name' => 'delete own tuberculosis_dot content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_encounter content'.
  $permissions['delete own tuberculosis_encounter content'] = array(
    'name' => 'delete own tuberculosis_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_follow_up content'.
  $permissions['delete own tuberculosis_follow_up content'] = array(
    'name' => 'delete own tuberculosis_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_health_education content'.
  $permissions['delete own tuberculosis_health_education content'] = array(
    'name' => 'delete own tuberculosis_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_medication content'.
  $permissions['delete own tuberculosis_medication content'] = array(
    'name' => 'delete own tuberculosis_medication content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_referral content'.
  $permissions['delete own tuberculosis_referral content'] = array(
    'name' => 'delete own tuberculosis_referral content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_symptom_review content'.
  $permissions['delete own tuberculosis_symptom_review content'] = array(
    'name' => 'delete own tuberculosis_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_treatment_review content'.
  $permissions['delete own tuberculosis_treatment_review content'] = array(
    'name' => 'delete own tuberculosis_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_diagnostics content'.
  $permissions['edit any tuberculosis_diagnostics content'] = array(
    'name' => 'edit any tuberculosis_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_dot content'.
  $permissions['edit any tuberculosis_dot content'] = array(
    'name' => 'edit any tuberculosis_dot content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_encounter content'.
  $permissions['edit any tuberculosis_encounter content'] = array(
    'name' => 'edit any tuberculosis_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_follow_up content'.
  $permissions['edit any tuberculosis_follow_up content'] = array(
    'name' => 'edit any tuberculosis_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_health_education content'.
  $permissions['edit any tuberculosis_health_education content'] = array(
    'name' => 'edit any tuberculosis_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_medication content'.
  $permissions['edit any tuberculosis_medication content'] = array(
    'name' => 'edit any tuberculosis_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_referral content'.
  $permissions['edit any tuberculosis_referral content'] = array(
    'name' => 'edit any tuberculosis_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_symptom_review content'.
  $permissions['edit any tuberculosis_symptom_review content'] = array(
    'name' => 'edit any tuberculosis_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_treatment_review content'.
  $permissions['edit any tuberculosis_treatment_review content'] = array(
    'name' => 'edit any tuberculosis_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_diagnostics content'.
  $permissions['edit own tuberculosis_diagnostics content'] = array(
    'name' => 'edit own tuberculosis_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_dot content'.
  $permissions['edit own tuberculosis_dot content'] = array(
    'name' => 'edit own tuberculosis_dot content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_encounter content'.
  $permissions['edit own tuberculosis_encounter content'] = array(
    'name' => 'edit own tuberculosis_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_follow_up content'.
  $permissions['edit own tuberculosis_follow_up content'] = array(
    'name' => 'edit own tuberculosis_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_health_education content'.
  $permissions['edit own tuberculosis_health_education content'] = array(
    'name' => 'edit own tuberculosis_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_medication content'.
  $permissions['edit own tuberculosis_medication content'] = array(
    'name' => 'edit own tuberculosis_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_referral content'.
  $permissions['edit own tuberculosis_referral content'] = array(
    'name' => 'edit own tuberculosis_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_symptom_review content'.
  $permissions['edit own tuberculosis_symptom_review content'] = array(
    'name' => 'edit own tuberculosis_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_treatment_review content'.
  $permissions['edit own tuberculosis_treatment_review content'] = array(
    'name' => 'edit own tuberculosis_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
