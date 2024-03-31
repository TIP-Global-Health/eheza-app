<?php

/**
 * @file
 * hedley_hiv.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_hiv_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create hiv_diagnostics content'.
  $permissions['create hiv_diagnostics content'] = array(
    'name' => 'create hiv_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_encounter content'.
  $permissions['create hiv_encounter content'] = array(
    'name' => 'create hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_follow_up content'.
  $permissions['create hiv_follow_up content'] = array(
    'name' => 'create hiv_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_health_education content'.
  $permissions['create hiv_health_education content'] = array(
    'name' => 'create hiv_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_medication content'.
  $permissions['create hiv_medication content'] = array(
    'name' => 'create hiv_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_referral content'.
  $permissions['create hiv_referral content'] = array(
    'name' => 'create hiv_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_symptom_review content'.
  $permissions['create hiv_symptom_review content'] = array(
    'name' => 'create hiv_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hiv_treatment_review content'.
  $permissions['create hiv_treatment_review content'] = array(
    'name' => 'create hiv_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_diagnostics content'.
  $permissions['delete any hiv_diagnostics content'] = array(
    'name' => 'delete any hiv_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_encounter content'.
  $permissions['delete any hiv_encounter content'] = array(
    'name' => 'delete any hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_follow_up content'.
  $permissions['delete any hiv_follow_up content'] = array(
    'name' => 'delete any hiv_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_health_education content'.
  $permissions['delete any hiv_health_education content'] = array(
    'name' => 'delete any hiv_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_medication content'.
  $permissions['delete any hiv_medication content'] = array(
    'name' => 'delete any hiv_medication content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_referral content'.
  $permissions['delete any hiv_referral content'] = array(
    'name' => 'delete any hiv_referral content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_symptom_review content'.
  $permissions['delete any hiv_symptom_review content'] = array(
    'name' => 'delete any hiv_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_treatment_review content'.
  $permissions['delete any hiv_treatment_review content'] = array(
    'name' => 'delete any hiv_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_diagnostics content'.
  $permissions['delete own hiv_diagnostics content'] = array(
    'name' => 'delete own hiv_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_encounter content'.
  $permissions['delete own hiv_encounter content'] = array(
    'name' => 'delete own hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_follow_up content'.
  $permissions['delete own hiv_follow_up content'] = array(
    'name' => 'delete own hiv_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_health_education content'.
  $permissions['delete own hiv_health_education content'] = array(
    'name' => 'delete own hiv_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_medication content'.
  $permissions['delete own hiv_medication content'] = array(
    'name' => 'delete own hiv_medication content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_referral content'.
  $permissions['delete own hiv_referral content'] = array(
    'name' => 'delete own hiv_referral content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_symptom_review content'.
  $permissions['delete own hiv_symptom_review content'] = array(
    'name' => 'delete own hiv_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_treatment_review content'.
  $permissions['delete own hiv_treatment_review content'] = array(
    'name' => 'delete own hiv_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_diagnostics content'.
  $permissions['edit any hiv_diagnostics content'] = array(
    'name' => 'edit any hiv_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_encounter content'.
  $permissions['edit any hiv_encounter content'] = array(
    'name' => 'edit any hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_follow_up content'.
  $permissions['edit any hiv_follow_up content'] = array(
    'name' => 'edit any hiv_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_health_education content'.
  $permissions['edit any hiv_health_education content'] = array(
    'name' => 'edit any hiv_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_medication content'.
  $permissions['edit any hiv_medication content'] = array(
    'name' => 'edit any hiv_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_referral content'.
  $permissions['edit any hiv_referral content'] = array(
    'name' => 'edit any hiv_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_symptom_review content'.
  $permissions['edit any hiv_symptom_review content'] = array(
    'name' => 'edit any hiv_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_treatment_review content'.
  $permissions['edit any hiv_treatment_review content'] = array(
    'name' => 'edit any hiv_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_diagnostics content'.
  $permissions['edit own hiv_diagnostics content'] = array(
    'name' => 'edit own hiv_diagnostics content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_encounter content'.
  $permissions['edit own hiv_encounter content'] = array(
    'name' => 'edit own hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_follow_up content'.
  $permissions['edit own hiv_follow_up content'] = array(
    'name' => 'edit own hiv_follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_health_education content'.
  $permissions['edit own hiv_health_education content'] = array(
    'name' => 'edit own hiv_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_medication content'.
  $permissions['edit own hiv_medication content'] = array(
    'name' => 'edit own hiv_medication content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_referral content'.
  $permissions['edit own hiv_referral content'] = array(
    'name' => 'edit own hiv_referral content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_symptom_review content'.
  $permissions['edit own hiv_symptom_review content'] = array(
    'name' => 'edit own hiv_symptom_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_treatment_review content'.
  $permissions['edit own hiv_treatment_review content'] = array(
    'name' => 'edit own hiv_treatment_review content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
