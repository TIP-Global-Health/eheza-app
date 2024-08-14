<?php

/**
 * @file
 * hedley_forms.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_forms_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create participant_consent content'.
  $permissions['create participant_consent content'] = array(
    'name' => 'create participant_consent content',
    'roles' => array(
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create participant_form content'.
  $permissions['create participant_form content'] = array(
    'name' => 'create participant_form content',
    'roles' => array(
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any participant_consent content'.
  $permissions['delete any participant_consent content'] = array(
    'name' => 'delete any participant_consent content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete any participant_form content'.
  $permissions['delete any participant_form content'] = array(
    'name' => 'delete any participant_form content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete own participant_consent content'.
  $permissions['delete own participant_consent content'] = array(
    'name' => 'delete own participant_consent content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete own participant_form content'.
  $permissions['delete own participant_form content'] = array(
    'name' => 'delete own participant_form content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit any participant_consent content'.
  $permissions['edit any participant_consent content'] = array(
    'name' => 'edit any participant_consent content',
    'roles' => array(
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any participant_form content'.
  $permissions['edit any participant_form content'] = array(
    'name' => 'edit any participant_form content',
    'roles' => array(
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own participant_consent content'.
  $permissions['edit own participant_consent content'] = array(
    'name' => 'edit own participant_consent content',
    'roles' => array(
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own participant_form content'.
  $permissions['edit own participant_form content'] = array(
    'name' => 'edit own participant_form content',
    'roles' => array(
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
