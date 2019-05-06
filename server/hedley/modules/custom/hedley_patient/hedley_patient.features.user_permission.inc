<?php
/**
 * @file
 * hedley_patient.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_patient_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create child content'.
  $permissions['create child content'] = array(
    'name' => 'create child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create mother content'.
  $permissions['create mother content'] = array(
    'name' => 'create mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child content'.
  $permissions['delete any child content'] = array(
    'name' => 'delete any child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any mother content'.
  $permissions['delete any mother content'] = array(
    'name' => 'delete any mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child content'.
  $permissions['delete own child content'] = array(
    'name' => 'delete own child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own mother content'.
  $permissions['delete own mother content'] = array(
    'name' => 'delete own mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child content'.
  $permissions['edit any child content'] = array(
    'name' => 'edit any child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any mother content'.
  $permissions['edit any mother content'] = array(
    'name' => 'edit any mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child content'.
  $permissions['edit own child content'] = array(
    'name' => 'edit own child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own mother content'.
  $permissions['edit own mother content'] = array(
    'name' => 'edit own mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
