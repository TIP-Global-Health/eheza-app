<?php

/**
 * @file
 * hedley_stock_management.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_stock_management_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create nurse_signature content'.
  $permissions['create nurse_signature content'] = array(
    'name' => 'create nurse_signature content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nurse_signature content'.
  $permissions['delete any nurse_signature content'] = array(
    'name' => 'delete any nurse_signature content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nurse_signature content'.
  $permissions['delete own nurse_signature content'] = array(
    'name' => 'delete own nurse_signature content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nurse_signature content'.
  $permissions['edit any nurse_signature content'] = array(
    'name' => 'edit any nurse_signature content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nurse_signature content'.
  $permissions['edit own nurse_signature content'] = array(
    'name' => 'edit own nurse_signature content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
