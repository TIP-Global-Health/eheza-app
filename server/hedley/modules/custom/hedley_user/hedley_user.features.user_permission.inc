<?php
/**
 * @file
 * hedley_user.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_user_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create nurse content'.
  $permissions['create nurse content'] = array(
    'name' => 'create nurse content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nurse content'.
  $permissions['delete any nurse content'] = array(
    'name' => 'delete any nurse content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nurse content'.
  $permissions['delete own nurse content'] = array(
    'name' => 'delete own nurse content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nurse content'.
  $permissions['edit any nurse content'] = array(
    'name' => 'edit any nurse content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nurse content'.
  $permissions['edit own nurse content'] = array(
    'name' => 'edit own nurse content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
