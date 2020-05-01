<?php

/**
 * @file
 * hedley_chw.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_chw_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create village content'.
  $permissions['create village content'] = array(
    'name' => 'create village content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any village content'.
  $permissions['delete any village content'] = array(
    'name' => 'delete any village content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own village content'.
  $permissions['delete own village content'] = array(
    'name' => 'delete own village content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any village content'.
  $permissions['edit any village content'] = array(
    'name' => 'edit any village content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own village content'.
  $permissions['edit own village content'] = array(
    'name' => 'edit own village content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
