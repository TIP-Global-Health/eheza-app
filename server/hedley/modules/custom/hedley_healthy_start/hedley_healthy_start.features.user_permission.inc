<?php

/**
 * @file
 * hedley_healthy_start.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_healthy_start_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create healthy_start_encounter content'.
  $permissions['create healthy_start_encounter content'] = array(
    'name' => 'create healthy_start_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any healthy_start_encounter content'.
  $permissions['delete any healthy_start_encounter content'] = array(
    'name' => 'delete any healthy_start_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own healthy_start_encounter content'.
  $permissions['delete own healthy_start_encounter content'] = array(
    'name' => 'delete own healthy_start_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any healthy_start_encounter content'.
  $permissions['edit any healthy_start_encounter content'] = array(
    'name' => 'edit any healthy_start_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own healthy_start_encounter content'.
  $permissions['edit own healthy_start_encounter content'] = array(
    'name' => 'edit own healthy_start_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
