<?php

/**
 * @file
 * hedley_device.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_device_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create device content'.
  $permissions['create device content'] = array(
    'name' => 'create device content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create sync_incident content'.
  $permissions['create sync_incident content'] = array(
    'name' => 'create sync_incident content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any device content'.
  $permissions['delete any device content'] = array(
    'name' => 'delete any device content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any sync_incident content'.
  $permissions['delete any sync_incident content'] = array(
    'name' => 'delete any sync_incident content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own device content'.
  $permissions['delete own device content'] = array(
    'name' => 'delete own device content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own sync_incident content'.
  $permissions['delete own sync_incident content'] = array(
    'name' => 'delete own sync_incident content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any device content'.
  $permissions['edit any device content'] = array(
    'name' => 'edit any device content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any sync_incident content'.
  $permissions['edit any sync_incident content'] = array(
    'name' => 'edit any sync_incident content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own device content'.
  $permissions['edit own device content'] = array(
    'name' => 'edit own device content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own sync_incident content'.
  $permissions['edit own sync_incident content'] = array(
    'name' => 'edit own sync_incident content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
