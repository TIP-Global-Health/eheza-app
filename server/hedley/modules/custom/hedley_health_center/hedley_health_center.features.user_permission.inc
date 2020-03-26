<?php

/**
 * @file
 * hedley_health_center.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_health_center_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create catchment_area content'.
  $permissions['create catchment_area content'] = array(
    'name' => 'create catchment_area content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create health_center content'.
  $permissions['create health_center content'] = array(
    'name' => 'create health_center content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any catchment_area content'.
  $permissions['delete any catchment_area content'] = array(
    'name' => 'delete any catchment_area content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any health_center content'.
  $permissions['delete any health_center content'] = array(
    'name' => 'delete any health_center content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own catchment_area content'.
  $permissions['delete own catchment_area content'] = array(
    'name' => 'delete own catchment_area content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own health_center content'.
  $permissions['delete own health_center content'] = array(
    'name' => 'delete own health_center content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any catchment_area content'.
  $permissions['edit any catchment_area content'] = array(
    'name' => 'edit any catchment_area content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any health_center content'.
  $permissions['edit any health_center content'] = array(
    'name' => 'edit any health_center content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own catchment_area content'.
  $permissions['edit own catchment_area content'] = array(
    'name' => 'edit own catchment_area content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own health_center content'.
  $permissions['edit own health_center content'] = array(
    'name' => 'edit own health_center content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
