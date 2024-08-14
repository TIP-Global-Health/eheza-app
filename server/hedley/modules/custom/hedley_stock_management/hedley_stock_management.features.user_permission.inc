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

  // Exported permission: 'create stock_update content'.
  $permissions['create stock_update content'] = array(
    'name' => 'create stock_update content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any stock_update content'.
  $permissions['delete any stock_update content'] = array(
    'name' => 'delete any stock_update content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own stock_update content'.
  $permissions['delete own stock_update content'] = array(
    'name' => 'delete own stock_update content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any stock_update content'.
  $permissions['edit any stock_update content'] = array(
    'name' => 'edit any stock_update content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own stock_update content'.
  $permissions['edit own stock_update content'] = array(
    'name' => 'edit own stock_update content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
