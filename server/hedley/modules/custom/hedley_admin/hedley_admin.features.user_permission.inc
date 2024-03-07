<?php

/**
 * @file
 * hedley_admin.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_admin_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'access content'.
  $permissions['access content'] = array(
    'name' => 'access content',
    'roles' => array(
      'E-ledger viewer' => 'E-ledger viewer',
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
