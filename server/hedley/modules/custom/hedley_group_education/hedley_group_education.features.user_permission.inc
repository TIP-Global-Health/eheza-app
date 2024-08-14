<?php

/**
 * @file
 * hedley_group_education.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_group_education_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create education_session content'.
  $permissions['create education_session content'] = array(
    'name' => 'create education_session content',
    'roles' => array(
      'E-ledger viewer' => 'E-ledger viewer',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any education_session content'.
  $permissions['delete any education_session content'] = array(
    'name' => 'delete any education_session content',
    'roles' => array(
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own education_session content'.
  $permissions['delete own education_session content'] = array(
    'name' => 'delete own education_session content',
    'roles' => array(
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any education_session content'.
  $permissions['edit any education_session content'] = array(
    'name' => 'edit any education_session content',
    'roles' => array(
      'E-ledger viewer' => 'E-ledger viewer',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own education_session content'.
  $permissions['edit own education_session content'] = array(
    'name' => 'edit own education_session content',
    'roles' => array(
      'E-ledger viewer' => 'E-ledger viewer',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
