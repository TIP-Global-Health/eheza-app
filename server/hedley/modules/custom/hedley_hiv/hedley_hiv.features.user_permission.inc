<?php

/**
 * @file
 * hedley_hiv.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_hiv_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create hiv_encounter content'.
  $permissions['create hiv_encounter content'] = array(
    'name' => 'create hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hiv_encounter content'.
  $permissions['delete any hiv_encounter content'] = array(
    'name' => 'delete any hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hiv_encounter content'.
  $permissions['delete own hiv_encounter content'] = array(
    'name' => 'delete own hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hiv_encounter content'.
  $permissions['edit any hiv_encounter content'] = array(
    'name' => 'edit any hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hiv_encounter content'.
  $permissions['edit own hiv_encounter content'] = array(
    'name' => 'edit own hiv_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
