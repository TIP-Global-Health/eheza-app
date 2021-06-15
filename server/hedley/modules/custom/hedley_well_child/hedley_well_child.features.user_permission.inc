<?php

/**
 * @file
 * hedley_well_child.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_well_child_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create well_child_encounter content'.
  $permissions['create well_child_encounter content'] = array(
    'name' => 'create well_child_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any well_child_encounter content'.
  $permissions['delete any well_child_encounter content'] = array(
    'name' => 'delete any well_child_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own well_child_encounter content'.
  $permissions['delete own well_child_encounter content'] = array(
    'name' => 'delete own well_child_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any well_child_encounter content'.
  $permissions['edit any well_child_encounter content'] = array(
    'name' => 'edit any well_child_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own well_child_encounter content'.
  $permissions['edit own well_child_encounter content'] = array(
    'name' => 'edit own well_child_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
