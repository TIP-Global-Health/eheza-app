<?php

/**
 * @file
 * hedley_tuberculosis.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_tuberculosis_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create tuberculosis_encounter content'.
  $permissions['create tuberculosis_encounter content'] = array(
    'name' => 'create tuberculosis_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete any tuberculosis_encounter content'.
  $permissions['delete any tuberculosis_encounter content'] = array(
    'name' => 'delete any tuberculosis_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete own tuberculosis_encounter content'.
  $permissions['delete own tuberculosis_encounter content'] = array(
    'name' => 'delete own tuberculosis_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit any tuberculosis_encounter content'.
  $permissions['edit any tuberculosis_encounter content'] = array(
    'name' => 'edit any tuberculosis_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit own tuberculosis_encounter content'.
  $permissions['edit own tuberculosis_encounter content'] = array(
    'name' => 'edit own tuberculosis_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  return $permissions;
}
