<?php

/**
 * @file
 * hedley_ncd.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_ncd_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create ncd_danger_signs content'.
  $permissions['create ncd_danger_signs content'] = array(
    'name' => 'create ncd_danger_signs content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete any ncd_danger_signs content'.
  $permissions['delete any ncd_danger_signs content'] = array(
    'name' => 'delete any ncd_danger_signs content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete own ncd_danger_signs content'.
  $permissions['delete own ncd_danger_signs content'] = array(
    'name' => 'delete own ncd_danger_signs content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit any ncd_danger_signs content'.
  $permissions['edit any ncd_danger_signs content'] = array(
    'name' => 'edit any ncd_danger_signs content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit own ncd_danger_signs content'.
  $permissions['edit own ncd_danger_signs content'] = array(
    'name' => 'edit own ncd_danger_signs content',
    'roles' => array(),
    'module' => 'node',
  );

  return $permissions;
}
