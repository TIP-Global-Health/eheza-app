<?php

/**
 * @file
 * hedley_reports.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_reports_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create report_data content'.
  $permissions['create report_data content'] = array(
    'name' => 'create report_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any report_data content'.
  $permissions['delete any report_data content'] = array(
    'name' => 'delete any report_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own report_data content'.
  $permissions['delete own report_data content'] = array(
    'name' => 'delete own report_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any report_data content'.
  $permissions['edit any report_data content'] = array(
    'name' => 'edit any report_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own report_data content'.
  $permissions['edit own report_data content'] = array(
    'name' => 'edit own report_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  return $permissions;
}
