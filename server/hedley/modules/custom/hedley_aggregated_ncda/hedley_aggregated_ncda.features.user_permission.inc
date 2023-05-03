<?php

/**
 * @file
 * hedley_aggregated_ncda.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_aggregated_ncda_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create ncda_individual_data content'.
  $permissions['create ncda_individual_data content'] = array(
    'name' => 'create ncda_individual_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any ncda_individual_data content'.
  $permissions['delete any ncda_individual_data content'] = array(
    'name' => 'delete any ncda_individual_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own ncda_individual_data content'.
  $permissions['delete own ncda_individual_data content'] = array(
    'name' => 'delete own ncda_individual_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any ncda_individual_data content'.
  $permissions['edit any ncda_individual_data content'] = array(
    'name' => 'edit any ncda_individual_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own ncda_individual_data content'.
  $permissions['edit own ncda_individual_data content'] = array(
    'name' => 'edit own ncda_individual_data content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  return $permissions;
}
