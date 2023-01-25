<?php

/**
 * @file
 * hedley_resilience.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_resilience_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create resilience_survey content'.
  $permissions['create resilience_survey content'] = array(
    'name' => 'create resilience_survey content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any resilience_survey content'.
  $permissions['delete any resilience_survey content'] = array(
    'name' => 'delete any resilience_survey content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own resilience_survey content'.
  $permissions['delete own resilience_survey content'] = array(
    'name' => 'delete own resilience_survey content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any resilience_survey content'.
  $permissions['edit any resilience_survey content'] = array(
    'name' => 'edit any resilience_survey content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own resilience_survey content'.
  $permissions['edit own resilience_survey content'] = array(
    'name' => 'edit own resilience_survey content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
