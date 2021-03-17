<?php

/**
 * @file
 * hedley_nutrition.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_nutrition_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create contributing_factors content'.
  $permissions['create contributing_factors content'] = array(
    'name' => 'create contributing_factors content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_health_education content'.
  $permissions['create nutrition_health_education content'] = array(
    'name' => 'create nutrition_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_send_to_hc content'.
  $permissions['create nutrition_send_to_hc content'] = array(
    'name' => 'create nutrition_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any contributing_factors content'.
  $permissions['delete any contributing_factors content'] = array(
    'name' => 'delete any contributing_factors content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_health_education content'.
  $permissions['delete any nutrition_health_education content'] = array(
    'name' => 'delete any nutrition_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_send_to_hc content'.
  $permissions['delete any nutrition_send_to_hc content'] = array(
    'name' => 'delete any nutrition_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own contributing_factors content'.
  $permissions['delete own contributing_factors content'] = array(
    'name' => 'delete own contributing_factors content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_health_education content'.
  $permissions['delete own nutrition_health_education content'] = array(
    'name' => 'delete own nutrition_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_send_to_hc content'.
  $permissions['delete own nutrition_send_to_hc content'] = array(
    'name' => 'delete own nutrition_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any contributing_factors content'.
  $permissions['edit any contributing_factors content'] = array(
    'name' => 'edit any contributing_factors content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_health_education content'.
  $permissions['edit any nutrition_health_education content'] = array(
    'name' => 'edit any nutrition_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_send_to_hc content'.
  $permissions['edit any nutrition_send_to_hc content'] = array(
    'name' => 'edit any nutrition_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own contributing_factors content'.
  $permissions['edit own contributing_factors content'] = array(
    'name' => 'edit own contributing_factors content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_health_education content'.
  $permissions['edit own nutrition_health_education content'] = array(
    'name' => 'edit own nutrition_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_send_to_hc content'.
  $permissions['edit own nutrition_send_to_hc content'] = array(
    'name' => 'edit own nutrition_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
