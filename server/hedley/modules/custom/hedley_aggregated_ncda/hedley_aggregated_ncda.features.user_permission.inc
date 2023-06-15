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

  // Exported permission: 'create child_scoreboard_encounter content'.
  $permissions['create child_scoreboard_encounter content'] = array(
    'name' => 'create child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_encounter content'.
  $permissions['delete any child_scoreboard_encounter content'] = array(
    'name' => 'delete any child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_encounter content'.
  $permissions['delete own child_scoreboard_encounter content'] = array(
    'name' => 'delete own child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_encounter content'.
  $permissions['edit any child_scoreboard_encounter content'] = array(
    'name' => 'edit any child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_encounter content'.
  $permissions['edit own child_scoreboard_encounter content'] = array(
    'name' => 'edit own child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
