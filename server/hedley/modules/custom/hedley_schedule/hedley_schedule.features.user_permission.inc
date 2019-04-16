<?php
/**
 * @file
 * hedley_schedule.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_schedule_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create clinic content'.
  $permissions['create clinic content'] = array(
    'name' => 'create clinic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create counseling_schedule content'.
  $permissions['create counseling_schedule content'] = array(
    'name' => 'create counseling_schedule content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create counseling_session content'.
  $permissions['create counseling_session content'] = array(
    'name' => 'create counseling_session content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create counseling_topic content'.
  $permissions['create counseling_topic content'] = array(
    'name' => 'create counseling_topic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create session content'.
  $permissions['create session content'] = array(
    'name' => 'create session content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any clinic content'.
  $permissions['delete any clinic content'] = array(
    'name' => 'delete any clinic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any counseling_schedule content'.
  $permissions['delete any counseling_schedule content'] = array(
    'name' => 'delete any counseling_schedule content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any counseling_session content'.
  $permissions['delete any counseling_session content'] = array(
    'name' => 'delete any counseling_session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any counseling_topic content'.
  $permissions['delete any counseling_topic content'] = array(
    'name' => 'delete any counseling_topic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any session content'.
  $permissions['delete any session content'] = array(
    'name' => 'delete any session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own clinic content'.
  $permissions['delete own clinic content'] = array(
    'name' => 'delete own clinic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own counseling_schedule content'.
  $permissions['delete own counseling_schedule content'] = array(
    'name' => 'delete own counseling_schedule content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own counseling_session content'.
  $permissions['delete own counseling_session content'] = array(
    'name' => 'delete own counseling_session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own counseling_topic content'.
  $permissions['delete own counseling_topic content'] = array(
    'name' => 'delete own counseling_topic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own session content'.
  $permissions['delete own session content'] = array(
    'name' => 'delete own session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any clinic content'.
  $permissions['edit any clinic content'] = array(
    'name' => 'edit any clinic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any counseling_schedule content'.
  $permissions['edit any counseling_schedule content'] = array(
    'name' => 'edit any counseling_schedule content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any counseling_session content'.
  $permissions['edit any counseling_session content'] = array(
    'name' => 'edit any counseling_session content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any counseling_topic content'.
  $permissions['edit any counseling_topic content'] = array(
    'name' => 'edit any counseling_topic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any session content'.
  $permissions['edit any session content'] = array(
    'name' => 'edit any session content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own clinic content'.
  $permissions['edit own clinic content'] = array(
    'name' => 'edit own clinic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own counseling_schedule content'.
  $permissions['edit own counseling_schedule content'] = array(
    'name' => 'edit own counseling_schedule content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own counseling_session content'.
  $permissions['edit own counseling_session content'] = array(
    'name' => 'edit own counseling_session content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own counseling_topic content'.
  $permissions['edit own counseling_topic content'] = array(
    'name' => 'edit own counseling_topic content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own session content'.
  $permissions['edit own session content'] = array(
    'name' => 'edit own session content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
