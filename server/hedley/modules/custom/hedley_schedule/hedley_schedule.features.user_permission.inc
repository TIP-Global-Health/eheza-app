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

  // Exported permission: 'create individual_participant content'.
  $permissions['create individual_participant content'] = array(
    'name' => 'create individual_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create pmtct_participant content'.
  $permissions['create pmtct_participant content'] = array(
    'name' => 'create pmtct_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_encounter content'.
  $permissions['create prenatal_encounter content'] = array(
    'name' => 'create prenatal_encounter content',
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

  // Exported permission: 'delete any individual_participant content'.
  $permissions['delete any individual_participant content'] = array(
    'name' => 'delete any individual_participant content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any pmtct_participant content'.
  $permissions['delete any pmtct_participant content'] = array(
    'name' => 'delete any pmtct_participant content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_encounter content'.
  $permissions['delete any prenatal_encounter content'] = array(
    'name' => 'delete any prenatal_encounter content',
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

  // Exported permission: 'delete own individual_participant content'.
  $permissions['delete own individual_participant content'] = array(
    'name' => 'delete own individual_participant content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own pmtct_participant content'.
  $permissions['delete own pmtct_participant content'] = array(
    'name' => 'delete own pmtct_participant content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_encounter content'.
  $permissions['delete own prenatal_encounter content'] = array(
    'name' => 'delete own prenatal_encounter content',
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

  // Exported permission: 'edit any individual_participant content'.
  $permissions['edit any individual_participant content'] = array(
    'name' => 'edit any individual_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any pmtct_participant content'.
  $permissions['edit any pmtct_participant content'] = array(
    'name' => 'edit any pmtct_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_encounter content'.
  $permissions['edit any prenatal_encounter content'] = array(
    'name' => 'edit any prenatal_encounter content',
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

  // Exported permission: 'edit own individual_participant content'.
  $permissions['edit own individual_participant content'] = array(
    'name' => 'edit own individual_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own pmtct_participant content'.
  $permissions['edit own pmtct_participant content'] = array(
    'name' => 'edit own pmtct_participant content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_encounter content'.
  $permissions['edit own prenatal_encounter content'] = array(
    'name' => 'edit own prenatal_encounter content',
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
