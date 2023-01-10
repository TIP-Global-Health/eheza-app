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

  // Exported permission: 'create acute_illness_encounter content'.
  $permissions['create acute_illness_encounter content'] = array(
    'name' => 'create acute_illness_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

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

  // Exported permission: 'create home_visit_encounter content'.
  $permissions['create home_visit_encounter content'] = array(
    'name' => 'create home_visit_encounter content',
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

  // Exported permission: 'create ncd_encounter content'.
  $permissions['create ncd_encounter content'] = array(
    'name' => 'create ncd_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_encounter content'.
  $permissions['create nutrition_encounter content'] = array(
    'name' => 'create nutrition_encounter content',
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

  // Exported permission: 'delete any acute_illness_encounter content'.
  $permissions['delete any acute_illness_encounter content'] = array(
    'name' => 'delete any acute_illness_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'delete any home_visit_encounter content'.
  $permissions['delete any home_visit_encounter content'] = array(
    'name' => 'delete any home_visit_encounter content',
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

  // Exported permission: 'delete any ncd_encounter content'.
  $permissions['delete any ncd_encounter content'] = array(
    'name' => 'delete any ncd_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_encounter content'.
  $permissions['delete any nutrition_encounter content'] = array(
    'name' => 'delete any nutrition_encounter content',
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

  // Exported permission: 'delete own acute_illness_encounter content'.
  $permissions['delete own acute_illness_encounter content'] = array(
    'name' => 'delete own acute_illness_encounter content',
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

  // Exported permission: 'delete own home_visit_encounter content'.
  $permissions['delete own home_visit_encounter content'] = array(
    'name' => 'delete own home_visit_encounter content',
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

  // Exported permission: 'delete own ncd_encounter content'.
  $permissions['delete own ncd_encounter content'] = array(
    'name' => 'delete own ncd_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_encounter content'.
  $permissions['delete own nutrition_encounter content'] = array(
    'name' => 'delete own nutrition_encounter content',
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

  // Exported permission: 'edit any acute_illness_encounter content'.
  $permissions['edit any acute_illness_encounter content'] = array(
    'name' => 'edit any acute_illness_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
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

  // Exported permission: 'edit any home_visit_encounter content'.
  $permissions['edit any home_visit_encounter content'] = array(
    'name' => 'edit any home_visit_encounter content',
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

  // Exported permission: 'edit any ncd_encounter content'.
  $permissions['edit any ncd_encounter content'] = array(
    'name' => 'edit any ncd_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_encounter content'.
  $permissions['edit any nutrition_encounter content'] = array(
    'name' => 'edit any nutrition_encounter content',
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

  // Exported permission: 'edit own acute_illness_encounter content'.
  $permissions['edit own acute_illness_encounter content'] = array(
    'name' => 'edit own acute_illness_encounter content',
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

  // Exported permission: 'edit own home_visit_encounter content'.
  $permissions['edit own home_visit_encounter content'] = array(
    'name' => 'edit own home_visit_encounter content',
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

  // Exported permission: 'edit own ncd_encounter content'.
  $permissions['edit own ncd_encounter content'] = array(
    'name' => 'edit own ncd_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_encounter content'.
  $permissions['edit own nutrition_encounter content'] = array(
    'name' => 'edit own nutrition_encounter content',
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
