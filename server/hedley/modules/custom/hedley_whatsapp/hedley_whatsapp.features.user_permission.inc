<?php

/**
 * @file
 * hedley_whatsapp.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_whatsapp_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create whatsapp_record content'.
  $permissions['create whatsapp_record content'] = array(
    'name' => 'create whatsapp_record content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any whatsapp_record content'.
  $permissions['delete any whatsapp_record content'] = array(
    'name' => 'delete any whatsapp_record content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own whatsapp_record content'.
  $permissions['delete own whatsapp_record content'] = array(
    'name' => 'delete own whatsapp_record content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any whatsapp_record content'.
  $permissions['edit any whatsapp_record content'] = array(
    'name' => 'edit any whatsapp_record content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own whatsapp_record content'.
  $permissions['edit own whatsapp_record content'] = array(
    'name' => 'edit own whatsapp_record content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
