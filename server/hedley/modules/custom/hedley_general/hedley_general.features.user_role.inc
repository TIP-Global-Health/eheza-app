<?php
/**
 * @file
 * hedley_general.features.user_role.inc
 */

/**
 * Implements hook_user_default_roles().
 */
function hedley_general_user_default_roles() {
  $roles = array();

  // Exported role: administrator.
  $roles['administrator'] = array(
    'name' => 'administrator',
    'weight' => 2,
  );

  // Exported role: nurse.
  $roles['nurse'] = array(
    'name' => 'nurse',
    'weight' => 3,
  );

  // Exported role: sync.
  $roles['sync'] = array(
    'name' => 'sync',
    'weight' => 2,
  );

  return $roles;
}
