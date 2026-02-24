<?php

/**
 * @file
 * hedley_family_nutrition.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_family_nutrition_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create aheza_child content'.
  $permissions['create aheza_child content'] = array(
    'name' => 'create aheza_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create aheza_mother content'.
  $permissions['create aheza_mother content'] = array(
    'name' => 'create aheza_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create family_nutrition_encounter content'.
  $permissions['create family_nutrition_encounter content'] = array(
    'name' => 'create family_nutrition_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create family_nutrition_muac_child content'.
  $permissions['create family_nutrition_muac_child content'] = array(
    'name' => 'create family_nutrition_muac_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create family_nutrition_muac_mother content'.
  $permissions['create family_nutrition_muac_mother content'] = array(
    'name' => 'create family_nutrition_muac_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any aheza_child content'.
  $permissions['delete any aheza_child content'] = array(
    'name' => 'delete any aheza_child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any aheza_mother content'.
  $permissions['delete any aheza_mother content'] = array(
    'name' => 'delete any aheza_mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any family_nutrition_encounter content'.
  $permissions['delete any family_nutrition_encounter content'] = array(
    'name' => 'delete any family_nutrition_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any family_nutrition_muac_child content'.
  $permissions['delete any family_nutrition_muac_child content'] = array(
    'name' => 'delete any family_nutrition_muac_child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any family_nutrition_muac_mother content'.
  $permissions['delete any family_nutrition_muac_mother content'] = array(
    'name' => 'delete any family_nutrition_muac_mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own aheza_child content'.
  $permissions['delete own aheza_child content'] = array(
    'name' => 'delete own aheza_child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own aheza_mother content'.
  $permissions['delete own aheza_mother content'] = array(
    'name' => 'delete own aheza_mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own family_nutrition_encounter content'.
  $permissions['delete own family_nutrition_encounter content'] = array(
    'name' => 'delete own family_nutrition_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own family_nutrition_muac_child content'.
  $permissions['delete own family_nutrition_muac_child content'] = array(
    'name' => 'delete own family_nutrition_muac_child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own family_nutrition_muac_mother content'.
  $permissions['delete own family_nutrition_muac_mother content'] = array(
    'name' => 'delete own family_nutrition_muac_mother content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any aheza_child content'.
  $permissions['edit any aheza_child content'] = array(
    'name' => 'edit any aheza_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any aheza_mother content'.
  $permissions['edit any aheza_mother content'] = array(
    'name' => 'edit any aheza_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_nutrition_encounter content'.
  $permissions['edit any family_nutrition_encounter content'] = array(
    'name' => 'edit any family_nutrition_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_nutrition_muac_child content'.
  $permissions['edit any family_nutrition_muac_child content'] = array(
    'name' => 'edit any family_nutrition_muac_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_nutrition_muac_mother content'.
  $permissions['edit any family_nutrition_muac_mother content'] = array(
    'name' => 'edit any family_nutrition_muac_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own aheza_child content'.
  $permissions['edit own aheza_child content'] = array(
    'name' => 'edit own aheza_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own aheza_mother content'.
  $permissions['edit own aheza_mother content'] = array(
    'name' => 'edit own aheza_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_nutrition_encounter content'.
  $permissions['edit own family_nutrition_encounter content'] = array(
    'name' => 'edit own family_nutrition_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_nutrition_muac_child content'.
  $permissions['edit own family_nutrition_muac_child content'] = array(
    'name' => 'edit own family_nutrition_muac_child content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_nutrition_muac_mother content'.
  $permissions['edit own family_nutrition_muac_mother content'] = array(
    'name' => 'edit own family_nutrition_muac_mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
