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

  // Exported permission: 'create family_nutrition_encounter content'.
  $permissions['create family_nutrition_encounter content'] = array(
    'name' => 'create family_nutrition_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete any family_nutrition_encounter content'.
  $permissions['delete any family_nutrition_encounter content'] = array(
    'name' => 'delete any family_nutrition_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'delete own family_nutrition_encounter content'.
  $permissions['delete own family_nutrition_encounter content'] = array(
    'name' => 'delete own family_nutrition_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_nutrition_encounter content'.
  $permissions['edit any family_nutrition_encounter content'] = array(
    'name' => 'edit any family_nutrition_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_nutrition_encounter content'.
  $permissions['edit own family_nutrition_encounter content'] = array(
    'name' => 'edit own family_nutrition_encounter content',
    'roles' => array(),
    'module' => 'node',
  );

  return $permissions;
}
