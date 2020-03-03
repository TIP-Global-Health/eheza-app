<?php

/**
 * @file
 * hedley_activity.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_activity_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create attendance content'.
  $permissions['create attendance content'] = array(
    'name' => 'create attendance content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_fbf content'.
  $permissions['create child_fbf content'] = array(
    'name' => 'create child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create family_planning content'.
  $permissions['create family_planning content'] = array(
    'name' => 'create family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create height content'.
  $permissions['create height content'] = array(
    'name' => 'create height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create lactation content'.
  $permissions['create lactation content'] = array(
    'name' => 'create lactation content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create mother_fbf content'.
  $permissions['create mother_fbf content'] = array(
    'name' => 'create mother_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create muac content'.
  $permissions['create muac content'] = array(
    'name' => 'create muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition content'.
  $permissions['create nutrition content'] = array(
    'name' => 'create nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create photo content'.
  $permissions['create photo content'] = array(
    'name' => 'create photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create weight content'.
  $permissions['create weight content'] = array(
    'name' => 'create weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any attendance content'.
  $permissions['delete any attendance content'] = array(
    'name' => 'delete any attendance content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_fbf content'.
  $permissions['delete any child_fbf content'] = array(
    'name' => 'delete any child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any family_planning content'.
  $permissions['delete any family_planning content'] = array(
    'name' => 'delete any family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any height content'.
  $permissions['delete any height content'] = array(
    'name' => 'delete any height content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any lactation content'.
  $permissions['delete any lactation content'] = array(
    'name' => 'delete any lactation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any mother_fbf content'.
  $permissions['delete any mother_fbf content'] = array(
    'name' => 'delete any mother_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any muac content'.
  $permissions['delete any muac content'] = array(
    'name' => 'delete any muac content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition content'.
  $permissions['delete any nutrition content'] = array(
    'name' => 'delete any nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any photo content'.
  $permissions['delete any photo content'] = array(
    'name' => 'delete any photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any weight content'.
  $permissions['delete any weight content'] = array(
    'name' => 'delete any weight content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own attendance content'.
  $permissions['delete own attendance content'] = array(
    'name' => 'delete own attendance content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_fbf content'.
  $permissions['delete own child_fbf content'] = array(
    'name' => 'delete own child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own family_planning content'.
  $permissions['delete own family_planning content'] = array(
    'name' => 'delete own family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own height content'.
  $permissions['delete own height content'] = array(
    'name' => 'delete own height content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own lactation content'.
  $permissions['delete own lactation content'] = array(
    'name' => 'delete own lactation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own mother_fbf content'.
  $permissions['delete own mother_fbf content'] = array(
    'name' => 'delete own mother_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own muac content'.
  $permissions['delete own muac content'] = array(
    'name' => 'delete own muac content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition content'.
  $permissions['delete own nutrition content'] = array(
    'name' => 'delete own nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own photo content'.
  $permissions['delete own photo content'] = array(
    'name' => 'delete own photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own weight content'.
  $permissions['delete own weight content'] = array(
    'name' => 'delete own weight content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any attendance content'.
  $permissions['edit any attendance content'] = array(
    'name' => 'edit any attendance content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_fbf content'.
  $permissions['edit any child_fbf content'] = array(
    'name' => 'edit any child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_planning content'.
  $permissions['edit any family_planning content'] = array(
    'name' => 'edit any family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any height content'.
  $permissions['edit any height content'] = array(
    'name' => 'edit any height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any lactation content'.
  $permissions['edit any lactation content'] = array(
    'name' => 'edit any lactation content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any mother_fbf content'.
  $permissions['edit any mother_fbf content'] = array(
    'name' => 'edit any mother_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any muac content'.
  $permissions['edit any muac content'] = array(
    'name' => 'edit any muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition content'.
  $permissions['edit any nutrition content'] = array(
    'name' => 'edit any nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any photo content'.
  $permissions['edit any photo content'] = array(
    'name' => 'edit any photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any weight content'.
  $permissions['edit any weight content'] = array(
    'name' => 'edit any weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own attendance content'.
  $permissions['edit own attendance content'] = array(
    'name' => 'edit own attendance content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_fbf content'.
  $permissions['edit own child_fbf content'] = array(
    'name' => 'edit own child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_planning content'.
  $permissions['edit own family_planning content'] = array(
    'name' => 'edit own family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own height content'.
  $permissions['edit own height content'] = array(
    'name' => 'edit own height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own lactation content'.
  $permissions['edit own lactation content'] = array(
    'name' => 'edit own lactation content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own mother_fbf content'.
  $permissions['edit own mother_fbf content'] = array(
    'name' => 'edit own mother_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own muac content'.
  $permissions['edit own muac content'] = array(
    'name' => 'edit own muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition content'.
  $permissions['edit own nutrition content'] = array(
    'name' => 'edit own nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own photo content'.
  $permissions['edit own photo content'] = array(
    'name' => 'edit own photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own weight content'.
  $permissions['edit own weight content'] = array(
    'name' => 'edit own weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
