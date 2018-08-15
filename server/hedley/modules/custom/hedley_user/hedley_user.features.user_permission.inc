<?php
/**
 * @file
 * hedley_user.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_user_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create child content'.
  $permissions['create child content'] = array(
    'name' => 'create child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create clinic content'.
  $permissions['create clinic content'] = array(
    'name' => 'create clinic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create examination content'.
  $permissions['create examination content'] = array(
    'name' => 'create examination content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create family_planning content'.
  $permissions['create family_planning content'] = array(
    'name' => 'create family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create group content'.
  $permissions['create group content'] = array(
    'name' => 'create group content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create height content'.
  $permissions['create height content'] = array(
    'name' => 'create height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create mother content'.
  $permissions['create mother content'] = array(
    'name' => 'create mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create muac content'.
  $permissions['create muac content'] = array(
    'name' => 'create muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition content'.
  $permissions['create nutrition content'] = array(
    'name' => 'create nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create photo content'.
  $permissions['create photo content'] = array(
    'name' => 'create photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create session content'.
  $permissions['create session content'] = array(
    'name' => 'create session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create weight content'.
  $permissions['create weight content'] = array(
    'name' => 'create weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child content'.
  $permissions['delete any child content'] = array(
    'name' => 'delete any child content',
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

  // Exported permission: 'delete any examination content'.
  $permissions['delete any examination content'] = array(
    'name' => 'delete any examination content',
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

  // Exported permission: 'delete any group content'.
  $permissions['delete any group content'] = array(
    'name' => 'delete any group content',
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

  // Exported permission: 'delete any mother content'.
  $permissions['delete any mother content'] = array(
    'name' => 'delete any mother content',
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

  // Exported permission: 'delete any session content'.
  $permissions['delete any session content'] = array(
    'name' => 'delete any session content',
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

  // Exported permission: 'delete own child content'.
  $permissions['delete own child content'] = array(
    'name' => 'delete own child content',
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

  // Exported permission: 'delete own examination content'.
  $permissions['delete own examination content'] = array(
    'name' => 'delete own examination content',
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

  // Exported permission: 'delete own group content'.
  $permissions['delete own group content'] = array(
    'name' => 'delete own group content',
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

  // Exported permission: 'delete own mother content'.
  $permissions['delete own mother content'] = array(
    'name' => 'delete own mother content',
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

  // Exported permission: 'delete own session content'.
  $permissions['delete own session content'] = array(
    'name' => 'delete own session content',
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

  // Exported permission: 'edit any child content'.
  $permissions['edit any child content'] = array(
    'name' => 'edit any child content',
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
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any examination content'.
  $permissions['edit any examination content'] = array(
    'name' => 'edit any examination content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any family_planning content'.
  $permissions['edit any family_planning content'] = array(
    'name' => 'edit any family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any group content'.
  $permissions['edit any group content'] = array(
    'name' => 'edit any group content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any height content'.
  $permissions['edit any height content'] = array(
    'name' => 'edit any height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any mother content'.
  $permissions['edit any mother content'] = array(
    'name' => 'edit any mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any muac content'.
  $permissions['edit any muac content'] = array(
    'name' => 'edit any muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition content'.
  $permissions['edit any nutrition content'] = array(
    'name' => 'edit any nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any photo content'.
  $permissions['edit any photo content'] = array(
    'name' => 'edit any photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any session content'.
  $permissions['edit any session content'] = array(
    'name' => 'edit any session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any weight content'.
  $permissions['edit any weight content'] = array(
    'name' => 'edit any weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child content'.
  $permissions['edit own child content'] = array(
    'name' => 'edit own child content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own clinic content'.
  $permissions['edit own clinic content'] = array(
    'name' => 'edit own clinic content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own examination content'.
  $permissions['edit own examination content'] = array(
    'name' => 'edit own examination content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own family_planning content'.
  $permissions['edit own family_planning content'] = array(
    'name' => 'edit own family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own group content'.
  $permissions['edit own group content'] = array(
    'name' => 'edit own group content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own height content'.
  $permissions['edit own height content'] = array(
    'name' => 'edit own height content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own mother content'.
  $permissions['edit own mother content'] = array(
    'name' => 'edit own mother content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own muac content'.
  $permissions['edit own muac content'] = array(
    'name' => 'edit own muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition content'.
  $permissions['edit own nutrition content'] = array(
    'name' => 'edit own nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own photo content'.
  $permissions['edit own photo content'] = array(
    'name' => 'edit own photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own session content'.
  $permissions['edit own session content'] = array(
    'name' => 'edit own session content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own weight content'.
  $permissions['edit own weight content'] = array(
    'name' => 'edit own weight content',
    'roles' => array(
      'administrator' => 'administrator',
      'nurse' => 'nurse',
    ),
    'module' => 'node',
  );

  return $permissions;
}
