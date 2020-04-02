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

  // Exported permission: 'create breast_exam content'.
  $permissions['create breast_exam content'] = array(
    'name' => 'create breast_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create core_physical_exam content'.
  $permissions['create core_physical_exam content'] = array(
    'name' => 'create core_physical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create danger_signs content'.
  $permissions['create danger_signs content'] = array(
    'name' => 'create danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create last_menstrual_period content'.
  $permissions['create last_menstrual_period content'] = array(
    'name' => 'create last_menstrual_period content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create medical_history content'.
  $permissions['create medical_history content'] = array(
    'name' => 'create medical_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create medication content'.
  $permissions['create medication content'] = array(
    'name' => 'create medication content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create nutrition_nutrition content'.
  $permissions['create nutrition_nutrition content'] = array(
    'name' => 'create nutrition_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create obstetric_history content'.
  $permissions['create obstetric_history content'] = array(
    'name' => 'create obstetric_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create obstetric_history_step2 content'.
  $permissions['create obstetric_history_step2 content'] = array(
    'name' => 'create obstetric_history_step2 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create obstetrical_exam content'.
  $permissions['create obstetrical_exam content'] = array(
    'name' => 'create obstetrical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create prenatal_family_planning content'.
  $permissions['create prenatal_family_planning content'] = array(
    'name' => 'create prenatal_family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_nutrition content'.
  $permissions['create prenatal_nutrition content'] = array(
    'name' => 'create prenatal_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create prenatal_photo content'.
  $permissions['create prenatal_photo content'] = array(
    'name' => 'create prenatal_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create resource content'.
  $permissions['create resource content'] = array(
    'name' => 'create resource content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create social_history content'.
  $permissions['create social_history content'] = array(
    'name' => 'create social_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create symptoms_general content'.
  $permissions['create symptoms_general content'] = array(
    'name' => 'create symptoms_general content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create symptoms_gi content'.
  $permissions['create symptoms_gi content'] = array(
    'name' => 'create symptoms_gi content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create symptoms_respiratory content'.
  $permissions['create symptoms_respiratory content'] = array(
    'name' => 'create symptoms_respiratory content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create vitals content'.
  $permissions['create vitals content'] = array(
    'name' => 'create vitals content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'delete any breast_exam content'.
  $permissions['delete any breast_exam content'] = array(
    'name' => 'delete any breast_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any core_physical_exam content'.
  $permissions['delete any core_physical_exam content'] = array(
    'name' => 'delete any core_physical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any danger_signs content'.
  $permissions['delete any danger_signs content'] = array(
    'name' => 'delete any danger_signs content',
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

  // Exported permission: 'delete any last_menstrual_period content'.
  $permissions['delete any last_menstrual_period content'] = array(
    'name' => 'delete any last_menstrual_period content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any medical_history content'.
  $permissions['delete any medical_history content'] = array(
    'name' => 'delete any medical_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any medication content'.
  $permissions['delete any medication content'] = array(
    'name' => 'delete any medication content',
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

  // Exported permission: 'delete any nutrition_nutrition content'.
  $permissions['delete any nutrition_nutrition content'] = array(
    'name' => 'delete any nutrition_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any obstetric_history content'.
  $permissions['delete any obstetric_history content'] = array(
    'name' => 'delete any obstetric_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any obstetric_history_step2 content'.
  $permissions['delete any obstetric_history_step2 content'] = array(
    'name' => 'delete any obstetric_history_step2 content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any obstetrical_exam content'.
  $permissions['delete any obstetrical_exam content'] = array(
    'name' => 'delete any obstetrical_exam content',
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

  // Exported permission: 'delete any prenatal_family_planning content'.
  $permissions['delete any prenatal_family_planning content'] = array(
    'name' => 'delete any prenatal_family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_nutrition content'.
  $permissions['delete any prenatal_nutrition content'] = array(
    'name' => 'delete any prenatal_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any prenatal_photo content'.
  $permissions['delete any prenatal_photo content'] = array(
    'name' => 'delete any prenatal_photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any resource content'.
  $permissions['delete any resource content'] = array(
    'name' => 'delete any resource content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any social_history content'.
  $permissions['delete any social_history content'] = array(
    'name' => 'delete any social_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any symptoms_general content'.
  $permissions['delete any symptoms_general content'] = array(
    'name' => 'delete any symptoms_general content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any symptoms_gi content'.
  $permissions['delete any symptoms_gi content'] = array(
    'name' => 'delete any symptoms_gi content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any symptoms_respiratory content'.
  $permissions['delete any symptoms_respiratory content'] = array(
    'name' => 'delete any symptoms_respiratory content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any vitals content'.
  $permissions['delete any vitals content'] = array(
    'name' => 'delete any vitals content',
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

  // Exported permission: 'delete own breast_exam content'.
  $permissions['delete own breast_exam content'] = array(
    'name' => 'delete own breast_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own core_physical_exam content'.
  $permissions['delete own core_physical_exam content'] = array(
    'name' => 'delete own core_physical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own danger_signs content'.
  $permissions['delete own danger_signs content'] = array(
    'name' => 'delete own danger_signs content',
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

  // Exported permission: 'delete own last_menstrual_period content'.
  $permissions['delete own last_menstrual_period content'] = array(
    'name' => 'delete own last_menstrual_period content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own medical_history content'.
  $permissions['delete own medical_history content'] = array(
    'name' => 'delete own medical_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own medication content'.
  $permissions['delete own medication content'] = array(
    'name' => 'delete own medication content',
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

  // Exported permission: 'delete own nutrition_nutrition content'.
  $permissions['delete own nutrition_nutrition content'] = array(
    'name' => 'delete own nutrition_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own obstetric_history content'.
  $permissions['delete own obstetric_history content'] = array(
    'name' => 'delete own obstetric_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own obstetric_history_step2 content'.
  $permissions['delete own obstetric_history_step2 content'] = array(
    'name' => 'delete own obstetric_history_step2 content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own obstetrical_exam content'.
  $permissions['delete own obstetrical_exam content'] = array(
    'name' => 'delete own obstetrical_exam content',
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

  // Exported permission: 'delete own prenatal_family_planning content'.
  $permissions['delete own prenatal_family_planning content'] = array(
    'name' => 'delete own prenatal_family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_nutrition content'.
  $permissions['delete own prenatal_nutrition content'] = array(
    'name' => 'delete own prenatal_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own prenatal_photo content'.
  $permissions['delete own prenatal_photo content'] = array(
    'name' => 'delete own prenatal_photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own resource content'.
  $permissions['delete own resource content'] = array(
    'name' => 'delete own resource content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own social_history content'.
  $permissions['delete own social_history content'] = array(
    'name' => 'delete own social_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own symptoms_general content'.
  $permissions['delete own symptoms_general content'] = array(
    'name' => 'delete own symptoms_general content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own symptoms_gi content'.
  $permissions['delete own symptoms_gi content'] = array(
    'name' => 'delete own symptoms_gi content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own symptoms_respiratory content'.
  $permissions['delete own symptoms_respiratory content'] = array(
    'name' => 'delete own symptoms_respiratory content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own vitals content'.
  $permissions['delete own vitals content'] = array(
    'name' => 'delete own vitals content',
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

  // Exported permission: 'edit any breast_exam content'.
  $permissions['edit any breast_exam content'] = array(
    'name' => 'edit any breast_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any core_physical_exam content'.
  $permissions['edit any core_physical_exam content'] = array(
    'name' => 'edit any core_physical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any danger_signs content'.
  $permissions['edit any danger_signs content'] = array(
    'name' => 'edit any danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any last_menstrual_period content'.
  $permissions['edit any last_menstrual_period content'] = array(
    'name' => 'edit any last_menstrual_period content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any medical_history content'.
  $permissions['edit any medical_history content'] = array(
    'name' => 'edit any medical_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any medication content'.
  $permissions['edit any medication content'] = array(
    'name' => 'edit any medication content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any nutrition_nutrition content'.
  $permissions['edit any nutrition_nutrition content'] = array(
    'name' => 'edit any nutrition_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any obstetric_history content'.
  $permissions['edit any obstetric_history content'] = array(
    'name' => 'edit any obstetric_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any obstetric_history_step2 content'.
  $permissions['edit any obstetric_history_step2 content'] = array(
    'name' => 'edit any obstetric_history_step2 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any obstetrical_exam content'.
  $permissions['edit any obstetrical_exam content'] = array(
    'name' => 'edit any obstetrical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any prenatal_family_planning content'.
  $permissions['edit any prenatal_family_planning content'] = array(
    'name' => 'edit any prenatal_family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_nutrition content'.
  $permissions['edit any prenatal_nutrition content'] = array(
    'name' => 'edit any prenatal_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any prenatal_photo content'.
  $permissions['edit any prenatal_photo content'] = array(
    'name' => 'edit any prenatal_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any resource content'.
  $permissions['edit any resource content'] = array(
    'name' => 'edit any resource content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any social_history content'.
  $permissions['edit any social_history content'] = array(
    'name' => 'edit any social_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any symptoms_general content'.
  $permissions['edit any symptoms_general content'] = array(
    'name' => 'edit any symptoms_general content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any symptoms_gi content'.
  $permissions['edit any symptoms_gi content'] = array(
    'name' => 'edit any symptoms_gi content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any symptoms_respiratory content'.
  $permissions['edit any symptoms_respiratory content'] = array(
    'name' => 'edit any symptoms_respiratory content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any vitals content'.
  $permissions['edit any vitals content'] = array(
    'name' => 'edit any vitals content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own breast_exam content'.
  $permissions['edit own breast_exam content'] = array(
    'name' => 'edit own breast_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own core_physical_exam content'.
  $permissions['edit own core_physical_exam content'] = array(
    'name' => 'edit own core_physical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own danger_signs content'.
  $permissions['edit own danger_signs content'] = array(
    'name' => 'edit own danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own last_menstrual_period content'.
  $permissions['edit own last_menstrual_period content'] = array(
    'name' => 'edit own last_menstrual_period content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own medical_history content'.
  $permissions['edit own medical_history content'] = array(
    'name' => 'edit own medical_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own medication content'.
  $permissions['edit own medication content'] = array(
    'name' => 'edit own medication content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own nutrition_nutrition content'.
  $permissions['edit own nutrition_nutrition content'] = array(
    'name' => 'edit own nutrition_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own obstetric_history content'.
  $permissions['edit own obstetric_history content'] = array(
    'name' => 'edit own obstetric_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own obstetric_history_step2 content'.
  $permissions['edit own obstetric_history_step2 content'] = array(
    'name' => 'edit own obstetric_history_step2 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own obstetrical_exam content'.
  $permissions['edit own obstetrical_exam content'] = array(
    'name' => 'edit own obstetrical_exam content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own prenatal_family_planning content'.
  $permissions['edit own prenatal_family_planning content'] = array(
    'name' => 'edit own prenatal_family_planning content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_nutrition content'.
  $permissions['edit own prenatal_nutrition content'] = array(
    'name' => 'edit own prenatal_nutrition content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own prenatal_photo content'.
  $permissions['edit own prenatal_photo content'] = array(
    'name' => 'edit own prenatal_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own resource content'.
  $permissions['edit own resource content'] = array(
    'name' => 'edit own resource content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own social_history content'.
  $permissions['edit own social_history content'] = array(
    'name' => 'edit own social_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own symptoms_general content'.
  $permissions['edit own symptoms_general content'] = array(
    'name' => 'edit own symptoms_general content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own symptoms_gi content'.
  $permissions['edit own symptoms_gi content'] = array(
    'name' => 'edit own symptoms_gi content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own symptoms_respiratory content'.
  $permissions['edit own symptoms_respiratory content'] = array(
    'name' => 'edit own symptoms_respiratory content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own vitals content'.
  $permissions['edit own vitals content'] = array(
    'name' => 'edit own vitals content',
    'roles' => array(
      'administrator' => 'administrator',
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
