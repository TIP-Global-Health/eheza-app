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

  // Exported permission: 'create birth_plan content'.
  $permissions['create birth_plan content'] = array(
    'name' => 'create birth_plan content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create child_fbf content'.
  $permissions['create child_fbf content'] = array(
    'name' => 'create child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create contributing_factors content'.
  $permissions['create contributing_factors content'] = array(
    'name' => 'create contributing_factors content',
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

  // Exported permission: 'create follow_up content'.
  $permissions['create follow_up content'] = array(
    'name' => 'create follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create group_health_education content'.
  $permissions['create group_health_education content'] = array(
    'name' => 'create group_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create group_ncda content'.
  $permissions['create group_ncda content'] = array(
    'name' => 'create group_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create group_send_to_hc content'.
  $permissions['create group_send_to_hc content'] = array(
    'name' => 'create group_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create mother_fbf content'.
  $permissions['create mother_fbf content'] = array(
    'name' => 'create mother_fbf content',
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

  // Exported permission: 'create nutrition_height content'.
  $permissions['create nutrition_height content'] = array(
    'name' => 'create nutrition_height content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_muac content'.
  $permissions['create nutrition_muac content'] = array(
    'name' => 'create nutrition_muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_ncda content'.
  $permissions['create nutrition_ncda content'] = array(
    'name' => 'create nutrition_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create nutrition_photo content'.
  $permissions['create nutrition_photo content'] = array(
    'name' => 'create nutrition_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create nutrition_weight content'.
  $permissions['create nutrition_weight content'] = array(
    'name' => 'create nutrition_weight content',
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

  // Exported permission: 'create pregnancy_testing content'.
  $permissions['create pregnancy_testing content'] = array(
    'name' => 'create pregnancy_testing content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'create prenatal_health_education content'.
  $permissions['create prenatal_health_education content'] = array(
    'name' => 'create prenatal_health_education content',
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

  // Exported permission: 'delete any birth_plan content'.
  $permissions['delete any birth_plan content'] = array(
    'name' => 'delete any birth_plan content',
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

  // Exported permission: 'delete any child_fbf content'.
  $permissions['delete any child_fbf content'] = array(
    'name' => 'delete any child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any contributing_factors content'.
  $permissions['delete any contributing_factors content'] = array(
    'name' => 'delete any contributing_factors content',
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

  // Exported permission: 'delete any follow_up content'.
  $permissions['delete any follow_up content'] = array(
    'name' => 'delete any follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any group_health_education content'.
  $permissions['delete any group_health_education content'] = array(
    'name' => 'delete any group_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any group_ncda content'.
  $permissions['delete any group_ncda content'] = array(
    'name' => 'delete any group_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any group_send_to_hc content'.
  $permissions['delete any group_send_to_hc content'] = array(
    'name' => 'delete any group_send_to_hc content',
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

  // Exported permission: 'delete any nutrition_height content'.
  $permissions['delete any nutrition_height content'] = array(
    'name' => 'delete any nutrition_height content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_muac content'.
  $permissions['delete any nutrition_muac content'] = array(
    'name' => 'delete any nutrition_muac content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_ncda content'.
  $permissions['delete any nutrition_ncda content'] = array(
    'name' => 'delete any nutrition_ncda content',
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

  // Exported permission: 'delete any nutrition_photo content'.
  $permissions['delete any nutrition_photo content'] = array(
    'name' => 'delete any nutrition_photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any nutrition_weight content'.
  $permissions['delete any nutrition_weight content'] = array(
    'name' => 'delete any nutrition_weight content',
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

  // Exported permission: 'delete any pregnancy_testing content'.
  $permissions['delete any pregnancy_testing content'] = array(
    'name' => 'delete any pregnancy_testing content',
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

  // Exported permission: 'delete any prenatal_health_education content'.
  $permissions['delete any prenatal_health_education content'] = array(
    'name' => 'delete any prenatal_health_education content',
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

  // Exported permission: 'delete own birth_plan content'.
  $permissions['delete own birth_plan content'] = array(
    'name' => 'delete own birth_plan content',
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

  // Exported permission: 'delete own child_fbf content'.
  $permissions['delete own child_fbf content'] = array(
    'name' => 'delete own child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own contributing_factors content'.
  $permissions['delete own contributing_factors content'] = array(
    'name' => 'delete own contributing_factors content',
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

  // Exported permission: 'delete own follow_up content'.
  $permissions['delete own follow_up content'] = array(
    'name' => 'delete own follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own group_health_education content'.
  $permissions['delete own group_health_education content'] = array(
    'name' => 'delete own group_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own group_ncda content'.
  $permissions['delete own group_ncda content'] = array(
    'name' => 'delete own group_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own group_send_to_hc content'.
  $permissions['delete own group_send_to_hc content'] = array(
    'name' => 'delete own group_send_to_hc content',
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

  // Exported permission: 'delete own nutrition_height content'.
  $permissions['delete own nutrition_height content'] = array(
    'name' => 'delete own nutrition_height content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_muac content'.
  $permissions['delete own nutrition_muac content'] = array(
    'name' => 'delete own nutrition_muac content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_ncda content'.
  $permissions['delete own nutrition_ncda content'] = array(
    'name' => 'delete own nutrition_ncda content',
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

  // Exported permission: 'delete own nutrition_photo content'.
  $permissions['delete own nutrition_photo content'] = array(
    'name' => 'delete own nutrition_photo content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own nutrition_weight content'.
  $permissions['delete own nutrition_weight content'] = array(
    'name' => 'delete own nutrition_weight content',
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

  // Exported permission: 'delete own pregnancy_testing content'.
  $permissions['delete own pregnancy_testing content'] = array(
    'name' => 'delete own pregnancy_testing content',
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

  // Exported permission: 'delete own prenatal_health_education content'.
  $permissions['delete own prenatal_health_education content'] = array(
    'name' => 'delete own prenatal_health_education content',
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

  // Exported permission: 'edit any birth_plan content'.
  $permissions['edit any birth_plan content'] = array(
    'name' => 'edit any birth_plan content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any child_fbf content'.
  $permissions['edit any child_fbf content'] = array(
    'name' => 'edit any child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any contributing_factors content'.
  $permissions['edit any contributing_factors content'] = array(
    'name' => 'edit any contributing_factors content',
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

  // Exported permission: 'edit any follow_up content'.
  $permissions['edit any follow_up content'] = array(
    'name' => 'edit any follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any group_health_education content'.
  $permissions['edit any group_health_education content'] = array(
    'name' => 'edit any group_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any group_ncda content'.
  $permissions['edit any group_ncda content'] = array(
    'name' => 'edit any group_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any group_send_to_hc content'.
  $permissions['edit any group_send_to_hc content'] = array(
    'name' => 'edit any group_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any mother_fbf content'.
  $permissions['edit any mother_fbf content'] = array(
    'name' => 'edit any mother_fbf content',
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

  // Exported permission: 'edit any nutrition_height content'.
  $permissions['edit any nutrition_height content'] = array(
    'name' => 'edit any nutrition_height content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_muac content'.
  $permissions['edit any nutrition_muac content'] = array(
    'name' => 'edit any nutrition_muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_ncda content'.
  $permissions['edit any nutrition_ncda content'] = array(
    'name' => 'edit any nutrition_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any nutrition_photo content'.
  $permissions['edit any nutrition_photo content'] = array(
    'name' => 'edit any nutrition_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any nutrition_weight content'.
  $permissions['edit any nutrition_weight content'] = array(
    'name' => 'edit any nutrition_weight content',
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

  // Exported permission: 'edit any pregnancy_testing content'.
  $permissions['edit any pregnancy_testing content'] = array(
    'name' => 'edit any pregnancy_testing content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit any prenatal_health_education content'.
  $permissions['edit any prenatal_health_education content'] = array(
    'name' => 'edit any prenatal_health_education content',
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

  // Exported permission: 'edit own birth_plan content'.
  $permissions['edit own birth_plan content'] = array(
    'name' => 'edit own birth_plan content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own child_fbf content'.
  $permissions['edit own child_fbf content'] = array(
    'name' => 'edit own child_fbf content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own contributing_factors content'.
  $permissions['edit own contributing_factors content'] = array(
    'name' => 'edit own contributing_factors content',
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

  // Exported permission: 'edit own follow_up content'.
  $permissions['edit own follow_up content'] = array(
    'name' => 'edit own follow_up content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own group_health_education content'.
  $permissions['edit own group_health_education content'] = array(
    'name' => 'edit own group_health_education content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own group_ncda content'.
  $permissions['edit own group_ncda content'] = array(
    'name' => 'edit own group_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own group_send_to_hc content'.
  $permissions['edit own group_send_to_hc content'] = array(
    'name' => 'edit own group_send_to_hc content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own mother_fbf content'.
  $permissions['edit own mother_fbf content'] = array(
    'name' => 'edit own mother_fbf content',
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

  // Exported permission: 'edit own nutrition_height content'.
  $permissions['edit own nutrition_height content'] = array(
    'name' => 'edit own nutrition_height content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_muac content'.
  $permissions['edit own nutrition_muac content'] = array(
    'name' => 'edit own nutrition_muac content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_ncda content'.
  $permissions['edit own nutrition_ncda content'] = array(
    'name' => 'edit own nutrition_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own nutrition_photo content'.
  $permissions['edit own nutrition_photo content'] = array(
    'name' => 'edit own nutrition_photo content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own nutrition_weight content'.
  $permissions['edit own nutrition_weight content'] = array(
    'name' => 'edit own nutrition_weight content',
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

  // Exported permission: 'edit own pregnancy_testing content'.
  $permissions['edit own pregnancy_testing content'] = array(
    'name' => 'edit own pregnancy_testing content',
    'roles' => array(
      'administrator' => 'administrator',
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

  // Exported permission: 'edit own prenatal_health_education content'.
  $permissions['edit own prenatal_health_education content'] = array(
    'name' => 'edit own prenatal_health_education content',
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
