<?php

/**
 * @file
 * hedley_acute_illness.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_acute_illness_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create acute_findings content'.
  $permissions['create acute_findings content'] = array(
    'name' => 'create acute_findings content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create acute_illness_danger_signs content'.
  $permissions['create acute_illness_danger_signs content'] = array(
    'name' => 'create acute_illness_danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create acute_illness_vitals content'.
  $permissions['create acute_illness_vitals content'] = array(
    'name' => 'create acute_illness_vitals content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create call_114 content'.
  $permissions['create call_114 content'] = array(
    'name' => 'create call_114 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create exposure content'.
  $permissions['create exposure content'] = array(
    'name' => 'create exposure content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create hc_contact content'.
  $permissions['create hc_contact content'] = array(
    'name' => 'create hc_contact content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create isolation content'.
  $permissions['create isolation content'] = array(
    'name' => 'create isolation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create malaria_testing content'.
  $permissions['create malaria_testing content'] = array(
    'name' => 'create malaria_testing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create medication_distribution content'.
  $permissions['create medication_distribution content'] = array(
    'name' => 'create medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create send_to_hc content'.
  $permissions['create send_to_hc content'] = array(
    'name' => 'create send_to_hc content',
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

  // Exported permission: 'create travel_history content'.
  $permissions['create travel_history content'] = array(
    'name' => 'create travel_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create treatment_history content'.
  $permissions['create treatment_history content'] = array(
    'name' => 'create treatment_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create treatment_ongoing content'.
  $permissions['create treatment_ongoing content'] = array(
    'name' => 'create treatment_ongoing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any acute_findings content'.
  $permissions['delete any acute_findings content'] = array(
    'name' => 'delete any acute_findings content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any acute_illness_danger_signs content'.
  $permissions['delete any acute_illness_danger_signs content'] = array(
    'name' => 'delete any acute_illness_danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any acute_illness_vitals content'.
  $permissions['delete any acute_illness_vitals content'] = array(
    'name' => 'delete any acute_illness_vitals content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any call_114 content'.
  $permissions['delete any call_114 content'] = array(
    'name' => 'delete any call_114 content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any exposure content'.
  $permissions['delete any exposure content'] = array(
    'name' => 'delete any exposure content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any hc_contact content'.
  $permissions['delete any hc_contact content'] = array(
    'name' => 'delete any hc_contact content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any isolation content'.
  $permissions['delete any isolation content'] = array(
    'name' => 'delete any isolation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any malaria_testing content'.
  $permissions['delete any malaria_testing content'] = array(
    'name' => 'delete any malaria_testing content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any medication_distribution content'.
  $permissions['delete any medication_distribution content'] = array(
    'name' => 'delete any medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any send_to_hc content'.
  $permissions['delete any send_to_hc content'] = array(
    'name' => 'delete any send_to_hc content',
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

  // Exported permission: 'delete any travel_history content'.
  $permissions['delete any travel_history content'] = array(
    'name' => 'delete any travel_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any treatment_history content'.
  $permissions['delete any treatment_history content'] = array(
    'name' => 'delete any treatment_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any treatment_ongoing content'.
  $permissions['delete any treatment_ongoing content'] = array(
    'name' => 'delete any treatment_ongoing content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own acute_findings content'.
  $permissions['delete own acute_findings content'] = array(
    'name' => 'delete own acute_findings content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own acute_illness_danger_signs content'.
  $permissions['delete own acute_illness_danger_signs content'] = array(
    'name' => 'delete own acute_illness_danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own acute_illness_vitals content'.
  $permissions['delete own acute_illness_vitals content'] = array(
    'name' => 'delete own acute_illness_vitals content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own call_114 content'.
  $permissions['delete own call_114 content'] = array(
    'name' => 'delete own call_114 content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own exposure content'.
  $permissions['delete own exposure content'] = array(
    'name' => 'delete own exposure content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own hc_contact content'.
  $permissions['delete own hc_contact content'] = array(
    'name' => 'delete own hc_contact content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own isolation content'.
  $permissions['delete own isolation content'] = array(
    'name' => 'delete own isolation content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own malaria_testing content'.
  $permissions['delete own malaria_testing content'] = array(
    'name' => 'delete own malaria_testing content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own medication_distribution content'.
  $permissions['delete own medication_distribution content'] = array(
    'name' => 'delete own medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own send_to_hc content'.
  $permissions['delete own send_to_hc content'] = array(
    'name' => 'delete own send_to_hc content',
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

  // Exported permission: 'delete own travel_history content'.
  $permissions['delete own travel_history content'] = array(
    'name' => 'delete own travel_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own treatment_history content'.
  $permissions['delete own treatment_history content'] = array(
    'name' => 'delete own treatment_history content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own treatment_ongoing content'.
  $permissions['delete own treatment_ongoing content'] = array(
    'name' => 'delete own treatment_ongoing content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any acute_findings content'.
  $permissions['edit any acute_findings content'] = array(
    'name' => 'edit any acute_findings content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any acute_illness_danger_signs content'.
  $permissions['edit any acute_illness_danger_signs content'] = array(
    'name' => 'edit any acute_illness_danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any acute_illness_vitals content'.
  $permissions['edit any acute_illness_vitals content'] = array(
    'name' => 'edit any acute_illness_vitals content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any call_114 content'.
  $permissions['edit any call_114 content'] = array(
    'name' => 'edit any call_114 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any exposure content'.
  $permissions['edit any exposure content'] = array(
    'name' => 'edit any exposure content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any hc_contact content'.
  $permissions['edit any hc_contact content'] = array(
    'name' => 'edit any hc_contact content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any isolation content'.
  $permissions['edit any isolation content'] = array(
    'name' => 'edit any isolation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any malaria_testing content'.
  $permissions['edit any malaria_testing content'] = array(
    'name' => 'edit any malaria_testing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any medication_distribution content'.
  $permissions['edit any medication_distribution content'] = array(
    'name' => 'edit any medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any send_to_hc content'.
  $permissions['edit any send_to_hc content'] = array(
    'name' => 'edit any send_to_hc content',
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

  // Exported permission: 'edit any travel_history content'.
  $permissions['edit any travel_history content'] = array(
    'name' => 'edit any travel_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any treatment_history content'.
  $permissions['edit any treatment_history content'] = array(
    'name' => 'edit any treatment_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any treatment_ongoing content'.
  $permissions['edit any treatment_ongoing content'] = array(
    'name' => 'edit any treatment_ongoing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own acute_findings content'.
  $permissions['edit own acute_findings content'] = array(
    'name' => 'edit own acute_findings content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own acute_illness_danger_signs content'.
  $permissions['edit own acute_illness_danger_signs content'] = array(
    'name' => 'edit own acute_illness_danger_signs content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own acute_illness_vitals content'.
  $permissions['edit own acute_illness_vitals content'] = array(
    'name' => 'edit own acute_illness_vitals content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own call_114 content'.
  $permissions['edit own call_114 content'] = array(
    'name' => 'edit own call_114 content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own exposure content'.
  $permissions['edit own exposure content'] = array(
    'name' => 'edit own exposure content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own hc_contact content'.
  $permissions['edit own hc_contact content'] = array(
    'name' => 'edit own hc_contact content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own isolation content'.
  $permissions['edit own isolation content'] = array(
    'name' => 'edit own isolation content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own malaria_testing content'.
  $permissions['edit own malaria_testing content'] = array(
    'name' => 'edit own malaria_testing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own medication_distribution content'.
  $permissions['edit own medication_distribution content'] = array(
    'name' => 'edit own medication_distribution content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own send_to_hc content'.
  $permissions['edit own send_to_hc content'] = array(
    'name' => 'edit own send_to_hc content',
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

  // Exported permission: 'edit own travel_history content'.
  $permissions['edit own travel_history content'] = array(
    'name' => 'edit own travel_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own treatment_history content'.
  $permissions['edit own treatment_history content'] = array(
    'name' => 'edit own treatment_history content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own treatment_ongoing content'.
  $permissions['edit own treatment_ongoing content'] = array(
    'name' => 'edit own treatment_ongoing content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
