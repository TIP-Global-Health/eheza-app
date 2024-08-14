<?php

/**
 * @file
 * hedley_ncda.features.user_permission.inc
 */

/**
 * Implements hook_user_default_permissions().
 */
function hedley_ncda_user_default_permissions() {
  $permissions = array();

  // Exported permission: 'create child_scoreboard_bcg_iz content'.
  $permissions['create child_scoreboard_bcg_iz content'] = array(
    'name' => 'create child_scoreboard_bcg_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_dtp_iz content'.
  $permissions['create child_scoreboard_dtp_iz content'] = array(
    'name' => 'create child_scoreboard_dtp_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_dtp_sa_iz content'.
  $permissions['create child_scoreboard_dtp_sa_iz content'] = array(
    'name' => 'create child_scoreboard_dtp_sa_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_encounter content'.
  $permissions['create child_scoreboard_encounter content'] = array(
    'name' => 'create child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_ipv_iz content'.
  $permissions['create child_scoreboard_ipv_iz content'] = array(
    'name' => 'create child_scoreboard_ipv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_mr_iz content'.
  $permissions['create child_scoreboard_mr_iz content'] = array(
    'name' => 'create child_scoreboard_mr_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_ncda content'.
  $permissions['create child_scoreboard_ncda content'] = array(
    'name' => 'create child_scoreboard_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_opv_iz content'.
  $permissions['create child_scoreboard_opv_iz content'] = array(
    'name' => 'create child_scoreboard_opv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_pcv13_iz content'.
  $permissions['create child_scoreboard_pcv13_iz content'] = array(
    'name' => 'create child_scoreboard_pcv13_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'create child_scoreboard_rotarix_iz content'.
  $permissions['create child_scoreboard_rotarix_iz content'] = array(
    'name' => 'create child_scoreboard_rotarix_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_bcg_iz content'.
  $permissions['delete any child_scoreboard_bcg_iz content'] = array(
    'name' => 'delete any child_scoreboard_bcg_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_dtp_iz content'.
  $permissions['delete any child_scoreboard_dtp_iz content'] = array(
    'name' => 'delete any child_scoreboard_dtp_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_dtp_sa_iz content'.
  $permissions['delete any child_scoreboard_dtp_sa_iz content'] = array(
    'name' => 'delete any child_scoreboard_dtp_sa_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_encounter content'.
  $permissions['delete any child_scoreboard_encounter content'] = array(
    'name' => 'delete any child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_ipv_iz content'.
  $permissions['delete any child_scoreboard_ipv_iz content'] = array(
    'name' => 'delete any child_scoreboard_ipv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_mr_iz content'.
  $permissions['delete any child_scoreboard_mr_iz content'] = array(
    'name' => 'delete any child_scoreboard_mr_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_ncda content'.
  $permissions['delete any child_scoreboard_ncda content'] = array(
    'name' => 'delete any child_scoreboard_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_opv_iz content'.
  $permissions['delete any child_scoreboard_opv_iz content'] = array(
    'name' => 'delete any child_scoreboard_opv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_pcv13_iz content'.
  $permissions['delete any child_scoreboard_pcv13_iz content'] = array(
    'name' => 'delete any child_scoreboard_pcv13_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete any child_scoreboard_rotarix_iz content'.
  $permissions['delete any child_scoreboard_rotarix_iz content'] = array(
    'name' => 'delete any child_scoreboard_rotarix_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_bcg_iz content'.
  $permissions['delete own child_scoreboard_bcg_iz content'] = array(
    'name' => 'delete own child_scoreboard_bcg_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_dtp_iz content'.
  $permissions['delete own child_scoreboard_dtp_iz content'] = array(
    'name' => 'delete own child_scoreboard_dtp_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_dtp_sa_iz content'.
  $permissions['delete own child_scoreboard_dtp_sa_iz content'] = array(
    'name' => 'delete own child_scoreboard_dtp_sa_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_encounter content'.
  $permissions['delete own child_scoreboard_encounter content'] = array(
    'name' => 'delete own child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_ipv_iz content'.
  $permissions['delete own child_scoreboard_ipv_iz content'] = array(
    'name' => 'delete own child_scoreboard_ipv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_mr_iz content'.
  $permissions['delete own child_scoreboard_mr_iz content'] = array(
    'name' => 'delete own child_scoreboard_mr_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_ncda content'.
  $permissions['delete own child_scoreboard_ncda content'] = array(
    'name' => 'delete own child_scoreboard_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_opv_iz content'.
  $permissions['delete own child_scoreboard_opv_iz content'] = array(
    'name' => 'delete own child_scoreboard_opv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_pcv13_iz content'.
  $permissions['delete own child_scoreboard_pcv13_iz content'] = array(
    'name' => 'delete own child_scoreboard_pcv13_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'delete own child_scoreboard_rotarix_iz content'.
  $permissions['delete own child_scoreboard_rotarix_iz content'] = array(
    'name' => 'delete own child_scoreboard_rotarix_iz content',
    'roles' => array(
      'administrator' => 'administrator',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_bcg_iz content'.
  $permissions['edit any child_scoreboard_bcg_iz content'] = array(
    'name' => 'edit any child_scoreboard_bcg_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_dtp_iz content'.
  $permissions['edit any child_scoreboard_dtp_iz content'] = array(
    'name' => 'edit any child_scoreboard_dtp_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_dtp_sa_iz content'.
  $permissions['edit any child_scoreboard_dtp_sa_iz content'] = array(
    'name' => 'edit any child_scoreboard_dtp_sa_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_encounter content'.
  $permissions['edit any child_scoreboard_encounter content'] = array(
    'name' => 'edit any child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_ipv_iz content'.
  $permissions['edit any child_scoreboard_ipv_iz content'] = array(
    'name' => 'edit any child_scoreboard_ipv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_mr_iz content'.
  $permissions['edit any child_scoreboard_mr_iz content'] = array(
    'name' => 'edit any child_scoreboard_mr_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_ncda content'.
  $permissions['edit any child_scoreboard_ncda content'] = array(
    'name' => 'edit any child_scoreboard_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_opv_iz content'.
  $permissions['edit any child_scoreboard_opv_iz content'] = array(
    'name' => 'edit any child_scoreboard_opv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_pcv13_iz content'.
  $permissions['edit any child_scoreboard_pcv13_iz content'] = array(
    'name' => 'edit any child_scoreboard_pcv13_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit any child_scoreboard_rotarix_iz content'.
  $permissions['edit any child_scoreboard_rotarix_iz content'] = array(
    'name' => 'edit any child_scoreboard_rotarix_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_bcg_iz content'.
  $permissions['edit own child_scoreboard_bcg_iz content'] = array(
    'name' => 'edit own child_scoreboard_bcg_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_dtp_iz content'.
  $permissions['edit own child_scoreboard_dtp_iz content'] = array(
    'name' => 'edit own child_scoreboard_dtp_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_dtp_sa_iz content'.
  $permissions['edit own child_scoreboard_dtp_sa_iz content'] = array(
    'name' => 'edit own child_scoreboard_dtp_sa_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_encounter content'.
  $permissions['edit own child_scoreboard_encounter content'] = array(
    'name' => 'edit own child_scoreboard_encounter content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_ipv_iz content'.
  $permissions['edit own child_scoreboard_ipv_iz content'] = array(
    'name' => 'edit own child_scoreboard_ipv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_mr_iz content'.
  $permissions['edit own child_scoreboard_mr_iz content'] = array(
    'name' => 'edit own child_scoreboard_mr_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_ncda content'.
  $permissions['edit own child_scoreboard_ncda content'] = array(
    'name' => 'edit own child_scoreboard_ncda content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_opv_iz content'.
  $permissions['edit own child_scoreboard_opv_iz content'] = array(
    'name' => 'edit own child_scoreboard_opv_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_pcv13_iz content'.
  $permissions['edit own child_scoreboard_pcv13_iz content'] = array(
    'name' => 'edit own child_scoreboard_pcv13_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  // Exported permission: 'edit own child_scoreboard_rotarix_iz content'.
  $permissions['edit own child_scoreboard_rotarix_iz content'] = array(
    'name' => 'edit own child_scoreboard_rotarix_iz content',
    'roles' => array(
      'administrator' => 'administrator',
      'sync' => 'sync',
    ),
    'module' => 'node',
  );

  return $permissions;
}
