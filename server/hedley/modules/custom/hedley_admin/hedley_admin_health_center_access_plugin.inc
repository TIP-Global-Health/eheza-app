<?php

/**
 * Access plugin that provides health-center-based access.
 */
class hedley_admin_health_center_access_plugin extends views_plugin_access {

  public function summary_title() {
    return t('Health-center-based access');
  }

  /**
   * Determine if the current user has access or not.
   */
  public function access($account) {

    return hedley_admin_health_center_access($account);
  }

  public function get_access_callback() {
    return ['hedley_admin_health_center_access', []];
  }

}
