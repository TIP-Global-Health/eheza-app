<?php

/**
 * @file
 * Class hedley_view_export_plugin_display_export.
 */

/**
 * Class hedley_view_export_plugin_display_export.
 */
// phpcs:ignore
class hedley_view_export_plugin_display_export extends views_plugin_display_page {

  /**
   * Hide the exposed filters.
   */
  // phpcs:ignore
  public function displays_exposed() {
    return FALSE;
  }

  /**
   * Turn off display path.
   */
  // phpcs:ignore
  public function has_path() {
    return FALSE;
  }

}
