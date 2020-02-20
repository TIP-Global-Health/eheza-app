<?php

/**
 * @file
 * Class hedley_view_export_plugin_display_export.
 */

/**
 * Class hedley_view_export_plugin_display_export.
 */
class hedley_view_export_plugin_display_export extends views_plugin_display_page {

  /**
   * Hide the exposed filters.
   */
  public function displays_exposed() {
    return FALSE;
  }

  /**
   * Turn off display path.
   */
  public function has_path() {
    return FALSE;
  }

}
