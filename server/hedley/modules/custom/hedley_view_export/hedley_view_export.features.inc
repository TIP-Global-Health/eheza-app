<?php

/**
 * @file
 * hedley_view_export.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function hedley_view_export_ctools_plugin_api($module = NULL, $api = NULL) {
  if ($module == "strongarm" && $api == "strongarm") {
    return array("version" => "1");
  }
}
