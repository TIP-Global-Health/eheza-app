<?php

/**
 * @file
 * Generate Circuit-specific demo content via Drush.
 */

/**
 * Implements hook_drush_command().
 *
 * This is needed in order for Drush pre hooks to be executed.
 *
 * @see _drush_invoke_hooks()
 */
function hedley_migrate_drush_command() {
  return [];
}
