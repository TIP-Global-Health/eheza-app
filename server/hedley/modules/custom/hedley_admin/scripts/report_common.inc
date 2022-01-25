<?php

/**
 * @file
 * Common parts for recurring report Drush scripts.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

require_once __DIR__ . '/HedleyAdminTextTable.php';
