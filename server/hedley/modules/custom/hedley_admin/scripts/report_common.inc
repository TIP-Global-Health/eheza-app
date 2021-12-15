<?php

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

require_once __DIR__ . '/HedleyAdminTextTable.php';
