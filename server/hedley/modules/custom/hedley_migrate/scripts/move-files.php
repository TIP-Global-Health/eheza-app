<?php

/**
 * @file
 * Move files to `private`.
 *
 * Run migrate-mothers first.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/move-files.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$result = db_select('file_managed', 'f')
  ->fields('f', ['fid', 'uri'])
  ->execute();

foreach ($result as $record) {
  if (strstr($record->uri, 'public://') && !strstr($record->uri, 'public://pictures/')) {
    $file = file_load($record->fid);
    file_move($file, str_replace('public://', 'private://', $file->uri));
    drush_print(format_string('Moved @uri', ['@uri' => $record->uri]));
  }
}

drush_print('------------------');
drush_print('Done!');
