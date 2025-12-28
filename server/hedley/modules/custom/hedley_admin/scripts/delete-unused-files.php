<?php

/**
 * @file
 * Deletes files that are not in use.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/delete-unused-files.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$fid = drush_get_option('fid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$base_query = db_select('file_managed', 'files')
  ->fields('files', ['fid']);

$count_query = clone $base_query;
$count_query->condition('fid', $fid, '>');
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no files in DB.");
  exit;
}

drush_print("$total files located.");

if ($total == 0) {
  drush_print("There are no files in DB.");
  exit;
}

$unused = 0;
$unused_size = 0;

$processed = 0;
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($fid) {
    $query->condition('fid', $fid, '>');
  }

  $rows = $query
    ->range(0, $batch)
    ->execute()
    ->fetchAllAssoc('fid');

  if (empty($rows)) {
    // No more items left.
    break;
  }

  $ids = array_keys($rows);
  foreach ($ids as $id) {
    $file = file_load($id);

    $file_usage = file_usage_list($file);

    if (!empty($file_usage['file']['node'])) {
      continue;
    }

    $unused++;
    $unused_size += (int) $file->filesize;

    $success = file_delete($file, TRUE);
    if (!$success) {
      drush_print("File with ID $id can't be deleted.");
    }
  }

  $fid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $fid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  drush_print("$processed files processed.");
}

drush_print("Total unused: $unused, total size: $unused_size");
drush_print('Done!');
