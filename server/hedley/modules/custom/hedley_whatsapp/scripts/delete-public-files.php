<?php

/**
 * @file
 * Deletes screenshots copied to public folder during delivery process.
 *
 * It should not take long for file to be fetched, so we keep only those that
 * were created during past hour.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_whatsapp/scripts/delete-public-files.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$fid = drush_get_option('fid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 10);

// Get the allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$hour_ago = strtotime('-1 hours');

$base_query = db_select('file_managed', 'fm');
$base_query->addField('fm', 'fid');
$base_query->condition('fm.uri', 'public://whatsapp-upload%', 'LIKE');
$base_query->condition('fm.timestamp', $hour_ago, '<');

$count_query = clone $base_query;
if ($fid) {
  $count_query->condition('n.fid', $fid, '>');
}
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no public files to delete.");
  return;
}

drush_print("Located $total public files for deletion.");

$processed = 0;
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($fid) {
    $query->condition('fid', $fid, '>');
  }

  $ids = $query
    ->range(0, $batch)
    ->execute()
    ->fetchCol();

  if (empty($ids)) {
    // No more items left.
    break;
  }

  $deleted = 0;
  $files = file_load_multiple($ids);
  foreach ($files as $file) {
    // Delete the file record from the file_managed table.
    db_delete('file_managed')->condition('fid', $file->fid)->execute();
    // Delete the file from the file system.
    file_delete($file, TRUE);
    $deleted++;
  }

  $fid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the file ID @fid', ['@fid' => $fid]));
    return;
  }

  $count = count($files);
  $processed += $count;
  drush_print("$count public files deleted.");
}

drush_print("Done! Total of $processed public files deleted.");
