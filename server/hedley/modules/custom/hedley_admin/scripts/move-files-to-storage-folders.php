<?php

/**
 * @file
 * Relocates files into storage folders.
 *
 * Make sure base folder ('private') does not explode.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/move-files-to-storage-folders.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$fid = drush_get_option('fid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 100);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 200);

$base_query = db_select('file_managed', 'fm')
  ->fields('fm', ['fid', 'timestamp'])
  ->orderBy('fm.fid');
$or = db_or();
$or->condition('fm.uri', 'private://image-%', 'LIKE');
$or->condition('fm.uri', 'private://signature-%', 'LIKE');
$base_query->condition($or);

$count_query = clone $base_query;
$count = $count_query->execute()->rowCount();

if ($count == 0) {
  drush_print("No files to relocate into storage folder.");
  exit;
}

drush_print("Located $count files to relocate into storage folder.");

while (TRUE) {
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

  $remaining = count($ids);
  if ($remaining == 0) {
    return;
  }

  $successful = [];
  $files = file_load_multiple($ids);
  foreach ($files as $file) {
    if (!file_exists($file->uri)) {
      drush_print("File ID $file->fid does not exist.");
      continue;
    }

    $target_folder = 'private://storage-' . date('m-Y', (int) $file->timestamp) . '/';
    if (!file_prepare_directory($target_folder, FILE_CREATE_DIRECTORY | FILE_MODIFY_PERMISSIONS)) {
      drush_print("Failed to create storage folder!");
      exit;
    }

    if (!file_move($file, str_replace('private://', $target_folder, $file->uri), FILE_EXISTS_ERROR)) {
      drush_print("Failed to relocate file ID $file->fid.");
      continue;
    }

    $successful[] = $file->fid;
  }

  $count = count($successful);
  drush_print("Successfully relocated $count files.");

  if ($count > 0) {
    drush_print("Resaving nodes that use them...");

    $usage_nodes_ids = db_select('file_usage', 'fu')
      ->fields('fu', ['id'])
      ->condition('fu.type', 'node')
      ->condition('fu.fid', $successful, 'IN')
      ->execute()
      ->fetchCol();

    if (!empty($usage_nodes_ids)) {
      $usage_nodes = node_load_multiple($usage_nodes_ids);
      foreach ($usage_nodes as $node) {
        node_save($node);
      }
    }
  }

  $fid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@fid' => $fid]));
    exit;
  }
}

drush_print("Done!");
