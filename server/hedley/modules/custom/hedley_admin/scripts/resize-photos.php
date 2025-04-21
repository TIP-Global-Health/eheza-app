<?php

/**
 * @file
 * Resizes large dimension images to standard size (600x800).
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/resize-photos.php.
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
$base_query->condition('filesize', 250000, '>');
$base_query->condition('filemime', 'image/jpeg');

$count_query = clone $base_query;
$count_query->condition('fid', $fid, '>');
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no images in DB.");
  exit;
}

drush_print("$total images located.");

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

  $ids = array_keys($rows);
  foreach ($ids as $id) {
    $file = file_load($id);
    list($width, $height) = getimagesize($file->uri);

    if ($width < 1000 && $height < 1000) {
      continue;
    }

    $new_height = 600;
    $new_width = 800;
    if ($width < $height) {
      $new_height = 800;
      $new_width = 600;
    }

    $image = image_load($file->uri);

    try {
      image_resize($image, $new_width, $new_height);
      image_save($image);
      file_save($file);
    }
    catch (Exception $e) {
      drush_print("Failed to resize photo with ID $file->fid");
    }

    $file_usage = file_usage_list($file);
    if (empty($file_usage['file']['node'])) {
      continue;
    }

    $nodes_ids = array_keys($file_usage['file']['node']);
    $nodes = node_load_multiple($nodes_ids);

    foreach ($nodes as $node) {
      $node->field_photo[LANGUAGE_NONE][0]['width'] = $width;
      $node->field_photo[LANGUAGE_NONE][0]['height'] = $height;
      node_save($node);
    }
  }

  $fid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $fid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  drush_print("$processed images processed.");
}

drush_print('Done!');
