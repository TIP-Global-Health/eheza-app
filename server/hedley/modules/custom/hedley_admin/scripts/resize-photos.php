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
$memory_limit = drush_get_option('memory_limit', 500);

$base_query = db_select('field_data_field_photo', 'photos')
  ->fields(
    'photos',
    [
      'field_photo_fid',
      'field_photo_width',
      'field_photo_height'
    ]
  );
$or = db_or()
  ->condition('field_photo_width', 1000, '>')
  ->condition('field_photo_height', 1000, '>');
$base_query->condition($or);
$base_query->groupBy('photos.field_photo_fid');

$count_query = clone $base_query;
$count_query->condition('field_photo_fid', $fid, '>');
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
    $query->condition('field_photo_fid', $fid, '>');
  }

  $rows = $query
    ->range(0, $batch)
    ->execute()
    ->fetchAll();

  if (empty($rows)) {
    // No more items left.
    break;
  }

  foreach ($rows as $row) {
    $file = file_load($row->field_photo_fid);

    $height = 600;
    $width = 800;
    if ($row->field_photo_width < $row->field_photo_height) {
      $height = 800;
      $width = 600;
    }

    $image = image_load($file->uri);

    try {
      image_resize($image, $width, $height);
      image_save($image);
      file_save($file);
    }
    catch (Exception $e) {
      drush_print("Failed to resize photo with ID $file->fid");
    }

    $file_usage = file_usage_list($file);
    $ids = array_keys($file_usage['file']['node']);

    if (empty($ids)) {
      continue;
    }

    $nodes = node_load_multiple($ids);
    foreach ($nodes as $node) {
      $node->field_photo[LANGUAGE_NONE][0]['width'] = $width;
      $node->field_photo[LANGUAGE_NONE][0]['height'] = $height;
      node_save($node);
    }
  }

  $last = end($rows);
  $fid = $last->field_photo_fid;

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $fid]));
    return;
  }

  $count = count($rows);
  $processed += $count;
  drush_print("$processed images processed.");
}

drush_print('Done!');
