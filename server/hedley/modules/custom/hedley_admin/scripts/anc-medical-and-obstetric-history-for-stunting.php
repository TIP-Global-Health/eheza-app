<?php

/**
 * @file
 * Rotates photos that were uploaded by old Chrome versions.
 *
 * Where the photo is a portrait, but got width 800 and height 600.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-medical-and-obstetric-history-for-stunting.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);


$base_query = db_select('field_data_field_newborn', 'newborns')
  ->fields('newborns', ['entity_id', 'field_newborn_target_id']);


$count_query = clone $base_query;
$count = $count_query->execute()->rowCount();

if ($count == 0) {
  drush_print("There are no newborns in DB.");
  exit;
}

drush_print("Located $count newborns.");
exit;

while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->condition('entity_id', $nid, '>');
  }

  $result = $query
    ->range(0, $batch)
    ->execute()
    ->fetchAllAssoc('entity_id');

  if (empty($result)) {
    // No more items left.
    break;
  }

  $ids = array_keys($result);
  $nodes = node_load_multiple($ids);
  $count = 0;
  foreach ($nodes as $node) {
    $fid = $node->field_photo[LANGUAGE_NONE][0]['fid'];
    $file = file_load($fid);
    $new_uri = str_replace('.jpg', '_r.jpg', $file->uri);

    $file = file_copy($file, $new_uri, 'FILE_EXISTS_RENAME');
    if (!$file) {
      drush_print("Failed to rotate photo with ID $node->nid - copy file phase");
      continue;
    }

    $image = image_load($file->uri);

    try {
      image_rotate($image, 90);
      image_save($image);
      file_save($file);
    }
    catch (Exception $e) {
      drush_print("Failed to rotate photo with ID $node->nid - rotate image phase");
    }

    $node->field_photo[LANGUAGE_NONE][0]['fid'] = $file->fid;
    $node->field_photo[LANGUAGE_NONE][0]['width'] = 600;
    $node->field_photo[LANGUAGE_NONE][0]['height'] = 800;
    node_save($node);
    $count++;
  }

  $nid = end($ids);

  drush_print("Successfully rotated $count photos.");

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done!");
