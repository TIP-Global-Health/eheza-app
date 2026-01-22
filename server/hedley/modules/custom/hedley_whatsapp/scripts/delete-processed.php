<?php

/**
 * @file
 * Deletes files of messages that were delivered.
 *
 * Since WhatsApp message was delivered, there's no point of storing the
 * report file, which can be over 1MB is size.
 * Also, we delete if all delivery attempts have failed.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_whatsapp/scripts/delete-processed.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get the allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$type = 'whatsapp_record';
// Maximal number of attempts of delivering message.
$delivery_attempts = variable_get('hedley_whatsapp_delivery_attempts', 5);

$base_query = hedley_general_create_db_select_query_excluding_deleted();
$base_query->addField('n', 'nid');
$base_query->condition('n.status', NODE_PUBLISHED);
$base_query->condition('n.type', $type);
$base_query->leftJoin('field_data_field_screenshot', 'ss', 'n.nid = ss.entity_id');
$base_query->isNotNull('ss.field_screenshot_fid');
$base_query->leftJoin('field_data_field_delivery_attempts', 'da', 'n.nid = da.entity_id');
$base_query->leftJoin('field_data_field_date_concluded', 'dc', 'n.nid = dc.entity_id');
$or = db_or();
$or->isNotNull('dc.field_date_concluded_value');
$or->condition('da.field_delivery_attempts_value', $delivery_attempts, '>=');
$base_query->condition($or);

$count_query = clone $base_query;
if ($nid) {
  $count_query->condition('n.nid', $nid, '>');
}
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no $type screenshots to delete.");
  return;
}

drush_print("Located $total $type screenshots for deletion.");

$processed = 0;
while ($processed < $total) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->condition('nid', $nid, '>');
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
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    // Get the file ID from the field_screenshot field.
    $file_id = $node->field_screenshot[LANGUAGE_NONE][0]['fid'];
    // Load the file object.
    $file = file_load($file_id);
    if (!$file) {
      // Could not load the file, so further processing is not possible.
      continue;
    }
    // Delete the file from the file system.
    file_delete($file, TRUE);
    // Remove the reference to the file from the field.
    unset($node->field_screenshot[LANGUAGE_NONE][0]);
    // Save the node without the reference to the file.
    node_save($node);
    // Delete the file record from the file_managed table.
    db_delete('file_managed')->condition('fid', $file_id)->execute();
    // Clear the entity cache to reflect the changes.
    entity_get_controller('node')->resetCache([$nid]);

    $deleted++;
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($nodes);
  $processed += $count;
  drush_print("$count $type screenshots deleted.");
}

drush_print("Done! Total of $processed $type screenshots deleted.");
