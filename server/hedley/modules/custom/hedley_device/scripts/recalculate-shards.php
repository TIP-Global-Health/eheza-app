<?php

/**
 * @file
 * Recalculate all shards.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_device/scripts/recalculate-shards.php.
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
$memory_limit = drush_get_option('memory_limit', 500);

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'person')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

if ($nid) {
  $base_query->propertyCondition('nid', $nid, '>');
}

while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '>');
  }

  $result = $query
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    // No more items left.
    break;
  }

  $ids = array_keys($result['node']);

  foreach ($ids as $id) {
    $params = [
      '@id' => $id,
    ];

    hedley_patient_recalculate_shards_for_person_content($id);

    drush_print(format_string('Recalculed shards for person @id.', $params));
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print('Done!');
