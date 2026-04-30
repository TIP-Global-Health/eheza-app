<?php

/**
 * @file
 * Recalculate all shards.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_patient/scripts/populate-sessions-shards.php.
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

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'session')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no sessions in DB.");
  exit;
}

drush_print("$total sessions located.");

$processed = 0;
while ($processed < $total) {
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
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $wrapper = entity_metadata_wrapper('node', $node);
    $health_center_id = $wrapper->field_clinic->field_health_center->getIdentifier();

    if (empty($health_center_id)) {
      drush_print("Can't resolve health center of clinic for session with ID $node->nid! Skipping...");

      continue;
    }

    $wrapper->field_shards->set([$health_center_id]);
    $wrapper->save();
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($nodes);
  $processed += $count;
  drush_print("$count sessions processed.");
}

drush_print('Done!');
