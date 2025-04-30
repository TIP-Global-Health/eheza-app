<?php

/**
 * @file
 * Generates new revisions for all entities of certain content type.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_admin/scripts/
 *             bump-revisions-for-content-type.php --type=[content type].
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

$type = drush_get_option('type', FALSE);
if (!$type) {
  drush_print('Please specify --type option');
  exit;
}

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyOrderBy('nid', 'ASC');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

drush_print("$count nodes of type $type located.");

$processed = 0;
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
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    node_save($node);
  }

  $nid = end($ids);
  $processed += count($nodes);

  // Explicitly unset large variables after use for memory optimization.
  unset($nodes);

  if ($processed % 5000 == 0) {
    drush_print("Processed $processed out of $count.");
  }

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done! New revisions were generated for all nodes of type $type.");
