<?php

/**
 * @file
 * Marks March 24 to March 28 content as deleted (for VHW clean-up).
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_admin/scripts/
 *             vhw-march-24-28-mark-as-deleted.
 */

if (!drupal_is_cli()) {
  // Prevent execution from the browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$types = [
  'individual_participant',
  'pmtct_participant',
  'relationship',
  'session',
];

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $types)
  ->propertyCondition('created', 1742774400, '>')
  ->propertyCondition('created', 1743206400, '<')
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of specified types in DB.");
  exit;
}

drush_print("$count nodes of specified types located.");

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
    $node->field_deleted[LANGUAGE_NONE][0]['value'] = TRUE;
    node_save($node);
  }

  $nid = end($ids);
  $processed += count($nodes);

  // Explicitly unset large variables after use for memory optimization.
  unset($nodes);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Completed clearing content.");
