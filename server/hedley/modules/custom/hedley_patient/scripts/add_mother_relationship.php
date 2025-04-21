<?php

/**
 * @file
 * Triggering the process of adding a mother "relationship".
 *
 * Adding this value to to all the existing "mother" entities.
 *
 * @run: drush scr profiles/hedley/modules/custom/hedley_patient/scripts/add_mother_relationship.php
 */

// Get the last node id.
$nid = drush_get_option('nid', 0);
// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);
// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);
$i = 0;
$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'mother')
  ->propertyOrderBy('nid', 'ASC')
  ->addTag('exclude_existing_relationships')
  ->addTag('DANGEROUS_ACCESS_CHECK_OPT_OUT');
if ($nid) {
  $base_query->propertyCondition('nid', $nid, '>');
}

$query_count = clone $base_query;
$count = $query_count->count()->execute();

if ($count) {
  drush_print(format_string('Found @count nodes with a missing relationship, updating those nodes with a "mother" relationship.', [
    '@count' => $count,
  ]));
}
else {
  drush_print(format_string('Did NOT find any node that needs setting a relationship, stopping the process.'));
  return;
}

while ($i < $count) {
  // Free up memory.
  drupal_static_reset();
  $query = clone $base_query;
  if ($nid) {
    $query
      ->propertyCondition('nid', $nid, '>');
  }
  $result = $query
    ->range(0, $batch)
    ->execute();
  if (empty($result['node'])) {
    return;
  }

  $ids = array_keys($result['node']);
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $wrapper = entity_metadata_wrapper('node', $node);

    try {
      // Set the relationship to "mother".
      $wrapper->field_relationship->set('mother');
      $wrapper->save();
    }
    catch (Exception $e) {
      $params = array(
        '@error' => $e->getMessage(),
        '@nid' => $node->nid,
        '@title' => $node->title,
      );
      drush_print(format_string('There was error updating the node(@nid) @title. More info: @error', $params), 'error');
    }
  }
  $i += count($nodes);
  $nid = end($ids);
  $params = array(
    '@start' => reset($ids),
    '@end' => end($ids),
    '@iterator' => $i,
    '@max' => $count,
  );
  drush_print(format_string('Process entities from id @start to id @end. Batch state: @iterator/@max', $params));
  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    $params = array(
      '@memory' => round(memory_get_usage() / 1048576),
      '@max_memory' => memory_get_usage(TRUE) / 1048576,
    );
    drush_print(format_string('Stopped before out of memory. Start process from the node ID @nid', array('@nid' => end($ids))), 'error');
    return;
  }
}
