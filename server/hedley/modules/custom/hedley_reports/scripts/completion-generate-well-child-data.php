<?php

/**
 * @file
 * Generates completion data for different types of encounters.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/completion-generate-well-child-data.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Flag to generate data only if it was not generated already.
$exclude_set = drush_get_option('exclude_set', FALSE);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

$type = 'individual_participant';
$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->entityCondition('bundle', $type)
  ->fieldCondition('field_encounter_type', 'value', 'well-child')
  ->propertyCondition('status', NODE_PUBLISHED);

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type for well child encounters in DB.");
  exit;
}

$total = 0;
drush_print("$count nodes of type $type for well child encounters located.");

while (TRUE) {
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
    hedley_reports_generate_completion_data_for_well_child($node, $exclude_set);
    $total++;

    $memory = round(memory_get_usage() / 1048576);
    if ($memory >= $memory_limit) {
      drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
      return;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  drush_print("Calculated so far: $total, Memory: $memory");

  // Free up memory.
  drupal_static_reset();

  $nid = end($ids);
}

drush_print("Done! Completion data calculated for $total participants.");
