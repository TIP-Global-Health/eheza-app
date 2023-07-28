<?php

/**
 * @file
 * Generates new revisions for all entities of certain content type.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_ncda/scripts/generate-data-for-all.php.
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
$memory_limit = drush_get_option('memory_limit', 250);

$type = 'village';
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

$total = 0;
drush_print("$count nodes of type $type located.");

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
  foreach ($ids as $id) {
    $residents = hedley_chw_get_village_residents($id);

    foreach ($residents as $resident) {
      hedley_ncda_calculate_aggregated_data_for_person($resident);
    }
    $count = count($residents);
    $total += $count;
    drush_print("Village $id: $count residents");

    $memory = round(memory_get_usage() / 1048576);

    if ($memory >= $memory_limit) {
      drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
      return;
    }

    drush_print("Memory: $memory");

    // Free up memory.
    drupal_static_reset();
  }

  $nid = end($ids);
}

drush_print("Done! There are $total residents.");
