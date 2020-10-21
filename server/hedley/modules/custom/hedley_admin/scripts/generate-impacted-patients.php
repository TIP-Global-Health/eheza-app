<?php

/**
 * @file
 * Recalculate all shards.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/generate-impacted-patients.php.
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
  ->propertyCondition('status', NODE_PUBLISHED);

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$total = $count_query->count()->execute();

if ($total == 0) {
  drush_print("There are no people in DB.");
  exit;
}

drush_print("$total people located.");

$measurement_types = hedley_admin_get_measurement_types();
$impacted_patients = [];

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
  foreach ($ids as $id) {
    $measurements_ids = hedley_admin_get_person_measurements($id, $measurement_types);
    if (empty($measurements_ids)) {
      continue;
    }

    if (count($measurements_ids) > 50) {
      $impacted_patients[] = $id;
      continue;
    }

    $measurements = node_load_multiple($measurements_ids);
    $first_timestamp = array_shift($measurements)->created;

    foreach($measurements as $measurement) {
      if (abs($first_timestamp - $measurement->created) > 14*24*3600) {
        $impacted_patients[] = $id;
        break;
      }
    }
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }

  $count = count($ids);
  $processed += $count;
  drush_print("$processed persons processed.");
}

drush_print('Done!');
$count = count($impacted_patients);
drush_print("$count patients impacted.");

