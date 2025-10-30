<?php

/**
 * @file
 * Clears statistics caches and recalculates statistics for all HCs.
 *
 * Execution:  drush scr
 *  profiles/hedley/modules/custom/hedley_stats/scripts/recalculate-stats.php.
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

$type = 'health_center';

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
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
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_NUTRITION);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_PRENATAL);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_ACUTE_ILLNESS);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_NCD);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_PMTCT);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_SPV);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_CHILD_SCOREBOARD);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_NUTRITION_INDIVIDUAL);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_NUTRITION_GROUP);
    hedley_stats_clear_caches_for_health_center($id, HEDLEY_STATS_GROUP_EDUCATION);

    // Add AQ item to re-calculate all the stats offline.
    hedley_general_add_task_to_advanced_queue_by_id(HEDLEY_STATS_CALCULATE_STATS, $id, [
      'health_center_nid' => $id,
    ]);
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done!");
