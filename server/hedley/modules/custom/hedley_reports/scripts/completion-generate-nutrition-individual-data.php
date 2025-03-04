<?php

/**
 * @file
 * Generates completion data for Nutrition encounters.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_reports/scripts/completion-generate-nutrition-individual-data.php.
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

$type = 'nutrition_encounter';
$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->entityCondition('bundle', $type)
  ->propertyCondition('status', NODE_PUBLISHED);

if ($exclude_set) {
  $base_query->addTag('exclude_set_reports_data');
}

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type in DB.");
  return;
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
  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $completion_data = hedley_reports_generate_completion_data_for_nutrition_individual_encounter($node);
    $node->field_reports_data[LANGUAGE_NONE][0]['value'] = json_encode($completion_data);
    node_save($node);
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

drush_print("Done! Completion data calculated for $total encounters.");
