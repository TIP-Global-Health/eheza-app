<?php

/**
 * @file
 * Drush commands.
 */

/**
 * Hook for defining drush commands.
 */
function hedley_activity_drush_command() {
  $items['recalculate-zscores'] = [
    'callback' => 'hedley_activity_recalculate_zscores',
    'description' => 'Recalculate all z-scores',
    'arguments' => [],
    'options' => [],
    'aliases' => ['rzs'],
  ];

  return $items;
}

/**
 * Recalculate all z-scores.
 */
function hedley_activity_recalculate_zscores() {
  $bundles = [
    'height',
    'weight',
  ];

  // If we ever run into out-of-memory errors doing this, then we should
  // switch to getting batches and looping.
  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->execute();

  if (empty($result['node']) {
    // No measurements to update.
    return;
  }

  $nids = array_keys($result['node']);
  foreach (node_load_multiple($nids) as $node) {
    // hedley_activity_node_presave will do the calculation, so we don't
    // need to think hard here.
    $wrapper = entity_metadata_wrapper('node', $node);
    $wrapper->field_z_score_age->set(NULL);
    $wrapper->save();
  }
}
