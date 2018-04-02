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

  $batch_size = 50;
  $offset = 0;

  $base_query = new EntityFieldQuery();
  $base_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->propertyOrderBy('nid', 'ASC');

  do {
    $query = clone $base_query;

    $result = $query
      ->range($offset, $batch_size)
      ->execute();

    if (empty($result['node'])) {
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

    $offset += $batch_size;
    $query_count = clone $base_query;
    $count = $query_count->count()->execute();

  } while ($count > 0);
}
