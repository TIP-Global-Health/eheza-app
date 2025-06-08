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

  $base_query = hedley_general_create_entity_field_query_excluding_deleted();
  $base_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->propertyOrderBy('nid', 'ASC');

  // Print total count.
  $query_count = clone $base_query;
  $count = $query_count->count()->execute();
  drush_print(dt('Found @count nodes.', ['@count' => $count]));

  while ($offset < $count) {
    // Free up memory.
    drupal_static_reset();

    $query = clone $base_query;

    $result = $query
      ->range($offset, $batch_size)
      ->execute();

    if (empty($result['node'])) {
      drush_print('Empty batch.');
      return;
    }

    drush_print(dt('Processing @low - @high of @count', [
      '@low' => $offset + 1,
      '@high' => min($offset + $batch_size, $count),
      '@count' => $count,
    ]));

    $nids = array_keys($result['node']);

    foreach (node_load_multiple($nids) as $node) {
      // hedley_activity_node_presave will do the calculation, so we don't
      // need to think hard here.
      $wrapper = entity_metadata_wrapper('node', $node);
      $wrapper->field_zscore_age->set(NULL);
      $wrapper->save();
    }

    $offset += $batch_size;
  }

  drush_print("Finished");
}
