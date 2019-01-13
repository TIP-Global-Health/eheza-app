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

  $items['set-measured-by'] = [
    'callback' => 'hedley_activity_set_measured_by',
    'description' => 'Fill in measured by where null',
    'arguments' => [],
    'options' => [],
    'aliases' => [],
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

/**
 * Fill in field_measured_by.
 */
function hedley_activity_set_measured_by() {
  $bundles = hedley_activity_get_measurements_content_types();

  $query = db_select('node', 'n');

  $users = $query
    ->fields('n', ['nid', 'title'])
    ->condition('n.type', 'user', '=')
    ->execute()
    ->fetchAllAssoc('title');

  $query = db_select('node', 'n');

  $query->join('users', 'u', 'n.uid = u.uid');

  $query->leftJoin(
    'field_data_field_measured_by',
    'mb',
    'n.nid = mb.entity_id'
  );

  $result = $query
    ->fields('n', ['nid'])
    ->fields('u', ['name'])
    ->isNull('mb.field_measured_by_target_id')
    ->condition('n.type', $bundles, 'IN')
    ->execute()
    ->fetchAll();

  foreach ($result as $node) {
    if (!empty($users[$node->name])) {
      drush_print('Updating node ' . $node->nid);
      $wrapper = entity_metadata_wrapper('node', $node->nid);
      $wrapper->field_measured_by->set($users[$node->name]->nid);
      $wrapper->save();
    }
  }
}
