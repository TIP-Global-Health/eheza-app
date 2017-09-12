<?php

/**
 * @file
 * Generate Hedley demo content via Drush.
 */

/**
 * Hook for defining drush commands.
 */
function hedley_migrate_drush_command() {
  $items['delete-child-measurements'] = [
    'callback' => 'hedley_delete_child_measurements',
    'description' => 'Delete all measurements for the given child ID.',
    'arguments' => [
      'child_id' => 'The ID of the child whose measurements will be deleted.',
    ],
    'options' => [],
    'aliases' => ['dcm'],
  ];
  return $items;
}

/**
 * Deletes all child measurements for the given child ID.
 */
function hedley_delete_child_measurements($child_id) {
  $bundles = [
    'height',
    'weight',
    'muac',
    'nutrition',
    'photo',
  ];

  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_child', 'target_id', $child_id)
    ->range(0, 200)
    ->execute();

  node_delete_multiple(array_keys($result['node']));
}
