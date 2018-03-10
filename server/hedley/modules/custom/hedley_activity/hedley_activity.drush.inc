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

  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', $bundles)
    ->propertyCondition('status', NODE_PUBLISHED)
    ->execute();

  if ($result['node']) {
    foreach (array_keys($result['node']) as $id) {
      // The presave hook will do the calculation, so we
      // don't need to think hard here.
      $wrapper = entity_metadata_wrapper('node', $id);
      $wrapper->field_z_score_age->set(NULL);
      $wrapper->save();
    }
  }
}
