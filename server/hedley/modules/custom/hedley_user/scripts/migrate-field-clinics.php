<?php

/**
 * @file
 * Migrate field-clinics to field_health_centers.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_user/scripts/migrate-field-clinics.php.
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

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'nurse')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

if ($nid) {
  $base_query->propertyCondition('nid', $nid, '>');
}

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
  $nodes = node_load_multiple($ids);

  foreach ($nodes as $node) {
    $nurse = entity_metadata_wrapper('node', $node);

    $params = [
      '@id' => $node->nid,
    ];

    $health_centers = [];
    foreach ($nurse->field_clinics as $clinic) {
      $health_centers[$clinic->field_health_center->getIdentifier()] = TRUE;
    }

    $keys = array_keys($health_centers);
    $nurse->field_health_centers->set($keys);
    $nurse->save();

    drush_print(format_string('Migrated Id @id.', $params));
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print('Done!');
