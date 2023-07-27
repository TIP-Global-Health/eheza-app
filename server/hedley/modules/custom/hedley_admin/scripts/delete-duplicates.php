<?php

/**
 * @file
 * Locates duplicates by UUID and deletes all, but first one.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/delete-duplicates.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Generate and execute query that will pull all UUIDs that appear
// more than once - an indication for duplicate content.
$query = db_select('field_data_field_uuid', 'uuid');
$query->fields('uuid', ['field_uuid_value']);
$query->addExpression('COUNT(uuid.field_uuid_value)', 'total');
$query->groupBy('uuid.field_uuid_value');
$query->havingCondition('total', 1, '>');
$query->range(0, 2000);
$result = $query->execute()->fetchAllAssoc('field_uuid_value');
$duplicate_uuids = array_keys($result);

$total = count($duplicate_uuids);
drush_print("Located $total instances of duplications.");

$chunks = array_chunk($duplicate_uuids, $batch);
$count = 0;

foreach ($chunks as $uuids) {
  // Free up memory.
  drupal_static_reset();

  foreach ($uuids as $uuid) {
    $query = new EntityFieldQuery();

    // Pull all nodes that share UUID.
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->fieldCondition('field_uuid', 'value', $uuid)
      ->range(0, 500)
      ->execute();

    if (empty($result['node'])) {
      continue;
    }

    $ids = array_keys($result['node']);
    // Extract first node from the list.
    // Since updates are performed using UUID, it does not really matter
    // which node we keep, as they all share same UUID.
    $first = array_shift($ids);
    $node = node_load($first);

    if ($node->type == 'person') {
      // For person, we resolve all nodes that reference it, and
      // delete them as well.
      // This makes sure we clear all relationships, group participations
      // and measurements that were taken for that person.
      foreach ($ids as $index => $id) {
        // Pull all reference made with 'field_person' field.
        $query1 = new EntityFieldQuery();
        $result1 = $query1
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_person', 'target_id', $id)
          ->range(0, 500)
          ->execute();

        // Pull all reference made with 'field_adult' field.
        $query2 = new EntityFieldQuery();
        $result2 = $query2
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_adult', 'target_id', $id)
          ->range(0, 500)
          ->execute();

        // Pull all reference made with 'field_related_to' field.
        $query3 = new EntityFieldQuery();
        $result3 = $query3
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_related_to', 'target_id', $id)
          ->range(0, 500)
          ->execute();

        // Combine the results of 3 queries.
        $result1 = empty($result1['node']) ? [] : array_keys($result1['node']);
        $result2 = empty($result2['node']) ? [] : array_keys($result2['node']);
        $result3 = empty($result3['node']) ? [] : array_keys($result3['node']);
        $result = array_merge($result1, $result2, $result3);

        $count += count($result);
        // Delete all nodes that reference the person.
        node_delete_multiple($result);
      }
    }

    $count += count($ids);
    // Delete the duplicates (all but the first one).
    node_delete_multiple($ids);
  }
}

drush_print("Done! Total of $count duplicate nodes were deleted.");
