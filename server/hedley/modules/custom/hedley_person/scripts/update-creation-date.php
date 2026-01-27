<?php

/**
 * @file
 * Updates created field for nodes migrated into Person CT.
 *
 * When Child and mother CT where migrated into to Person CT, we lost
 * original creation date. This script restores original 'created' value.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/update-creation-date.php.
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

$base_query = hedley_general_create_entity_field_query_excluding_deleted();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', ['child', 'mother'], 'IN')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->propertyOrderBy('nid', 'ASC');

if ($nid) {
  $base_query->propertyCondition('nid', $nid, '>');
}

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type mother/child in DB.");
  exit;
}

$total = 0;
drush_print("$count nodes of type mother/child located.");

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
  $patients = node_load_multiple($ids);

  foreach ($patients as $patient) {
    $wrapper = entity_metadata_wrapper('node', $patient);
    $person = $wrapper->field_person->value();
    if ($person) {
      $person->created = $patient->created;
      node_save($person);
    }
  }

  $nid = end($ids);
  $total += count($patients);
  drush_print("Updated so far: $total");

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print('Done!');
