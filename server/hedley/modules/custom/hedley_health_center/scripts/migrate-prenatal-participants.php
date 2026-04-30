<?php

/**
 * @file
 * Migrate existing prenatal participants into individual participants.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_health_center/scripts/migrate-prenatal-participants.php.
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
  ->propertyCondition('type', 'prenatal_participant')
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
    $old = entity_metadata_wrapper('node', $node);

    $params = [
      '@id' => $node->nid,
    ];

    $new_node = entity_create('node', [
      'type' => 'individual_participant',
      'uid' => $node->uid,
    ]);

    $new = entity_metadata_wrapper('node', $new_node);

    $new->title->set($old->label());
    $new->field_person->set($old->field_person->getIdentifier());
    $new->field_expected->set($old->field_expected->value());
    $new->field_encounter_type->set($old->field_encounter_type->value());

    $new->save();

    // Reasign all encounters for old participant.
    $query = hedley_general_create_entity_field_query_excluding_deleted();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_prenatal_participant', 'target_id', $node->nid)
      ->execute();

    if (empty($result['node'])) {
      continue;
    }

    $encounter_ids = array_keys($result['node']);
    $encounter_nodes = node_load_multiple($encounter_ids);
    $new_id = $new->getIdentifier();

    foreach ($encounter_nodes as $encounter_node) {
      $encounter_wrapper = entity_metadata_wrapper('node', $encounter_node);
      $encounter_wrapper->field_individual_participant->set($new_id);
      $encounter_wrapper->save();
    }

    drush_print(format_string('Migrated Id @id.', $params));
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print('Done!');
