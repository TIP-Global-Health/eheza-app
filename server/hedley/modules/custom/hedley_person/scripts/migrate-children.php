<?php

/**
 * @file
 * Migrate existing children to Person.
 *
 * Run migrate-mothers first.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/migrate-children.php.
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
  ->propertyCondition('type', 'child')
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
    $child = entity_metadata_wrapper('node', $node);

    $params = [
      '@id' => $node->nid,
    ];

    $person_node = $child->field_person->value();
    if (empty($person_node)) {
      $person_node = entity_create('node', [
        'type' => 'person',
        'uid' => $node->uid,
      ]);
    }

    $person = entity_metadata_wrapper('node', $person_node);

    $person->title->set($child->title->value());
    $person->field_photo->set($child->field_avatar->value());
    $person->field_birth_date->set($child->field_date_birth->value());
    $person->field_gender->set($child->field_gender->value());

    $person->save();

    $child->field_person->set($person);
    $child->save();

    $mother_id = $child->field_mother->field_person->getIdentifier();
    if (!empty($mother_id)) {
      $relationship_query = hedley_general_create_entity_field_query_excluding_deleted();
      $relationship_query
        ->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'relationship')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->fieldCondition('field_person', 'target_id', $mother_id)
        ->fieldCondition('field_related_to', 'target_id', $person->getIdentifier())
        ->fieldCondition('field_related_by', 'value', HEDLEY_PERSON_RELATED_BY_PARENT_OF);

      $relationship_result = $relationship_query->execute();

      if (empty($relationship_result['node'])) {
        $relationship_node = entity_create('node', [
          'type' => 'relationship',
          'uid' => $node->uid,
        ]);

        $relationship = entity_metadata_wrapper('node', $relationship_node);

        $relationship->field_person->set($mother_id);
        $relationship->field_related_to->set($person);
        $relationship->field_related_by->set(HEDLEY_PERSON_RELATED_BY_PARENT_OF);

        $relationship->save();
      }
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
