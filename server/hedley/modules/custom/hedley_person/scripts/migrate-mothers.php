<?php

/**
 * @file
 * Migrate existing mothers to Person. Run this before migrating children.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/migrate-mothers.php.
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
  ->propertyCondition('type', 'mother')
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
    $mother = entity_metadata_wrapper('node', $node);

    $params = [
      '@id' => $node->nid,
    ];

    $person_node = $mother->field_person->value();
    if (empty($person_node)) {
      $person_node = entity_create('node', [
        'type' => 'person',
        'uid' => $node->uid,
      ]);
    }

    $person = entity_metadata_wrapper('node', $person_node);

    $person->title->set($mother->title->value());
    $person->field_photo->set($mother->field_avatar->value());
    $person->field_birth_date->set($mother->field_date_birth->value());
    $person->field_education_level->set($mother->field_education_level->value());
    $person->field_ubudehe->set($mother->field_ubudehe->value());
    $person->field_clinic->set($mother->field_clinic->value());
    $person->field_gender->set(HEDLEY_PERSON_GENDER_FEMALE);

    $person->save();

    $mother->field_person->set($person);
    $mother->save();

    drush_print(format_string('Migrated Id @id.', $params));
  }

  $nid = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print('Done!');
