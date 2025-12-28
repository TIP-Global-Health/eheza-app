<?php

/**
 * @file
 * Migrate field_clinic on Person to a PMTCT Participant.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_schedule/scripts/migrate-clinic.php.
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

// We're just testing that field_clinic has a value.
$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'person')
  ->propertyCondition('status', NODE_PUBLISHED)
  ->fieldCondition('field_clinic')
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
    $mother_id = $node->nid;

    // Set health center for mother according to setting at it's clinic.
    $mother->field_health_center->set($mother->field_clinic->field_health_center->getIdentifier());
    $mother->save();

    $params = [
      '@id' => $node->nid,
    ];

    $relationship_query = new EntityFieldQuery();
    $relationship_query
      ->entityCondition('entity_type', 'node')
      ->propertyCondition('type', 'relationship')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_person', 'target_id', $mother_id);

    $relationship_result = $relationship_query->execute();

    if (empty($relationship_result['node'])) {
      drush_print(format_string('No child found for mother @id.', $params));
      continue;
    }

    $relationship_ids = array_keys($relationship_result['node']);
    $relationship_nodes = node_load_multiple($relationship_ids);

    foreach ($relationship_nodes as $relationship_node) {
      $relationship = entity_metadata_wrapper('node', $relationship_node);

      // Set health center for child according to setting at mother's clinic.
      $child = entity_metadata_wrapper('node', $relationship->field_related_to->value());
      $child->field_health_center->set($mother->field_clinic->field_health_center->getIdentifier());
      $child->save();

      $participation_query = new EntityFieldQuery();
      $participation_query
        ->entityCondition('entity_type', 'node')
        ->propertyCondition('type', 'pmtct_participant')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->fieldCondition('field_person', 'target_id', $relationship->field_related_to->getIdentifier())
        ->fieldCondition('field_adult', 'target_id', $relationship->field_person->getIdentifier());

      $participation_result = $participation_query->execute();

      if (empty($participation_result['node'])) {
        $participation_node = entity_create('node', [
          'type' => 'pmtct_participant',
          'uid' => $relationship_node->uid,
        ]);

        $participation = entity_metadata_wrapper('node', $participation_node);

        $participation->field_person->set($relationship->field_related_to->getIdentifier());
        $participation->field_adult->set($relationship->field_person->getIdentifier());

        switch ($relationship->field_related_by->value()) {
          case HEDLEY_PERSON_RELATED_BY_PARENT_OF:
            if ($participation->field_adult->field_gender->value() == HEDLEY_PERSON_GENDER_MALE) {
              $activities = HEDLEY_SCHEDULE_PMTCT_ACTIVITIES_CAREGIVER;
            }
            else {
              $activities = HEDLEY_SCHEDULE_PMTCT_ACTIVITIES_MOTHER;
            }
            break;

          case HEDLEY_PERSON_RELATED_BY_CAREGIVER_FOR:
            $activities = HEDLEY_SCHEDULE_PMTCT_ACTIVITIES_CAREGIVER;
            break;
        }

        $participation->field_adult_activities->set($activities);

        $birth_date = $relationship->field_related_to->field_birth_date->value();

        // By default, we'll start expecting the child as of their birth date.
        $participation->field_expected->set([
          'value' => date('Y-m-d', $birth_date),
          'value2' => NULL,
        ]);

        $participation->field_clinic->set($mother->field_clinic->getIdentifier());

        $participation->save();
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
