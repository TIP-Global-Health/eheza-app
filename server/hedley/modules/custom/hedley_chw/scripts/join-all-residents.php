<?php

/**
 * @file
 * At each village, makes sure that village CHW group got all residents.
 *
 * The resident is adult+child pairs, where
 * adult is the resident of the CHW group village.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_chw/scripts/join-all-residents.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

// Get all villages.
$query = new EntityFieldQuery();
$result = $query
  ->entityCondition('entity_type', 'node')
  ->entityCondition('bundle', 'village')
  ->execute();

if (empty($result['node'])) {
  drush_print('There are no villages in DB.');
  exit;
}

$villages = array_keys($result['node']);
$total_villages = count($villages);
drush_print("There are $total_villages villages.");

foreach ($villages as $village) {
  drush_print("Processing village with ID $village.");

  $residents = hedley_chw_get_village_residents($village);
  if (empty($residents)) {
    drush_print("There are no residents in village with ID $village.");
    continue;
  }

  $total_created = 0;
  $batch_size = drush_get_option('batch', 50);
  $offset = 0;

  // A query that loads relationship for village. Since we create relations
  // only for adults, we won't get duplicates here.
  $relationships_query = new EntityFieldQuery();
  $relationships_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'relationship')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_person', 'target_id', $residents, 'IN')
    ->propertyOrderBy('nid', 'ASC')
    ->addTag('exclude_deleted');

  $relationships_query_count = clone $relationships_query;
  $count = $relationships_query_count->count()->execute();

  drush_print("There are $count relationships.");

  if ($count == 0) {
    drush_print("There are no relationships in village with ID $village.");
    continue;
  }

  $chw_group = hedley_chw_find_village_group($village);
  if (empty($chw_group)) {
    drush_print("Can't located CHW group for village $village.");
    continue;
  }

  // Execute creation in a batch.
  while ($offset < $count) {
    $query = clone $relationships_query;

    $result = $query
      ->range($offset, $batch_size)
      ->execute();

    if (empty($result['node'])) {
      break;
    }

    $keys = array_keys($result['node']);
    $relationships = node_load_multiple($keys);

    foreach ($relationships as $relationship_id) {
      $relationship = node_load($relationship_id);
      $relationship_wrapper = entity_metadata_wrapper('node', $relationship);

      // Skip relationships where child age is over 5 years.
      $child = $relationship_wrapper->field_related_to->getIdentifier();
      $child_wrapper = entity_metadata_wrapper('node', $child);
      $child_birth_date = $child_wrapper->field_birth_date->value();
      if ($child_birth_date < strtotime("-5 year")) {
        continue;
      }

      $adult = $relationship_wrapper->field_person->getIdentifier();

      $already_participate = hedley_chw_participate_in_group($child, $adult, $chw_group);
      if ($already_participate) {
        continue;
      }

      $related_by = $relationship_wrapper->field_related_by->value();
      $adult_activities = $related_by == HEDLEY_PERSON_RELATED_BY_CAREGIVER_FOR ?
        HEDLEY_SCHEDULE_PMTCT_ACTIVITIES_CAREGIVER : HEDLEY_SCHEDULE_PMTCT_ACTIVITIES_MOTHER;

      drush_print('Before add');
      hedley_chw_add_group_participation($adult, $child, $chw_group, $adult_activities, $relationship->uid);
      $total_created++;
    }

    $offset += $batch_size;

    // Free up memory.
    drupal_static_reset();
  }

  drush_print("Created $total_created participations for village with ID $village.");

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Last processed village ID: @nid', ['@nid' => $village]));
    return;
  }
}

drush_print('Done!');
