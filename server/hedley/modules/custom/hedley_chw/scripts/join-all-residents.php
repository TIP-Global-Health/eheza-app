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
$memory_limit = drush_get_option('memory_limit', 800);

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
drush_print("Located $total_villages villages.");

foreach ($villages as $village) {
  drush_print('');
  drush_print("Processing village with ID $village.");

  $residents = hedley_chw_get_village_residents($village);
  if (empty($residents)) {
    drush_print("There are no residents in village with ID $village.");
    continue;
  }

  $total_created = 0;
  $batch_size = drush_get_option('batch', 50);
  $offset = 0;

  // A query that loads participation for adult residents of the village.
  $participations_query = new EntityFieldQuery();
  $participations_query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'pmtct_participant')
    ->propertyCondition('status', NODE_PUBLISHED)
    ->fieldCondition('field_adult', 'target_id', $residents, 'IN')
    ->propertyOrderBy('nid', 'ASC')
    ->addTag('exclude_deleted');

  $participations_query_count = clone $participations_query;
  $count = $participations_query_count->count()->execute();

  if ($count == 0) {
    drush_print("There are no participations for village with ID $village.");
    continue;
  }

  $chw_group = hedley_chw_find_village_group($village);
  if (empty($chw_group)) {
    drush_print("Can't located CHW group for village $village.");
    continue;
  }

  // Execute creation in a batch.
  while ($offset < $count) {
    $query = clone $participations_query;

    $result = $query
      ->range($offset, $batch_size)
      ->execute();

    if (empty($result['node'])) {
      break;
    }

    $keys = array_keys($result['node']);
    $participations = node_load_multiple($keys);

    foreach ($participations as $participation) {
      $participation_wrapper = entity_metadata_wrapper('node', $participation);

      // Do not proceed, if we can't resolve participation group.
      $clinic = $participation_wrapper->field_clinic->getIdentifier();
      if (empty($clinic)) {
        drush_print("Participation with ID $participation->nid don't have clinic assigned.");
        continue;
      }

      $clinic_wrapper = entity_metadata_wrapper('node', $clinic);
      $clinic_type = $clinic_wrapper->field_group_type->value();
      $allowed_types = [
        HEDLEY_PERSON_CLINIC_TYPE_FBF,
        HEDLEY_PERSON_CLINIC_TYPE_PMTCT,
      ];

      if (!in_array($clinic_type, $allowed_types)) {
        continue;
      }

      // Skip participations where child age is over 5 years.
      $child = $participation_wrapper->field_person->getIdentifier();
      $child_wrapper = entity_metadata_wrapper('node', $child);
      $child_birth_date = $child_wrapper->field_birth_date->value();
      if ($child_birth_date < strtotime("-5 year")) {
        continue;
      }

      $adult = $participation_wrapper->field_adult->getIdentifier();

      $already_participate = hedley_chw_participate_in_group($child, $adult, $chw_group);
      if ($already_participate) {
        continue;
      }

      $adult_activities = $participation_wrapper->field_adult_activities->value();
      hedley_person_create_pmtct_participant($adult, $child, $adult_activities, $chw_group, $participation->uid);
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
