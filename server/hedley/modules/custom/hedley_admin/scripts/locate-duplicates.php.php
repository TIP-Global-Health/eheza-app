<?php

/**
 * @file
 * Locates duplicates by UUID.
 *
 * Execution:
 *   drush scr profiles/hedley/modules/custom/hedley_admin/scripts/locate-duplicates.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

$node_types = [
  'catchment_area' => 'catchment_areas',
  'clinic' => 'clinics',
  'counseling_schedule' => 'counseling-schedule',
  'counseling_topic' => 'counseling-topics',
  'health_center' => 'health_centers',
  'nurse' => 'nurses',
  'participant_form' => 'participants-form',
  'person' => 'people',
  'pmtct_participant' => 'pmtct-participants',
  'relationship' => 'relationships',
  'village' => 'villages',
  'session' => 'sessions',
];

$shard_types = [
  'attendance' => 'attendances',
  'counseling_session' => 'counseling-sessions',
  'family_planning' => 'family-plannings',
  'height' => 'heights',
  'muac' => 'muacs',
  'nutrition' => 'nutritions',
  'participant_consent' => 'participants-consent',
  'photo' => 'photos',
  'weight' => 'weights',
];

$query = db_select('field_data_field_uuid', 'uuid');
$query->fields('uuid', ['field_uuid_value']);
$query->addExpression('COUNT(uuid.field_uuid_value)', 'total');
$query->groupBy('uuid.field_uuid_value');
$query->havingCondition('total', 1, '>');
$query->range(0,1000);
$result = $query->execute()->fetchAllAssoc('field_uuid_value');

$duplicate_uuids = array_keys($result);
$chunks = array_chunk($duplicate_uuids, $batch);
$count = 0;

foreach ($chunks as $chunk) {
  drupal_static_reset();

  foreach ($chunk as $uuid) {
    $query = new EntityFieldQuery();

    $result = $query
      ->entityCondition('entity_type', 'node')
      ->fieldCondition('field_uuid', 'value', $uuid)
//      ->propertyCondition('type', array_keys($node_types), 'IN')
//      ->propertyOrderBy('nid', 'DESC')
      ->range(0,1000)
      ->execute();

    if (empty($result['node'])) {
      continue;
    }

    $ids = array_keys($result['node']);
    // $first = array_shift($ids);

//    $node = node_load($first);
    $node = node_load($ids[0]);

    if ($node->type == 'person') {
      foreach($ids as $index => $id) {
        $query1 = new EntityFieldQuery();
        $result1 = $query1
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_person', 'target_id', $id)
          ->range(0,1000)
          ->execute();

        $query2 = new EntityFieldQuery();
        $result2 = $query2
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_adult', 'target_id', $id)
          ->range(0,1000)
          ->execute();

        $query3 = new EntityFieldQuery();
        $result3 = $query3
          ->entityCondition('entity_type', 'node')
          ->fieldCondition('field_related_to', 'target_id', $id)
          ->range(0,1000)
          ->execute();

         $result1 = empty($result1['node']) ? [] :  array_keys($result1['node']);
         $result2 = empty($result2['node']) ? [] :  array_keys($result2['node']);
         $result3 = empty($result3['node']) ? [] :  array_keys($result3['node']);
         $result = array_merge($result1, $result2, $result3);

         $count += count($result);
         // node_delete_multiple($result);
      }
    }

    $count += count($ids);
    // node_delete_multiple($ids);
  }
}

drush_print("Done! Total of $count duplicate measurements deleted.");

