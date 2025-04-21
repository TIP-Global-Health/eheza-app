<?php

/**
 * @file
 * Copies the assessment from Follow Up to Nutrition measurement.
 *
 * Handles Group and Individual Nutrition encounters.
 *
 * Execution:
 *   drush scr profiles/hedley/modules/custom/hedley_admin/scripts/populate-nutrition-assessment.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

$config = [
  // Group Encounter.
  'follow_up' => [
    'session_field' => 'field_session',
    'target_type' => 'nutrition',
  ],
  // Nutrition Encounter.
  'nutrition_follow_up' => [
    'session_field' => 'field_nutrition_encounter',
    'target_type' => 'nutrition_nutrition',
  ],
];

foreach ($config as $type => $data) {
  $nid = drush_get_option('nid', 0);

  $session_field = $data['session_field'];
  $target_type = $data['target_type'];

  $base_query = base_query_for_bundle($type);
  $count_query = clone $base_query;
  $count_query->propertyCondition('nid', $nid, '>');
  $count = $count_query->count()->execute();

  if ($count == 0) {
    drush_print("There are no nodes of type $type in DB.");
    continue;
  }

  drush_print('');
  drush_print("$count nodes of type $type located.");
  drush_print('');

  $processed = $updated = 0;
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
      $wrapper = entity_metadata_wrapper('node', $node);
      $person_id = $wrapper->field_person->getIdentifier();
      $session_id = $wrapper->{$session_field}->getIdentifier();
      $assessment = $wrapper->field_nutrition_assesment->value();

      if (empty($person_id) || empty($session_id) || empty($assessment)) {
        continue;
      }

      // Trace Nutrition measurement for the Follow up.
      $trace_query = new EntityFieldQuery();
      $trace_query
        ->entityCondition('entity_type', 'node')
        ->propertyCondition('type', $target_type)
        ->fieldCondition('field_person', 'target_id', $person_id)
        ->fieldCondition($session_field, 'target_id', $session_id)
        ->range(0, $batch);

      $result = $trace_query->execute();

      if (empty($result['node'])) {
        // Nutrition measurement was not located for the Follow Up.
        continue;
      }

      $nutrition_id = key($result['node']);
      $target_wrapper = entity_metadata_wrapper('node', $nutrition_id);
      $target_wrapper->field_nutrition_assesment->set($assessment);
      $target_wrapper->save();
      $updated++;

      wlog("Follow Up ID: $node->nid => Nutrition ID: $nutrition_id");
    }

    $nid = end($ids);

    if (round(memory_get_usage() / 1048576) >= $memory_limit) {
      drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
      return;
    }

    $count = count($ids);
    $processed += $count;
    drush_print("$processed nodes processed.");
  }

  drush_print('');
  drush_print("Updated Assessment for $updated nodes of type $target_type.");
  drush_print('');
}

/**
 * Generate base query.
 */
function base_query_for_bundle($bundle) {
  $base_query = new EntityFieldQuery();
  $base_query
    ->entityCondition('entity_type', 'node')
    ->propertyCondition('type', $bundle)
    ->propertyOrderBy('nid');

  return $base_query;
}

/**
 * Prints log based on verbosity option.
 */
function wlog($message) {
  // Get the option that will determine if output should be verbose or not.
  $verbose = drush_get_option('verbose', FALSE);

  if ($verbose !== FALSE) {
    drush_print($message);
  }
}
