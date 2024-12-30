<?php

/**
 * @file
 * Generates sample DB (form currently installed DB).
 *
 * Execution: drush scr
 *  profiles/hedley/modules/custom/hedley_admin/scripts/generate-sample-db.php.
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
$memory_limit = drush_get_option('memory_limit', 500);


$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes in DB.");
  exit;
}

drush_print("There are $count nodes at DB.");

$processed = 0;
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
    process_node($node);
  }

  $nid = end($ids);
  $processed += count($nodes);

  // Explicitly unset large variables after use for memory optimization.
  unset($nodes);

  if ($processed % 5000 == 0) {
    drush_print("Processed $processed out of $count.");
  }

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done!");

function process_node($node) {
  if ($node->type === 'catchment_area') {
    return;
  }

  $sample_health_centers_ids = [7091, 7092, 28589];

  $bundles_to_delete = [
    'child',
    'counseling_schedule',
    'counseling_topic',
    'mother',
    'participant_form',
    'report_data',
    'resilience_survey',
    'sync_incident',
    'whatsapp_record',
    'photo',
    'nutrition_photo',
    'prenatal_photo',
    'well_child_photo',
    'nurse',
    'device',
  ];
  if (in_array($node->type, $bundles_to_delete)) {
    node_delete($node);
    return;
  }

  if ($node->type === 'health_center') {
    if (!in_array($node->nid, $sample_health_centers_ids)) {
      node_delete($node);
    };
    return;
  }

  if ($node->type === 'village') {
    $health_center_id = $node->field_health_center[LANGUAGE_NONE][0]['target_id'];
    if (!in_array($health_center_id , $sample_health_centers_ids)) {
      node_delete($node);
    };
    return;
  }

  if ($node->type === 'person') {
    $shards = sanitise_shards($node->field_shards[LANGUAGE_NONE], $sample_health_centers_ids);
    if (empty($shards)) {
      node_delete($node);
    }
    else {
      $node->field_shards[LANGUAGE_NONE] = $shards;
      $node->field_national_id_number[LANGUAGE_NONE][0]['value'] = '';
      $node->field_phone_number[LANGUAGE_NONE][0]['value'] = '';
      // @todo: change name
      node_save($node);
    }

    return;
  }

  // For any other node type:
  $shards = sanitise_shards($node->field_shards[LANGUAGE_NONE], $sample_health_centers_ids);
  if (empty($shards)) {
    node_delete($node);
  }
  else {
    $node->field_shards[LANGUAGE_NONE] = $shards;
    node_save($node);
  }
}

function sanitise_shards($shards, $sample_health_centers_ids) {
  foreach ($shards as $index => $shard) {
    if (!in_array($shard['target_id'] , $sample_health_centers_ids)) {
      unset($shards[$index]);
    };
  }

  return array_values($shards);
}
