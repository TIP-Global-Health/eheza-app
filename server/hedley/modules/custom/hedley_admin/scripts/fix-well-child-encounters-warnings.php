<?php

/**
 * @file
 * Locates all Well child encounters where Head Circumferences value was set,
 * and checks if 'no-head-circumference-warning' warning is set (which is wrong,
 * as Head Circumferences has valid value).
 * If found, 'no-head-circumference-warning' warning is deleted.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/fix-well-child-encounters-warnings.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$entity_id = drush_get_option('entity_id', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 200);

// Locate all Head Circumferences measurements that got value.
// Note: when skipped, value is ste to 0.
$query = db_select('field_data_field_head_circumference', 'hc')
  ->fields('hc', ['entity_id']);
$query->condition('field_head_circumference_value', 0, '<>');
$head_circumferences = $query
  ->execute()
  ->fetchCol('entity_id');

if (empty($head_circumferences)) {
  drush_print("There are no Head Circumferences measurements with set value.");
  return;
}

// Query to resolve well child encounters to which resolved
// Head Circumferences measurements belong.
$base_query = db_select('field_data_field_well_child_encounter', 'wce')
  ->fields('wce', ['field_well_child_encounter_target_id']);
$base_query->condition('entity_id', $head_circumferences, 'IN');

$count_query = clone $base_query;
$count_query->condition('entity_id', $entity_id, '>');
$executed = $count_query->execute();
$total = $executed->rowCount();

if ($total == 0) {
  drush_print("There are no Well Child encounters to process.");
  exit;
}

drush_print("Located $total Well Child encounters for processing.");

$updated = 0;
while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($entity_id) {
    $query->condition('entity_id', $entity_id, '>');
  }

  $ids = $query
    ->range(0, $batch)
    ->execute()
    ->fetchCol('field_well_child_encounter_target_id');

  if (count($ids) == 0) {
    // No more items to process.
    break;
  }

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $unset = FALSE;
    $warnings = $node->field_encounter_warnings[LANGUAGE_NONE];
    foreach ($warnings as $index => $warning) {
      if ($warning['value'] == 'no-head-circumference-warning') {
        unset($warnings[$index]);
        $unset = TRUE;
        break;
      }
    }

    if (!$unset) {
      continue;
    }

    if (empty($warnings)) {
      $warnings = ['value' => 'none'];
    }
    $node->field_encounter_warnings[LANGUAGE_NONE] = [$warnings];
    node_save($node);
    $updated++;
  }

  $entity_id = end($ids);

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @entity_id', ['@entity_id' => $entity_id]));
    return;
  }

  drush_print("Update warning at  $updated encounters.");
}

drush_print('Done!');
