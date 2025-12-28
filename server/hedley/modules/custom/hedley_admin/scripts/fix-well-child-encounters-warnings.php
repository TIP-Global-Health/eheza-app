<?php

/**
 * @file
 * Locates all Well child encounters where Head Circumferences value was set.
 *
 * Checks if 'no-head-circumference-warning' warning is set (which is wrong,
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

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 500);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// Locate all Head Circumferences measurements that got value.
// Note: when skipped, value is set to 0.
$query = db_select('field_data_field_head_circumference', 'hc')
  ->fields('hc', ['entity_id']);
$query->condition('field_head_circumference_value', 0, '<>');
$head_circumferences = $query
  ->execute()
  ->fetchCol();

if (empty($head_circumferences)) {
  drush_print("There are no Head Circumferences measurements with set value.");
  return;
}

// Query to resolve well child encounters to which resolved
// Head Circumferences measurements belong.
$query = db_select('field_data_field_well_child_encounter', 'wce')
  ->fields('wce', ['field_well_child_encounter_target_id']);
$query->condition('entity_id', $head_circumferences, 'IN');
$ids = $query->execute()->fetchCol();

$total = count($ids);

if ($total == 0) {
  drush_print("There are no Well Child encounters to process.");
  return;
}

$ids = array_unique($ids);
drush_print("Located $total Well Child encounters for processing.");

$chunks = array_chunk($ids, $batch);
$updated = $precessed = 0;
foreach ($chunks as $chunk) {
  $nodes = node_load_multiple($chunk);
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

    if (empty($warnings) || ($warnings[0]['value'] === NULL)) {
      $warnings = ['value' => 'none'];
    }
    $node->field_encounter_warnings[LANGUAGE_NONE] = [$warnings];
    node_save($node);
    $updated++;
  }

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory.'));
    return;
  }

  $precessed += count($chunk);

  drush_print("Processed: $precessed, Updated: $updated.");
}

drush_print('Done!');
