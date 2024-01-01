<?php

/**
 * @file
 * Assigns Nutrition encounter type to all encounters that got measurements.
 * If measurement created by nurse - encounter type set to nurse.
 * If by CHW, set to chw.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_admin/scripts/assign-nutrition-encounter-type.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$fid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 500);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 200);

// Load IDs of all health workers with role 'nurse'.
$query = db_select('field_data_field_role', 'r')
  ->fields('r', ['entity_id']);
$query->condition('field_role_value', 'nurse');
$nurses_ids = $query
  ->execute()
  ->fetchCol('entity_id');

// Load one measurement per nutrition encounter.
// Measurement has a record of nurse that recorded it, so
// we associate between encounter and nurse that recorded a
// measurement within this encounter.
$base_query = db_select('field_data_field_nutrition_encounter', 'ne')
  ->fields('ne', ['entity_id', 'field_nutrition_encounter_target_id']);
$base_query->groupBy('ne.field_nutrition_encounter_target_id');
$base_query->orderBy('entity_id');

$count_query = clone $base_query;
$count = $count_query->execute()->rowCount();

if ($count == 0) {
  drush_print("There are no nutrition encounters with measurements in DB.");
  return;
}

drush_print("Located $count nutrition encounters with measurements.");

while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->condition('entity_id', $nid, '>');
  }

  $rows = $query
    ->range(0, $batch)
    ->execute()
    ->fetchAll();

  if (empty($rows)) {
    // No more items left.
    break;
  }

  $measurements_ids = [];
  $encounters_ids = [];
  foreach ($rows as $row) {
    $measurements_ids[] = $row->entity_id;
    $encounters_ids[] = $row->field_nutrition_encounter_target_id;
  }

  $measurements = node_load_multiple($measurements_ids);
  $encounters = node_load_multiple($encounters_ids);
  $encounters_map = [];
  foreach ($encounters as $encounter) {
    $encounters_map[$encounter->nid] = $encounter;
  }

  foreach ($measurements as $measurement) {
    $encounter_id = $measurement->field_nutrition_encounter[LANGUAGE_NONE][0]['target_id'];
    $nurse_id = $measurement->field_nurse[LANGUAGE_NONE][0]['target_id'];
    $encounter_type = in_array($nurse_id, $nurses_ids) ? 'nurse' : 'chw';
    $encounters_map[$encounter_id]->field_nutrition_encounter_type[LANGUAGE_NONE][0]['value'] = $encounter_type;
    node_save($encounters_map[$encounter_id]);
  }

  $nid = end($measurements_ids);
  $count = count($encounters_ids);
  drush_print("Successfully updated $count encounters.");

  if (round(memory_get_usage() / 1048576) >= $memory_limit) {
    drush_print(dt('Stopped before out of memory. Start process from the node ID @nid', ['@nid' => $nid]));
    return;
  }
}

drush_print("Done!");