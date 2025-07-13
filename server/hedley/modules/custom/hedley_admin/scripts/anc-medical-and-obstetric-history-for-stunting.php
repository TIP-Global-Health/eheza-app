<?php

/**
 * @file
 * Rotates photos that were uploaded by old Chrome versions.
 *
 * Where the photo is a portrait, but got width 800 and height 600.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-medical-and-obstetric-history-for-stunting.php.
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


$query = db_select('field_data_field_newborn', 'newborns')
  ->fields('newborns', ['entity_id', 'field_newborn_target_id']);

$newborns = $query
  ->execute()
  ->fetchAll();

if (empty($newborns)) {
  // No more items left.
  drush_print("There are no newborns in DB.");
  return;
}

// Create mapping between newborn ID and its pregnancy.
$newborns_mapping = [];
foreach ($newborns as $newborn) {
  $newborns_mapping[$newborn['field_newborn_target_id']] = $newborn['entity_id'];
  $newborns_ids[] = $newborn['field_newborn_target_id'];
}

$newborns_ids = array_keys($newborns_mapping);
// Now we want all height (all types) measurements that contain
// z-score that indicates stunting.
$newborns_heights = db_select('field_data_field_person', 'persons')
  ->fields('persons', ['entity_id'])
  ->condition('bundle', HEDLEY_ACTIVITY_HEIGHT_BUNDLES, 'IN')
  ->condition('field_person_target_id', $newborns_ids, 'IN')
  ->execute()
  ->fetchAllAssoc('entity_id');
$newborns_heights_ids = array_keys($newborns_heights);
if (empty($newborns_heights_ids)) {
  drush_print("There are no height measurements for newborns in DB.");
  return;
}

$stunting_heights = db_select('field_data_field_zscore_age', 'zscores')
  ->fields('zscores', ['entity_id'])
  ->condition('field_zscore_age_value', -2, '<')
  ->execute()
  ->fetchAllAssoc('entity_id');
$stunting_heights_ids = array_keys($stunting_heights);
if (empty($stunting_heights_ids)) {
  drush_print("There are no height measurements indicating stunting for newborns in DB.");
  return;
}

drush_print("Done!");
