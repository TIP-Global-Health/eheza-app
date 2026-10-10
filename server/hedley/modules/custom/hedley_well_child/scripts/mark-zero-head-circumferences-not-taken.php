<?php

/**
 * @file
 * Marks head circumferences typed as 0 cm as "not taken".
 *
 * Their encounters' microcephaly warning is replaced with the "not taken" one.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_well_child/
 *   scripts/mark-zero-head-circumferences-not-taken.php [--dry_run].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be changed, without changing it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Head circumferences of 0 cm without the "not-taken" note. A nurse typed 0
// instead of ticking "not taken", and the 0 was scored as microcephaly.
$query = db_select('field_data_field_head_circumference', 'hc');
$query->join('node', 'n', 'n.nid = hc.entity_id');
$query->leftJoin('field_data_field_measurement_notes', 'mn', "mn.entity_type = 'node' AND mn.entity_id = hc.entity_id AND mn.deleted = 0 AND mn.field_measurement_notes_value = 'not-taken'");
hedley_general_apply_exclude_deleted($query, 'n');

$ids = $query
  ->fields('hc', ['entity_id'])
  ->condition('hc.entity_type', 'node')
  ->condition('hc.deleted', 0)
  ->condition('hc.field_head_circumference_value', 0)
  ->isNull('mn.entity_id')
  ->execute()
  ->fetchCol();

$count = count($ids);
if ($count == 0) {
  drush_print('No head circumference was typed as 0 cm.');
  return;
}

drush_print("$count head circumferences were typed as 0 cm.");

$warnings_replaced = 0;
foreach ($ids as $id) {
  $head_circumference = entity_metadata_wrapper('node', $id);
  $encounter = $head_circumference->field_well_child_encounter;
  $warnings = $encounter->field_encounter_warnings->value();
  $microcephaly = array_search('warning-head-circumference-microcephaly', $warnings);

  if ($dry_run) {
    $flagged = $microcephaly === FALSE ? 'no microcephaly warning' : 'microcephaly warning';
    drush_print("  Head circumference $id, encounter {$encounter->getIdentifier()}: $flagged.");
    continue;
  }

  // Saving writes a new revision, which devices download.
  $head_circumference->field_measurement_notes->set(['not-taken']);
  $head_circumference->save();

  if ($microcephaly === FALSE) {
    continue;
  }

  $warnings[$microcephaly] = 'no-head-circumference-warning';
  $encounter->field_encounter_warnings->set(array_values(array_unique($warnings)));
  $encounter->save();
  $warnings_replaced++;
}

if ($dry_run) {
  drush_print('Dry run. Nothing was changed.');
  return;
}

drush_print("Done! Marked $count as not taken, and replaced $warnings_replaced microcephaly warnings.");
