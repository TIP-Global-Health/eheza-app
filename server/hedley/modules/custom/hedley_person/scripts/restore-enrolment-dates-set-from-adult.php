<?php

/**
 * @file
 * Restores enrolment dates that an adult's birth-date edit overwrote.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_person/scripts/
 *   restore-enrolment-dates-set-from-adult.php [--dry_run] [--batch=50].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be restored, without restoring it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Get the number of nodes to be processed at once.
$batch = max(1, (int) drush_get_option('batch', 50));

// Participations starting on a birth date their adult has ever had, before
// the child was born. Restored ones no longer match, so re-running resumes.
$query = db_select('node', 'n');
$query->join('field_data_field_person', 'child', 'child.entity_id = n.nid');
$query->join('field_data_field_adult', 'adult', 'adult.entity_id = n.nid');
$query->join('field_data_field_expected', 'ex', 'ex.entity_id = n.nid');
$query->join('field_data_field_birth_date', 'cb', 'cb.entity_id = child.field_person_target_id');
// Revisions too: the adult's birth date may have been edited again since.
$query->join('field_revision_field_birth_date', 'ab', "ab.entity_id = adult.field_adult_target_id AND ab.entity_type = 'node' AND ab.deleted = 0");
hedley_general_apply_exclude_deleted($query, 'n');
// The shared helper takes one alias, so the child and adult are filtered here.
$query->leftJoin('field_data_field_deleted', 'cd', 'cd.entity_id = child.field_person_target_id');
$query->leftJoin('field_data_field_deleted', 'ad', 'ad.entity_id = adult.field_adult_target_id');
$query
  ->fields('n', ['nid'])
  ->condition('n.type', 'pmtct_participant')
  ->condition(db_or()->isNull('cd.field_deleted_value')->condition('cd.field_deleted_value', 0))
  ->condition(db_or()->isNull('ad.field_deleted_value')->condition('ad.field_deleted_value', 0))
  // Dates are compared by day; stored values may carry a time.
  ->where('LEFT(ex.field_expected_value, 10) = LEFT(ab.field_birth_date_value, 10)')
  ->where('ex.field_expected_value < cb.field_birth_date_value')
  ->distinct()
  ->orderBy('n.nid');

$nids = $query->execute()->fetchCol();

if (empty($nids)) {
  drush_print('No participation starts on a birth date of its adult.');
  return;
}

$count = count($nids);
drush_print("$count participations start on a birth date of their adult.");

// Formats a field_expected value by day, for example "2024-01-24 - none".
$format = function (array $dates) {
  return substr($dates['value'], 0, 10) . ' - ' . (empty($dates['value2']) ? 'none' : substr($dates['value2'], 0, 10));
};

$restored = 0;
foreach (array_chunk($nids, $batch) as $chunk) {
  foreach (node_load_multiple($chunk) as $participant) {
    $wrapper = entity_metadata_wrapper('node', $participant);
    // The dates a child's birth-date edit sets. Devices holding the
    // participation download it again.
    $expected = hedley_person_expected_dates_for_child_birth_date($wrapper, $wrapper->field_person->field_birth_date->value());

    drush_print("Participation $participant->nid: " . $format($wrapper->field_expected->value()) . ' becomes ' . $format($expected) . '.');

    if ($dry_run) {
      continue;
    }

    $wrapper->field_expected->set($expected);
    $wrapper->save();
    $restored++;
  }

  // Free up memory.
  drupal_static_reset();
  entity_get_controller('node')->resetCache();
}

if ($dry_run) {
  drush_print('Dry run: nothing was restored.');
  return;
}

drush_print("Done! Restored $restored participations.");
