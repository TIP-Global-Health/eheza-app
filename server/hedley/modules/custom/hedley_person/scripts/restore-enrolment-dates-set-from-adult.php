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
$batch = drush_get_option('batch', 50);

// An adult's birth-date edit used to reset the participations they attend to
// start on that date. Restored ones no longer match, so re-running resumes.
$query = db_select('node', 'n');
$query->join('field_data_field_person', 'child', 'child.entity_id = n.nid');
$query->join('field_data_field_adult', 'adult', 'adult.entity_id = n.nid');
$query->join('field_data_field_expected', 'ex', 'ex.entity_id = n.nid');
$query->join('field_data_field_birth_date', 'cb', 'cb.entity_id = child.field_person_target_id');
$query->join('field_data_field_birth_date', 'ab', 'ab.entity_id = adult.field_adult_target_id');
hedley_general_apply_exclude_deleted($query, 'n');
$query
  ->fields('n', ['nid'])
  ->condition('n.type', 'pmtct_participant')
  // Dates are compared by day; stored values may carry a time.
  ->where('LEFT(ex.field_expected_value, 10) = LEFT(ab.field_birth_date_value, 10)')
  ->where('ab.field_birth_date_value < cb.field_birth_date_value')
  ->orderBy('n.nid');

$nids = $query->execute()->fetchCol();

if (empty($nids)) {
  drush_print('No participation starts on its adult\'s birth date.');
  return;
}

$count = count($nids);
drush_print("$count participations start on their adult's birth date.");

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
