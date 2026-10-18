<?php

/**
 * @file
 * Restores Acute Illness and Prenatal diagnoses the device did not store.
 *
 * Execution: drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/restore-lost-encounter-diagnoses.php
 *   --file=<path> [--dry_run].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

$file = drush_get_option('file');
if (empty($file) || !is_readable($file)) {
  drush_print('Provide a readable list with --file=<path>.');
  return;
}

// Report what would be restored, without restoring it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// The diagnosis field of each encounter type. Acute Illness holds one
// diagnosis, Prenatal a set of them.
$fields = [
  'acute_illness_encounter' => 'field_acute_illness_diagnosis',
  'prenatal_encounter' => 'field_prenatal_diagnoses',
];

$restored = $skipped = 0;
// Each line holds a nid, a tab and the codes to restore, for example
// "5436892<TAB>anemia,moderate-risk-of-preeclampsia". A header is skipped.
foreach (file($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
  $columns = explode("\t", $line);
  if (!ctype_digit($columns[0]) || empty($columns[1])) {
    continue;
  }

  $nid = $columns[0];
  $codes = array_map('trim', explode(',', $columns[1]));
  $codes_text = implode(', ', $codes);

  $node = node_load($nid);
  if (empty($node) || empty($fields[$node->type])) {
    drush_print("Encounter $nid: not an Acute Illness or Prenatal encounter, skipped.");
    $skipped++;
    continue;
  }

  $wrapper = entity_metadata_wrapper('node', $node);
  if ($wrapper->field_deleted->value()) {
    drush_print("Encounter $nid: deleted, skipped.");
    $skipped++;
    continue;
  }

  $field_name = $fields[$node->type];
  $unknown = array_diff($codes, array_keys(list_allowed_values(field_info_field($field_name))));
  if (!empty($unknown)) {
    drush_print("Encounter $nid: unknown diagnosis " . implode(', ', $unknown) . ', skipped.');
    $skipped++;
    continue;
  }

  $stored = (array) $wrapper->{$field_name}->value();
  $stored = array_values(array_diff($stored, ['none']));
  $missing = array_values(array_diff($codes, $stored));
  $stored_text = empty($stored) ? 'none' : implode(', ', $stored);
  if (empty($missing)) {
    drush_print("Encounter $nid: already holds $codes_text.");
    $skipped++;
    continue;
  }

  if ($node->type == 'acute_illness_encounter') {
    // A diagnosis stored since the list was made is kept, not replaced.
    if (!empty($stored) || count($codes) > 1) {
      drush_print("Encounter $nid: stores $stored_text, not $codes_text, skipped.");
      $skipped++;
      continue;
    }
    $value = $codes[0];
  }
  else {
    // The listed codes join the stored ones, which are all kept.
    $value = array_merge($stored, $missing);
  }

  drush_print("Encounter $nid: $stored_text becomes " . implode(', ', (array) $value) . '.');

  if ($dry_run) {
    continue;
  }

  $wrapper->{$field_name}->set($value);
  // Keep the stored value in the history. Devices holding the encounter
  // download it again.
  $node->revision = TRUE;
  $node->log = 'Restored a diagnosis the device computed but did not store.';
  $wrapper->save();
  $restored++;
}

if ($dry_run) {
  drush_print("Dry run: nothing was restored. Skipped $skipped.");
  return;
}

drush_print("Done! Restored $restored encounters, skipped $skipped. Now run profiles/hedley/modules/custom/hedley_stats/scripts/recalculate-stats.php.");
