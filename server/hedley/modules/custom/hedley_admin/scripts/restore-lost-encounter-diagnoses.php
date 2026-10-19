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

// The codes each field allows.
$allowed = array_map(function ($field_name) {
  return array_keys(list_allowed_values(field_info_field($field_name)));
}, $fields);

// Splits "anemia, none,anemia" into ['anemia']; none is not a diagnosis.
$parse_codes = function ($text) {
  $codes = array_map('trim', explode(',', $text));
  $codes = array_unique($codes);
  return array_values(array_diff($codes, ['', 'none']));
};

// Lists the codes for a message, or none when there are no codes.
$codes_text = function (array $codes) {
  return empty($codes) ? 'none' : implode(', ', $codes);
};

$restored = $skipped = $failed = 0;
$seen = [];
// Each line holds a nid, the codes to restore, and the codes stored when the
// list was made, separated by tabs. For example "5436892<TAB>anemia<TAB>none".
foreach (file($file, FILE_IGNORE_NEW_LINES) as $index => $line) {
  $line_number = $index + 1;
  if ($index == 0) {
    // Remove a byte order mark, which some editors add.
    $line = preg_replace('/^\xEF\xBB\xBF/', '', $line);
  }
  if (trim($line) === '') {
    continue;
  }

  $columns = explode("\t", trim($line));
  if ($index == 0 && !ctype_digit($columns[0])) {
    drush_print("Line 1: \"$line\" taken as the header.");
    continue;
  }

  $codes = isset($columns[1]) ? $parse_codes($columns[1]) : [];
  if (!ctype_digit($columns[0]) || empty($codes) || !isset($columns[2])) {
    drush_print("Line $line_number: \"$line\" is not a nid, codes and stored codes, skipped.");
    $skipped++;
    continue;
  }

  $nid = $columns[0];
  // What the encounter stored when the list was made.
  $listed_stored = $parse_codes($columns[2]);

  if (isset($seen[$nid])) {
    drush_print("Encounter $nid: listed twice, skipped.");
    $skipped++;
    continue;
  }
  $seen[$nid] = TRUE;

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
  $unknown = array_diff($codes, $allowed[$node->type]);
  if (!empty($unknown)) {
    drush_print("Encounter $nid: unknown diagnosis " . implode(', ', $unknown) . ', skipped.');
    $skipped++;
    continue;
  }

  $is_acute_illness = $node->type == 'acute_illness_encounter';
  if ($is_acute_illness && count($codes) > 1) {
    drush_print("Encounter $nid: Acute Illness holds one diagnosis, not " . $codes_text($codes) . ', skipped.');
    $skipped++;
    continue;
  }

  $stored = array_values(array_diff((array) $wrapper->{$field_name}->value(), ['none']));
  if (empty(array_diff($codes, $stored))) {
    drush_print("Encounter $nid: already holds " . $codes_text($codes) . '.');
    $skipped++;
    continue;
  }

  // Diagnoses stored since the list was made are kept, and the encounter is
  // left for a person to check.
  if (array_diff($stored, $listed_stored) || array_diff($listed_stored, $stored)) {
    drush_print("Encounter $nid: stores " . $codes_text($stored) . ', the list expected ' . $codes_text($listed_stored) . ', skipped.');
    $skipped++;
    continue;
  }

  // Acute Illness holds one diagnosis, so a stored one is never replaced.
  if ($is_acute_illness && !empty($stored)) {
    drush_print("Encounter $nid: Acute Illness stores " . $codes_text($stored) . ', which is kept, skipped.');
    $skipped++;
    continue;
  }

  // Prenatal keeps the stored codes and adds the missing ones.
  $value = $is_acute_illness ? $codes[0] : array_values(array_unique(array_merge($stored, $codes)));
  drush_print("Encounter $nid: " . $codes_text($stored) . ' becomes ' . $codes_text((array) $value) . '.');

  if ($dry_run) {
    $restored++;
    continue;
  }

  try {
    $wrapper->{$field_name}->set($value);
    // Keep the stored value in the history. Devices holding the encounter
    // download it again.
    $node->revision = TRUE;
    $node->log = 'Restored a diagnosis the device computed but did not store.';
    $wrapper->save();
    $restored++;
  }
  catch (Exception $e) {
    drush_print("Encounter $nid: not saved, " . $e->getMessage());
    $failed++;
  }
}

if ($dry_run) {
  drush_print("Dry run: nothing was restored. Would restore $restored, skip $skipped.");
  return;
}

drush_print("Done! Restored $restored encounters, skipped $skipped, failed $failed.");

// Statistics are not recalculated for saves made from drush.
if ($restored > 0) {
  drush_print('Now run profiles/hedley/modules/custom/hedley_stats/scripts/recalculate-stats.php.');
}
