<?php

/**
 * @file
 * Clears stored z-scores that no real measurement could produce.
 *
 * Those of "not taken" head circumferences, and ones beyond the limits.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_activity/scripts/
 *   clear-implausible-zscores.php [--dry_run].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be cleared, without clearing it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Each z-score field, the bundles holding it, and the limits that apply.
$targets = [
  ['field_zscore_age', HEDLEY_ACTIVITY_HEIGHT_BUNDLES, 'length_for_age'],
  ['field_zscore_age', HEDLEY_ACTIVITY_WEIGHT_BUNDLES, 'weight_for_age'],
  [
    'field_zscore_age',
    [HEDLEY_ACTIVITY_WELL_CHILD_HEAD_CIRCUMFERENCE_CONTENT_TYPE],
    'head_circumference_for_age',
  ],
  ['field_zscore_length', HEDLEY_ACTIVITY_WEIGHT_BUNDLES, 'weight_for_length'],
  ['field_zscore_bmi', HEDLEY_ACTIVITY_WEIGHT_BUNDLES, 'bmi_for_age'],
];

$cleared = [];
foreach ($targets as $target) {
  list($field, $bundles, $kind) = $target;
  list($lowest, $highest) = HEDLEY_ACTIVITY_ZSCORE_LIMITS[$kind];
  $column = $field . '_value';

  $query = db_select("field_data_$field", 'z');
  $query
    ->fields('z', ['entity_id', 'revision_id'])
    ->condition('z.entity_type', 'node')
    ->condition('z.deleted', 0)
    ->condition('z.bundle', $bundles, 'IN');

  $beyond_limits = db_or()
    ->condition("z.$column", $lowest, '<')
    ->condition("z.$column", $highest, '>');

  if ($kind == 'head_circumference_for_age') {
    // A "not taken" head circumference, whatever its z-score.
    $query->leftJoin('field_data_field_measurement_notes', 'n', "n.entity_type = 'node' AND n.entity_id = z.entity_id AND n.deleted = 0 AND n.field_measurement_notes_value = 'not-taken'");
    $beyond_limits->isNotNull('n.entity_id');
  }

  $rows = $query
    ->condition($beyond_limits)
    ->distinct()
    ->execute()
    ->fetchAllKeyed();

  drush_print(dt('@field on @kind: @count to clear.', [
    '@field' => $field,
    '@kind' => $kind,
    '@count' => count($rows),
  ]));

  if ($dry_run || empty($rows)) {
    continue;
  }

  // Deleted directly, not by saving the node: a save writes a new revision,
  // which every device downloads again. Devices do not read these z-scores.
  foreach (array_chunk($rows, 1000, TRUE) as $chunk) {
    db_delete("field_data_$field")
      ->condition('entity_type', 'node')
      ->condition('entity_id', array_keys($chunk), 'IN')
      ->execute();

    // Earlier revisions keep their values; only the current one is cleared.
    db_delete("field_revision_$field")
      ->condition('entity_type', 'node')
      ->condition('revision_id', array_values($chunk), 'IN')
      ->execute();
  }

  $cleared += $rows;
}

if ($dry_run) {
  drush_print('Dry run. Nothing was cleared.');
  return;
}

foreach (array_keys($cleared) as $nid) {
  cache_clear_all("field:node:$nid", 'cache_field');
}

// Dashboards keep counting the cleared values until statistics are
// recalculated.
drush_print(dt('Done! Cleared z-scores of @count measurements. Now run hedley_stats/scripts/recalculate-stats.php.', [
  '@count' => count($cleared),
]));
