<?php

/**
 * @file
 * Recalculates NCDA data where the low birth weight indicator changed.
 *
 * The indicator is calculated once and stored on the person, so a change to
 * how it is calculated leaves what is already stored behind. Two groups of
 * children are affected: those whose birth weight is between the threshold the
 * indicator used to apply and the one it applies now, who were not counted as
 * born underweight and now are, and those whose birth weight is too small to
 * be read, who were counted as born underweight and now count as unknown.
 *
 * Everyone else calculates to what is already stored, so they are left alone.
 * Recalculating a person writes a new revision, which every device holding
 * that person then downloads again.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_ncda/scripts/
 *   recalculate-low-birth-weight.php [--dry_run] [--batch=50].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be recalculated, without recalculating it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Get the number of nodes to be processed at once.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// The threshold the indicator applied before issue #2035. Recorded here
// because it no longer appears anywhere else: it is what the stored data was
// calculated with, so it is what says whose data is now out of date.
$previous_threshold = 2000;

// Birth weight is resolved from the newborn exam, and from the NCDA
// questionnaires when the newborn exam has none. Both are searched, since
// either can hold the weight that produced the stored indicator.
$sources = [
  [
    'table' => 'field_data_field_weight',
    'column' => 'field_weight_value',
    // The field carries every weight measurement, so it is the bundle that
    // says which rows are birth weights.
    'bundle' => HEDLEY_ACTIVITY_WELL_CHILD_PREGNANCY_SUMMARY_CONTENT_TYPE,
  ],
  [
    'table' => 'field_data_field_birth_weight',
    'column' => 'field_birth_weight_value',
    // The field exists only on the NCDA questionnaires.
    'bundle' => NULL,
  ],
];

$affected = [];
foreach ($sources as $source) {
  $query = db_select($source['table'], 'f');
  $query->join('field_data_field_person', 'fp', 'fp.entity_id = f.entity_id');
  $query->join('node', 'n', 'n.nid = fp.field_person_target_id');
  hedley_general_apply_exclude_deleted($query, 'n');

  $bands = db_or()
    ->condition('f.' . $source['column'], HEDLEY_NCDA_MINIMAL_PLAUSIBLE_BIRTH_WEIGHT, '<')
    ->condition(
      db_and()
        ->condition('f.' . $source['column'], $previous_threshold, '>=')
        ->condition('f.' . $source['column'], HEDLEY_NCDA_LOW_BIRTH_WEIGHT_THRESHOLD, '<')
    );

  $query
    ->fields('fp', ['field_person_target_id'])
    ->condition('f.deleted', 0)
    ->condition('n.status', NODE_PUBLISHED)
    ->condition('n.type', 'person')
    ->condition($bands);

  if (!empty($source['bundle'])) {
    $query->condition('f.bundle', $source['bundle']);
  }

  $affected = array_merge($affected, $query->execute()->fetchCol());
}

$affected = array_values(array_unique($affected));
sort($affected);

$count = count($affected);
if ($count == 0) {
  drush_print('No children are affected. Nothing to recalculate.');
  return;
}

drush_print("$count children are affected.");

$total = 0;
foreach (array_chunk($affected, $batch) as $chunk) {
  $persons = node_load_multiple($chunk);
  foreach ($persons as $person) {
    if ($dry_run) {
      $stored = 'not calculated yet';
      $items = field_get_items('node', $person, 'field_ncda_data');
      if ($items) {
        $decoded = json_decode($items[0]['value'], TRUE);
        if (!isset($decoded['low_birth_weight'])) {
          $stored = 'unknown';
        }
        else {
          $stored = $decoded['low_birth_weight'] ? 'born underweight' : 'not born underweight';
        }
      }

      drush_print("  Person $person->nid currently reads: $stored");
      $total++;
      continue;
    }

    if (hedley_ncda_calculate_aggregated_data_for_person($person)) {
      $total++;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before running out of memory, after @total of @count children. Run again to continue.', [
      '@total' => $total,
      '@count' => $count,
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

if ($dry_run) {
  drush_print("Dry run. $total children would be recalculated.");
  return;
}

drush_print("Done! Recalculated NCDA data for $total of $count children.");
