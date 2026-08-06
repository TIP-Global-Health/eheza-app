<?php

/**
 * @file
 * Converts birth weights that were recorded in kilograms into grams.
 *
 * The birth weight is asked for and stored in grams, but a share of the
 * records hold the weight in kilograms - a 3.4 where 3400 was meant. They read
 * as far below any threshold, so the child counts as born underweight on their
 * scorecard and in the aggregated one, while the weight itself is nonsense.
 *
 * Only weights that can be read as kilograms are converted. Two groups are
 * left as they are, and the script reports both:
 * - Weights that would convert to more than a newborn can weigh. They are not
 *   kilograms of birth weight either, and are most likely the weight the child
 *   had on the day, entered in the wrong field. Their measurements are named
 *   so they can be looked at.
 * - Weights between the two scales, which are neither grams nor kilograms and
 *   which nothing says how to read.
 *
 * Converting changes which children the aggregated indicator applies to, so
 * this recalculates each child once all of their weights are converted. That
 * leaves it independent of recalculate-low-birth-weight.php, which covers the
 * children whose weight was already in grams, and it means anything the script
 * has finished is finished: a converted weight no longer matches what the
 * script looks for, so a run that stops early can simply be run again.
 *
 * Note that saving a measurement queues the ordinary recalculation for its
 * child as well, so a run leaves one queued job per child behind, repeating
 * work this script has already done. They are harmless, but they occupy the
 * queue for a while - the run says how many.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_ncda/scripts/
 *   convert-kilogram-birth-weights.php [--dry_run] [--batch=50].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be converted, without converting it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Get the number of children to be processed at once.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// A weight below this can only be kilograms: it is far under what the lightest
// newborn on record weighs in grams.
$kilograms_ceiling = 10;

// The heaviest a newborn can be, in grams. Mirrors
// getInputConstraintsBirthWeight.maxVal on the client, which refuses anything
// above it on entry.
$grams_ceiling = 7000;

// Birth weight is recorded at the newborn exam, and at the NCDA questionnaire
// for children who did not have one.
$sources = [
  [
    'field' => 'field_weight',
    'table' => 'field_data_field_weight',
    'column' => 'field_weight_value',
    // The field carries every weight measurement, so it is the bundle that
    // says which rows are birth weights.
    'bundle' => HEDLEY_ACTIVITY_WELL_CHILD_PREGNANCY_SUMMARY_CONTENT_TYPE,
  ],
  [
    'field' => 'field_birth_weight',
    'table' => 'field_data_field_birth_weight',
    'column' => 'field_birth_weight_value',
    // The field exists only on the NCDA questionnaires.
    'bundle' => NULL,
  ],
];

// Convertible weights, grouped by the child they belong to: a child with more
// than one of them has to have all of them converted before being
// recalculated, or the recalculation reads whichever the module resolves
// first, which may be one this run has not reached yet.
$convert = [];
$orphans = [];
$too_heavy = [];
$unreadable = [];
foreach ($sources as $source) {
  $query = db_select($source['table'], 'f');
  $query->join('node', 'n', 'n.nid = f.entity_id');
  $query->leftJoin('field_data_field_person', 'fp', 'fp.entity_id = f.entity_id AND fp.entity_type = f.entity_type AND fp.deleted = 0');
  hedley_general_apply_exclude_deleted($query, 'n');

  $query
    ->fields('f', ['entity_id', $source['column']])
    ->fields('fp', ['field_person_target_id'])
    ->condition('f.deleted', 0)
    ->condition('f.entity_type', 'node')
    ->condition('n.status', NODE_PUBLISHED)
    ->condition('f.' . $source['column'], 0, '>')
    ->condition('f.' . $source['column'], HEDLEY_NCDA_MINIMAL_PLAUSIBLE_BIRTH_WEIGHT, '<');

  if (!empty($source['bundle'])) {
    $query->condition('f.bundle', $source['bundle']);
  }

  foreach ($query->execute() as $row) {
    $value = $row->{$source['column']};
    $item = [
      'nid' => $row->entity_id,
      'field' => $source['field'],
      'from' => $value,
      'to' => $value * 1000,
    ];

    if ($value >= $kilograms_ceiling) {
      // Between the two scales. Neither reading makes it a birth weight.
      $unreadable[] = $item;
      continue;
    }

    if ($item['to'] > $grams_ceiling) {
      $too_heavy[] = $item;
      continue;
    }

    if (empty($row->field_person_target_id)) {
      $orphans[] = $item;
      continue;
    }

    $convert[$row->field_person_target_id][] = $item;
  }
}

$weights = array_sum(array_map('count', $convert)) + count($orphans);
if ($weights == 0 && empty($too_heavy) && empty($unreadable)) {
  drush_print('No birth weight is recorded in kilograms. Nothing to convert.');
  return;
}

drush_print("$weights birth weights are recorded in kilograms, belonging to " . count($convert) . ' children.');

if (!empty($orphans)) {
  drush_print(count($orphans) . ' of them belong to no child, and are converted without a recalculation.');
}

if (!empty($too_heavy)) {
  drush_print(count($too_heavy) . ' weights would convert to more than a newborn can weigh, and are left as they are:');
  foreach ($too_heavy as $item) {
    drush_print("  Measurement {$item['nid']}: {$item['from']} would become {$item['to']}");
  }
}

if (!empty($unreadable)) {
  drush_print(count($unreadable) . ' weights are between the two scales and cannot be read either way. They are left as they are, and the children they belong to read as unknown:');
  foreach ($unreadable as $item) {
    drush_print("  Measurement {$item['nid']}: {$item['from']}");
  }
}

if ($dry_run) {
  drush_print('Dry run. Nothing was converted.');
  return;
}

/**
 * Writes a birth weight in grams over the one recorded in kilograms.
 *
 * @param array $item
 *   The measurement, its field, and the value to write.
 *
 * @return bool
 *   TRUE if the weight was converted.
 */
function _hedley_ncda_convert_birth_weight(array $item) {
  $node = node_load($item['nid']);
  if (!$node) {
    drush_print("  Measurement {$item['nid']} could not be loaded. Skipping.");
    return FALSE;
  }

  $node->{$item['field']}[LANGUAGE_NONE][0]['value'] = $item['to'];
  node_save($node);

  return TRUE;
}

$converted = 0;
$recalculated = 0;
foreach ($orphans as $item) {
  $converted += (int) _hedley_ncda_convert_birth_weight($item);
}

foreach (array_chunk($convert, $batch, TRUE) as $chunk) {
  foreach ($chunk as $person_id => $items) {
    foreach ($items as $item) {
      $converted += (int) _hedley_ncda_convert_birth_weight($item);
    }

    $child = node_load($person_id);
    if (!$child) {
      drush_print("  Person {$person_id} does not load. Their weights are converted, but nothing was recalculated.");
      continue;
    }

    if (hedley_ncda_calculate_aggregated_data_for_person($child)) {
      $recalculated++;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before running out of memory, after @converted weights. Every child converted so far is recalculated, so run again to continue.', [
      '@converted' => $converted,
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

drush_print("Done! Converted $converted birth weights and recalculated $recalculated children.");
drush_print("Saving the measurements also queued a recalculation for each of those children. Those jobs repeat what this run has done, and will occupy the queue until they are through.");
