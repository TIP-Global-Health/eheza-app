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
 * left as they are:
 * - Weights that would convert to more than a newborn can weigh. They are not
 *   kilograms of birth weight either, and are most likely the weight the child
 *   had on the day, entered in the wrong field. The script names them so they
 *   can be looked at.
 * - Weights between the two scales, which are neither grams nor kilograms and
 *   which nothing says how to read.
 *
 * Converting changes which children the aggregated indicator applies to, so
 * this recalculates every child it converts a weight for. That leaves it
 * independent of recalculate-low-birth-weight.php, which covers the children
 * whose weight was already in grams.
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

// Get the number of nodes to be processed at once.
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

$convert = [];
$skipped = [];
foreach ($sources as $source) {
  $query = db_select($source['table'], 'f');
  $query->join('node', 'n', 'n.nid = f.entity_id');
  hedley_general_apply_exclude_deleted($query, 'n');

  $query
    ->fields('f', ['entity_id', $source['column']])
    ->condition('f.deleted', 0)
    ->condition('f.entity_type', 'node')
    ->condition('n.status', NODE_PUBLISHED)
    ->condition('f.' . $source['column'], 0, '>')
    ->condition('f.' . $source['column'], $kilograms_ceiling, '<');

  if (!empty($source['bundle'])) {
    $query->condition('f.bundle', $source['bundle']);
  }

  foreach ($query->execute() as $row) {
    $grams = $row->{$source['column']} * 1000;
    $item = [
      'nid' => $row->entity_id,
      'field' => $source['field'],
      'from' => $row->{$source['column']},
      'to' => $grams,
    ];

    if ($grams > $grams_ceiling) {
      $skipped[] = $item;
      continue;
    }

    $convert[] = $item;
  }
}

if (empty($convert) && empty($skipped)) {
  drush_print('No birth weight is recorded in kilograms. Nothing to convert.');
  return;
}

drush_print(count($convert) . ' birth weights are recorded in kilograms.');

if (!empty($skipped)) {
  drush_print(count($skipped) . ' more would convert to more than a newborn can weigh, and are left as they are:');
  foreach ($skipped as $item) {
    drush_print("  Measurement {$item['nid']}: {$item['from']} would become {$item['to']}");
  }
}

if ($dry_run) {
  drush_print('Dry run. Nothing was converted.');
  return;
}

// Convert, remembering whose aggregated data the conversion invalidates.
$persons = [];
$converted = 0;
foreach (array_chunk($convert, $batch) as $chunk) {
  $nodes = node_load_multiple(array_column($chunk, 'nid'));
  foreach ($chunk as $item) {
    if (empty($nodes[$item['nid']])) {
      drush_print("  Measurement {$item['nid']} could not be loaded. Skipping.");
      continue;
    }

    $node = $nodes[$item['nid']];
    $node->{$item['field']}[LANGUAGE_NONE][0]['value'] = $item['to'];
    node_save($node);
    $converted++;

    $person = field_get_items('node', $node, 'field_person');
    if ($person) {
      $persons[$person[0]['target_id']] = $person[0]['target_id'];
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before running out of memory, after @converted weights. Run again to continue.', [
      '@converted' => $converted,
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

drush_print("Converted $converted birth weights. Recalculating " . count($persons) . ' children.');

$recalculated = 0;
foreach (array_chunk($persons, $batch) as $chunk) {
  foreach (node_load_multiple($chunk) as $person) {
    if (hedley_ncda_calculate_aggregated_data_for_person($person)) {
      $recalculated++;
    }
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Ran out of memory while recalculating, after @total children. The weights are converted; run recalculate-low-birth-weight.php to finish.', [
      '@total' => $recalculated,
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

drush_print("Done! Converted $converted birth weights and recalculated $recalculated children.");
