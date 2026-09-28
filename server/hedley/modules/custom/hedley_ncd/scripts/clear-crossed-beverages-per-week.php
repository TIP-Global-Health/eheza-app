<?php

/**
 * @file
 * Clears the drinks-per-week values that hold a cigarettes count.
 *
 * The sync encoder sent the cigarettes count under both the beverages and the
 * cigarettes key, so on every record it wrote, field_beverages_per_week holds
 * the patient's cigarettes count. The drinks count the nurse entered never
 * left the device and cannot be recovered. What can be corrected is the value
 * standing in its place: a wrong number under the alcohol question reads as a
 * measurement, and is worse than the empty field the app itself stores for
 * every patient who does not drink.
 *
 * Records written by a client that predates the crossed encoder are kept.
 * They are told apart by the signature the crossed write leaves: both keys
 * carried one value, so a record it wrote has the two counts equal, or both
 * empty. A record holding a drinks count with no cigarettes count beside it
 * cannot have come from that encoder, and its value is real. Devices running
 * such a client are still in the field, so this matters in practice, not only
 * in principle.
 *
 * That signature cannot tell a crossed write from a patient who genuinely
 * drinks and smokes the same number per week. Read the dry run before the
 * real one.
 *
 * Clearing a record writes a new revision, which every device holding it then
 * downloads again.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_ncd/scripts/
 *   clear-crossed-beverages-per-week.php [--dry_run] [--batch=50] [--nid=0].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be cleared, without clearing it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Resume after this record, for a run that stopped on memory. A cleared
// record no longer matches, so a plain re-run resumes on its own; this is
// here for a run that has to be repeated deliberately.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed at once.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// The crossed encoder reached production with the release that carried it, in
// January 2024. Records last saved before then were written correctly, and a
// drinks count that happens to equal a cigarettes count is theirs to keep.
// The bound is on the last save, not the creation, because the last save is
// what wrote the values standing today.
$crossed_from = strtotime('2024-01-18');

$query = db_select('field_data_field_beverages_per_week', 'b');
$query->join('node', 'n', 'n.nid = b.entity_id');
// Field tables keep the rows of deleted field instances and are keyed by
// entity type as well, so both are named rather than left to the fact that
// these fields are only on nodes today.
$query->leftJoin('field_data_field_cigarettes_per_week', 'c', 'c.entity_id = b.entity_id AND c.entity_type = b.entity_type AND c.deleted = b.deleted');
hedley_general_apply_exclude_deleted($query, 'n');

$query
  ->fields('b', ['entity_id'])
  ->condition('b.entity_type', 'node')
  ->condition('b.deleted', 0)
  ->condition('n.type', 'ncd_social_history')
  ->condition('n.status', NODE_PUBLISHED)
  ->condition('n.changed', $crossed_from, '>=')
  ->isNotNull('b.field_beverages_per_week_value')
  // The crossed write left one value in both keys.
  ->where('b.field_beverages_per_week_value = c.field_cigarettes_per_week_value')
  ->distinct();

$affected = $query->execute()->fetchCol();
sort($affected);

if ($nid) {
  $affected = array_values(array_filter($affected, function ($record_id) use ($nid) {
    return $record_id > $nid;
  }));
}

$count = count($affected);
if ($count == 0) {
  drush_print('No social history record holds a cigarettes count under the alcohol question.');
  return;
}

drush_print("$count social history records hold a cigarettes count under the alcohol question.");

$total = 0;
foreach (array_chunk($affected, $batch) as $chunk) {
  $nodes = node_load_multiple($chunk);
  foreach ($nodes as $record) {
    $count_held = $record->field_beverages_per_week[LANGUAGE_NONE][0]['value'];

    if ($dry_run) {
      drush_print("  Record {$record->nid} would lose the value $count_held.");
      $total++;
      continue;
    }

    $record->field_beverages_per_week[LANGUAGE_NONE] = [];
    node_save($record);
    $total++;
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before running out of memory, after @total of @count records. Run again with --nid=@nid to continue.', [
      '@total' => $total,
      '@count' => $count,
      '@nid' => end($chunk),
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

if ($dry_run) {
  drush_print("Dry run. $total records would be cleared.");
  return;
}

drush_print("Done! Cleared the drinks count of $total of $count records.");
