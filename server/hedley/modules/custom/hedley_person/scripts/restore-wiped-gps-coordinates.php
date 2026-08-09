<?php

/**
 * @file
 * Restores GPS coordinates that editing a person erased.
 *
 * The person edit form has no coordinate inputs, so it reported Nothing for
 * them, and the resulting patch wrote that over whatever the device recorded
 * at registration. One ordinary edit - a corrected name - was enough. The
 * fields have no other writer and no screen that can restore them.
 *
 * What the edit did not touch is the node's revisions, so the erased values
 * are still there to be read back. This restores the most recent revision
 * that held both coordinates, to persons that hold neither now.
 *
 * Restoring a person writes a new revision, which every device holding that
 * person then downloads again.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_person/scripts/
 *   restore-wiped-gps-coordinates.php [--dry_run] [--batch=50] [--nid=0].
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Report what would be restored, without restoring it.
$dry_run = (bool) drush_get_option('dry_run', FALSE);

// Resume after this person, for a run that stopped on memory. A restored
// person no longer matches, so a plain re-run resumes on its own; this is
// here for a run that has to be repeated deliberately.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed at once.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 800);

// Persons whose revisions hold a latitude, and whose current data holds none.
// Longitude is not named here: it is read per person below, since a person
// missing only one of the two is a case to report rather than to guess at.
$query = db_select('field_revision_field_latitude', 'r');
$query->join('node', 'n', 'n.nid = r.entity_id');
// Field tables keep the rows of deleted field instances and are keyed by
// entity type as well, so both are named rather than left to the fact that
// this field is only on nodes today.
$query->leftJoin('field_data_field_latitude', 'd', 'd.entity_id = r.entity_id AND d.entity_type = r.entity_type AND d.deleted = r.deleted');
hedley_general_apply_exclude_deleted($query, 'n');

$query
  ->fields('r', ['entity_id'])
  ->condition('r.entity_type', 'node')
  ->condition('r.deleted', 0)
  ->isNotNull('r.field_latitude_value')
  ->condition('n.type', 'person')
  ->condition('n.status', NODE_PUBLISHED)
  ->isNull('d.entity_id')
  ->distinct();

$affected = $query->execute()->fetchCol();
sort($affected);

if ($nid) {
  $affected = array_values(array_filter($affected, function ($person_id) use ($nid) {
    return $person_id > $nid;
  }));
}

$count = count($affected);
if ($count == 0) {
  drush_print('No person is missing coordinates that a revision still holds.');
  return;
}

drush_print("$count persons lost their coordinates.");

$total = 0;
$incomplete = 0;
foreach (array_chunk($affected, $batch) as $chunk) {
  foreach ($chunk as $person_id) {
    // The newest revision that recorded a latitude. Longitude is taken from
    // that same revision, so the pair that is restored is the pair that was
    // read together from one device at one moment.
    $revision = db_select('field_revision_field_latitude', 'r')
      ->fields('r', ['revision_id', 'field_latitude_value'])
      ->condition('entity_id', $person_id)
      ->condition('entity_type', 'node')
      ->condition('deleted', 0)
      ->isNotNull('field_latitude_value')
      ->orderBy('revision_id', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchAssoc();

    $longitude = db_select('field_revision_field_longitude', 'r')
      ->fields('r', ['field_longitude_value'])
      ->condition('entity_id', $person_id)
      ->condition('entity_type', 'node')
      ->condition('deleted', 0)
      ->condition('revision_id', $revision['revision_id'])
      ->execute()
      ->fetchField();

    if ($longitude === FALSE || !isset($longitude)) {
      // Half a coordinate places nobody, so it is left alone and reported.
      drush_print("  Person $person_id has a latitude to restore but no longitude alongside it. Skipped.");
      $incomplete++;
      continue;
    }

    if ($dry_run) {
      drush_print("  Person $person_id would be restored to {$revision['field_latitude_value']}, $longitude.");
      $total++;
      continue;
    }

    $person = node_load($person_id);
    if (!$person) {
      continue;
    }

    $person->field_latitude[LANGUAGE_NONE][0]['value'] = $revision['field_latitude_value'];
    $person->field_longitude[LANGUAGE_NONE][0]['value'] = $longitude;
    node_save($person);
    $total++;
  }

  $memory = round(memory_get_usage() / 1048576);
  if ($memory >= $memory_limit) {
    drush_print(dt('Stopped before running out of memory, after @total of @count persons. Run again with --nid=@nid to continue.', [
      '@total' => $total,
      '@count' => $count,
      '@nid' => end($chunk),
    ]));
    return;
  }

  // Free up memory.
  drupal_static_reset();
}

if ($incomplete) {
  drush_print("$incomplete of them had no longitude to go with the latitude, and were skipped.");
}

if ($dry_run) {
  drush_print("Dry run. $total persons would be restored.");
  return;
}

drush_print("Done! Restored coordinates for $total of $count persons.");
