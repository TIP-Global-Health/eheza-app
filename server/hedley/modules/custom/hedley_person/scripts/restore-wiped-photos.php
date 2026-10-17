<?php

/**
 * @file
 * Restores person photos that editing a person erased.
 *
 * The edit form re-sends the photo it was seeded with, and the sync encoder
 * replaced that with the ID of the file uploaded for it. A photo that came
 * down from the backend was never uploaded from this device and has no such
 * ID, so NULL went up instead and emptied the field. One ordinary edit - a
 * corrected name - was enough, and an address edit propagates to the person's
 * children, so it took their photos too.
 *
 * What the edit did not touch is the node's revisions, and emptying an image
 * field does not delete the file, so the erased photo is still there to be
 * read back. This restores the most recent revision that held a photo, to
 * persons that hold none now.
 *
 * Restoring a person writes a new revision, which every device holding that
 * person then downloads again.
 *
 * Execution: drush scr profiles/hedley/modules/custom/hedley_person/scripts/
 *   restore-wiped-photos.php [--dry_run] [--batch=50] [--nid=0].
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

// Persons whose revisions hold a photo, and whose current data holds none.
$query = db_select('field_revision_field_photo', 'r');
$query->join('node', 'n', 'n.nid = r.entity_id');
// Field tables keep the rows of deleted field instances and are keyed by
// entity type as well, so both are named rather than left to the fact that
// this field is only on nodes today.
$query->leftJoin('field_data_field_photo', 'd', 'd.entity_id = r.entity_id AND d.entity_type = r.entity_type AND d.deleted = r.deleted');
hedley_general_apply_exclude_deleted($query, 'n');

$query
  ->fields('r', ['entity_id'])
  ->condition('r.entity_type', 'node')
  ->condition('r.deleted', 0)
  ->isNotNull('r.field_photo_fid')
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
  drush_print('No person is missing a photo that a revision still holds.');
  return;
}

drush_print("$count persons lost their photo.");

$total = 0;
$missing = 0;
foreach (array_chunk($affected, $batch) as $chunk) {
  foreach ($chunk as $person_id) {
    // The newest revision that recorded a photo. The whole item is taken from
    // that one revision, so the dimensions restored are the ones of the file
    // restored alongside them.
    $item = db_select('field_revision_field_photo', 'r')
      ->fields('r', [
        'field_photo_fid',
        'field_photo_alt',
        'field_photo_title',
        'field_photo_width',
        'field_photo_height',
      ])
      ->condition('entity_id', $person_id)
      ->condition('entity_type', 'node')
      ->condition('deleted', 0)
      ->isNotNull('field_photo_fid')
      ->orderBy('revision_id', 'DESC')
      ->range(0, 1)
      ->execute()
      ->fetchAssoc();

    $file = file_load($item['field_photo_fid']);
    if (!$file || !file_exists(drupal_realpath($file->uri))) {
      // A field pointing at a file that is not there shows a broken image,
      // which is worse than the empty field it replaces.
      drush_print("  Person $person_id has a photo to restore but its file is gone. Skipped.");
      $missing++;
      continue;
    }

    if ($dry_run) {
      drush_print("  Person $person_id would be restored to {$file->uri}.");
      $total++;
      continue;
    }

    $person = node_load($person_id);
    if (!$person) {
      continue;
    }

    $person->field_photo[LANGUAGE_NONE][0] = [
      'fid' => $item['field_photo_fid'],
      'alt' => $item['field_photo_alt'],
      'title' => $item['field_photo_title'],
      'width' => $item['field_photo_width'],
      'height' => $item['field_photo_height'],
    ];
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

if ($missing) {
  drush_print("$missing of them point at a file that is gone, and were skipped.");
}

if ($dry_run) {
  drush_print("Dry run. $total persons would be restored.");
  return;
}

drush_print("Done! Restored the photo of $total of $count persons.");
