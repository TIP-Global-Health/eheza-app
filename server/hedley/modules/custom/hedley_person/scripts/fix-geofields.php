<?php

/**
 * @file
 * Sets correct values at Geo fields.
 *
 * During import process, some patients were given incorrect
 * Geo fields values. This script fixes that.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_person/scripts/fix-geofields.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

// Get the last node id.
$nid = drush_get_option('nid', 0);

// Get the number of nodes to be processed.
$batch = drush_get_option('batch', 50);

// Get allowed memory limit.
$memory_limit = drush_get_option('memory_limit', 500);

$mapping = [
  'province' => [
    'AMAJYARUGU' => 'Amajyaruguru',
    'North' => 'Amajyaruguru',
    'North Province' => 'Amajyaruguru',
    'Northern Province' => 'Amajyaruguru',
    'UBURASIRAZUBA' => 'Iburasirazuba',
    'Kigali City' => 'Umujyi wa kigali',
  ],
  'district' => [
    'NYAMATA' => 'Bugesera',
  ],
];

foreach ($mapping as $geofield => $updates) {
  $geofield_name = "field_$geofield";
  $base_query = db_select("field_data_$geofield_name", 'gf')
    ->fields('gf', ['entity_id']);
  $base_query->condition('bundle', 'person');

  foreach ($updates as $current => $correct) {
    $query = clone $base_query;
    $query->condition("{$geofield_name}_value", $current);
    $ids = $query->execute()->fetchCol();
    $total = count($ids);

    if ($total == 0) {
      drush_print("There are no $geofield values set to $current.");
      continue;
    }

    drush_print("There are $total $geofield values set to $current.");

    $chunks = array_chunk($ids, $batch);
    $precessed = 0;
    foreach ($chunks as $chunk) {
      $nodes = node_load_multiple($chunk);
      foreach ($nodes as $node) {
        $node->{$geofield_name}[LANGUAGE_NONE][0]['value'] = $correct;
        node_save($node);
      }

      if (round(memory_get_usage() / 1048576) >= $memory_limit) {
        drush_print(dt('Stopped before out of memory.'));
        return;
      }

      $precessed += count($chunk);

      drush_print("Processed: $precessed/$total.");
    }
  }
}

drush_print('Done!');
