<?php

/**
 * @file
 * Creates villages.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/migrate-villages.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

drush_print('Starting migrating villages!');

$migrate_dir = drupal_get_path('module', 'hedley_migrate');
$source_file = $migrate_dir . '/csv/chw_deploy/village.csv';
$handler = fopen($source_file, 'r');
while ($row = fgetcsv($handler)) {
  $node = entity_create('node', [
    'type' => 'village',
    'uid' => 1,
  ]);

  $wrapper = entity_metadata_wrapper('node', $node);
  $village_name = $wrapper->label();
  $wrapper->field_province->set($row[1]);
  $district = $wrapper->field_district->set($row[2]);
  $wrapper->field_sector->set($row[3]);
  $wrapper->field_cell->set($row[4]);
  $wrapper->field_village->set($row[5]);
  $wrapper->field_health_center->set($row[6]);
  $wrapper->save();
}

drush_print('------------------');
drush_print('Done!');
