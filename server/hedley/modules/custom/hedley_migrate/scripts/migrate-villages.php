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
$source_file = $migrate_dir . '/csv/villages-import-jan-22.csv';
$handler = fopen($source_file, 'r');

$health_centers = [];

while ($row = fgetcsv($handler)) {
  $health_center_name = $row[5];
  $village_name = $row[4];

  if (empty($health_centers[$health_center_name])) {
    $health_center_id = hedley_person_resolve_content_by_name('health_center', $health_center_name);
    if (empty($health_center_id)) {
      drush_print("Could not resolve health center $health_center_name");
      drush_print("Skipping creation of $village_name village");
      continue;
    }

    $health_centers[$health_center_name] = $health_center_id;
  }

  $node = entity_create('node', [
    'type' => 'village',
    'uid' => 1,
  ]);

  $wrapper = entity_metadata_wrapper('node', $node);
  $wrapper->field_province->set($row[0]);
  $wrapper->field_district->set($row[1]);
  $wrapper->field_sector->set($row[2]);
  $wrapper->field_cell->set($row[3]);
  $wrapper->field_village->set($village_name);
  $wrapper->field_health_center->set($health_centers[$health_center_name]);
  $wrapper->save();

  drush_print("$village_name village created");
}

drush_print('------------------');
drush_print('Done!');
