<?php

/**
 * @file
 * Populate the type for all clinics.
 *
 * Drush scr
 * profiles/hedley/modules/custom/hedley_migrate/scripts/export-nurses-data.php.
 */

if (!drupal_is_cli()) {
  // Prevent execution from browser.
  return;
}

drush_print('Starting export!');


$nurses = [
  [
    'id',
    'title',
    'field_role',
    'field_pin_code',
    'created',
    // New.
    'field_resilience_program',
    'field_resilience_start_date',
    'field_resilience_role',
    'field_birth_date',
    'field_gender',
    'field_education_level',
    'field_ubudehe',
    'field_marital_status',
    'field_next_reminder',
  //  'field_resilience_messages',
  ],
];

$nurses_ids = [190];
foreach ($nurses_ids as $nurse_id) {
  $wrapper = entity_metadata_wrapper('node', $nurse_id);

  $nurses[$nurse_id] = [
    $nurse_id,
    str_replace(',', ' ', $wrapper->label()),
    implode('|', $wrapper->field_role->value()),
    $wrapper->field_pin_code->value(),
    $wrapper->created->raw(),

    $wrapper->field_resilience_program->value(),
    hedley_migrate_export_date_field($wrapper->field_resilience_start_date->value()),
    $wrapper->field_resilience_role->value(),
    hedley_migrate_export_date_field($wrapper->field_birth_date->value()),

    $wrapper->field_gender->value(),
    $wrapper->field_education_level->value(),
    $wrapper->field_ubudehe->value(),
    $wrapper->field_marital_status->value(),
    $wrapper->field_next_reminder->raw(),
 //   $wrapper->field_resilience_messages->value(),
  ];
}

$mapping = [
  'nurse' => array_values($nurses),
];


foreach ($mapping as $name => $rows) {
  $content = [];
  foreach ($rows as $row) {
    $content[] = implode(',', $row);
  }

  $path = drupal_get_path('module', 'hedley_migrate') . '/csv';
  $fp = fopen("$path/$name.csv", 'w');
  fwrite($fp, implode(PHP_EOL, $content));

  fclose($fp);
}

drush_print('Done!');
