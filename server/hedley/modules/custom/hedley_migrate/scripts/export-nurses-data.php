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
$surveys = [
  [
    'id',
    'title',
    'field_nurse',
    'field_date_measured',
    'field_resilience_survey_type',
    'field_resilience_survey_signs',
  ],
];

$nurses_ids = [
  1627763, // Joseph HITIMANA
  1627758, // Mary Rose UWIZEYIMANA
  3856395, // Ndahayo Safi
  1627761, // Esther Nyiranzigiyimana
  3128895, // Nyirabashyitsi Patricie
  1592985, // Gentille NYIRAMATUNGO
  1592988, // Seraphine MUKAMURENZI
  4196825, // Hyacinthe MUKAKALISA
  1627766, // Louise UWIMANA KABERA
  1627765, // Beathe UMULISA
  1627760, // Alexie BENIHIRWE
  1627759, // Ancille KABAGWIRE
  4196850, // Mukandashimye Demetrie
  1592987, // Jacqueline KAYITESI
  1592989, // Ulimuze UTAZIRUBANDA
  4004781, // Nyirahitimana Dative
];
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
$surveys_ids = hedley_migrate_resolve_for_export('resilience_survey', 'field_nurse', [$nurses_ids]);
foreach ($surveys_ids as $survey_id) {
  $wrapper = entity_metadata_wrapper('node', $survey_id);

  $surveys[$survey_id] = [
    $survey_id,
    str_replace(',', ' ', $wrapper->label()),
    $wrapper->field_nurse->getIdentifier(),
    hedley_migrate_export_date_field($wrapper->field_date_measured->value()),
    $wrapper->field_resilience_survey_type->value(),
    implode('|', $wrapper->field_resilience_survey_signs->value()),
  ];
}

$mapping = [
  'nurse' => array_values($nurses),
  'resilience_survey' => array_values($surveys),
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
