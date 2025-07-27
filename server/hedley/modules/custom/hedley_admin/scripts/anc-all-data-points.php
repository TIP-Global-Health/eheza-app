<?php

/**
 * @file
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-all-data-points.php.
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
$memory_limit = drush_get_option('memory_limit', 800);

$data = [];
$type = 'prenatal_encounter';
$data['measurements_types'] = hedley_general_get_measurement_types([$type]);
$skipped_fields = [
  'field_person',
  'field_date_measured',
  'field_nurse',
  'field_prenatal_encounter',
  'field_uuid',
  'field_shards',
];
foreach ($data['measurements_types'] as $measurement_type) {
  $fields = field_info_instances('node', $measurement_type);
  foreach ($fields as $field_name => $field_instance) {
    if (in_array($field_name, $skipped_fields)) {
      continue;
    }

    $field_info = field_info_field($field_name);
    $data[$measurement_type][$field_name] = [
      'type' => $field_info['type'],
      'multivalue' => $field_instance['cardinality'] == 1,
    ];
  }
}

$labels = ['Pregnancy ID', 'Patient ID', 'Patient Birth Date'];
foreach ($data['measurements_types'] as $measurement_type) {
  $measurement_name = format_name($measurement_type);
  foreach ($data[$measurement_type] as $field_name => $field_info) {
    $name = format_name($field_name);
    $labels[] = "$measurement_name: $name";
  }
}

$labels = implode(",", $labels);
drush_print($labels);
return;

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyOrderBy('nid');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

drush_print("$count nodes of type $type located.");

$processed = 0;
while (TRUE) {
  // Free up memory.
  drupal_static_reset();

  $query = clone $base_query;
  if ($nid) {
    $query->propertyCondition('nid', $nid, '>');
  }

  $result = $query
    ->range(0, $batch)
    ->execute();

  if (empty($result['node'])) {
    // No more items left.
    break;
  }

  $ids = array_keys($result['node']);
  $encounters = node_load_multiple($ids);
  foreach ($encounters as $encounter) {
    $pregnancy_id = $encounter->field_individual_participant[LANGUAGE_NONE][0]['target_id'];
    $pregnancy = node_load($pregnancy_id);
    $patient_id = $pregnancy->field_person[LANGUAGE_NONE][0]['target_id'];
    $patient = node_load($patient_id);
    $birth_date = explode(' ', $patient->field_birth_date[LANGUAGE_NONE][0]['value'])[0];
  }
}



drush_print("Done!");


function format_name($name) {
  // Remove common prefixes
  $clean_name = preg_replace('/^(prenatal_|field_)/', '', $name);

  // Replace underscores with spaces
  $clean_name = str_replace('_', ' ', $clean_name);

  // Capitalize first letter of each word
  return ucwords($clean_name);
}