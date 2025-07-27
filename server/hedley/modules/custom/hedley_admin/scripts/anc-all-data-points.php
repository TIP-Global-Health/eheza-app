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

$labels = [];
foreach ($data['measurements_types'] as $measurement_type) {
  foreach ($data[$measurement_type] as $field_name => $field_info) {
    $labels[] = "$measurement_type:$field_name";
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
}



drush_print("Done!");
