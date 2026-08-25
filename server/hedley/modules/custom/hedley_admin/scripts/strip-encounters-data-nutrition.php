<?php

/**
 * @file
 * Records all data points of a Prenatal encounter.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/strip-encounters-data-nutrition.php.
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
$type = 'nutrition_encounter';
$data['measurements_types'] = hedley_general_get_measurement_types([$type]);
$skipped_fields = [
  'field_person',
  'field_date_measured',
  'field_nurse',
  'field_nutrition_encounter',
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
    if ($field_info['type'] == 'image') {
      continue;
    }

    $data[$measurement_type][$field_name] = [
      'type' => $field_info['type'],
      'multivalue' => $field_info['cardinality'] == -1,
    ];
  }
}

$labels = [
  'Encounter ID',
  'Encounter Type',
  'Encounter Date',
  'Patient ID',
  'Patient Birth Date',
];
foreach ($data['measurements_types'] as $measurement_type) {
  $measurement_name = format_name($measurement_type);
  if (empty($data[$measurement_type])) {
    continue;
  }
  foreach ($data[$measurement_type] as $field_name => $field_info) {
    $name = format_name($field_name);
    $labels[] = "$measurement_name: $name";
  }
}

$labels = implode(",", $labels);
drush_print($labels);

$base_query = new EntityFieldQuery();
$base_query
  ->entityCondition('entity_type', 'node')
  ->propertyCondition('type', $type)
  ->propertyOrderBy('nid')
  ->addTag('exclude_deleted');

$count_query = clone $base_query;
$count_query->propertyCondition('nid', $nid, '>');
$count = $count_query->count()->execute();

if ($count == 0) {
  drush_print("There are no nodes of type $type in DB.");
  exit;
}

while (TRUE) {
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
  $nid = end($ids);
  $encounters = node_load_multiple($ids);
  foreach ($encounters as $encounter) {
    $participant_id = $encounter->field_individual_participant[LANGUAGE_NONE][0]['target_id'];
    $participant = node_load($participant_id);
    $patient_id = $participant->field_person[LANGUAGE_NONE][0]['target_id'];
    $patient = node_load($patient_id);
    $birth_date = explode(' ', $patient->field_birth_date[LANGUAGE_NONE][0]['value'])[0];
    $encounter_type = $encounter->field_nutrition_encounter_type[LANGUAGE_NONE][0]['value'];
    $encounter_type = empty($encounter_type) ? 'Nurse' : hedley_general_get_field_sign_label('field_nutrition_encounter_type', $encounter_type);
    $encounter_date = explode(' ', $encounter->field_scheduled_date[LANGUAGE_NONE][0]['value'])[0];

    $out = [
      $encounter->nid,
      $encounter_type,
      $encounter_date,
      $patient_id,
      $birth_date,
    ];

    // Get all measurements that belong to encounter.
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', $data['measurements_types'], 'IN')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_nutrition_encounter', 'target_id', $encounter->nid)
      ->propertyOrderBy('nid')
      ->addTag('exclude_deleted')
      ->execute();

    if (empty($result['node'])) {
      // Skip encounter without any measurements.
      continue;
    }

    // As there's a possibility of multiple measurements of same type
    // (due to glitches), make sure to take only the most recent ones.
    $measurements = [];
    $ids = array_keys($result['node']);
    $nodes = node_load_multiple($ids);
    foreach ($nodes as $node) {
      $measurements[$node->type] = $node;
    }
    // Explicitly unset large variables after use, for memory optimization.
    unset($nodes);

    // Collect values.
    foreach ($data['measurements_types'] as $measurement_type) {
      $measurement = $measurements[$measurement_type];
      if (empty($data[$measurement_type])) {
        continue;
      }

      foreach ($data[$measurement_type] as $field_name => $field_info) {
        if (empty($measurement)) {
          $out[] = '';
          continue;
        }

        switch ($field_info['type']) {
          case 'datetime':
            $out[] = explode(' ', $measurement->{$field_name}[LANGUAGE_NONE][0]['value'])[0];
            break;

          case 'entityreference':
            $out[] = $measurement->{$field_name}[LANGUAGE_NONE][0]['target_id'];
            break;

          case 'list_text':
            if ($field_info['multivalue']) {
              $values = [];
              $field_values = $measurement->{$field_name}[LANGUAGE_NONE];
              if (empty($field_values)) {
                $out[] = '';
              }
              else {
                foreach ($field_values as $item) {
                  $values[] = hedley_general_get_field_sign_label($field_name, $item['value']);
                }
                $out[] = implode(' & ', $values);
              }
            }
            else {
              $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
              $out[] = hedley_general_get_field_sign_label($field_name, $value);
            }
            break;

          case 'list_boolean':
            $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
            $out[] = $value ? 'true' : 'false';
            break;

          default:
            $out[] = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
        }
      }
    }
    // Explicitly unset large variables after use, for memory optimization.
    unset($measurements);

    // Print values.
    $out = implode(',', $out);
    drush_print($out);

    // Free up memory.
    drupal_static_reset();
  }
  // Explicitly unset large variables after use, for memory optimization.
  unset($encounters);
}

/**
 * Formats given value from snake case to readable.
 *
 * @param string $value
 *   The value.
 *
 * @return string
 *   Formated readable value.
 */
function format_name($value) {
  // Remove common prefixes.
  $clean_name = preg_replace('/^(nutrition_|field_)/', '', $value);

  // Replace underscores with spaces.
  $clean_name = str_replace('_', ' ', $clean_name);
  $clean_name = str_replace('\'', '`', $clean_name);

  // Capitalize first letter of each word.
  return ucwords($clean_name);
}
