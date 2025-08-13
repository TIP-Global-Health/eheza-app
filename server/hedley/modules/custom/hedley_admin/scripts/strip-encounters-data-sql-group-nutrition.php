<?php

/**
 * @file
 * Records all data points of a Prenatal session.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/strip-sessions-data-sql-group-nutrition.php.
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
$type = 'session';
$data['child']['measurements_types'] = [
  'child_fbf',
  'follow_up',
  'group_health_education',
  'group_ncda',
  'group_send_to_hc',
  'height',
  'muac',
  'nutrition',
  'weight',
];
$data['mother']['measurements_types'] = [
  'family_planning',
  'lactation',
  'mother_fbf',
];

$skipped_fields = [
  'field_person',
  'field_date_measured',
  'field_nurse',
  'field_session',
  'field_uuid',
  'field_shards',
  // Deprecated fields bellow.
  'field_child',
  'field_mother',
];

foreach ($data['child']['measurements_types'] as $measurement_type) {
  $fields = field_info_instances('node', $measurement_type);
  foreach ($fields as $field_name => $field_instance) {
    if (in_array($field_name, $skipped_fields)) {
      continue;
    }

    $field_info = field_info_field($field_name);
    $data['child'][$measurement_type][$field_name] = [
      'type' => $field_info['type'],
      'multivalue' => $field_info['cardinality'] == -1,
    ];
  }
}
foreach ($data['mother']['measurements_types'] as $measurement_type) {
  $fields = field_info_instances('node', $measurement_type);
  foreach ($fields as $field_name => $field_instance) {
    if (in_array($field_name, $skipped_fields)) {
      continue;
    }

    $field_info = field_info_field($field_name);
    $data['mother'][$measurement_type][$field_name] = [
      'type' => $field_info['type'],
      'multivalue' => $field_info['cardinality'] == -1,
    ];
  }
}

$labels['child'] = $labels['mother'] = [
  'Session ID' => 'INTEGER NOT NULL',
  'Patient ID' => 'INTEGER NOT NULL',
  'Session Date' => 'TIMESTAMP NOT NULL',
];

$measurements_types_by_patient = [
  'child' => $data['child']['measurements_types'],
  'mother' => $data['mother']['measurements_types'],
];

foreach ($measurements_types_by_patient as $patient_type => $measurements_types) {
  foreach ($measurements_types as $measurement_type) {
    $measurement_name = format_name($measurement_type);
    if (empty($data[$patient_type][$measurement_type])) {
      continue;
    }
    foreach ($data[$patient_type][$measurement_type] as $field_name => $field_info) {
      $name = format_name($field_name);
      switch ($field_info['type']) {
        case 'datetime':
          $value = 'TIMESTAMP';
          break;

        case 'entityreference':
        case 'number_integer':
          $value = 'INTEGER';
          break;

        case 'list_boolean':
          $value = 'BOOLEAN';
          break;

        case 'number_float':
          $value = 'FLOAT';
          break;

        default:
          $value = 'TEXT';
          break;
      }

      $labels[$patient_type]["${measurement_name}__$name"] = $value;
    }
  }
}

$patient_types = array_keys($measurements_types_by_patient);
$table_prefix = 'public.group_nutrition_';

$columns = [];
foreach ($patient_types as $patient_type) {
  $table = $table_prefix . $patient_type;
  drush_print("DROP TABLE $table;");
  drush_print("CREATE TABLE $table (");
  foreach ($labels[$patient_type] as $label => $column_type) {
    $column_name = strtolower(str_replace(' ', '_', $label));
    $columns[$patient_type][] = $column_name;
    $columns_definition = "  $column_name $column_type,";
    drush_print($columns_definition);
  }
  drush_print('  PRIMARY KEY (session_id, patient_id)');

  drush_print(");");
}

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
  $sessions = node_load_multiple($ids);
  foreach ($sessions as $session) {
    $session_date = $session->field_scheduled_date[LANGUAGE_NONE][0]['value'];
    foreach ($measurements_types_by_patient as $patient_type => $measurements_types) {
      // Get all measurements that belong to session.
      $query = new EntityFieldQuery();
      $result = $query
        ->entityCondition('entity_type', 'node')
        ->entityCondition('bundle', $data[$patient_type]['measurements_types'], 'IN')
        ->propertyCondition('status', NODE_PUBLISHED)
        ->fieldCondition('field_session', 'target_id', $session->nid)
        ->propertyOrderBy('nid')
        ->addTag('exclude_deleted')
        ->execute();

      if (empty($result['node'])) {
        // Skip session without any measurements.
        continue;
      }

      // As there's a possibility of multiple measurements of same type
      // (due to glitches), make sure to take only the most recent ones.
      $measurements_by_patient = [];
      $ids = array_keys($result['node']);
      $nodes = node_load_multiple($ids);
      foreach ($nodes as $node) {
        $patient_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
        $measurements_by_patient[$patient_id][$node->type] = $node;
      }
      // Explicitly unset large variables after use, for memory optimization.
      unset($nodes);

      // Collect values.
      foreach ($measurements_by_patient as $patient_id => $measurements) {
        $values = [
          $session->nid,
          $patient_id,
          '\'' . $session_date . '\'',
        ];

        foreach ($data[$patient_type]['measurements_types'] as $measurement_type) {
          $measurement = $measurements[$measurement_type];
          if (empty($data[$patient_type][$measurement_type])) {
            continue;
          }

          foreach ($data[$patient_type][$measurement_type] as $field_name => $field_info) {
            if (empty($measurement)) {
              $values[] = 'NULL';
              continue;
            }

            switch ($field_info['type']) {
              case 'entityreference':
                $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['target_id'];
                $values[] = !empty($value) ? $value : 'NULL';
                break;

              case 'list_text':
                if ($field_info['multivalue']) {
                  $field_values = $measurement->{$field_name}[LANGUAGE_NONE];
                  if (empty($field_values)) {
                    $values[] = 'NULL';
                  }
                  else {
                    $set_values = [];
                    foreach ($field_values as $item) {
                      $set_values[] = hedley_general_get_field_sign_label($field_name, $item['value']);
                    }
                    $values[] = '\'' . implode('&', $set_values) . '\'';
                  }
                }
                else {
                  $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
                  $values[] = '\'' . hedley_general_get_field_sign_label($field_name, $value) . '\'';
                }
                break;

              case 'list_boolean':
                $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
                $values[] = $value ? 'true' : 'false';
                break;

              case 'number_integer':
              case 'number_float':
                $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
                $values[] = !empty($value) ? $value : 'NULL';
                break;

              default:
                $value = $measurement->{$field_name}[LANGUAGE_NONE][0]['value'];
                $values[] = !empty($value) ? '\'' . $value . '\'' : 'NULL';
            }
          }
        }

        // Explicitly unset large variables after use, for memory optimization.
        unset($measurements);

        // Print values.
        $columns_string = implode(',', $columns[$patient_type]);
        $values_string = implode(',', $values);
        $table = $table_prefix . $patient_type;
        drush_print("INSERT INTO $table ($columns_string) VALUES ($values_string);");
      }

      // Explicitly unset large variables after use, for memory optimization.
      unset($measurements_by_patient);

      // Free up memory.
      drupal_static_reset();
    }
  }
  // Explicitly unset large variables after use, for memory optimization.
  unset($sessions);
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
