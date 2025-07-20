<?php

/**
 * @file
 * Associate children with stunting to medical & obstetric history at ANC.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-medical-and-obstetric-history-for-outcome.php.
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

$query = db_select('node', 'n');
$query->join('field_data_field_last_menstrual_period', 'lmp', 'n.nid = lmp.entity_id');
$query->join('field_data_field_date_measured', 'm', 'n.nid = m.entity_id');

$query->fields('n', array('nid'))
  ->condition('n.type', 'last_menstrual_period')
  ->where('DATE_ADD(lmp.field_last_menstrual_period_value, INTERVAL 4 MONTH) > m.field_date_measured_value');

$results = $query->execute()->fetchAllAssoc('nid');
$lmp_ids = array_keys($results);

$chunks = array_chunk($lmp_ids, $batch);
$processed = 0;
$data = [];
foreach ($chunks as $ids) {
  // Free up memory.
  drupal_static_reset();

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $encounter_id = $node->field_prenatal_encounter[LANGUAGE_NONE][0]['target_id'];
    $encounter = node_load($encounter_id);
    $pregnancy_id = $encounter->field_individual_participant[LANGUAGE_NONE][0]['target_id'];
    $pregnancy = node_load($pregnancy_id);
    $outcome = $pregnancy->field_outcome[LANGUAGE_NONE][0]['value'];
    if (empty($outcome)) {
      continue;
    }

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'prenatal_encounter')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_individual_participant', 'target_id', $pregnancy_id)
      ->propertyOrderBy('nid')
      ->execute();

    if (empty($result['node'])) {
      continue;
    }

    $data[$pregnancy_id] = [
      'mother_id' => $pregnancy->field_person[LANGUAGE_NONE][0]['target_id'],
      'outcome' => get_field_sign_label('field_outcome', $outcome)
    ];

    $encounters = array_keys($result['node']);

    // Add medical history data.
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'medical_history')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_prenatal_encounter', 'target_id', $encounters, 'IN')
      ->range(0, 1)
      ->execute();

    if (!empty($result['node'])) {
      $medical_history_signs = [];
      $medical_history = node_load(key($result['node']));
      $fields = [
        'field_medical_history',
        'field_physical_condition_history',
        'field_infectious_disease_history',
        'field_mental_health_issues',
      ];
      foreach ($fields as $field) {
        $field_values = $medical_history->{$field}[LANGUAGE_NONE];
        if (empty($field_values)) {
          continue;
        }
        foreach ($field_values as $item) {
          if (in_array($item['value'], ['none', 'migrate'])) {
            continue;
          }
          $medical_history_signs[] = get_field_sign_label($field, $item['value']);
        }
      }
      $data[$pregnancy_id]['medical'] = implode(' & ', $medical_history_signs);
    }

    // Add obstetric history data.
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'obstetric_history_step2')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_prenatal_encounter', 'target_id', $encounters, 'IN')
      ->range(0, 1)
      ->execute();

    if (!empty($result['node'])) {
      $obstetric_history_signs = [];
      $obstetric_history = node_load(key($result['node']));
      $fields = [
        'field_obstetric_history',
        'field_previous_delivery_period',
        'field_obstetric_history_step2',
        'field_previous_delivery',
      ];
      foreach ($fields as $field) {
        $field_values = $obstetric_history->{$field}[LANGUAGE_NONE];
        if (empty($field_values)) {
          continue;
        }
        foreach ($field_values as $item) {
          if (in_array($item['value'], ['none', 'migrate', 'neither'])) {
            continue;
          }
          $obstetric_history_signs[] = get_field_sign_label($field, $item['value']);
        }
      }
      $data[$pregnancy_id]['obstetric'] = implode(' & ', $obstetric_history_signs);
    }
  }
  $processed++;
  drush_print("Processed $processed chunks.");
}

// Print results in CSV format.
drush_print();
drush_print("Mother ID,Medical,Obstetric,Outcome");
foreach ($data as $item) {
  $mother_id = $item['mother_id'];
  $medical = $item['medical'];
  $obstetric = $item['obstetric'];
  $outcome = $item['outcome'];
  drush_print("$mother_id,$medical,$obstetric,$outcome");
}

/**
 * Resolves the label of the value for given field.
 */
function get_field_sign_label($field, $value) {
  $field_info = field_info_field($field);
  $allowed_values = $field_info['settings']['allowed_values'];

  return isset($allowed_values[$value]) ? $allowed_values[$value] : $value;
}
