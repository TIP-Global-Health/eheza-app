<?php

/**
 * @file
 * Rotates photos that were uploaded by old Chrome versions.
 *
 * Where the photo is a portrait, but got width 800 and height 600.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-medical-and-obstetric-history-for-stunting.php.
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


$query = db_select('field_data_field_newborn', 'newborns')
  ->fields('newborns', ['entity_id', 'field_newborn_target_id']);

$newborns = $query
  ->execute()
  ->fetchAll();

if (empty($newborns)) {
  // No more items left.
  drush_print("There are no newborns in DB.");
  return;
}

// Create mapping between newborn ID and its pregnancy.
$newborns_mapping = [];
foreach ($newborns as $newborn) {
  $newborns_mapping[$newborn->field_newborn_target_id] = $newborn->entity_id;
}

$newborns_ids = array_keys($newborns_mapping);
// Now we want all height (all types) measurements that contain
// z-score that indicates stunting.
$newborns_heights = db_select('field_data_field_person', 'persons')
  ->fields('persons', ['entity_id'])
  ->condition('bundle', HEDLEY_ACTIVITY_HEIGHT_BUNDLES, 'IN')
  ->condition('field_person_target_id', $newborns_ids, 'IN')
  ->execute()
  ->fetchAllAssoc('entity_id');
$newborns_heights_ids = array_keys($newborns_heights);
if (empty($newborns_heights_ids)) {
  drush_print("There are no height measurements for newborns in DB.");
  return;
}

$stunting_heights = db_select('field_data_field_zscore_age', 'zscores')
  ->fields('zscores', ['entity_id'])
  ->condition('entity_id', $newborns_heights_ids, 'IN')
  ->condition('field_zscore_age_value', -2, '<')
  ->execute()
  ->fetchAllAssoc('entity_id');
$stunting_heights_ids = array_keys($stunting_heights);
if (empty($stunting_heights_ids)) {
  drush_print("There are no height measurements indicating stunting for newborns in DB.");
  return;
}

$data = [];
$chunks = array_chunk($stunting_heights_ids, $batch);
foreach ($chunks as $ids) {
  // Free up memory.
  drupal_static_reset();

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $child_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
    $data[$child_id] = [
      'child_id' => $child_id,
      'pregnancy_id' => $newborns_mapping[$child_id],
    ];
  }
}

$chunks = array_chunk($data, $batch);
foreach ($chunks as $chunk) {
  foreach ($chunk as $child_data) {
    $pregnancy = node_load($child_data['pregnancy_id']);
    $data[$child_data['child_id']]['mother_id'] = $pregnancy->field_person[LANGUAGE_NONE][0]['target_id'];

    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'prenatal_encounter')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_individual_participant', 'target_id', $child_data['pregnancy_id'])
      ->propertyOrderBy('nid')
      ->execute();

    if (empty($result['node'])) {
      continue;
    }
    $encounters = array_keys($result['node']);

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
      $data[$child_data['child_id']]['medical'] = implode(' & ', $medical_history_signs);
    }

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
      $data[$child_data['child_id']]['obstetric'] = implode(' & ', $obstetric_history_signs);
    }
  }
}

drush_print("Child ID,Mother ID,Medical,Obstetric");
foreach ($data as $item) {
  $child_id = $item['child_id'];
  $mother_id = $item['mother_id'];
  $medical = $item['medical'];
  $obstetric = $item['obstetric'];
  drush_print("$child_id,$mother_id,$medical,$obstetric");
}


drush_print("Done!");

/**
 * Resolves the label of the value for given field.
 */
function get_field_sign_label($field, $value) {
  $field_info = field_info_field($field);
  $allowed_values = $field_info['settings']['allowed_values'];

  return isset($allowed_values[$value]) ? $allowed_values[$value] : $value;
}
