<?php

/**
 * @file
 * Associate children with stunting to medical & obstetric history at ANC.
 *
 * Execution:  drush scr
 *   profiles/hedley/modules/custom/hedley_admin/scripts/anc-socio-economics-for-first-show-up.php.
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

$query = db_select('node', 'n')
  ->condition('n.type', 'last_menstrual_period')
  ->condition('n.status', NODE_PUBLISHED)
  // Order by NID ASC, so of there are multiple LMP measurements for
  // pregnancy (in case nurse encounter replaces data of chw encounter),
  // the latter LMP data will override the former one.
  ->orderBy('n.nid');

$query->join('field_data_field_last_menstrual_period', 'lmp', 'n.nid = lmp.entity_id');
$query->join('field_data_field_date_measured', 'm', 'n.nid = m.entity_id');
$query->fields('n', ['nid'])
  ->addExpression('DATEDIFF(m.field_date_measured_value, lmp.field_last_menstrual_period_value)', 'days_difference');

$results = $query->execute()->fetchAllAssoc('nid');
$lmp_ids = array_keys($results);

$chunks = array_chunk($lmp_ids, $batch);
$data = [];
foreach ($chunks as $ids) {
  // Free up memory.
  drupal_static_reset();

  $nodes = node_load_multiple($ids);
  foreach ($nodes as $node) {
    $days = $results[$node->nid]->days_difference;
    if ($days == 0) {
      continue;
    }

    $encounter_id = $node->field_prenatal_encounter[LANGUAGE_NONE][0]['target_id'];
    $encounter = node_load($encounter_id);
    $pregnancy_id = $encounter->field_individual_participant[LANGUAGE_NONE][0]['target_id'];
    if (empty($pregnancy_id)) {
      continue;
    }

    $mother_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
    $mother = node_load($mother_id);
    $data[$pregnancy_id] = [
      'mother_id' => $mother_id,
      'education_level' => get_field_sign_label('field_education_level', $mother->field_education_level[LANGUAGE_NONE][0]['value']),
      'marital_status' => get_field_sign_label('field_marital_status', $mother->field_marital_status[LANGUAGE_NONE][0]['value']),
      'ubudehe' => $mother->field_ubudehe[LANGUAGE_NONE][0]['value'],
      'days' => $days,
    ];
  }
}

drush_print();
drush_print("Mother ID,Education level,Marital Status,Family Ubudehe,First visit day");
foreach ($data as $item) {
  $mother_id = $item['mother_id'];
  $education_level = $item['education_level'];
  $marital_status = $item['marital_status'];
  $ubudehe = $item['ubudehe'];
  $days = $item['days'];
  drush_print("$mother_id,$education_level,$marital_status,$ubudehe,$days");
}

/**
 * Resolves the label of the value for given field.
 */
function get_field_sign_label($field, $value) {
  $field_info = field_info_field($field);
  $allowed_values = $field_info['settings']['allowed_values'];

  return isset($allowed_values[$value]) ? $allowed_values[$value] : $value;
}