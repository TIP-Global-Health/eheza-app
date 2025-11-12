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
  // Order by NID ASC, so if there are multiple LMP measurements for
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

    $encounters = array_keys($result['node']);
    $mother_id = $node->field_person[LANGUAGE_NONE][0]['target_id'];
    $mother = node_load($mother_id);
    $pregnancy = node_load($pregnancy_id);
    $birth_date = explode(' ', $mother->field_birth_date[LANGUAGE_NONE][0]['value'])[0];
    $first_encounter_date = explode(' ', $pregnancy->field_expected[LANGUAGE_NONE][0]['value'])[0];
    $age = (new DateTime($birth_date))->diff(new DateTime($first_encounter_date))->y;

    $data[$pregnancy_id] = [
      'mother_id' => $mother_id,
      'birth_date' => $birth_date,
      'age' => $age,
      'first_encounter_date' => $first_encounter_date,
      'concluded_date' => explode(' ', $pregnancy->field_date_concluded[LANGUAGE_NONE][0]['value'])[0],
      'education_level' => hedley_general_get_field_sign_label('field_education_level', $mother->field_education_level[LANGUAGE_NONE][0]['value']),
      'marital_status' => hedley_general_get_field_sign_label('field_marital_status', $mother->field_marital_status[LANGUAGE_NONE][0]['value']),
      'ubudehe' => $mother->field_ubudehe[LANGUAGE_NONE][0]['value'],
      'days' => $days,
    ];

    // Add social history data.
    $query = new EntityFieldQuery();
    $result = $query
      ->entityCondition('entity_type', 'node')
      ->entityCondition('bundle', 'social_history')
      ->propertyCondition('status', NODE_PUBLISHED)
      ->fieldCondition('field_prenatal_encounter', 'target_id', $encounters, 'IN')
      // Take most recent one, so in case nurse encounter replaces data of
      // chw encounter, we get the info recorded by nurse.
      ->propertyOrderBy('nid', 'DESC')
      ->range(0, 1)
      ->execute();

    if (!empty($result['node'])) {
      $accompanied = '';
      $social_history = node_load(key($result['node']));

      $field_values = $social_history->field_social_history[LANGUAGE_NONE];
      foreach ($field_values as $item) {
        if ($item['value'] == 'accompanied-by-partner') {
          $accompanied = 'Yes';
          break;
        }
      }
      if (empty($accompanied)) {
        $accompanied = 'No';
      }

      $data[$pregnancy_id]['accompanied'] = $accompanied;
    }
  }
}

drush_print();
drush_print("Mother ID,Birth date,Age,First encounter date,Concluded date,Education level,Marital Status,Family Ubudehe,First visit day,Accompanied by partner");
foreach ($data as $item) {
  $mother_id = $item['mother_id'];
  $birth_date = $item['birth_date'];
  $age = $item['age'];
  $first_encounter_date = $item['first_encounter_date'];
  $concluded_date = $item['concluded_date'];
  $education_level = $item['education_level'];
  $marital_status = $item['marital_status'];
  $ubudehe = $item['ubudehe'];
  $days = $item['days'];
  $accompanied = $item['accompanied'];
  drush_print("$mother_id,$birth_date,$age,$first_encounter_date,$concluded_date,$education_level,$marital_status,$ubudehe,$days,$accompanied");
}
