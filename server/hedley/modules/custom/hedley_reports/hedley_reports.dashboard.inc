<?php

/**
 * @file
 * Per-pregnancy records for the HealthyStart dashboards.
 *
 * One JSON record per antenatal participant, written by a queue, served in
 * batches to the Elm app. See docs/healthy-start-dashboards.md.
 */

/**
 * Advanced queue that writes one pregnancy's dashboard record.
 */
define('HEDLEY_REPORTS_CALCULATE_DASHBOARD_DATA', 'hedley_reports_calculate_dashboard_data');

/**
 * Format version carried by every record, so a decoder can skip old shapes.
 */
define('HEDLEY_REPORTS_DASHBOARD_DATA_VERSION', 1);

/**
 * Checks whether dashboard records are maintained on this installation.
 *
 * @return bool
 *   TRUE when the Healthy Start feature is enabled.
 */
function hedley_reports_dashboard_data_enabled() {
  return (bool) variable_get('hedley_admin_feature_healthy_start_enabled', FALSE);
}

/**
 * Resolves the first cohort's enrolment date as a timestamp.
 *
 * @return int|null
 *   Midnight of the configured date, or NULL while the setting is empty.
 */
function hedley_reports_dashboard_first_cohort_timestamp() {
  $date = variable_get('hedley_admin_healthy_start_first_cohort_date', '');

  return empty($date) ? NULL : strtotime($date);
}

/**
 * Encounter types of a newborn's routine care visits.
 *
 * @return array
 *   Participant encounter types, keyed by the code the record uses.
 */
function hedley_reports_dashboard_child_visit_types() {
  return [
    'nutrition' => HEDLEY_STATS_NUTRITION_ENCOUNTER_TYPE,
    'well-child' => HEDLEY_STATS_SPV_ENCOUNTER_TYPE,
  ];
}

/**
 * Checks whether a pregnancy belongs to the HealthyStart cohort.
 *
 * Antenatal, and registered on or after the first cohort date. Without that
 * setting, every antenatal pregnancy counts.
 *
 * @param object $participant
 *   The individual participant node.
 *
 * @return bool
 *   TRUE when the pregnancy gets a dashboard record.
 */
function hedley_reports_dashboard_pregnancy_in_cohort($participant) {
  if ($participant->type != HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE) {
    return FALSE;
  }

  $encounter_type = !empty($participant->field_encounter_type[LANGUAGE_NONE][0]['value'])
    ? $participant->field_encounter_type[LANGUAGE_NONE][0]['value']
    : '';
  if ($encounter_type != HEDLEY_STATS_PRENATAL_ENCOUNTER_TYPE) {
    return FALSE;
  }

  $first_cohort = hedley_reports_dashboard_first_cohort_timestamp();

  return empty($first_cohort) || $participant->created >= $first_cohort;
}

/**
 * Finds the pregnancies a newborn was registered on.
 *
 * @param int $child_nid
 *   The child's person node ID.
 *
 * @return array
 *   IDs of the antenatal participants whose field_newborn is the child.
 */
function hedley_reports_dashboard_pregnancies_by_newborn($child_nid) {
  $query = hedley_general_create_db_select_query_excluding_deleted();
  $query->join('field_data_field_newborn', 'nb', 'nb.entity_id = n.nid');
  $query->condition('n.type', HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE);
  $query->condition('nb.field_newborn_target_id', $child_nid);
  $query->addField('n', 'nid');

  return $query->execute()->fetchCol();
}

/**
 * Resolves which pregnancies a saved node changes the record of.
 *
 * @param object $node
 *   The node that was saved.
 *
 * @return array
 *   IDs of antenatal participants, possibly empty.
 */
function hedley_reports_dashboard_pregnancies_for_node($node) {
  $child_visit_bundles = [
    HEDLEY_ACTIVITY_NUTRITION_ENCOUNTER_CONTENT_TYPE,
    HEDLEY_ACTIVITY_WELL_CHILD_ENCOUNTER_CONTENT_TYPE,
  ];

  if ($node->type == HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE) {
    return [$node->nid];
  }

  if ($node->type == 'person') {
    return hedley_reports_dashboard_pregnancies_by_newborn($node->nid);
  }

  if ($node->type == HEDLEY_ACTIVITY_PRENATAL_ENCOUNTER_CONTENT_TYPE) {
    return empty($node->field_individual_participant[LANGUAGE_NONE][0]['target_id'])
      ? []
      : [$node->field_individual_participant[LANGUAGE_NONE][0]['target_id']];
  }

  if (in_array($node->type, $child_visit_bundles)) {
    if (empty($node->field_individual_participant[LANGUAGE_NONE][0]['target_id'])) {
      return [];
    }
    $wrapper = entity_metadata_wrapper('node', $node->field_individual_participant[LANGUAGE_NONE][0]['target_id']);
    $child_nid = $wrapper->field_person->value(['identifier' => TRUE]);

    return empty($child_nid) ? [] : hedley_reports_dashboard_pregnancies_by_newborn($child_nid);
  }

  return [];
}

/**
 * Queues a rewrite of the dashboard records a saved node affects.
 *
 * @param object $node
 *   The node that was saved.
 */
function hedley_reports_trigger_dashboard_data_recalculation($node) {
  if (!hedley_reports_dashboard_data_enabled()) {
    return;
  }

  foreach (hedley_reports_dashboard_pregnancies_for_node($node) as $participant_id) {
    $participant = node_load($participant_id);
    if (!$participant || !hedley_reports_dashboard_pregnancy_in_cohort($participant)) {
      continue;
    }

    hedley_general_add_task_to_advanced_queue_by_id(HEDLEY_REPORTS_CALCULATE_DASHBOARD_DATA, $participant_id, [
      'participant_id' => $participant_id,
    ]);
  }
}

/**
 * Advanced queue worker; writes the dashboard record of one pregnancy.
 *
 * @param object $queue_item
 *   The item object to process.
 *
 * @return array
 *   Array with "status" and "result" keys.
 */
function hedley_reports_calculate_dashboard_data_worker($queue_item) {
  if (empty($queue_item->data['participant_id'])) {
    return [
      'status' => ADVANCEDQUEUE_STATUS_FAILURE,
      'result' => t('Does not have Participant Node ID.'),
    ];
  }

  $participant_id = $queue_item->data['participant_id'];
  try {
    $participant = node_load($participant_id);
    $written = $participant && hedley_reports_generate_dashboard_data_for_pregnancy($participant);
  }
  catch (Exception $e) {
    return [
      'status' => ADVANCEDQUEUE_STATUS_FAILURE_RETRY,
      'result' => $e->getMessage(),
    ];
  }

  $params = ['@nid' => $participant_id];

  return [
    'status' => ADVANCEDQUEUE_STATUS_SUCCESS,
    'result' => $written
    ? t('Dashboard data has been calculated for pregnancy ID: @nid', $params)
    : t('Pregnancy ID: @nid is outside the HealthyStart cohort.', $params),
  ];
}

/**
 * Builds and stores the dashboard record of a pregnancy.
 *
 * Written with field_attach_update(), so no node hooks fire and devices do
 * not resync the pregnancy.
 *
 * @param object $participant
 *   The antenatal individual participant node.
 * @param bool $exclude_set
 *   When TRUE, a pregnancy that already has a record is left as it is.
 *
 * @return bool
 *   TRUE when a record was written.
 */
function hedley_reports_generate_dashboard_data_for_pregnancy($participant, $exclude_set = FALSE) {
  if (!hedley_reports_dashboard_pregnancy_in_cohort($participant)) {
    return FALSE;
  }

  if ($exclude_set && !empty($participant->field_reports_data[LANGUAGE_NONE][0]['value'])) {
    return FALSE;
  }

  $record = hedley_reports_dashboard_pregnancy_record($participant);
  $participant->field_reports_data[LANGUAGE_NONE][0]['value'] = json_encode($record);
  field_attach_update('node', $participant);
  entity_get_controller('node')->resetCache([$participant->nid]);

  return TRUE;
}

/**
 * Builds the dashboard record of a pregnancy.
 *
 * @param object $participant
 *   The antenatal individual participant node.
 *
 * @return array
 *   The record, as documented in docs/healthy-start-dashboards.md.
 */
function hedley_reports_dashboard_pregnancy_record($participant) {
  $outcome_mapping = hedley_reports_pregnancy_outcome_mapping();
  $outcome = !empty($participant->field_outcome[LANGUAGE_NONE][0]['value'])
    ? $participant->field_outcome[LANGUAGE_NONE][0]['value']
    : '';
  $outcome_location = !empty($participant->field_outcome_location[LANGUAGE_NONE][0]['value'])
    ? $participant->field_outcome_location[LANGUAGE_NONE][0]['value']
    : '';

  $record = [
    'v' => HEDLEY_REPORTS_DASHBOARD_DATA_VERSION,
    'id' => (int) $participant->nid,
    'hc' => !empty($participant->field_shards[LANGUAGE_NONE][0]['target_id'])
    ? (int) $participant->field_shards[LANGUAGE_NONE][0]['target_id']
    : NULL,
    'created' => date('Y-m-d', $participant->created),
    'edd' => hedley_reports_dashboard_field_date($participant, 'field_expected_date_concluded'),
    'dc' => hedley_reports_dashboard_field_date($participant, 'field_date_concluded'),
    'o' => isset($outcome_mapping[$outcome]) ? $outcome_mapping[$outcome] : NULL,
    'ol' => $outcome_location ? ($outcome_location == 'home' ? 'h' : 'f') : NULL,
    'visits' => [],
    'child' => NULL,
  ];

  $encounters_ids = hedley_person_load_individual_participant_encounters_ids($participant->nid);
  foreach (node_load_multiple($encounters_ids) as $encounter) {
    if ($encounter->type != HEDLEY_ACTIVITY_PRENATAL_ENCOUNTER_CONTENT_TYPE || $encounter->status != NODE_PUBLISHED) {
      continue;
    }
    $record['visits'][] = [
      'd' => hedley_reports_dashboard_field_date($encounter, 'field_scheduled_date'),
      't' => !empty($encounter->field_prenatal_encounter_type[LANGUAGE_NONE][0]['value'])
      ? $encounter->field_prenatal_encounter_type[LANGUAGE_NONE][0]['value']
      : 'nurse',
      'n' => hedley_reports_dashboard_field_date($encounter, 'field_next_visit_date'),
    ];
  }
  usort($record['visits'], 'hedley_reports_dashboard_compare_visits');

  if (!empty($participant->field_newborn[LANGUAGE_NONE][0]['target_id'])) {
    $record['child'] = hedley_reports_dashboard_child_record($participant->field_newborn[LANGUAGE_NONE][0]['target_id']);
  }

  return $record;
}

/**
 * Builds the child part of a pregnancy's record.
 *
 * @param int $child_nid
 *   The newborn's person node ID.
 *
 * @return array|null
 *   Birth date, gender and routine care visits, or NULL when the child is
 *   gone.
 */
function hedley_reports_dashboard_child_record($child_nid) {
  $child = node_load($child_nid);
  if (!$child || $child->status != NODE_PUBLISHED || !empty($child->field_deleted[LANGUAGE_NONE][0]['value'])) {
    return NULL;
  }

  $gender = !empty($child->field_gender[LANGUAGE_NONE][0]['value'])
    ? $child->field_gender[LANGUAGE_NONE][0]['value']
    : '';
  $record = [
    'id' => (int) $child->nid,
    'bd' => hedley_reports_dashboard_field_date($child, 'field_birth_date'),
    'g' => $gender ? substr($gender, 0, 1) : NULL,
    'visits' => [],
  ];

  $visit_types = hedley_reports_dashboard_child_visit_types();
  $participants_ids = hedley_person_individual_participants_for_person($child->nid, array_values($visit_types));
  $encounters_by_participant = hedley_person_load_individual_participants_encounters_ids($participants_ids);
  $codes_by_bundle = [
    HEDLEY_ACTIVITY_NUTRITION_ENCOUNTER_CONTENT_TYPE => 'nutrition',
    HEDLEY_ACTIVITY_WELL_CHILD_ENCOUNTER_CONTENT_TYPE => 'well-child',
  ];
  foreach ($encounters_by_participant as $encounters_ids) {
    foreach (node_load_multiple($encounters_ids) as $encounter) {
      if (!isset($codes_by_bundle[$encounter->type]) || $encounter->status != NODE_PUBLISHED) {
        continue;
      }
      $record['visits'][] = [
        'd' => hedley_reports_dashboard_field_date($encounter, 'field_scheduled_date'),
        't' => $codes_by_bundle[$encounter->type],
      ];
    }
  }
  usort($record['visits'], 'hedley_reports_dashboard_compare_visits');

  return $record;
}

/**
 * Reads a date field as YYYY-MM-DD.
 *
 * @param object $node
 *   The node holding the field.
 * @param string $field_name
 *   The date field.
 *
 * @return string|null
 *   The date, or NULL when the field is empty.
 */
function hedley_reports_dashboard_field_date($node, $field_name) {
  if (empty($node->{$field_name}[LANGUAGE_NONE][0]['value'])) {
    return NULL;
  }

  return date('Y-m-d', strtotime($node->{$field_name}[LANGUAGE_NONE][0]['value']));
}

/**
 * Orders visits by date, earliest first.
 *
 * @param array $a
 *   A visit entry.
 * @param array $b
 *   Another visit entry.
 *
 * @return int
 *   The comparison result for usort().
 */
function hedley_reports_dashboard_compare_visits(array $a, array $b) {
  return strcmp((string) $a['d'], (string) $b['d']);
}

/**
 * Serves one batch of dashboard records to the Elm app.
 *
 * Statistical Queries protocol: records after $from_nid by node ID, the count
 * remaining and the last ID sent.
 *
 * @param int $from_nid
 *   Pregnancies with a greater node ID are sent.
 * @param int $batch
 *   Most records in one batch.
 * @param int|null $health_center
 *   Health center ID to limit the records to, or NULL for all.
 *
 * @return array
 *   An array with "batch", "total_remaining" and "last" keys.
 */
function hedley_reports_generate_dashboard_data_for_restful($from_nid, $batch, $health_center) {
  $build = function () use ($from_nid, $health_center) {
    $q = db_select('field_data_field_reports_data', 'fr');
    $q->innerJoin('node', 'n', 'n.nid = fr.entity_id');
    $q->leftJoin('field_data_field_deleted', 'fd', 'fd.entity_id = n.nid');
    $q->condition('fr.entity_type', 'node')
      ->condition('fr.bundle', HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE)
      ->condition('fr.deleted', 0)
      ->condition('n.status', NODE_PUBLISHED)
      ->condition('n.nid', $from_nid, '>')
      ->isNotNull('fr.field_reports_data_value')
      ->condition(db_or()->isNull('fd.field_deleted_value')->condition('fd.field_deleted_value', 0));
    if (!empty($health_center)) {
      $alias = $q->innerJoin('field_data_field_shards', 'fs',
        "%alias.entity_id = n.nid AND %alias.entity_type = 'node' AND %alias.deleted = 0");
      $q->condition("$alias.field_shards_target_id", $health_center);
    }
    return $q;
  };

  $count_q = $build();
  $count_q->addExpression('COUNT(*)', 'cnt');
  $count = (int) $count_q->execute()->fetchField();

  if ($count === 0) {
    return [
      'batch' => [],
      'total_remaining' => 0,
      'last' => 0,
    ];
  }

  $data_q = $build();
  $data_q->fields('n', ['nid'])
    ->fields('fr', ['field_reports_data_value'])
    ->orderBy('n.nid')
    ->range(0, $batch);
  $rows = $data_q->execute()->fetchAll();

  $output = [];
  $last = 0;
  foreach ($rows as $r) {
    $output[] = json_decode($r->field_reports_data_value);
    $last = (int) $r->nid;
  }

  return [
    'batch' => $output,
    'total_remaining' => $count - count($rows),
    'last' => $last,
  ];
}
