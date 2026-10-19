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
define('HEDLEY_REPORTS_DASHBOARD_DATA_VERSION', 3);

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
 * The individual encounters that are a newborn's routine care visits.
 *
 * Group nutrition attendance is the third kind of visit; it has no
 * participant, so it is read on its own.
 *
 * @return array
 *   Keyed by encounter bundle: the code the record uses and the
 *   participant encounter type.
 */
function hedley_reports_dashboard_child_visit_bundles() {
  return [
    HEDLEY_ACTIVITY_NUTRITION_ENCOUNTER_CONTENT_TYPE => [
      'code' => 'nutrition',
      'encounter_type' => HEDLEY_STATS_NUTRITION_ENCOUNTER_TYPE,
    ],
    HEDLEY_ACTIVITY_WELL_CHILD_ENCOUNTER_CONTENT_TYPE => [
      'code' => 'well-child',
      'encounter_type' => HEDLEY_STATS_SPV_ENCOUNTER_TYPE,
    ],
  ];
}

/**
 * Measurement bundles the record reads; each points at a prenatal encounter.
 *
 * @return array
 *   Content type names.
 */
function hedley_reports_dashboard_measurement_bundles() {
  return [
    'medical_history',
    'obstetrical_exam',
    'prenatal_hiv_test',
    'prenatal_outside_care',
    'prenatal_ultrasound',
    HEDLEY_ACTIVITY_PRENATAL_ASPIRIN_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_CALCIUM_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_MEDICATION_DISTRIBUTION_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_MMS_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_NUTRITION_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_OBSTETRIC_HISTORY_STEP2_CONTENT_TYPE,
    HEDLEY_ACTIVITY_PRENATAL_REFERRAL_CONTENT_TYPE,
  ];
}

/**
 * Checks whether a pregnancy belongs to the HealthyStart cohort.
 *
 * Live, antenatal, and registered on or after the first cohort date. Without
 * that setting, every such pregnancy counts.
 *
 * @param object $participant
 *   The individual participant node.
 *
 * @return bool
 *   TRUE when the pregnancy gets a dashboard record.
 */
function hedley_reports_dashboard_pregnancy_in_cohort($participant) {
  if ($participant->type != HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE
    || $participant->status != NODE_PUBLISHED
    || hedley_reports_dashboard_field_value($participant, 'field_deleted')) {
    return FALSE;
  }

  if (hedley_reports_dashboard_field_value($participant, 'field_encounter_type') != HEDLEY_STATS_PRENATAL_ENCOUNTER_TYPE) {
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
 * A person may be a mother or a newborn. Content the record does not read
 * resolves to nothing.
 *
 * @param object $node
 *   The node that was saved.
 *
 * @return array
 *   IDs of antenatal participants, possibly empty.
 */
function hedley_reports_dashboard_pregnancies_for_node($node) {
  if ($node->type == HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE) {
    return [$node->nid];
  }

  if ($node->type == 'person') {
    return array_merge(
      hedley_person_individual_participants_for_person($node->nid, [HEDLEY_STATS_PRENATAL_ENCOUNTER_TYPE]),
      hedley_reports_dashboard_pregnancies_by_newborn($node->nid)
    );
  }

  if ($node->type == HEDLEY_ACTIVITY_PRENATAL_ENCOUNTER_CONTENT_TYPE) {
    $participant_id = hedley_reports_dashboard_field_value($node, 'field_individual_participant', 'target_id');

    return $participant_id ? [$participant_id] : [];
  }

  if (in_array($node->type, hedley_reports_dashboard_measurement_bundles())) {
    $encounter_id = hedley_reports_dashboard_field_value($node, 'field_prenatal_encounter', 'target_id');
    $encounter = $encounter_id ? node_load($encounter_id) : NULL;

    return $encounter ? hedley_reports_dashboard_pregnancies_for_node($encounter) : [];
  }

  if (isset(hedley_reports_dashboard_child_visit_bundles()[$node->type])) {
    $participant_id = hedley_reports_dashboard_field_value($node, 'field_individual_participant', 'target_id');
    if (!$participant_id) {
      return [];
    }
    $wrapper = entity_metadata_wrapper('node', $participant_id);
    $child_nid = $wrapper->field_person->value(['identifier' => TRUE]);

    return empty($child_nid) ? [] : hedley_reports_dashboard_pregnancies_by_newborn($child_nid);
  }

  if (isset(hedley_reports_dashboard_child_content_fields()[$node->type])) {
    $child_nid = hedley_reports_dashboard_field_value($node, 'field_person', 'target_id');

    return $child_nid ? hedley_reports_dashboard_pregnancies_by_newborn($child_nid) : [];
  }

  return [];
}

/**
 * Content on the child the record reads, and the fields it reads from it.
 *
 * @return array
 *   Keyed by content type: field name => column read.
 */
function hedley_reports_dashboard_child_content_fields() {
  $shared = [
    'field_date_measured' => 'value',
    'field_deleted' => 'value',
    'field_person' => 'target_id',
  ];

  return [
    HEDLEY_ACTIVITY_ATTENDANCE_CONTENT_TYPE => ['field_attended' => 'value'] + $shared,
    HEDLEY_ACTIVITY_WELL_CHILD_PREGNANCY_SUMMARY_CONTENT_TYPE => ['field_weight' => 'value'] + $shared,
  ];
}

/**
 * Checks whether a save of content on the child left the record as it is.
 *
 * Attendance is the most numerous synced content; an unchanged or unpublished
 * save costs nothing.
 *
 * @param object $node
 *   The saved node, with its "original" when it is an update.
 *
 * @return bool
 *   TRUE when no record needs rewriting.
 */
function hedley_reports_dashboard_save_changes_nothing($node) {
  $fields = hedley_reports_dashboard_child_content_fields();
  if (!isset($fields[$node->type]) || empty($node->original)) {
    return FALSE;
  }
  $original = $node->original;
  if ($node->status != NODE_PUBLISHED && $original->status != NODE_PUBLISHED) {
    return TRUE;
  }
  if ($node->status != $original->status) {
    return FALSE;
  }
  foreach ($fields[$node->type] as $field_name => $column) {
    if (hedley_reports_dashboard_field_value($node, $field_name, $column) != hedley_reports_dashboard_field_value($original, $field_name, $column)) {
      return FALSE;
    }
  }

  return TRUE;
}

/**
 * Queues a rewrite of the dashboard records a saved node affects.
 *
 * @param object $node
 *   The node that was saved.
 */
function hedley_reports_trigger_dashboard_data_recalculation($node) {
  if (!hedley_reports_dashboard_data_enabled() || hedley_reports_dashboard_save_changes_nothing($node)) {
    return;
  }

  $participants_ids = hedley_reports_dashboard_pregnancies_for_node($node);
  if (!empty($node->original)) {
    // A node moved to another pregnancy or child also leaves one.
    $participants_ids = array_merge($participants_ids, hedley_reports_dashboard_pregnancies_for_node($node->original));
  }

  foreach (array_unique($participants_ids) as $participant_id) {
    $participant = $participant_id == $node->nid ? $node : node_load($participant_id);
    if (!$participant || !hedley_reports_dashboard_pregnancy_in_cohort($participant)) {
      continue;
    }

    // Only a queued item absorbs the save; a running one gets a successor,
    // so a change made during the build is not lost.
    hedley_general_add_task_to_advanced_queue_by_id(HEDLEY_REPORTS_CALCULATE_DASHBOARD_DATA, $participant_id, [
      'participant_id' => $participant_id,
    ], [ADVANCEDQUEUE_STATUS_QUEUED]);
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
  $params = ['@nid' => $participant_id];
  $participant = node_load($participant_id);
  if (!$participant) {
    return [
      'status' => ADVANCEDQUEUE_STATUS_FAILURE,
      'result' => t('Participant node ID: @nid does not exist.', $params),
    ];
  }

  try {
    $written = hedley_reports_generate_dashboard_data_for_pregnancy($participant);
  }
  catch (Exception $e) {
    return [
      'status' => ADVANCEDQUEUE_STATUS_FAILURE_RETRY,
      'result' => $e->getMessage(),
    ];
  }

  $result = $written
    ? t('Dashboard data has been calculated for pregnancy ID: @nid', $params)
    : t('Pregnancy ID: @nid is outside the HealthyStart cohort.', $params);

  return [
    'status' => ADVANCEDQUEUE_STATUS_SUCCESS,
    'result' => $result,
  ];
}

/**
 * Builds and stores the dashboard record of a pregnancy.
 *
 * Reloaded right before the write, so a sync landing during the build is
 * kept. field_attach_update() fires no node hooks.
 *
 * @param object $participant
 *   The antenatal individual participant node.
 * @param bool $exclude_set
 *   When TRUE, a pregnancy whose record is of the current version is left
 *   as it is.
 *
 * @return bool
 *   TRUE when a record was written.
 */
function hedley_reports_generate_dashboard_data_for_pregnancy($participant, $exclude_set = FALSE) {
  if (!hedley_reports_dashboard_pregnancy_in_cohort($participant)) {
    return FALSE;
  }

  if ($exclude_set && hedley_reports_dashboard_record_is_current($participant)) {
    return FALSE;
  }

  $record = hedley_reports_dashboard_pregnancy_record($participant);
  entity_get_controller('node')->resetCache([$participant->nid]);
  $fresh = node_load($participant->nid);
  if (!$fresh) {
    return FALSE;
  }
  $fresh->field_reports_data[LANGUAGE_NONE][0]['value'] = hedley_reports_dashboard_encode_record($record);
  field_attach_update('node', $fresh);
  entity_get_controller('node')->resetCache([$fresh->nid]);

  return TRUE;
}

/**
 * Checks whether a pregnancy holds a record of the current format version.
 *
 * @param object $participant
 *   The individual participant node.
 *
 * @return bool
 *   TRUE when nothing needs rewriting after a format change.
 */
function hedley_reports_dashboard_record_is_current($participant) {
  $existing = hedley_reports_dashboard_field_value($participant, 'field_reports_data');

  return $existing && strpos($existing, hedley_reports_dashboard_record_version_prefix()) === 0;
}

/**
 * Encodes a record; the version is its first key, as the prefix test expects.
 *
 * @param array $record
 *   The record built for a pregnancy.
 *
 * @return string
 *   JSON, starting with hedley_reports_dashboard_record_version_prefix().
 */
function hedley_reports_dashboard_encode_record(array $record) {
  return json_encode(['v' => HEDLEY_REPORTS_DASHBOARD_DATA_VERSION] + $record);
}

/**
 * The opening of a record of the current version.
 *
 * @return string
 *   For example `{"v":3,`.
 */
function hedley_reports_dashboard_record_version_prefix() {
  return rtrim(json_encode(['v' => HEDLEY_REPORTS_DASHBOARD_DATA_VERSION]), '}') . ',';
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
  $outcome = hedley_reports_dashboard_field_value($participant, 'field_outcome');
  $outcome_location = hedley_reports_dashboard_field_value($participant, 'field_outcome_location');
  $mother_id = hedley_reports_dashboard_field_value($participant, 'field_person', 'target_id');
  $mother = $mother_id ? node_load($mother_id) : NULL;
  $shard = hedley_reports_dashboard_field_value($participant, 'field_shards', 'target_id');

  $record = [
    'id' => (int) $participant->nid,
    'hc' => $shard ? (int) $shard : NULL,
    'created' => date('Y-m-d', $participant->created),
    'mbd' => $mother ? hedley_reports_dashboard_field_date($mother, 'field_birth_date') : NULL,
    'edd' => hedley_reports_dashboard_field_date($participant, 'field_expected_date_concluded'),
    'dc' => hedley_reports_dashboard_field_date($participant, 'field_date_concluded'),
    'o' => isset($outcome_mapping[$outcome]) ? $outcome_mapping[$outcome] : NULL,
    'ol' => $outcome_location ? ($outcome_location == 'home' ? 'h' : 'f') : NULL,
    'visits' => [],
    'us' => [],
    'risks' => [],
    'child' => NULL,
  ];

  $encounters_ids = hedley_person_load_individual_participant_encounters_ids($participant->nid);
  $encounters = array_filter(node_load_multiple($encounters_ids), function ($encounter) {
    return $encounter->type == HEDLEY_ACTIVITY_PRENATAL_ENCOUNTER_CONTENT_TYPE && $encounter->status == NODE_PUBLISHED;
  });
  $measurements = hedley_reports_dashboard_measurements_by_encounter(array_keys($encounters));
  $diagnosis_mapping = hedley_reports_prenatal_diagnosis_mapping();
  $risk_sources = [
    'csection' => FALSE,
    'hiv' => $mother && hedley_reports_dashboard_field_value($mother, 'field_hiv_status') == 'positive',
    'ncd' => FALSE,
    'underweight' => FALSE,
  ];
  $all_diagnoses = [];

  foreach ($encounters as $encounter) {
    $encounter_measurements = isset($measurements[$encounter->nid]) ? $measurements[$encounter->nid] : [];
    $diagnoses = hedley_reports_dashboard_diagnosis_codes(
      hedley_reports_dashboard_field_values($encounter, 'field_prenatal_diagnoses'),
      $diagnosis_mapping
    );
    $all_diagnoses = array_merge($all_diagnoses, $diagnoses);
    $indicators = hedley_reports_dashboard_encounter_indicators($encounter, $encounter_measurements);

    $record['visits'][] = [
      'd' => hedley_reports_dashboard_field_date($encounter, 'field_scheduled_date'),
      't' => hedley_reports_dashboard_field_value($encounter, 'field_prenatal_encounter_type') ?: 'nurse',
      'n' => hedley_reports_dashboard_field_date($encounter, 'field_next_visit_date'),
      'dx' => $diagnoses,
      'ind' => $indicators,
    ];

    foreach ($encounter_measurements as $measurement) {
      hedley_reports_dashboard_collect_measurement($measurement, $record['us'], $risk_sources);
      // Conditions another facility diagnosed count as risks, but are not
      // this visit's own diagnoses.
      if ($measurement->type == 'prenatal_outside_care') {
        $all_diagnoses = array_merge($all_diagnoses, hedley_reports_dashboard_diagnosis_codes(
          hedley_reports_dashboard_field_values($measurement, 'field_prenatal_diagnoses'),
          $diagnosis_mapping
        ));
      }
    }
  }
  usort($record['visits'], 'hedley_reports_dashboard_compare_visits');
  sort($record['us']);

  $record['risks'] = hedley_reports_dashboard_risks($record, $all_diagnoses, $risk_sources);

  $newborn_id = hedley_reports_dashboard_field_value($participant, 'field_newborn', 'target_id');
  if ($newborn_id) {
    $record['child'] = hedley_reports_dashboard_child_record($newborn_id);
  }

  return $record;
}

/**
 * Loads the measurements the record reads, grouped by prenatal encounter.
 *
 * @param array $encounters_ids
 *   The pregnancy's prenatal encounter IDs.
 *
 * @return array
 *   Measurement nodes keyed by encounter ID.
 */
function hedley_reports_dashboard_measurements_by_encounter(array $encounters_ids) {
  $grouped = [];
  if (empty($encounters_ids)) {
    return $grouped;
  }

  $query = hedley_general_create_db_select_query_excluding_deleted();
  $query->join('field_data_field_prenatal_encounter', 'pe', 'pe.entity_id = n.nid');
  $query->condition('n.type', hedley_reports_dashboard_measurement_bundles(), 'IN');
  $query->condition('n.status', NODE_PUBLISHED);
  $query->condition('pe.field_prenatal_encounter_target_id', $encounters_ids, 'IN');
  $query->addField('n', 'nid');
  $query->addField('pe', 'field_prenatal_encounter_target_id', 'encounter_id');

  $ids_by_encounter = [];
  foreach ($query->execute() as $row) {
    $ids_by_encounter[$row->nid] = $row->encounter_id;
  }
  foreach (node_load_multiple(array_keys($ids_by_encounter)) as $nid => $measurement) {
    $grouped[$ids_by_encounter[$nid]][] = $measurement;
  }

  return $grouped;
}

/**
 * Maps diagnosis values to their Statistical Queries codes.
 *
 * @param array $diagnoses
 *   Diagnosis values.
 * @param array $mapping
 *   Diagnosis values to codes; values without a code are dropped.
 *
 * @return array
 *   Distinct diagnosis codes.
 */
function hedley_reports_dashboard_diagnosis_codes(array $diagnoses, array $mapping) {
  $codes = [];
  foreach ($diagnoses as $diagnosis) {
    if (isset($mapping[$diagnosis])) {
      $codes[] = $mapping[$diagnosis];
    }
  }

  return array_values(array_unique($codes));
}

/**
 * Codes of the indicators an encounter recorded.
 *
 * Adequate GWG comes from the encounter itself; MMS, aspirin, calcium and
 * the ultrasound referral from its measurements, as in the person data.
 *
 * @param object $encounter
 *   The prenatal encounter node.
 * @param array $measurements
 *   The encounter's measurement nodes.
 *
 * @return array
 *   Distinct indicator codes.
 */
function hedley_reports_dashboard_encounter_indicators($encounter, array $measurements) {
  $mapping = hedley_reports_prenatal_indicator_mapping();
  $administered = function ($measurement) {
    return in_array('administered-today', hedley_reports_dashboard_field_values($measurement, 'field_administration_note'));
  };

  $indicators = [];
  $encounter_indicators = hedley_reports_dashboard_field_values($encounter, 'field_prenatal_indicators');
  if (in_array('adequate-gwg', $encounter_indicators)) {
    $indicators[] = 'gwg';
  }
  if (in_array('inadequate-gwg', $encounter_indicators)) {
    $indicators[] = 'gwg-inadequate';
  }

  foreach ($measurements as $measurement) {
    switch ($measurement->type) {
      case HEDLEY_ACTIVITY_PRENATAL_ASPIRIN_CONTENT_TYPE:
        if ($administered($measurement)) {
          $indicators[] = 'aspirin';
        }
        break;

      case HEDLEY_ACTIVITY_PRENATAL_CALCIUM_CONTENT_TYPE:
        if ($administered($measurement)) {
          $indicators[] = 'calcium';
        }
        break;

      case HEDLEY_ACTIVITY_PRENATAL_MMS_CONTENT_TYPE:
        if ($administered($measurement)) {
          $indicators[] = 'mms';
        }
        break;

      case HEDLEY_ACTIVITY_PRENATAL_MEDICATION_DISTRIBUTION_CONTENT_TYPE:
        if (in_array('aspirin', hedley_reports_dashboard_field_values($measurement, 'field_prescribed_medication'))) {
          $indicators[] = 'aspirin';
        }
        break;

      case HEDLEY_ACTIVITY_PRENATAL_REFERRAL_CONTENT_TYPE:
        if (in_array('us', hedley_reports_dashboard_field_values($measurement, 'field_referrals'))) {
          $indicators[] = 'us';
        }
        break;
    }
  }

  $codes = [];
  foreach (array_unique($indicators) as $indicator) {
    $codes[] = $mapping[$indicator];
  }

  return $codes;
}

/**
 * Reads a measurement for the ultrasound dates and the high-risk sources.
 *
 * @param object $measurement
 *   The measurement node.
 * @param array $ultrasounds
 *   Ultrasound dates collected so far; extended in place.
 * @param array $risk_sources
 *   Flags for csection, hiv, ncd and underweight; raised in place.
 */
function hedley_reports_dashboard_collect_measurement($measurement, array &$ultrasounds, array &$risk_sources) {
  switch ($measurement->type) {
    case 'prenatal_ultrasound':
      $date = hedley_reports_dashboard_field_date($measurement, 'field_execution_date')
        ?: hedley_reports_dashboard_field_date($measurement, 'field_date_measured');
      if ($date) {
        $ultrasounds[] = $date;
      }
      break;

    case 'medical_history':
      $history = hedley_reports_dashboard_field_values($measurement, 'field_medical_history');
      $ncd_conditions = [
        'asthma',
        'cardiac-disease',
        'diabetes',
        'hypertension-before-pregnancy',
        'renal-disease',
      ];
      if (array_intersect($history, $ncd_conditions)) {
        $risk_sources['ncd'] = TRUE;
      }
      if (in_array('hiv', $history)) {
        $risk_sources['hiv'] = TRUE;
      }
      break;

    case 'obstetrical_exam':
      if (in_array(hedley_reports_dashboard_field_value($measurement, 'field_c_section_scar'), ['horizontal', 'vertical'])) {
        $risk_sources['csection'] = TRUE;
      }
      break;

    case HEDLEY_ACTIVITY_PRENATAL_OBSTETRIC_HISTORY_STEP2_CONTENT_TYPE:
      if ((int) hedley_reports_dashboard_field_value($measurement, 'field_c_sections') > 0) {
        $risk_sources['csection'] = TRUE;
      }
      break;

    case 'prenatal_hiv_test':
      if (hedley_reports_dashboard_field_value($measurement, 'field_test_result') == 'positive') {
        $risk_sources['hiv'] = TRUE;
      }
      break;

    case HEDLEY_ACTIVITY_PRENATAL_NUTRITION_CONTENT_TYPE:
      $muac = (float) hedley_reports_dashboard_field_value($measurement, 'field_muac');
      $weight = (float) hedley_reports_dashboard_field_value($measurement, 'field_weight');
      $height = (float) hedley_reports_dashboard_field_value($measurement, 'field_height');
      $bmi = ($weight > 0 && $height > 0) ? $weight / pow($height / 100, 2) : 0;
      if (($muac > 0 && $muac < 21) || ($bmi > 0 && $bmi < 17.5)) {
        $risk_sources['underweight'] = TRUE;
      }
      break;
  }
}

/**
 * The high-risk criteria a pregnancy meets, as HealthyStart listed them.
 *
 * @param array $record
 *   The record built so far, for the dates and the mother's birth date.
 * @param array $diagnoses
 *   Diagnosis codes of every visit.
 * @param array $risk_sources
 *   Flags raised while reading the measurements.
 *
 * @return array
 *   Risk codes, in a fixed order.
 */
function hedley_reports_dashboard_risks(array $record, array $diagnoses, array $risk_sources) {
  $diagnosed = function (array $codes) use ($diagnoses) {
    return (bool) array_intersect($codes, $diagnoses);
  };

  $age = NULL;
  if ($record['mbd']) {
    $first_visit = $record['created'];
    foreach ($record['visits'] as $visit) {
      if ($visit['d']) {
        $first_visit = $visit['d'];
        break;
      }
    }
    $age = (int) ((strtotime($first_visit) - strtotime($record['mbd'])) / (365.25 * 86400));
  }

  $risks = [
    // Risk of pre-eclampsia: the diagnoses that start low-dose aspirin.
    'pe' => $diagnosed(['m2', 'n2']),
    // Severe anaemia, with or without malaria.
    'anemia' => $diagnosed(['q', 's', 't']),
    // Diabetes or gestational diabetes, diagnosed by the app's glucose rule.
    'diabetes' => $diagnosed(['t1', 'u1']),
    'malaria' => $diagnosed(['o', 'p', 'q']),
    'csection' => $risk_sources['csection'],
    // Chronic hypertension diagnosed, or an NCD in the medical history.
    'ncd' => $risk_sources['ncd'] || $diagnosed(['a']),
    'age' => $age !== NULL && ($age < 18 || $age > 40),
    'hiv' => $risk_sources['hiv'] || $diagnosed(['h', 'i']),
    'underweight' => $risk_sources['underweight'],
  ];

  return array_keys(array_filter($risks));
}

/**
 * Builds the child part of a pregnancy's record.
 *
 * @param int $child_nid
 *   The newborn's person node ID.
 *
 * @return array|null
 *   Birth date, gender, birth weight and routine care visits, or NULL when
 *   the child is gone.
 */
function hedley_reports_dashboard_child_record($child_nid) {
  $child = node_load($child_nid);
  if (!$child || $child->status != NODE_PUBLISHED || hedley_reports_dashboard_field_value($child, 'field_deleted')) {
    return NULL;
  }

  $gender = hedley_reports_dashboard_field_value($child, 'field_gender');
  $record = [
    'id' => (int) $child->nid,
    'bd' => hedley_reports_dashboard_field_date($child, 'field_birth_date'),
    'g' => $gender ? substr($gender, 0, 1) : NULL,
    'bw' => NULL,
    'bwd' => NULL,
    'visits' => [],
  ];

  // Birth weight in grams, from the earliest newborn exam that has one; on
  // one date, the lowest node id.
  $bw_nid = NULL;
  $summaries_ids = hedley_general_get_person_measurements($child->nid, [HEDLEY_ACTIVITY_WELL_CHILD_PREGNANCY_SUMMARY_CONTENT_TYPE]);
  foreach (node_load_multiple($summaries_ids) as $summary) {
    $weight = hedley_reports_dashboard_field_value($summary, 'field_weight');
    $date = hedley_reports_dashboard_field_date($summary, 'field_date_measured');
    if ($summary->status != NODE_PUBLISHED || !$weight || !$date) {
      continue;
    }
    if (empty($record['bwd']) || $date < $record['bwd'] || ($date == $record['bwd'] && $summary->nid < $bw_nid)) {
      $bw_nid = (int) $summary->nid;
      $record['bw'] = (int) round($weight);
      $record['bwd'] = $date;
    }
  }

  $visit_bundles = hedley_reports_dashboard_child_visit_bundles();
  $participants_ids = hedley_person_individual_participants_for_person($child->nid, array_column($visit_bundles, 'encounter_type'));
  $encounters_ids = hedley_person_load_individual_participants_encounters_ids($participants_ids);
  $encounters_ids = $encounters_ids ? array_merge(...array_values($encounters_ids)) : [];
  foreach (node_load_multiple($encounters_ids) as $encounter) {
    if (!isset($visit_bundles[$encounter->type]) || $encounter->status != NODE_PUBLISHED) {
      continue;
    }
    $record['visits'][] = [
      'd' => hedley_reports_dashboard_field_date($encounter, 'field_scheduled_date'),
      't' => $visit_bundles[$encounter->type]['code'],
    ];
  }

  // Group nutrition sessions the child attended.
  $attendances_ids = hedley_general_get_person_measurements($child->nid, [HEDLEY_ACTIVITY_ATTENDANCE_CONTENT_TYPE]);
  foreach (node_load_multiple($attendances_ids) as $attendance) {
    if ($attendance->status != NODE_PUBLISHED || !hedley_reports_dashboard_field_value($attendance, 'field_attended')) {
      continue;
    }
    $record['visits'][] = [
      'd' => hedley_reports_dashboard_field_date($attendance, 'field_date_measured'),
      't' => 'group-nutrition',
    ];
  }
  usort($record['visits'], 'hedley_reports_dashboard_compare_visits');

  return $record;
}

/**
 * Reads the first value of a field.
 *
 * @param object $node
 *   The node holding the field.
 * @param string $field_name
 *   The field.
 * @param string $column
 *   The column to read, "value" unless given.
 *
 * @return mixed|null
 *   The value, or NULL when the field is empty.
 */
function hedley_reports_dashboard_field_value($node, $field_name, $column = 'value') {
  return empty($node->{$field_name}[LANGUAGE_NONE][0][$column])
    ? NULL
    : $node->{$field_name}[LANGUAGE_NONE][0][$column];
}

/**
 * Reads every value of a multi-value list field.
 *
 * @param object $node
 *   The node holding the field.
 * @param string $field_name
 *   The field.
 *
 * @return array
 *   The values, possibly empty.
 */
function hedley_reports_dashboard_field_values($node, $field_name) {
  return empty($node->{$field_name}[LANGUAGE_NONE])
    ? []
    : array_column($node->{$field_name}[LANGUAGE_NONE], 'value');
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
  $value = hedley_reports_dashboard_field_value($node, $field_name);

  return $value ? date('Y-m-d', strtotime($value)) : NULL;
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
  $loaded = hedley_reports_load_reports_data_batch($from_nid, $batch, [HEDLEY_ACTIVITY_INDIVIDUAL_PARTICIPANT_CONTENT_TYPE], $health_center);

  $output = [];
  $last = 0;
  foreach ($loaded['rows'] as $row) {
    $output[] = json_decode($row->field_reports_data_value);
    $last = (int) $row->nid;
  }

  return [
    'batch' => $output,
    'total_remaining' => $loaded['count'] - count($loaded['rows']),
    'last' => $last,
  ];
}
