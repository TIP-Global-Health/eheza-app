<?php

/**
 * @file
 * Drush commands collection.
 */

const HEDLEY_PATIENT_ENCOUNTER_TYPE_VALUE_MAPPING = [
  'acute_illness_encounter' => 'acute-illness',
  'prenatal_encounter' => 'antenatal',
  'child_scoreboard_encounter' => 'child-scoreboard',
  'home_visit_encounter' => 'home-visit',
  'ncd_encounter' => 'ncd',
  'nutrition_encounter' => 'nutrition',
  'well_child_encounter' => 'well-child',
];

/**
 * Implements hook_drush_command().
 */
function hedley_patient_drush_command() {
  $items = [];
  $items['patient-measurements'] = [
    'callback' => 'hedley_patient_patient_measurements_report',
    'description' => 'Patient report of number of measurements and their type.',
    'options' => [
      'patient' => 'Node ID of the patient.',
    ],
    'aliases' => ['ptnt-msmnts'],
  ];
  $items['consolidate-patients'] = [
    'callback' => 'hedley_patient_consolidate_patients',
    'description' => 'Consolidates data of 2 patient - used when there are duplicate instances of same patient.',
    'options' => [
      'original' => 'Node ID of the patient to which all data is consolidated.',
      'duplicate' => 'Node ID of the patient which data is moved to the original patient.',
    ],
    'aliases' => ['cns-ptnts'],
  ];

  return $items;
}

/**
 * Command callback for 'drush patient-measurements'.
 *
 * For each type of encounter, reports the number of measurement patient has.
 */
function hedley_patient_patient_measurements_report() {
  $patient = drush_get_option('patient');
  if (empty($patient)) {
    drush_print('You have to specify --patient parameter.');
    return;
  }

  $wrapper = entity_metadata_wrapper('node', $patient);
  if ($wrapper->getBundle() !== 'person') {
    drush_print('ERROR! Patient node type is not of a person. Please fix and try again.');
    return;
  }

  // Get all measurements that belong to duplicate patient.
  $measurements = hedley_general_get_person_measurements($patient);
  // If no measurements found, mark duplicate patient as 'Deleted'.
  if (empty($measurements)) {
    drush_print('Patient got no measurements.');
    return;
  }

  $measurements_by_encounter_type = _group_measurements_by_encounter_type($measurements);
  foreach ($measurements_by_encounter_type as $encounter_type => $measurements) {
    $count = count($measurements);

    if ($encounter_type == 'session') {
      $encounter_indicator = 'group encounter';
    }
    else {
      $encounter_indicator = str_replace('_', ' ', $encounter_type);
    }

    drush_print("Patient got $count measurements from $encounter_indicator.");
  }
}

/**
 * Command callback for 'drush consolidate-patients'.
 *
 * Validates, then executes consolidation process.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_patients() {
  drush_print('Running validations ...');
  drush_print('');

  $original = drush_get_option('original');
  $duplicate = drush_get_option('duplicate');
  if (empty($original) || empty($duplicate)) {
    drush_print('ERROR! You have to specify both --original and --duplicate parameter.');
    return;
  }

  $result = _consolidate_patients_validate($original, $duplicate);
  foreach ($result['messages'] as $message) {
    drush_print($message);
  }

  if ($result['success']) {
    drush_print('Executing consolidation ...');
    drush_print('');

    $messages = _consolidate_patients_execute($original, $duplicate);
    foreach ($messages as $message) {
      drush_print($message);
    }
  }
}

/**
 * Runs validations before executing consolidation process.
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_patients_validate($original, $duplicate) {
  $return = [
    'success' => FALSE,
    'messages' => [],
  ];

  $wrapper_original = entity_metadata_wrapper('node', $original);
  $wrapper_duplicate = entity_metadata_wrapper('node', $duplicate);

  if ($wrapper_original->getBundle() !== 'person') {
    $return['messages'][] = 'ERROR! Original patient node type is not of a person. Please fix and try again.';
    return $return;
  }

  if ($wrapper_duplicate->getBundle() !== 'person') {
    $return['messages'][] = 'ERROR! Duplicate patient node type is not of a person. Please fix and try again.';
    return $return;
  }

  $health_center_original = $wrapper_original->field_health_center->getIdentifier();
  $health_center_duplicate = $wrapper_duplicate->field_health_center->getIdentifier();
  // Do not allow consolidation, if patients are assigned to
  // different health centers.
  if ($health_center_original !== $health_center_duplicate) {
    $return['messages'][] = 'ERROR! Patients do not belong to same Health Center.';
    return $return;
  }

  // Get all measurements that belong to duplicate patient.
  $measurements_of_duplicate = hedley_general_get_person_measurements($duplicate);
  $measurements_by_encounter_type = _group_measurements_by_encounter_type($measurements_of_duplicate);
  foreach ($measurements_by_encounter_type as $encounter_type => $measurements) {
    // Making sure execution function exists.
    $function = '_consolidate_' . $encounter_type . '_content_execute';
    if (!function_exists($function)) {
      $return['messages'][] = "ERROR! Mandatory $function() function does not exist.";
      return $return;
    }

    // Making sure validation function exists.
    $function = '_consolidate_' . $encounter_type . '_content_validate';
    if (!function_exists($function)) {
      $return['messages'][] = "ERROR! Mandatory $function() function does not exist.";
      return $return;
    }

    // Running validation function.
    if ($encounter_type == 'session') {
      $function_result = $function($measurements, $original);
    }
    else {
      $function_result = $function($measurements, $original, $duplicate);
    }
    // If consolidation failed, we make no changes, so that a fix could be
    // applied, and command could be executed again.
    if (!$function_result['success']) {
      $return['messages'] = array_merge($return['messages'], $function_result['message']);
      return $return;
    }
  }

  $return['success'] = TRUE;
  $return['messages'][] = 'SUCCESS! All validations passed!';

  return $return;
}

/**
 * Executes consolidation process.
 *
 * All content is relocated from 'duplicate' patient to 'original' patient.
 * Once completed, marks 'duplicate' patient as deleted.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_patients_execute($original, $duplicate) {
  $wrapper_duplicate = entity_metadata_wrapper('node', $duplicate);

  // Get all measurements that belong to duplicate patient.
  $measurements_of_duplicate = hedley_general_get_person_measurements($duplicate);
  $measurements_by_encounter_type = _group_measurements_by_encounter_type($measurements_of_duplicate);

  $messages = [];
  foreach ($measurements_by_encounter_type as $encounter_type => $measurements) {
    $function = '_consolidate_' . $encounter_type . '_content_execute';

    if ($encounter_type == 'session') {
      $messages[] = $function($measurements, $original);
    }
    else {
      $messages[] = $function($measurements, $original, $duplicate);
    }
  }

  // Mark duplicate patient as Deleted.
  $wrapper_duplicate->field_deleted->set(TRUE);
  $wrapper_duplicate->save();

  $messages[] = 'SUCCESS! All consolidations completed!';
  return $messages;
}

/**
 * Performs validations before consolidation of group encounters content.
 *
 * @param array $measurements
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 *
 * @return array
 *   Result and corresponding messages.
 */
function _consolidate_session_content_validate(array $measurements, $original) {
  $return = [
    'success' => FALSE,
    'messages' => [],
  ];

  $count = count($measurements);
  $return['messages'][] = "Duplicate got $count group encounter measurements.";

  $clinics_duplicate = [];
  // Generate a list of Groups to which measurements (of the duplicate) belong.
  foreach ($measurements as $measurement) {
    $wrapper = entity_metadata_wrapper('node', $measurement);
    // Resolve the ID of Group encounter, at which measurement was taken.
    $session = $wrapper->field_session->getIdentifier();
    $wrapper_session = entity_metadata_wrapper('node', $session);
    // Resolve the ID of Group of that Group encounter.
    $clinic = $wrapper_session->field_clinic->getIdentifier();
    // Store the ID of Group.
    if (!in_array($clinic, $clinics_duplicate)) {
      $clinics_duplicate[] = $clinic;
    }
  }

  $list = implode(',', $clinics_duplicate);
  $count = count($clinics_duplicate);
  $return['messages'][] = "Measurements belong to $count clinics: $list";

  $participants_original = hedley_person_pmtct_participants_for_person($original);
  $clinics_original = [];
  // Generate a list of Groups which the 'original' patient attends.
  foreach ($participants_original as $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $clinics_original[] = $wrapper_participant->field_clinic->getIdentifier();
  }

  $list = implode(',', $clinics_original);
  $count = count($clinics_original);
  $return['messages'][] = "Original participates in $count groups: $list";

  // Verify that the 'original' patient participates in all the groups
  // to which measurements of the duplicate belong.
  foreach ($clinics_duplicate as $clinic) {
    if (!in_array($clinic, $clinics_original)) {
      $return['messages'][] = "ERROR! Original is not participating in clinic $clinic. Please fix and try again.";
      return $return;
    }
  }

  $return['success'] = TRUE;

  return $return;
}

/**
 * Performs consolidation of group encounters content.
 *
 * @param array $measurements
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_session_content_execute(array $measurements, $original) {
  _associate_content_by_field($measurements, 'field_person', $original);

  return 'Consolidation of Group encounters content is completed.';
}

/**
 * Performs validations before consolidation of Prenatal encounters content.
 *
 * For now, it only prints some data. No failure option.
 *
 * @param array $measurements
 *   A list of Nutrition measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @return array
 *   Result and corresponding messages.
 */
function _consolidate_prenatal_encounter_content_validate(array $measurements, $original, $duplicate) {
  $return = [
    'success' => TRUE,
    'messages' => [],
  ];

  $count = count($measurements);
  $return['messages'][] = "Duplicate got $count prenatal_encounter measurements.";

  $participants_original = hedley_person_individual_participants_for_person($original, ['antenatal'], 'DESC');
  $count = count($participants_original);
  $return['messages'][] = "Original got $count prenatal_encounter participants.";

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, ['antenatal'], 'DESC');
  $count = count($participants_duplicate);
  $return['messages'][] = "Duplicate got $count prenatal_encounter participants.";

  return $return;
}

/**
 * Performs consolidation of Antenatal encounters content.
 *
 * There may be multiple participants (represent pregnancy) while only one
 * (latest) can be in process. Others are completed (or should
 * be treated as such).
 *
 * Algorithm:
 *
 * 1. Closed participants are associated fully (participant itself, encounters
 *    of that participant, measurements taken at encounters).
 * 2. Ongoing participant:
 *   - If target person has ongoing participant:
 *      - Associate all encounters with target participant.
 *      - Associate measurements of those encounters with target person.
 *      - Mark source participants as deleted.
 *   - If not, associated fully (same as step 1).
 *
 * @param array $measurements
 *   A list of group measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @return string
 *   Success message.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_prenatal_encounter_content_execute(array $measurements, $original, $duplicate) {
  $participants_original = hedley_person_individual_participants_for_person($original, ['antenatal'], 'DESC');
  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, ['antenatal'], 'DESC');

  // Search for ongoing participant (pregnancy) of original patient.
  // If found, record it's ID.
  $participant_original_ongoing = FALSE;
  foreach ($participants_original as $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    if (empty($wrapper_participant->field_date_concluded->value())) {
      $participant_original_ongoing = $participant;
      // There can be only one ongoing pregnancy, therefore, there's not
      // point to continue.
      break;
    }
  }

  // If original patient does not have an ongoing pregnancy, associate
  // participants and measurements of duplicate with the original patient.
  if (!$participant_original_ongoing) {
    _associate_content_by_field($participants_duplicate, 'field_person', $original);
    _associate_content_by_field($measurements, 'field_person', $original);

    return 'Consolidation of Prenatal encounters content is completed.';
  }

  // If we got here, we know that original patient has an ongoing pregnancy.
  // We search for ongoing pregnancy of duplicate patient.
  // If it exists, we want to associate all it's encounters with ongoing
  // pregnancy of original patient.
  $participant_duplicate_ongoing = FALSE;
  foreach ($participants_duplicate as $index => $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    if (empty($wrapper_participant->field_date_concluded->value())) {
      // We found ongoing pregnancy of duplicate patient. We record it and
      // remove from the list of total pregnancies of the duplicate,
      // because it requires different handling.
      $participant_duplicate_ongoing = $participant;
      unset($participants_duplicate[$index]);
      break;
    }
  }

  // If duplicate patient got an ongoing pregnancy, associate all it's
  // encounters with ongoing pregnancy of original patient.
  // Afterwards, mark ongoing pregnancy of duplicate as deleted.
  if ($participant_duplicate_ongoing) {
    $encounters = hedley_person_encounters_for_individual_participant($participant_duplicate_ongoing);
    _associate_content_by_field($encounters, 'field_individual_participant', $participant_original_ongoing);

    $wrapper_participant = entity_metadata_wrapper('node', $participant_duplicate_ongoing);
    $wrapper_participant->field_deleted->set(TRUE);
    $wrapper_participant->save();
  }

  // Associate remaining  participants and measurements of duplicate,
  // with the original patient.
  _associate_content_by_field($participants_duplicate, 'field_person', $original);
  _associate_content_by_field($measurements, 'field_person', $original);

  return 'Consolidation of Prenatal encounters content is completed.';
}

/**
 * Performs validations before consolidation of Nutrition encounters content.
 *
 * @param array $measurements
 *   A list of Nutrition measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_validate()
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_nutrition_encounter_content_validate(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_validate($measurements, $original, $duplicate, 'nutrition_encounter');
}

/**
 * Performs consolidation of Nutrition encounters content.
 *
 * @param array $measurements
 *   A list of Nutrition measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_execute()
 *
 * @return string
 *   Success message.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_nutrition_encounter_content_execute(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_execute($measurements, $original, $duplicate, 'nutrition_encounter');
}

/**
 * Performs validation before consolidation of Home Visit encounters content.
 *
 * @param array $measurements
 *   A list of Home Visit measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_validate()
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_home_visit_encounter_content_validate(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_validate($measurements, $original, $duplicate, 'home_visit_encounter');
}

/**
 * Performs consolidation of Home Visit encounters content.
 *
 * @param array $measurements
 *   A list of Home Visit measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_execute()
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_home_visit_encounter_content_execute(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_execute($measurements, $original, $duplicate, 'home_visit_encounter');
}

/**
 * Performs validations before consolidation of Well Child encounters content.
 *
 * @param array $measurements
 *   A list of Well Child measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_validate()
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_well_child_encounter_content_validate(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_validate($measurements, $original, $duplicate, 'well_child_encounter');
}

/**
 * Performs consolidation of Well Child encounters content.
 *
 * @param array $measurements
 *   A list of Well Child measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_execute()
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_well_child_encounter_content_execute(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_execute($measurements, $original, $duplicate, 'well_child_encounter');
}

/**
 * Performs validations before consolidation of NCD encounters content.
 *
 * @param array $measurements
 *   A list of NCD measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_validate()
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_ncd_encounter_content_validate(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_validate($measurements, $original, $duplicate, 'ncd_encounter');
}

/**
 * Performs consolidation of NCD encounters content.
 *
 * @param array $measurements
 *   A list of NCD measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_execute()
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_ncd_encounter_content_execute(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_execute($measurements, $original, $duplicate, 'ncd_encounter');
}

/**
 * Runs validations before consolidation of Child Scoreboard encounters content.
 *
 * @param array $measurements
 *   Child Scoreboard measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_validate()
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_child_scoreboard_encounter_content_validate(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_validate($measurements, $original, $duplicate, 'child_scoreboard_encounter');
}

/**
 * Performs consolidation of Child Scoreboard encounters content.
 *
 * @param array $measurements
 *   Child Scoreboard measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @see: _consolidate_single_participant_content_execute()
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_child_scoreboard_encounter_content_execute(array $measurements, $original, $duplicate) {
  return _consolidate_single_participant_content_execute($measurements, $original, $duplicate, 'child_scoreboard_encounter');
}

/**
 * Performs validations before consolidation for single participant use cases.
 *
 * This is a case where we have single participant, with multiple encounters.
 *
 * Original patient must not have multiple participants, since we would not
 * know to which of them content should be transferred.
 * Duplicate patient may have multiple participants (though should not, per
 * system design), because all content is being transferred. If measurements
 * exist, it should have at least one participant, because otherwise we can't
 * transfer encounters, to which measurements are associated.
 *
 * @param array $measurements
 *   A list of measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 * @param string $encounter_type
 *   The type of encounter.
 *
 * @return array
 *   Result and corresponding messages.
 *
 * @throws \EntityMetadataWrapperException
 */
function _consolidate_single_participant_content_validate(array $measurements, $original, $duplicate, $encounter_type) {
  $return = [
    'success' => FALSE,
    'messages' => [],
  ];

  $encounter_type_value = HEDLEY_PATIENT_ENCOUNTER_TYPE_VALUE_MAPPING[$encounter_type];
  if (empty($encounter_type_value)) {
    $return['messages'][] = "Error: $encounter_type does not appear in mapping (not supported).";
    return $return;
  }

  $count_measurements = count($measurements);
  $return['messages'][] = "Duplicate got $count_measurements $encounter_type measurements.";

  $participants_original = hedley_person_individual_participants_for_person($original, [$encounter_type_value], 'DESC');
  $count_original = count($participants_original);
  if ($count_original > 1) {
    $return['messages'][] = 'Error: There are multiple participants for original, while only one is allowed. Can not determine with which participant to consolidate.';
    return $return;
  }

  $return['messages'][] = "Original got $count_original $encounter_type participants.";

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, [$encounter_type_value], 'DESC');
  $count_duplicate = count($participants_duplicate);
  if ($count_duplicate == 0 && $count_measurements > 0) {
    $return['messages'][] = "Error: Duplicate got 0 $encounter_type participants. Can not pull encounters data for duplicate.";
    return $return;
  }

  $return['success'] = TRUE;
  return $return;
}

/**
 * Performs consolidation for single participant use cases.
 *
 * Single participant use case, is a case where all encounters belong
 * to single participant.
 * So, in case original got a participant, we associate all encounters
 * of duplicate with that participant. If not, we associate the participant
 * as well.
 * In both cases, we associate all measurements of duplicate with the original.
 *
 * @param array $measurements
 *   A list of measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 * @param string $encounter_type
 *   The type of encounter.
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_single_participant_content_execute(array $measurements, $original, $duplicate, $encounter_type) {
  $encounter_type_value = HEDLEY_PATIENT_ENCOUNTER_TYPE_VALUE_MAPPING[$encounter_type];

  $participants_original = hedley_person_individual_participants_for_person($original, [$encounter_type_value], 'DESC');
  $count_original = count($participants_original);
  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, [$encounter_type_value], 'DESC');

  if ($count_original == 0) {
    // If original patient does not have a participant, associate
    // one (last, if multiple) participant with the original patient.
    $original_participant = array_pop($participants_duplicate);
    // If there's no such participant, we can skip - validation has passed,
    // so there are no more measurements.
    if (empty($original_participant)) {
      return "Consolidation of $encounter_type encounters content is completed.";
    }

    _associate_content_by_field([$original_participant], 'field_person', $original);
  }
  else {
    // We know there is single participant.
    $original_participant = $participants_original[0];
  }

  // Associate all encounters of the duplicate with participant of
  // original patient. The participants of the duplicate are marked as deleted.
  foreach ($participants_duplicate as $participant) {
    $encounters = hedley_person_encounters_for_individual_participant($participant);
    _associate_content_by_field($encounters, 'field_individual_participant', $original_participant);

    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $wrapper_participant->field_deleted->set(TRUE);
    $wrapper_participant->save();
  }

  // Associate all the measurements with the 'original' patient.
  _associate_content_by_field($measurements, 'field_person', $original);

  return "Consolidation of $encounter_type encounters content is completed.";
}

/**
 * Performs validations before consolidation of Acute Illness content.
 *
 * For now, it only prints some data. No failure option.
 *
 * @param array $measurements
 *   A list of Nutrition measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @return bool
 *   True, if validation was successful.
 */
function _consolidate_acute_illness_encounter_content_validate(array $measurements, $original, $duplicate) {
  $return = [
    'success' => TRUE,
    'messages' => [],
  ];

  $count = count($measurements);
  $return['messages'][] = "Duplicate got $count acute_illness measurements.";

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, ['acute_illness'], 'DESC');
  $count = count($participants_duplicate);
  $return['messages'][] = "Duplicate got $count acute_illness Participants.";

  return $return;
}

/**
 * Performs consolidation of Acute Illness encounters content.
 *
 * In this case we allow having multiple participants (illnesses) ongoing
 * in parallel. Therefore, we can only transfer the complete content, without
 * any additional logic.
 *
 * @param array $measurements
 *   A list of Acute Illness measurements belonging to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @return string
 *   Success message.
 *
 * @throws EntityMetadataWrapperException
 */
function _consolidate_acute_illness_encounter_content_execute(array $measurements, $original, $duplicate) {
  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, ['acute_illness'], 'DESC');

  // Associate all  participants and measurements of duplicate,
  // with the original patient.
  _associate_content_by_field($participants_duplicate, 'field_person', $original);
  _associate_content_by_field($measurements, 'field_person', $original);

  return 'Consolidation of Acute Illness encounters content is completed.';
}

/**
 * Groups inputs measurements by the type of encounter to which they belong.
 *
 * Encounter types:
 *   - Group.
 *   - Prenatal.
 *   - Nutrition.
 *   - Acute Illness.
 *   - Home visit.
 *   - Well child.
 *   - NCD.
 *   - Child Scorecard.
 *
 * @param array $measurements
 *   A list of measurements IDs.
 *
 * @return array[]
 *   List of measurements, grouped by encounter type.
 */
function _group_measurements_by_encounter_type(array $measurements) {
  $encounter_types = hedley_general_get_encounter_types();
  $measurements_by_encounter_type = [];
  foreach ($encounter_types as $encounter_type) {
    $measurements_by_encounter_type[$encounter_type] = [];
  }

  // Group the measurements using encounter type indicator field.
  foreach ($measurements as $measurement) {
    $wrapper = entity_metadata_wrapper('node', $measurement);

    foreach ($encounter_types as $encounter_type) {
      if ($wrapper->__isset("field_$encounter_type")) {
        $measurements_by_encounter_type[$encounter_type][] = $measurement;
        break;
      }
    }
  }

  return $measurements_by_encounter_type;
}

/**
 * Makes the association of content, using the 'associating' field.
 *
 * For example, associates measurement with a person, using 'field' person.
 *
 * @param array $content_ids
 *   A list of Node IDs of the content we wish to associate.
 * @param string $field
 *   The 'association' field.
 * @param int $target_id
 *   Node ID to which we associate the content.
 *
 * @throws \EntityMetadataWrapperException
 */
function _associate_content_by_field(array $content_ids, $field, $target_id) {
  foreach ($content_ids as $id) {
    $wrapper = entity_metadata_wrapper('node', $id);
    $wrapper->{$field}->set($target_id);
    $wrapper->save();
  }
}
