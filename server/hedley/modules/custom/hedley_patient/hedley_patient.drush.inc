<?php

/**
 * @file
 * Drush commands collection.
 */

/**
 * Implements hook_drush_command().
 */
function hedley_patient_drush_command() {
  $items = [];
  $items['consolidate-patients'] = [
    'callback' => 'hedley_patient_consolidate_patients',
    'description' => 'Consolidates data of 2 patient - used when there are duplicate instances of same patient.',
    'options' => [
      'original' => 'Patient to which all data is consolidated.',
      'duplicate' => 'Patient which data is moved to the original patient.'
    ],
    'aliases' => ['cns-ptnts'],
  ];

  return $items;
}

/**
 * Command callback for 'drush consolidate-patients'.
 *
 * Relocates content form the 'duplicate' patient to the 'original' patient.
 * Once completed, marks 'duplicate' patient as deleted.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_patients() {
  $original = drush_get_option('original');
  $duplicate = drush_get_option('duplicate');

  if (empty($original) || empty($duplicate)) {
    drush_print('You have to specify both --original and --duplicate parameter.');
    return;
  }

  $wrapper_original = entity_metadata_wrapper('node', $original);
  $wrapper_duplicate = entity_metadata_wrapper('node', $duplicate);

  if ($wrapper_original->getBundle() !== 'person') {
    drush_print('Error! Original patient node type is not of a person. Please fix and try again.');
    return;
  }

  if ($wrapper_duplicate->getBundle() !== 'person') {
    drush_print('Error! Duplicate patient node type is not of a person. Please fix and try again.');
    return;
  }

  $health_center_original = $wrapper_original->field_health_center->getIdentifier();
  $health_center_duplicate = $wrapper_duplicate->field_health_center->getIdentifier();
  // Do not allow consolidation, if patients are assigned to
  // different health centers.
  if ($health_center_original !== $health_center_duplicate) {
    drush_print('Error! Patients do not belong to same Health Center.');
  }

  // Get all measurements that belong to duplicate patient.
  $measurements_of_duplicate = hedley_general_get_person_measurements($duplicate);
  // If no measurements found, mark duplicate patient as 'Deleted'.
  if (count($measurements_of_duplicate) == 0) {
    drush_print("Duplicate patient got no measurements. Marking it as deleted.");
    $wrapper_duplicate->field_deleted->set(TRUE);
    $wrapper_duplicate->save();
    return;
  }

  $group_measurements = [];
  $antenatal_measurements = [];
  $nutrition_measurements = [];
  $acute_illness_measurements = [];

  // Group the measurements using encounter type indicator field.
  foreach ($measurements_of_duplicate as $measurement) {
    $wrapper = entity_metadata_wrapper('node', $measurement);

    if ($wrapper->__isset('field_session')) {
      $group_measurements[] = $measurement;
      continue;
    }

    if ($wrapper->__isset('field_prenatal_encounter')) {
      $antenatal_measurements[] = $measurement;
      continue;
    }

    if ($wrapper->__isset('field_nutrition_encounter')) {
      $nutrition_measurements[] = $measurement;
      continue;
    }

    if ($wrapper->__isset('field_acute_illness_encounter')) {
      $acute_illness_measurements[] = $measurement;
    }
  }

  // Consolidate group measurements.
  $success = hedley_patient_consolidate_group_content($group_measurements, $original);
  // If consolidation failed, we make no changes, so that a fix could be
  // applied, and command could be executed again.
  if (!$success) {
    return;
  }

  // Consolidate group measurements.
  hedley_patient_consolidate_antenatal_content($antenatal_measurements, $original, $duplicate);
  hedley_patient_consolidate_nutrition_content($nutrition_measurements, $original, $duplicate);
  hedley_patient_consolidate_acute_illness_content($acute_illness_measurements, $original, $duplicate);
  
  // Mark duplicate patient as Deleted.
  $wrapper_duplicate->field_deleted->set(TRUE);
  $wrapper_duplicate->save();
}

/**
 * Performs consolidation of group encounters content.
 *
 * @param array $measurements
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 *
 * @return bool
 *   True, if consolidation was successful.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_group_content(array $measurements, $original) {
  $count = count($measurements);
  drush_print("Duplicate got $count group measurements.");

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
  drush_print("Measurements belong to $count clinics: $list");

  $participants_original = hedley_person_pmtct_participants_for_person($original);
  $clinics_original = [];
  // Generate a list of Groups which the 'original' patient attends.
  foreach ($participants_original as $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $clinics_original[] = $wrapper_participant->field_clinic->getIdentifier();
  }

  $list = implode(',', $clinics_original);
  $count = count($clinics_original);
  drush_print("Original participates in $count groups: $list");

  // Verify that the 'original' patient participates in all the groups
  // to which measurements of the duplicate belong.
  foreach ($clinics_duplicate as $clinic) {
    if (!in_array($clinic, $clinics_original)) {
      drush_print("Error! Original is not participating in clinic $clinic. Please fix and try again.");
      return FALSE;
    }
  }

  // If we got this far, it's safe to transfer the measurements.
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);

  // Report success.
  return TRUE;
}

/**
 * Performs consolidation of Antenatal encounters content.
 *
 * There may be multiple participants (represent pregnancy) while only one
 * (latest) can be in process. Others are completed (or should
 * be treated as such).
 *
 * Algorithm:

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
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_antenatal_content(array $measurements, $original, $duplicate) {
  $count = count($measurements);
  drush_print("Duplicate got $count Antenatal measurements.");

  $participants_original = hedley_person_individual_participants_for_person($original, 'antenatal', 'DESC');
  $count = count($participants_original);
  drush_print("Original got $count Participants.");

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, 'antenatal', 'DESC');
  $count = count($participants_duplicate);
  drush_print("Duplicate got $count Participants.");

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
    drush_print('Original ongoing Participant NOT FOUND.');

    hedley_patient_associate_content_by_field($participants_duplicate, 'field_person', $original);
    hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
    return;
  }

  drush_print("Original ongoing Participant: $participant_original_ongoing.");

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
    drush_print("Duplicate ongoing Participant: $participant_duplicate_ongoing.");

    $encounters = hedley_person_encounters_for_individual_participant($participant_duplicate_ongoing);
    hedley_patient_associate_content_by_field($encounters, 'field_individual_participant', $participant_original_ongoing);

    $wrapper_participant = entity_metadata_wrapper('node', $participant_duplicate_ongoing);
    $wrapper_participant->field_deleted->set(TRUE);
    $wrapper_participant->save();
  }
  else {
    drush_print('Duplicate ongoing Participant NOT FOUND.');
  }

  // Associate remaining  participants and measurements of duplicate,
  // with the original patient.
  hedley_patient_associate_content_by_field($participants_duplicate, 'field_person', $original);
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
}

/**
 * Performs consolidation of Nutrition encounters content.
 *
 * This is a case where we have single participant, with multiple encounters.
 * So, in case original got a participant, we transfer all associate encounters
 * of duplicate with that participant. If not, we associate the participant
 * as well.

 * In both cases, we associate all measurements of duplicate with the original.
 *
 * @param array $measurements
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_nutrition_content(array $measurements, $original, $duplicate) {
  $count = count($measurements);
  drush_print("Duplicate got $count Nutrition measurements.");

  $participants_original = hedley_person_individual_participants_for_person($original, 'nutrition', 'DESC');
  $count = count($participants_original);
  $list = implode(',', $participants_original);
  drush_print("Original got $count Participants: $list");

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, 'nutrition', 'DESC');
  $count = count($participants_duplicate);
  $list = implode(',', $participants_duplicate);
  drush_print("Duplicate got $count Participants: $list");

  // Search for ongoing participant of original patient.
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

  // If original patient does not have an ongoing participant, associate
  // participants and measurements of duplicate with the original patient.
  if (!$participant_original_ongoing) {
    hedley_patient_associate_content_by_field($participants_duplicate, 'field_person', $original);
    hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
    return;
  }

  // If we got here, we know that original patient has an ongoing participant.
  // We associate all encounters of the duplicate with it.
  // The participants of the duplicate are marked as deleted.
  foreach ($participants_duplicate as $participant) {
    $encounters = hedley_person_encounters_for_individual_participant($participant);
    hedley_patient_associate_content_by_field($encounters, 'field_individual_participant', $participant_original_ongoing);

    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $wrapper_participant->field_deleted->set(TRUE);
    $wrapper_participant->save();
  }

  // Associate all the measurements with the 'original' patient.
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
}

/**
 * Performs consolidation of Acute Illness encounters content.
 *
 * In this case we allow having multiple participants (illnesses) ongoing
 * in parallel. Therefore, we can only transfer the complete content, without
 * any additional logic.
 *
 * @param array $measurements
 *   A list of group measurements that belong to the 'duplicate' patient.
 * @param int $original
 *   Node ID of the 'original' patient.
 * @param int $duplicate
 *   Node ID of the 'duplicate' patient.
 *
 * @throws \EntityMetadataWrapperException
 */
function hedley_patient_consolidate_acute_illness_content(array $measurements, $original, $duplicate) {
  $count = count($measurements);
  drush_print("Duplicate got $count Acute Illness measurements.");

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, 'acute_illness', 'DESC');
  $count = count($participants_duplicate);
  drush_print("Duplicate got $count Participants.");

  // Associate all  participants and measurements of duplicate,
  // with the original patient.
  hedley_patient_associate_content_by_field($participants_duplicate, 'field_person', $original);
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
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
function hedley_patient_associate_content_by_field($content_ids, $field, $target_id) {
  foreach ($content_ids as $id) {
    $wrapper = entity_metadata_wrapper('node', $id);
    $wrapper->{$field}->set($target_id);
    $wrapper->save();
  }
}
