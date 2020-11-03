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

  $measurements_of_duplicate = hedley_general_get_person_measurements($duplicate);
  if (count($measurements_of_duplicate) == 0) {
    drush_print("Duplicate got no measurements. Marking it as deleted.");
    $wrapper_duplicate->field_deleted->set(TRUE);
    $wrapper_duplicate->save();
    return;
  }

  $group_measurements = [];
  $antenatal_measurements = [];
  $nutrition_measurements = [];
  $acute_illness_measurements = [];

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

function hedley_patient_consolidate_group_content($measurements, $original) {
  $count = count($measurements);
  drush_print("Duplicate got $count group measurements.");

  $clinics_duplicate = [];
  foreach ($measurements as $measurement) {
    $wrapper = entity_metadata_wrapper('node', $measurement);
    $session = $wrapper->field_session->getIdentifier();
    $wrapper_session = entity_metadata_wrapper('node', $session);
    $clinic = $wrapper_session->field_clinic->getIdentifier();
    if (!in_array($clinic, $clinics_duplicate)) {
      $clinics_duplicate[] = $clinic;
    }
  }

  $list = implode(',', $clinics_duplicate);
  $count = count($clinics_duplicate);
  drush_print("Measurements belong to $count clinics: $list");

  $participants_original = hedley_person_pmtct_participants_for_person($original);
  $clinics_original = [];
  foreach ($participants_original as $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $clinics_original[] = $wrapper_participant->field_clinic->getIdentifier();
  }

  $list = implode(',', $clinics_original);
  $count = count($clinics_original);
  drush_print("Original participates in $count groups: $list");

  foreach ($clinics_duplicate as $clinic) {
    if (!in_array($clinic, $clinics_original)) {
      drush_print("Error! Original is not participating in clinic $clinic. Please fix and try again.");
      return FALSE;
    }
  }

  // If we got this far, it's safe to transfer the measurements.
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
  
  return TRUE;
}

function hedley_patient_consolidate_antenatal_content($measurements, $original, $duplicate) {
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

    $encounters = hedley_person_encounters_for_individual_participant($participant_duplicate_ongoing, 'prenatal_encounter');
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

function hedley_patient_consolidate_nutrition_content($measurements, $original, $duplicate) {
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
    $encounters = hedley_person_encounters_for_individual_participant($participant, 'nutrition_encounter');
    hedley_patient_associate_content_by_field($encounters, 'field_individual_participant', $participant_original_ongoing);

    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    $wrapper_participant->field_deleted->set(TRUE);
    $wrapper_participant->save();
  }
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
}

function hedley_patient_consolidate_acute_illness_content($measurements, $original, $duplicate) {
  $count = count($measurements);
  drush_print("Duplicate got $count Acute Illness measurements.");

  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, 'acute_illness', 'DESC');
  $count = count($participants_duplicate);
  drush_print("Duplicate got $count Participants.");

  hedley_patient_associate_content_by_field($participants_duplicate, 'field_person', $original);
  hedley_patient_associate_content_by_field($measurements, 'field_person', $original);
}

function hedley_patient_associate_content_by_field($content_ids, $field, $target_id) {
  foreach ($content_ids as $id) {
    $wrapper = entity_metadata_wrapper('node', $id);
    $wrapper->{$field}->set($target_id);
    $wrapper->save();
  }
}
