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
      $group_measurements[] = $wrapper;
      continue;
    }

    if ($wrapper->__isset('field_prenatal_encounter')) {
      $antenatal_measurements[] = $wrapper;
      continue;
    }

    if ($wrapper->__isset('field_nutrition_encounter')) {
      $nutrition_measurements[] = $wrapper;
      continue;
    }

    if ($wrapper->__isset('field_acute_illness_encounter')) {
      $acute_illness_measurements[] = $wrapper;
    }
  }

  hedley_patient_process_group_measurements($group_measurements, $original);
  hedley_patient_process_antenatal_measurements($antenatal_measurements, $original);


  // Mark duplicate as Deleted.
//  $wrapper_duplicate->field_deleted->set(TRUE);
//  $wrapper_duplicate->save();
}

function hedley_patient_process_group_measurements($wrappers, $original) {
  $count = count($wrappers);
  drush_print("Duplicate got $count group measurements.");

  $clinics_duplicate = [];
  foreach ($wrappers as $wrapper) {
    $session = $wrapper->field_session->getIdentifier();
    if (empty($session)) {
      continue;
    }

    $wrapper_session = entity_metadata_wrapper('node', $session);
    $clinic = $wrapper_session->field_clinic->getIdentifier();
    if (!in_array($clinic, $clinics_duplicate)) {
      $clinics_duplicate[] = $clinic;
    }
  }

  $list = implode(',', $clinics_duplicate);
  $count = count($clinics_duplicate);
  drush_print("Measurements belong to $count clinics: $list");

  $participations_of_original = hedley_person_pmtct_participants_for_person($original);
  $clinics_original = [];
  foreach ($participations_of_original as $participation) {
    $wrapper_participation = entity_metadata_wrapper('node', $participation);
    $clinics_original[] = $wrapper_participation->field_clinic->getIdentifier();
  }

  $list = implode(',', $clinics_original);
  $count = count($clinics_original);
  drush_print("Original participates in $count groups: $list");

  foreach ($clinics_duplicate as $clinic) {
    if (!in_array($clinic, $clinics_original)) {
      drush_print("Error! Original is not participating in clinic $clinic. Please fix and try again.");
      return;
    }
  }

  // If we got this far, it's safe to transfer the measurements.
  hedley_patient_associate_measurements_with_person($wrappers, $original);
}

function hedley_patient_associate_measurements_with_person($wrappers, $person_id) {
  foreach ($wrappers as $wrapper) {
    $wrapper->field_person->set($person_id);
    $wrapper->save();
  }
}

function hedley_patient_process_antenatal_measurements($wrappers_measurements, $original, $duplicate) {
  $count = count($wrappers_measurements);
  drush_print("Duplicate got $count Antenatal measurements.");

  $participants_original = hedley_person_individual_participants_for_person($original, 'antenatal', 'DESC');
  $participants_duplicate = hedley_person_individual_participants_for_person($duplicate, 'antenatal', 'DESC');

  $participant_original_ongoing = FALSE;
  foreach ($participants_original as $participant) {
    $wrapper_participant = entity_metadata_wrapper('node', $participant);
    if (empty($wrapper_participant->field_date_concluded->value())) {
      $participant_original_ongoing = $participant;
      break;
    }
  }

  if (!$participant_original_ongoing) {
    foreach ($participants_duplicate as $participant) {
      $wrapper_participant = entity_metadata_wrapper('node', $participant);
      $wrapper_participant->field_person->set($original);
      $wrapper_participant->save();
    }
  }
  else {
    $participant_duplicate_ongoing = FALSE;
    foreach ($participants_duplicate as $index => $participant) {
      $wrapper_participant = entity_metadata_wrapper('node', $participant);
      if (empty($wrapper_participant->field_date_concluded->value())) {
        $participant_duplicate_ongoing = $participant;
        unset($participants_duplicate[$index]);
        break;
      }
    }

    foreach ($participants_duplicate as $participant) {
      $wrapper_participant = entity_metadata_wrapper('node', $participant);
      $wrapper_participant->field_person->set($original);
      $wrapper_participant->save();
    }

    if ($participant_duplicate_ongoing) {
      $encounters = hedley_person_encounters_for_individual_participant($participant_duplicate_ongoing, 'prenatal_encounter');
      foreach ($encounters as $encounter) {
        $wrapper_encounter = entity_metadata_wrapper('node', $encounter);
        $wrapper_encounter->field_individual_participant->set($participant_original_ongoing);
        $wrapper_encounter->save();
      }

      $wrapper_participant = entity_metadata_wrapper('node', $participant_duplicate_ongoing);
      $wrapper_participant->field_deleted->set(TRUE);
      $wrapper_participant->save();
    }

  }

  hedley_patient_associate_measurements_with_person($wrappers_measurements, $original);
}

function hedley_patient_process_nutrition_measurements($wrappers, $original) {
  $count = count($wrappers);
  drush_print("Duplicate got $count Nutrition measurements.");
}

function hedley_patient_process_acute_illness_measurements($wrappers, $original) {
  $count = count($wrappers);
  drush_print("Duplicate got $count Acute Illness measurements.");
}
