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

  hedley_patient_preocess_group_measurements($group_measurements, $original);

  // Mark duplicate as Deleted.
//  $wrapper_duplicate->field_deleted->set(TRUE);
//  $wrapper_duplicate->save();
}

function hedley_patient_preocess_group_measurements($wrappers, $original) {
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
  foreach ($group_measurements as $wrapper) {
    $wrapper->field_person->set($original);
    $wrapper->save();
  }
}

