<?php

/**
 * @file
 * Standalone tests of the Acute Illness completion rules.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * The split cases match the app's splitByInitialNurseEncounter tests
 * (Pages/AcuteIllness/Activity/Test.elm).
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Builds encounter data with the given measurements.
 */
function encounter($id, $type, array $measurements_by_type = []) {
  return [
    'id' => $id,
    'encounter_type' => $type,
    'measurements_by_type' => $measurements_by_type,
  ];
}

/**
 * Builds a Malaria test measurement with the given result.
 */
function malaria_test($result) {
  $malaria_testing = new stdClass();
  $malaria_testing->field_malaria_rapid_test[LANGUAGE_NONE][0]['value'] = $result;
  return ['malaria_testing' => $malaria_testing];
}

/**
 * Builds a medication distribution measurement with the given medications.
 */
function medication_prescribed(array $medications) {
  $medication_distribution = new stdClass();
  $medication_distribution->field_prescribed_medication[LANGUAGE_NONE] = array_map(function ($medication) {
    return ['value' => $medication];
  }, $medications);
  return ['medication_distribution' => $medication_distribution];
}

/**
 * Returns the IDs of the encounters the follow-up rules read.
 */
function initial_with_subsequents_ids(array $encounters) {
  return array_column(hedley_reports_acute_illness_initial_with_subsequents($encounters), 'id');
}

/**
 * Tells whether a follow-up with fever expects an activity.
 */
function follow_up_expects($activity, array $previous_encounters, $diagnosis = NULL) {
  $encounter_data = [
    'fever' => TRUE,
    'measurements_by_type' => [],
    'illness_diagnosis' => $diagnosis,
    'is_initial' => FALSE,
    'start_date_obj' => new DateTime('2024-01-14'),
    'initial_with_subsequents' => hedley_reports_acute_illness_initial_with_subsequents($previous_encounters),
  ];
  $expected = [];
  hedley_reports_acute_illness_add_expected_malaria_testing($encounter_data, $expected);
  hedley_reports_acute_illness_add_expected_treatment_ongoing($encounter_data, $expected);
  return in_array($activity, $expected);
}

// Which earlier encounters the follow-up rules read.
assert_same(initial_with_subsequents_ids([
  encounter('chw1', 'chw-encounter'),
  encounter('chw2', 'chw-encounter'),
]), ['chw1', 'chw2'], 'CHW encounters are all read', $passed, $failed);

assert_same(initial_with_subsequents_ids([
  encounter('nurse', 'nurse-encounter'),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), ['nurse', 'nurseSubsequent'], 'when nurse ran the first encounter, all encounters are read', $passed, $failed);

assert_same(initial_with_subsequents_ids([
  encounter('chw1', 'chw-encounter'),
  encounter('chw2', 'chw-encounter'),
  encounter('nurse', 'nurse-encounter'),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), ['nurse', 'nurseSubsequent'], 'when nurse took over, encounters from the nurse encounter on are read', $passed, $failed);

assert_same(initial_with_subsequents_ids([
  encounter('chw1', 'chw-encounter'),
  encounter('nurse', 'nurse-encounter'),
]), ['nurse'], 'when nurse took over at the last encounter, only that encounter is read', $passed, $failed);

assert_same(initial_with_subsequents_ids([]), [], 'no encounters', $passed, $failed);

// Malaria test at a follow-up with fever.
assert_same(follow_up_expects('malaria_testing', [
  encounter('nurse', 'nurse-encounter', malaria_test('positive')),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), FALSE, 'no Malaria test after a positive one at the initial nurse encounter', $passed, $failed);

assert_same(follow_up_expects('malaria_testing', [
  encounter('nurse', 'nurse-encounter', malaria_test('negative')),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), TRUE, 'a Malaria test after a negative one at the initial nurse encounter', $passed, $failed);

assert_same(follow_up_expects('malaria_testing', [
  encounter('chw', 'chw-encounter', malaria_test('positive-and-pregnant')),
]), FALSE, 'no Malaria test after a positive one at a CHW encounter', $passed, $failed);

assert_same(follow_up_expects('malaria_testing', [
  encounter('chw', 'chw-encounter', malaria_test('negative')),
]), TRUE, 'a Malaria test after a negative one', $passed, $failed);

assert_same(follow_up_expects('malaria_testing', [
  encounter('chw', 'chw-encounter', malaria_test('negative')),
], 'covid19-low-risk'), FALSE, 'no Malaria test when COVID was diagnosed', $passed, $failed);

assert_same(follow_up_expects('malaria_testing', [
  encounter('chw', 'chw-encounter', malaria_test('positive')),
  encounter('nurse', 'nurse-encounter'),
]), TRUE, 'a Malaria test when the positive one was before nurse took over', $passed, $failed);

// Ongoing Treatment at a follow-up.
assert_same(follow_up_expects('treatment_ongoing', [
  encounter('nurse', 'nurse-encounter', medication_prescribed(['coartem'])),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), TRUE, 'Ongoing Treatment after medication prescribed at the initial nurse encounter', $passed, $failed);

assert_same(follow_up_expects('treatment_ongoing', [
  encounter('nurse', 'nurse-encounter', medication_prescribed(['lemon-juice-or-honey'])),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), FALSE, 'no Ongoing Treatment when only lemon juice or honey was given', $passed, $failed);

assert_same(follow_up_expects('treatment_ongoing', [
  encounter('chw', 'chw-encounter', medication_prescribed(['coartem'])),
  encounter('nurse', 'nurse-encounter'),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), FALSE, 'no Ongoing Treatment when medication was prescribed before nurse took over', $passed, $failed);

report_results('Acute Illness completion', $passed, $failed);
