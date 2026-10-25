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

$passed = 0;
$failed = 0;

/**
 * Compares a result with the expected value and records the outcome.
 */
function assert_same($result, $expected, $case, &$passed, &$failed) {
  if ($result === $expected) {
    $passed++;
    return;
  }
  $failed++;
  echo "FAIL: $case\n  expected: " . var_export($expected, TRUE) . "\n  got: " . var_export($result, TRUE) . "\n";
}

/**
 * Builds encounter data, with a Malaria test result when one is given.
 */
function encounter($id, $type, $malaria_result = NULL) {
  $measurements_by_type = [];
  if (!empty($malaria_result)) {
    $malaria_testing = new stdClass();
    $malaria_testing->field_malaria_rapid_test[LANGUAGE_NONE][0]['value'] = $malaria_result;
    $measurements_by_type['malaria_testing'] = $malaria_testing;
  }

  return [
    'id' => $id,
    'encounter_type' => $type,
    'measurements_by_type' => $measurements_by_type,
  ];
}

/**
 * Returns the IDs of the encounters the follow-up rules read.
 */
function initial_with_subsequents_ids(array $encounters) {
  return array_column(hedley_reports_acute_illness_initial_with_subsequents($encounters), 'id');
}

/**
 * Tells whether a follow-up with fever expects a Malaria test.
 */
function follow_up_expects_malaria_test(array $previous_encounters) {
  $encounter_data = [
    'fever' => TRUE,
    'measurements_by_type' => [],
    'illness_diagnosis' => NULL,
    'is_initial' => FALSE,
    'initial_with_subsequents' => hedley_reports_acute_illness_initial_with_subsequents($previous_encounters),
  ];
  $expected = [];
  hedley_reports_acute_illness_add_expected_malaria_testing($encounter_data, $expected);
  return in_array('malaria_testing', $expected);
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
assert_same(follow_up_expects_malaria_test([
  encounter('nurse', 'nurse-encounter', 'positive'),
  encounter('nurseSubsequent', 'nurse-encounter-subsequent'),
]), FALSE, 'no Malaria test after a positive one at the initial nurse encounter', $passed, $failed);

assert_same(follow_up_expects_malaria_test([
  encounter('chw', 'chw-encounter', 'positive-and-pregnant'),
]), FALSE, 'no Malaria test after a positive one at a CHW encounter', $passed, $failed);

assert_same(follow_up_expects_malaria_test([
  encounter('chw', 'chw-encounter', 'negative'),
]), TRUE, 'a Malaria test after a negative one', $passed, $failed);

assert_same(follow_up_expects_malaria_test([
  encounter('chw', 'chw-encounter', 'positive'),
  encounter('nurse', 'nurse-encounter'),
]), TRUE, 'a Malaria test when the positive one was before nurse took over', $passed, $failed);

$total = $passed + $failed;
echo "\n";
echo "Acute Illness completion tests: $total total, $passed passed, $failed failed.\n";

if ($failed > 0) {
  echo "FAILED\n";
  exit(1);
}

echo "OK\n";
exit(0);
