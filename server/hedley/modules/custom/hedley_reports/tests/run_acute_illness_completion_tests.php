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

/**
 * Builds a symptoms measurement: the named symptoms last 2 days, others 0.
 *
 * Every period field of the measurement is stored, as the app stores them.
 */
function symptoms(array $fields, array $present) {
  $measurement = new stdClass();
  foreach ($fields as $field) {
    $measurement->{$field}[LANGUAGE_NONE][0]['value'] = in_array($field, $present) ? 2 : 0;
  }
  return $measurement;
}

/**
 * Builds the measurements of an initial encounter with the given symptoms.
 *
 * $malaria_result is the Malaria test result, or NULL when no test was run.
 */
function symptom_measurements(array $general, array $respiratory, array $gi, $malaria_result) {
  $measurements_by_type = [
    'symptoms_general' => symptoms([
      'field_fever_period',
      'field_chills_period',
      'field_night_sweats_period',
      'field_body_aches_period',
      'field_headache_period',
    ], $general),
    'symptoms_respiratory' => symptoms([
      'field_cough_period',
      'field_shortness_of_breath_period',
      'field_nasal_congestion_period',
      'field_blood_in_sputum_period',
      'field_sore_throat_period',
      'field_loss_of_smell_period',
      'field_stabbing_chest_pain_period',
    ], $respiratory),
    'symptoms_gi' => symptoms([
      'field_bloody_diarrhea_period',
      'field_non_bloody_diarrhea_period',
      'field_nausea_period',
      'field_vomiting_period',
      'field_abdominal_pain_period',
    ], $gi),
  ];
  if ($malaria_result !== NULL) {
    $measurements_by_type += malaria_test($malaria_result);
  }
  return $measurements_by_type;
}

/**
 * Builds the data of an initial Acute Illness encounter.
 *
 * No exposure or travel history is recorded, so no COVID sign is present.
 */
function initial_encounter_data(array $measurements_by_type, $fever, $taken_by = 'nurse', $diagnosis = 'none') {
  return [
    'taken_by' => $taken_by,
    'start_date_obj' => new DateTime('2024-01-14'),
    'fever' => $fever,
    'measurements_by_type' => $measurements_by_type,
    'is_initial' => TRUE,
    'age_in_months' => 24,
    'illness_diagnosis' => $diagnosis,
    'initial_with_subsequents' => [],
  ];
}

/**
 * Runs a function, and returns the PHP errors it raised.
 */
function errors_raised(callable $run) {
  $errors = [];
  set_error_handler(function ($errno, $message) use (&$errors) {
    $errors[] = $message;
    return TRUE;
  });
  $run();
  restore_error_handler();
  return $errors;
}

/**
 * Returns the "Undefined variable" errors among the given ones.
 */
function undefined_variable_errors(array $errors) {
  return array_values(array_filter($errors, function ($message) {
    return strpos($message, 'Undefined variable') !== FALSE;
  }));
}

/**
 * Tells whether the encounter expects COVID testing.
 */
function covid_testing_expected(array $encounter_data) {
  $expected = [];
  errors_raised(function () use ($encounter_data, &$expected) {
    hedley_reports_acute_illness_add_expected_covid_testing($encounter_data, $expected);
  });
  return in_array('covid_testing', $expected);
}

/**
 * Returns the "Undefined variable" errors raised by the COVID testing rule.
 */
function covid_testing_undefined_variables(array $encounter_data) {
  return undefined_variable_errors(errors_raised(function () use ($encounter_data) {
    $expected = [];
    hedley_reports_acute_illness_add_expected_covid_testing($encounter_data, $expected);
  }));
}

/**
 * Returns the "Undefined variable" errors raised by every expectation rule.
 *
 * The rules are called in the order the completion data generation calls
 * them.
 */
function expectation_rules_undefined_variables(array $encounter_data) {
  return undefined_variable_errors(errors_raised(function () use ($encounter_data) {
    $expected = hedley_reports_acute_illness_generate_expected_initial_activities($encounter_data);
    hedley_reports_acute_illness_add_expected_malaria_testing($encounter_data, $expected);
    hedley_reports_acute_illness_add_expected_covid_testing($encounter_data, $expected);
    hedley_reports_acute_illness_add_expected_treatment_ongoing($encounter_data, $expected);
    hedley_reports_acute_illness_add_physical_exam_activities($encounter_data, $expected);
    hedley_reports_acute_illness_add_next_steps_activities($encounter_data, $expected);
  }));
}

// COVID testing at a nurse encounter: fever, a malaria test that isn't
// positive and two or more general symptoms, with a GI symptom recorded.
// The app asks for the COVID test here; no respiratory symptom is recorded,
// so only the general symptoms can make the test expected.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_fever_period', 'field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE)), TRUE, 'fever, a malaria test that is not positive and two or more of chills, night sweats, body aches and headache make COVID testing expected, also when a GI symptom is recorded', $passed, $failed);

// The same, with three general symptoms, another GI symptom, and a malaria
// test result that is neither positive nor negative.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  [
    'field_night_sweats_period',
    'field_body_aches_period',
    'field_headache_period',
  ],
  [],
  ['field_non_bloody_diarrhea_period', 'field_vomiting_period'],
  'indeterminate'
), TRUE)), TRUE, 'with a GI symptom, three general symptoms, fever and an indeterminate malaria test make COVID testing expected', $passed, $failed);

// One general symptom is not enough, with or without a GI symptom.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period'],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE)), FALSE, 'with a GI symptom, a single general symptom does not make COVID testing expected', $passed, $failed);

// Without fever, general symptoms do not make COVID testing expected.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'negative'
), FALSE)), FALSE, 'with a GI symptom and two general symptoms but no fever, COVID testing is not expected', $passed, $failed);

// A positive malaria test, in either of its stored forms.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'positive'
), TRUE)), FALSE, 'with a GI symptom, two general symptoms and fever, a positive malaria test means COVID testing is not expected', $passed, $failed);

assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'positive-and-pregnant'
), TRUE)), FALSE, 'with a GI symptom, two general symptoms and fever, a positive-and-pregnant malaria test means COVID testing is not expected', $passed, $failed);

// No malaria test was recorded at all.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  NULL
), TRUE)), FALSE, 'with a GI symptom, two general symptoms and fever but no malaria test, COVID testing is not expected', $passed, $failed);

// COVID testing is a nurse activity.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE, 'chw')), FALSE, 'at a CHW encounter, COVID testing is not expected', $passed, $failed);

// Without a GI symptom, the general symptoms rule applies today.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  [],
  'negative'
), TRUE)), TRUE, 'without a GI symptom, fever, a negative malaria test and two general symptoms make COVID testing expected', $passed, $failed);

// GI and respiratory symptoms with fever, as today.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  [],
  ['field_cough_period'],
  ['field_nausea_period'],
  'negative'
), TRUE)), TRUE, 'with a GI symptom, a respiratory symptom, fever and a malaria test that is not positive, COVID testing is expected', $passed, $failed);

assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  ['field_sore_throat_period'],
  ['field_abdominal_pain_period'],
  'negative'
), TRUE)), TRUE, 'with a GI symptom, a respiratory symptom, two general symptoms, fever and a negative malaria test, COVID testing is expected', $passed, $failed);

assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  [],
  ['field_cough_period'],
  ['field_nausea_period'],
  'positive'
), TRUE)), FALSE, 'with a GI symptom, a respiratory symptom and fever, a positive malaria test means COVID testing is not expected', $passed, $failed);

// No "Undefined variable" notice. With a GI symptom, the COVID testing rule
// is where such a notice came from, once fever and a malaria test that isn't
// positive are recorded and no respiratory symptom is. The whole set of
// expectation rules is also checked, in the order the generation calls them.
assert_same(covid_testing_undefined_variables(initial_encounter_data(symptom_measurements(
  [],
  ['field_cough_period'],
  ['field_nausea_period'],
  'negative'
), TRUE)), [], 'no Undefined variable notice from the COVID testing rule with a GI symptom and a respiratory symptom', $passed, $failed);

assert_same(covid_testing_undefined_variables(initial_encounter_data(symptom_measurements(
  ['field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE)), [], 'generating Acute Illness completion data logs no Undefined variable notice', $passed, $failed);

assert_same(expectation_rules_undefined_variables(initial_encounter_data(symptom_measurements(
  ['field_fever_period', 'field_chills_period', 'field_headache_period'],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE, 'nurse', 'covid19-low-risk')), [], 'no Undefined variable notice from any expectation rule for an encounter with a GI symptom', $passed, $failed);

assert_same(expectation_rules_undefined_variables(initial_encounter_data(symptom_measurements(
  ['field_fever_period', 'field_chills_period', 'field_headache_period'],
  [],
  [],
  'negative'
), TRUE, 'nurse', 'covid19-low-risk')), [], 'no Undefined variable notice from any expectation rule for an encounter without a GI symptom', $passed, $failed);

// A GI symptom with fever and a negative malaria test, but no general or
// respiratory symptom: COVID testing is not expected, and no notice either.
assert_same(covid_testing_expected(initial_encounter_data(symptom_measurements(
  [],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE)), FALSE, 'with a GI symptom, fever and a negative malaria test but no general or respiratory symptom, COVID testing is not expected', $passed, $failed);

assert_same(covid_testing_undefined_variables(initial_encounter_data(symptom_measurements(
  [],
  [],
  ['field_nausea_period'],
  'negative'
), TRUE)), [], 'no Undefined variable notice from the COVID testing rule with a GI symptom and no other symptom', $passed, $failed);

report_results('Acute Illness completion', $passed, $failed);
