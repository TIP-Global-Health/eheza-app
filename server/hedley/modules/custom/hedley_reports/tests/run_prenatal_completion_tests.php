<?php

/**
 * @file
 * Standalone tests of the Prenatal completion rules for CHW Next Steps.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Builds a danger signs measurement with the given signs.
 */
function danger_signs(array $signs) {
  $measurement = new stdClass();
  $measurement->field_danger_signs[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $signs);
  return $measurement;
}

/**
 * Returns the activities expected of a CHW encounter with given measurements.
 *
 * @param string $type
 *   The encounter type.
 * @param array $activities
 *   The activities recorded at the encounter, keyed by name.
 * @param array $expected
 *   The activities expected before Health Education and Next Steps are added.
 * @param array $previous_nurse_encounters
 *   The nurse encounters before this one.
 */
function chw_expected($type, array $activities, array $expected = ['danger_signs'], array $previous_nurse_encounters = []) {
  $encounter_data = [
    'encounter_type' => $type,
    'measurements_by_type' => $activities,
    'previous_nurse_encounters_data' => $previous_nurse_encounters,
    'start_date_obj' => new DateTime('2026-01-01'),
  ];
  hedley_reports_prenatal_add_health_education_activity_for_chw($encounter_data, $expected);
  hedley_reports_prenatal_add_next_steps_activities($encounter_data, $expected);
  return $expected;
}

/**
 * Tells which of the given activities are expected.
 */
function expected_among(array $expected, array $activities) {
  return array_values(array_intersect($activities, $expected));
}

$next_steps = [
  'appointment_confirmation',
  'prenatal_send_to_hc',
  'prenatal_follow_up',
];
$recorded = new stdClass();
$no_signs = danger_signs(['none']);
$with_signs = danger_signs(['vaginal-bleeding']);

assert_same(
  expected_among(chw_expected('chw-1', ['danger_signs' => $no_signs]), $next_steps),
  [],
  'chw-1, no danger signs, no Health Education: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ]), $next_steps),
  ['appointment_confirmation', 'prenatal_follow_up'],
  'chw-1, no danger signs, Health Education done: Appointment Confirmation and Follow Up',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', ['danger_signs' => $with_signs]), $next_steps),
  ['prenatal_send_to_hc', 'prenatal_follow_up'],
  'chw-1, danger signs, no Health Education: Send to HC and Follow Up',
  $passed,
  $failed
);

assert_same(
  // The rules read a missing danger signs record as an undefined index.
  expected_among(@chw_expected('chw-1', []), $next_steps),
  [],
  'chw-1, danger signs not recorded: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'last_menstrual_period', 'pregnancy_testing']), $next_steps),
  [],
  'chw-1, first visit without LMP and pregnancy testing: no Next Steps',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-1', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'last_menstrual_period'], [['id' => 1]]), $next_steps),
  ['appointment_confirmation', 'prenatal_follow_up'],
  'chw-1 after a nurse visit: a missing LMP does not hold Next Steps back',
  $passed,
  $failed
);

assert_same(
  expected_among(chw_expected('chw-2', [
    'danger_signs' => $no_signs,
    'prenatal_health_education' => $recorded,
  ], ['danger_signs', 'birth_plan']), $next_steps),
  [],
  'chw-2, no danger signs, no Birth Plan: no Next Steps',
  $passed,
  $failed
);

foreach (['chw-2', 'chw-3'] as $type) {
  $has_birth_plan = $type == 'chw-2';
  $base_expected = $has_birth_plan ? ['danger_signs', 'birth_plan'] : ['danger_signs'];
  $base_activities = $has_birth_plan ? ['birth_plan' => $recorded] : [];

  assert_same(
    expected_among(chw_expected($type, $base_activities + ['danger_signs' => $no_signs], $base_expected), $next_steps),
    [],
    "$type, no danger signs, no Health Education: no Next Steps",
    $passed,
    $failed
  );

  assert_same(
    expected_among(chw_expected($type, $base_activities + [
      'danger_signs' => $no_signs,
      'prenatal_health_education' => $recorded,
    ], $base_expected), $next_steps),
    ['appointment_confirmation', 'prenatal_follow_up'],
    "$type, no danger signs, Health Education done: Appointment Confirmation and Follow Up",
    $passed,
    $failed
  );

  assert_same(
    expected_among(chw_expected($type, ['danger_signs' => $with_signs]), $next_steps),
    ['prenatal_send_to_hc', 'prenatal_follow_up'],
    "$type, danger signs: Send to HC and Follow Up",
    $passed,
    $failed
  );
}

report_results('Prenatal completion', $passed, $failed);
