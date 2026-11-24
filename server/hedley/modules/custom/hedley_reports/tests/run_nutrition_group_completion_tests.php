<?php

/**
 * @file
 * Standalone tests of the Nutrition Group completion rules for mothers.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * From 13 Oct 2024 (release 1.13.0) the app offers Mother FBF to every
 * breastfeeding mother at an FBF or ACHI group; before that, only to a
 * breastfeeding mother whose Ubudehe is 1 or 2.
 */

define('LANGUAGE_NONE', 'und');

require_once dirname(__DIR__) . '/hedley_reports.module';
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * The mapping the group completion data uses for mother activities.
 */
const MOTHER_MAPPING = [
  'family_planning' => 'a',
  'mother_fbf' => 'b',
  'lactation' => 'c',
];

/**
 * Builds a mother's person node with the given Ubudehe.
 *
 * NULL builds the shape node_load gives when no Ubudehe is recorded. Values
 * are strings, as node_load gives them.
 */
function mother($ubudehe) {
  $person = new stdClass();
  $person->field_ubudehe = $ubudehe === NULL ? [] : [LANGUAGE_NONE => [['value' => $ubudehe]]];
  return $person;
}

/**
 * Builds a Lactation measurement with the given signs.
 */
function lactation(array $signs) {
  $measurement = new stdClass();
  $measurement->field_lactation_signs[LANGUAGE_NONE] = array_map(function ($sign) {
    return ['value' => $sign];
  }, $signs);
  return $measurement;
}

/**
 * Builds a measurement node that only needs to exist.
 */
function measurement() {
  return new stdClass();
}

/**
 * Returns the mother's expected activities, ignoring PHP notices.
 */
function expected_activities($person, array $measurements_by_type, $clinic_type, $date) {
  set_error_handler(function () {
    return TRUE;
  });
  $expected = hedley_reports_nutrition_group_mother_expected_activities($person, $measurements_by_type, $clinic_type, new DateTime($date));
  restore_error_handler();
  return $expected;
}

/**
 * Tells whether Mother FBF is expected for the mother.
 */
function mother_fbf_expected($person, array $measurements_by_type, $clinic_type, $date) {
  return in_array('mother_fbf', expected_activities($person, $measurements_by_type, $clinic_type, $date));
}

/**
 * Tells whether Mother FBF counts as expected, and as completed.
 *
 * Returns [expected, completed], read from the stored completion string.
 */
function mother_fbf_completion($person, array $measurements_by_type, $clinic_type, $date) {
  $expected = expected_activities($person, $measurements_by_type, $clinic_type, $date);
  $result = hedley_reports_generate_completion_result($expected, $measurements_by_type, MOTHER_MAPPING);
  list($expected_part, $completed_part) = explode('|', $result);
  $code = MOTHER_MAPPING['mother_fbf'];
  return [
    in_array($code, explode(',', $expected_part)),
    in_array($code, explode(',', $completed_part)),
  ];
}

$breastfeeding = ['lactation' => lactation(['breastfeeding'])];

// From 13 Oct 2024, every breastfeeding mother at an FBF or ACHI group.
foreach (['fbf', 'achi'] as $clinic_type) {
  foreach (['1', '2', '3', '4', '0'] as $ubudehe) {
    assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, $clinic_type, '2024-10-13'), TRUE, "from 13 Oct 2024, a breastfeeding mother at an $clinic_type group with Ubudehe $ubudehe is expected to receive Mother FBF", $passed, $failed);
  }
}

assert_same(mother_fbf_expected(mother('3'), $breastfeeding, 'fbf', '2025-03-01'), TRUE, 'Nutrition Group sessions from 13 Oct 2024: a breastfeeding mother at an FBF or ACHI group is expected to receive Mother FBF, whatever her Ubudehe', $passed, $failed);

assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, 'fbf', '2024-10-13'), TRUE, 'Nutrition Group sessions from 13 Oct 2024: a breastfeeding mother at an FBF or ACHI group is expected to receive Mother FBF, including none recorded', $passed, $failed);

assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, 'achi', '2025-03-01'), TRUE, 'from 13 Oct 2024, a breastfeeding mother at an ACHI group with no Ubudehe recorded is expected to receive Mother FBF', $passed, $failed);

// The Lactation signs field holds several values; the app looks for
// breastfeeding among them, wherever it is stored.
assert_same(mother_fbf_expected(mother('3'), ['lactation' => lactation(['none', 'breastfeeding'])], 'fbf', '2024-10-13'), TRUE, 'from 13 Oct 2024, a mother whose Lactation signs include breastfeeding after another value is expected to receive Mother FBF', $passed, $failed);

// The other activities stay as they are on the new rule.
assert_same(expected_activities(mother('4'), $breastfeeding, 'fbf', '2024-10-13'), [
  'family_planning',
  'lactation',
  'mother_fbf',
], 'from 13 Oct 2024, a breastfeeding mother at an FBF group is expected Family Planning, Lactation and Mother FBF', $passed, $failed);

// Before 13 Oct 2024, only Ubudehe 1 or 2.
foreach (['fbf', 'achi'] as $clinic_type) {
  foreach (['1', '2'] as $ubudehe) {
    assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, $clinic_type, '2024-10-12'), TRUE, "before 13 Oct 2024, a breastfeeding mother at an $clinic_type group with Ubudehe $ubudehe is expected to receive Mother FBF", $passed, $failed);
  }
  foreach (['3', '4', '0'] as $ubudehe) {
    assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, $clinic_type, '2024-10-12'), FALSE, "before 13 Oct 2024, a breastfeeding mother at an $clinic_type group with Ubudehe $ubudehe is not expected to receive Mother FBF", $passed, $failed);
  }
  assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, $clinic_type, '2024-10-12'), FALSE, "before 13 Oct 2024, a breastfeeding mother at an $clinic_type group with no Ubudehe recorded is not expected to receive Mother FBF", $passed, $failed);
}

assert_same(
  [
    mother_fbf_expected(mother('2'), $breastfeeding, 'fbf', '2023-05-01'),
    mother_fbf_expected(mother('3'), $breastfeeding, 'fbf', '2023-05-01'),
  ],
  [TRUE, FALSE],
  'Nutrition Group sessions before 13 Oct 2024 keep today\'s rule: Mother FBF is expected only when Ubudehe is 1 or 2',
  $passed,
  $failed
);

// Before the FBF feature was launched, no Lactation and no Mother FBF.
assert_same(expected_activities(mother('1'), $breastfeeding, 'fbf', '2020-01-01'), ['family_planning'], 'before the FBF launch, a breastfeeding mother with Ubudehe 1 is expected only Family Planning', $passed, $failed);

// A mother who isn't breastfeeding, from 13 Oct 2024 and before.
foreach (['2024-10-12', '2024-10-13'] as $date) {
  foreach (['1', '3'] as $ubudehe) {
    assert_same(mother_fbf_expected(mother($ubudehe), ['lactation' => lactation(['none'])], 'fbf', $date), FALSE, "on $date, a mother with Ubudehe $ubudehe who isn't breastfeeding is not expected to receive Mother FBF", $passed, $failed);
    assert_same(mother_fbf_expected(mother($ubudehe), [], 'fbf', $date), FALSE, "on $date, a mother with Ubudehe $ubudehe and no Lactation recorded is not expected to receive Mother FBF", $passed, $failed);
  }
  assert_same(mother_fbf_expected(mother(NULL), ['lactation' => lactation(['none'])], 'achi', $date), FALSE, "on $date, a mother with no Ubudehe recorded who isn't breastfeeding is not expected to receive Mother FBF", $passed, $failed);
}

assert_same(mother_fbf_expected(mother(NULL), ['lactation' => lactation(['none'])], 'fbf', '2025-03-01'), FALSE, 'A mother who isn\'t breastfeeding is not expected to receive Mother FBF, as today', $passed, $failed);

// A breastfeeding mother at another type of group, from 13 Oct 2024 and
// before.
foreach (['2024-10-12', '2024-10-13'] as $date) {
  foreach (['pmtct', 'sorwathe', 'chw'] as $clinic_type) {
    foreach (['1', '3'] as $ubudehe) {
      assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, $clinic_type, $date), FALSE, "on $date, a breastfeeding mother with Ubudehe $ubudehe at a $clinic_type group is not expected to receive Mother FBF", $passed, $failed);
    }
    assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, $clinic_type, $date), FALSE, "on $date, a breastfeeding mother with no Ubudehe recorded at a $clinic_type group is not expected to receive Mother FBF", $passed, $failed);
  }
}

assert_same(mother_fbf_expected(mother('4'), $breastfeeding, 'pmtct', '2025-03-01'), FALSE, 'A mother who attends another type of group is not expected to receive Mother FBF, as today', $passed, $failed);

// A Mother FBF that was given counts as completed when it was expected.
$breastfeeding_and_given = $breastfeeding + [
  'family_planning' => measurement(),
  'mother_fbf' => measurement(),
];

assert_same(mother_fbf_completion(mother('3'), $breastfeeding_and_given, 'fbf', '2024-10-13'), [TRUE, TRUE], 'A Mother FBF given to a mother it is expected for counts as completed', $passed, $failed);

assert_same(mother_fbf_completion(mother(NULL), $breastfeeding_and_given, 'achi', '2025-03-01'), [TRUE, TRUE], 'from 13 Oct 2024, a Mother FBF given to a breastfeeding mother with no Ubudehe recorded counts as completed', $passed, $failed);

assert_same(mother_fbf_completion(mother('1'), $breastfeeding_and_given, 'fbf', '2024-10-12'), [TRUE, TRUE], 'before 13 Oct 2024, a Mother FBF given to a breastfeeding mother with Ubudehe 1 counts as completed', $passed, $failed);

// Expected but not given: expected, not completed.
assert_same(mother_fbf_completion(mother('4'), $breastfeeding, 'fbf', '2024-10-13'), [TRUE, FALSE], 'from 13 Oct 2024, a Mother FBF not given to a breastfeeding mother with Ubudehe 4 is expected and not completed', $passed, $failed);

// Not expected: a distribution given anyway is not counted.
assert_same(mother_fbf_completion(mother('3'), $breastfeeding_and_given, 'fbf', '2024-10-12'), [FALSE, FALSE], 'before 13 Oct 2024, a Mother FBF given to a mother with Ubudehe 3 is not expected and not counted', $passed, $failed);

assert_same(mother_fbf_completion(mother('3'), ['lactation' => lactation(['none'])] + $breastfeeding_and_given, 'fbf', '2024-10-13'), [FALSE, FALSE], 'from 13 Oct 2024, a Mother FBF given to a mother who isn\'t breastfeeding is not expected and not counted', $passed, $failed);

report_results('Nutrition Group completion', $passed, $failed);
