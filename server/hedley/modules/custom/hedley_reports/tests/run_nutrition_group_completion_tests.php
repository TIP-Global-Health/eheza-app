<?php

/**
 * @file
 * Standalone tests of the Nutrition Group completion rules for mothers.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * From 13 Oct 2024 (release 1.13.0) the app offers Mother FBF to every
 * breastfeeding mother at an FBF group; before that, only to a breastfeeding
 * mother whose Ubudehe is 1 or 2. At ACHI groups the app shows no mother
 * activities at all, while children there still get Aheza (child FBF).
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

// From 13 Oct 2024, every breastfeeding mother at an FBF group. ACHI
// mothers are covered by their own section below: they get no activities.
foreach (['1', '2', '3', '4', '0'] as $ubudehe) {
  assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, 'fbf', '2024-10-13'), TRUE, "from 13 Oct 2024, a breastfeeding mother at an fbf group with Ubudehe $ubudehe is expected to receive Mother FBF", $passed, $failed);
}

assert_same(mother_fbf_expected(mother('3'), $breastfeeding, 'fbf', '2025-03-01'), TRUE, 'Nutrition Group sessions from 13 Oct 2024: a breastfeeding mother at an FBF or ACHI group is expected to receive Mother FBF, whatever her Ubudehe', $passed, $failed);

assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, 'fbf', '2024-10-13'), TRUE, 'Nutrition Group sessions from 13 Oct 2024: a breastfeeding mother at an FBF or ACHI group is expected to receive Mother FBF, including none recorded', $passed, $failed);

assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, 'fbf', '2025-03-01'), TRUE, 'from 13 Oct 2024, a breastfeeding mother at an FBF group with no Ubudehe recorded is expected to receive Mother FBF', $passed, $failed);

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
foreach (['1', '2'] as $ubudehe) {
  assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, 'fbf', '2024-10-12'), TRUE, "before 13 Oct 2024, a breastfeeding mother at an fbf group with Ubudehe $ubudehe is expected to receive Mother FBF", $passed, $failed);
}
foreach (['3', '4', '0'] as $ubudehe) {
  assert_same(mother_fbf_expected(mother($ubudehe), $breastfeeding, 'fbf', '2024-10-12'), FALSE, "before 13 Oct 2024, a breastfeeding mother at an fbf group with Ubudehe $ubudehe is not expected to receive Mother FBF", $passed, $failed);
}
assert_same(mother_fbf_expected(mother(NULL), $breastfeeding, 'fbf', '2024-10-12'), FALSE, "before 13 Oct 2024, a breastfeeding mother at an fbf group with no Ubudehe recorded is not expected to receive Mother FBF", $passed, $failed);

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
  assert_same(mother_fbf_expected(mother(NULL), ['lactation' => lactation(['none'])], 'fbf', $date), FALSE, "on $date, a mother with no Ubudehe recorded who isn't breastfeeding is not expected to receive Mother FBF", $passed, $failed);
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

assert_same(mother_fbf_completion(mother(NULL), $breastfeeding_and_given, 'fbf', '2025-03-01'), [TRUE, TRUE], 'from 13 Oct 2024, a Mother FBF given to a breastfeeding mother with no Ubudehe recorded counts as completed', $passed, $failed);

assert_same(mother_fbf_completion(mother('1'), $breastfeeding_and_given, 'fbf', '2024-10-12'), [TRUE, TRUE], 'before 13 Oct 2024, a Mother FBF given to a breastfeeding mother with Ubudehe 1 counts as completed', $passed, $failed);

// Expected but not given: expected, not completed.
assert_same(mother_fbf_completion(mother('4'), $breastfeeding, 'fbf', '2024-10-13'), [TRUE, FALSE], 'from 13 Oct 2024, a Mother FBF not given to a breastfeeding mother with Ubudehe 4 is expected and not completed', $passed, $failed);

// Not expected: a distribution given anyway is not counted.
assert_same(mother_fbf_completion(mother('3'), $breastfeeding_and_given, 'fbf', '2024-10-12'), [FALSE, FALSE], 'before 13 Oct 2024, a Mother FBF given to a mother with Ubudehe 3 is not expected and not counted', $passed, $failed);

assert_same(mother_fbf_completion(mother('3'), ['lactation' => lactation(['none'])] + $breastfeeding_and_given, 'fbf', '2024-10-13'), [FALSE, FALSE], 'from 13 Oct 2024, a Mother FBF given to a mother who isn\'t breastfeeding is not expected and not counted', $passed, $failed);

/**
 * Splits a stored completion string into its expected and completed codes.
 *
 * Example: 'a,c|a' gives [['a', 'c'], ['a']], and '|' gives [[], []].
 */
function completion_codes($result) {
  list($expected_part, $completed_part) = explode('|', $result);
  return [
    array_values(array_filter(explode(',', $expected_part), 'strlen')),
    array_values(array_filter(explode(',', $completed_part), 'strlen')),
  ];
}

/**
 * Returns the stored completion codes for a mother at a group session.
 */
function mother_completion_codes($person, array $measurements_by_type, $clinic_type, $date) {
  $expected = expected_activities($person, $measurements_by_type, $clinic_type, $date);
  return completion_codes(hedley_reports_generate_completion_result($expected, $measurements_by_type, MOTHER_MAPPING));
}

// Every mother activity recorded at the session, as if the nurse had filled
// them all in anyway.
$all_given = $breastfeeding + [
  'family_planning' => measurement(),
  'mother_fbf' => measurement(),
];

// The shapes of a mother's session measurements: breastfeeding, a Lactation
// that says not breastfeeding, no Lactation recorded (the stored shape that
// means "no answer"), nothing at all, and every activity recorded.
$mother_measurement_shapes = [
  'breastfeeding' => $breastfeeding,
  'not breastfeeding' => ['lactation' => lactation(['none'])],
  'Lactation with no signs' => ['lactation' => lactation([])],
  'no measurements' => [],
  'every activity recorded' => $all_given,
];

// Session dates around each rule: before the FBF launch (15 Jun 2020), the
// first ACHI sessions (Dec 2020), the day before and the day of the Mother
// FBF Ubudehe change (13 Oct 2024), and a later session.
$session_dates = [
  '2020-01-01',
  '2020-06-15',
  '2020-12-01',
  '2024-10-12',
  '2024-10-13',
  '2025-03-01',
];

// Ubudehe 1 (entitled to Mother FBF before 13 Oct 2024), 3 (not entitled),
// and none recorded.
$ubudehe_values = ['1', '3', NULL];

// Criterion 1: an ACHI mother is expected to do nothing, so nothing is
// missed. The app shows no mother activities at ACHI groups at all.
assert_same(
  [
    expected_activities(mother('3'), $breastfeeding, 'achi', '2025-03-01'),
    mother_completion_codes(mother('3'), $breastfeeding, 'achi', '2025-03-01'),
  ],
  [[], [[], []]],
  'A mother at an ACHI group session is expected to do no activities, so nothing counts as missed for her',
  $passed,
  $failed
);

// Boundary cases for criterion 1: every Ubudehe, every date, every shape of
// measurements. A distribution recorded anyway is not counted either.
foreach ($session_dates as $date) {
  foreach ($ubudehe_values as $ubudehe) {
    $ubudehe_label = $ubudehe === NULL ? 'no Ubudehe recorded' : "Ubudehe $ubudehe";
    foreach ($mother_measurement_shapes as $shape_label => $measurements_by_type) {
      assert_same(expected_activities(mother($ubudehe), $measurements_by_type, 'achi', $date), [], "on $date, a mother at an ACHI group with $ubudehe_label and $shape_label is expected to do no activities", $passed, $failed);
      assert_same(mother_completion_codes(mother($ubudehe), $measurements_by_type, 'achi', $date), [[], []], "on $date, a mother at an ACHI group with $ubudehe_label and $shape_label has nothing expected and nothing counted in her completion data", $passed, $failed);
    }
  }
}

// Criterion 2: FBF mothers keep today's rule. Family Planning and Lactation
// always (from the FBF launch), Mother FBF only when breastfeeding.
assert_same(
  [
    expected_activities(mother('4'), $breastfeeding, 'fbf', '2025-03-01'),
    expected_activities(mother('4'), ['lactation' => lactation(['none'])], 'fbf', '2025-03-01'),
    expected_activities(mother('4'), [], 'fbf', '2025-03-01'),
  ],
  [
    ['family_planning', 'lactation', 'mother_fbf'],
    ['family_planning', 'lactation'],
    ['family_planning', 'lactation'],
  ],
  'A mother at an FBF group session is expected to do Family Planning, Lactation and, when breastfeeding, Mother FBF, as today',
  $passed,
  $failed
);

// Boundary cases for criterion 2, across dates and Ubudehe values: before
// 13 Oct 2024 Mother FBF also needs Ubudehe 1 or 2; before the FBF launch
// only Family Planning.
foreach ($session_dates as $date) {
  foreach ($ubudehe_values as $ubudehe) {
    $ubudehe_label = $ubudehe === NULL ? 'no Ubudehe recorded' : "Ubudehe $ubudehe";
    $after_launch = $date >= '2020-06-15';
    $entitled = $date >= '2024-10-13' || $ubudehe === '1';
    foreach ($mother_measurement_shapes as $shape_label => $measurements_by_type) {
      $breastfeeds = in_array($shape_label, ['breastfeeding', 'every activity recorded']);
      $expected = ['family_planning'];
      if ($after_launch) {
        $expected[] = 'lactation';
        if ($breastfeeds && $entitled) {
          $expected[] = 'mother_fbf';
        }
      }
      assert_same(expected_activities(mother($ubudehe), $measurements_by_type, 'fbf', $date), $expected, "on $date, a mother at an FBF group with $ubudehe_label and $shape_label is expected " . implode(', ', $expected) . ', as today', $passed, $failed);
    }
  }
}

// Criterion 3: every other group type expects Family Planning only, whether
// or not the mother breastfeeds.
assert_same(
  [
    expected_activities(mother('1'), $breastfeeding, 'pmtct', '2025-03-01'),
    expected_activities(mother('1'), $breastfeeding, 'sorwathe', '2025-03-01'),
    expected_activities(mother('1'), $breastfeeding, 'chw', '2025-03-01'),
  ],
  [['family_planning'], ['family_planning'], ['family_planning']],
  'A mother at any other group type is expected to do Family Planning, as today',
  $passed,
  $failed
);

// Boundary cases for criterion 3.
foreach (['pmtct', 'sorwathe', 'chw'] as $clinic_type) {
  foreach ($session_dates as $date) {
    foreach ($ubudehe_values as $ubudehe) {
      $ubudehe_label = $ubudehe === NULL ? 'no Ubudehe recorded' : "Ubudehe $ubudehe";
      foreach ($mother_measurement_shapes as $shape_label => $measurements_by_type) {
        assert_same(expected_activities(mother($ubudehe), $measurements_by_type, $clinic_type, $date), ['family_planning'], "on $date, a mother at a $clinic_type group with $ubudehe_label and $shape_label is expected Family Planning only, as today", $passed, $failed);
      }
    }
  }
}

// Criterion 4: the child side of the group completion data adds Aheza (child
// FBF) when hedley_reports_fbf_activities_shown() says so. Children at ACHI
// groups keep it. The child's other activities are built inline in
// hedley_reports_generate_completion_data_for_nutrition_group_encounter(),
// which needs Drupal, so only the Aheza part is pinned here.
assert_same(
  [
    hedley_reports_fbf_activities_shown('achi', new DateTime('2020-12-01')),
    hedley_reports_fbf_activities_shown('achi', new DateTime('2024-10-12')),
    hedley_reports_fbf_activities_shown('achi', new DateTime('2025-03-01')),
  ],
  [TRUE, TRUE, TRUE],
  'A child at an ACHI group session is expected to do the same activities as today, including Aheza (child FBF)',
  $passed,
  $failed
);

// Boundary cases for criterion 4: FBF children keep Aheza; other group types
// never get it; nobody gets it before the FBF launch on 15 Jun 2020.
foreach (['achi', 'fbf'] as $clinic_type) {
  assert_same(hedley_reports_fbf_activities_shown($clinic_type, new DateTime('2020-06-15')), TRUE, "a child at a $clinic_type group on the FBF launch day is expected Aheza (child FBF), as today", $passed, $failed);
  assert_same(hedley_reports_fbf_activities_shown($clinic_type, new DateTime('2020-06-14')), FALSE, "a child at a $clinic_type group the day before the FBF launch is not expected Aheza (child FBF), as today", $passed, $failed);
}
foreach (['pmtct', 'sorwathe', 'chw'] as $clinic_type) {
  foreach (['2020-06-15', '2025-03-01'] as $date) {
    assert_same(hedley_reports_fbf_activities_shown($clinic_type, new DateTime($date)), FALSE, "on $date, a child at a $clinic_type group is not expected Aheza (child FBF), as today", $passed, $failed);
  }
}

// Criterion 5: regenerating the data stores, for each ACHI mother, the
// string built by hedley_reports_generate_completion_result() from the
// mother's expected activities. Family Planning ('a') and Lactation ('c')
// must never count as missed in it, for any ACHI attendance from Dec 2020 to
// Nov 2024.
$achi_mothers_missing = [];
foreach (['2020-12-01', '2022-06-01', '2024-10-12', '2024-10-13', '2024-11-30'] as $date) {
  foreach ($ubudehe_values as $ubudehe) {
    foreach ($mother_measurement_shapes as $shape_label => $measurements_by_type) {
      list($expected_codes, $completed_codes) = mother_completion_codes(mother($ubudehe), $measurements_by_type, 'achi', $date);
      $missed = array_values(array_diff($expected_codes, $completed_codes));
      if (array_intersect(['a', 'c'], $missed)) {
        $achi_mothers_missing[] = "$date, Ubudehe " . var_export($ubudehe, TRUE) . ", $shape_label: missed " . implode(',', $missed);
      }
    }
  }
}
assert_same($achi_mothers_missing, [], 'After the Nutrition Group completion data is regenerated, no ACHI mother shows Family Planning or Lactation as missed', $passed, $failed);

report_results('Nutrition Group completion', $passed, $failed);
