<?php

/**
 * @file
 * Standalone tests of when the next dose of a vaccine is due.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly.
 * The cases match the vaccination tests of the server Elm scoreboard
 * (Pages/Scoreboard/Test.elm), which has its own copy of the schedule.
 */

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

const BIRTH_DATE = '2024-01-01';

/**
 * Returns the date this many weeks after birth, as Y-m-d.
 */
function weeks_old($weeks) {
  return (new DateTime(BIRTH_DATE))->modify("+$weeks weeks")->format('Y-m-d');
}

/**
 * Returns the day before a Y-m-d date.
 */
function day_before($date) {
  return (new DateTime($date))->modify('-1 day')->format('Y-m-d');
}

/**
 * Tells whether a vaccine is expected on a date, given the doses given.
 *
 * @param string $vaccine
 *   The vaccine, like 'opv'.
 * @param array $dates
 *   The dates its doses were given, in order.
 * @param string $site
 *   The site.
 * @param string $date
 *   The day of the encounter, as Y-m-d.
 */
function is_expected($vaccine, array $dates, $site, $date) {
  $vaccines = ['bcg', 'opv', 'dtp', 'dtp_sa', 'pcv13', 'rotarix', 'ipv', 'mr'];
  $history = array_fill_keys($vaccines, []);
  foreach (array_values($dates) as $index => $dose_date) {
    $history[$vaccine]['dose-' . ($index + 1)] = $dose_date;
  }
  $birth_date_obj = new DateTime(BIRTH_DATE);
  $initial_opv = hedley_reports_initial_opv_administered(['vaccination_progress' => $history], $birth_date_obj);
  $expected = hedley_reports_resolve_expected_immunisations([$vaccine], $history, $initial_opv, $site, $birth_date_obj, new DateTime($date), '%s');
  return $expected === [$vaccine];
}

/**
 * Checks that a dose is due on a date, and not the day before.
 */
function assert_due_on($vaccine, array $dates, $site, $due_date, $case, &$passed, &$failed) {
  $result = [
    is_expected($vaccine, $dates, $site, day_before($due_date)),
    is_expected($vaccine, $dates, $site, $due_date),
  ];
  assert_same($result, [FALSE, TRUE], $case, $passed, $failed);
}

/**
 * Checks that no dose is due, even years later.
 */
function assert_complete($vaccine, array $dates, $site, $case, &$passed, &$failed) {
  assert_same(is_expected($vaccine, $dates, $site, '2040-01-01'), FALSE, $case, $passed, $failed);
}

// OPV.
assert_due_on('opv', [weeks_old(6)], 'rwanda', weeks_old(10), 'OPV without the birth dose: the second dose is 4 weeks after the first', $passed, $failed);
assert_complete('opv', [weeks_old(6), weeks_old(10), weeks_old(14)], 'rwanda', 'OPV without the birth dose: three doses complete the course', $passed, $failed);
assert_due_on('opv', ['2024-01-05'], 'rwanda', weeks_old(6), 'OPV after the birth dose: the second dose is at 6 weeks of age', $passed, $failed);
assert_complete('opv', [weeks_old(2), weeks_old(6), weeks_old(10)], 'rwanda', 'OPV: a first dose at 14 days is not a birth dose, so three doses complete the course', $passed, $failed);
assert_due_on('opv', [BIRTH_DATE, weeks_old(6), weeks_old(10)], 'rwanda', weeks_old(14), 'OPV after the birth dose: a fourth dose follows the third', $passed, $failed);
assert_complete('opv', [BIRTH_DATE, weeks_old(6), weeks_old(10), weeks_old(14)], 'rwanda', 'OPV after the birth dose: four doses complete the course', $passed, $failed);

// IPV.
assert_due_on('ipv', [weeks_old(14)], 'rwanda', weeks_old(36), 'IPV on Rwanda: the second dose is at 36 weeks of age, when that is later', $passed, $failed);
assert_due_on('ipv', [weeks_old(34)], 'rwanda', weeks_old(38), 'IPV on Rwanda: the second dose is 28 days after the first, when that is later', $passed, $failed);
assert_complete('ipv', [weeks_old(14), weeks_old(36)], 'rwanda', 'IPV on Rwanda: two doses complete the course', $passed, $failed);
assert_complete('ipv', [weeks_old(14)], 'burundi', 'IPV on Burundi: one dose completes the course', $passed, $failed);

// DTP standalone.
assert_complete('dtp_sa', ['2025-07-01'], 'burundi', 'DTP standalone: one dose completes the course', $passed, $failed);

// MR.
$first_mr = weeks_old(36);
$six_months_later = (new DateTime($first_mr))->modify('+6 months')->format('Y-m-d');
$nine_months_later = (new DateTime($first_mr))->modify('+9 months')->format('Y-m-d');
assert_due_on('mr', [$first_mr], 'rwanda', $six_months_later, 'MR on Rwanda: the second dose is 6 months after the first', $passed, $failed);
assert_due_on('mr', [$first_mr], 'burundi', $nine_months_later, 'MR on Burundi: the second dose is 9 months after the first', $passed, $failed);
assert_complete('mr', [$first_mr, weeks_old(75)], 'burundi', 'MR: two doses complete the course', $passed, $failed);

// A vaccine with no dose given is expected.
assert_same(is_expected('bcg', [], 'rwanda', BIRTH_DATE), TRUE, 'a vaccine with no dose given is expected', $passed, $failed);

$total = $passed + $failed;
echo "\n";
echo "Expected immunisation tests: $total total, $passed passed, $failed failed.\n";

if ($failed > 0) {
  echo "FAILED\n";
  exit(1);
}

echo "OK\n";
exit(0);
