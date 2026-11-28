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
require_once __DIR__ . '/assertions.inc';

$passed = 0;
$failed = 0;

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
  $vaccines = [
    'bcg',
    'opv',
    'dtp',
    'dtp_sa',
    'pcv13',
    'rotarix',
    'ipv',
    'mr',
    'hpv',
  ];
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

// Doses given on the 29th to the 31st. The app adds the months and stops on
// the last day of a shorter month: an MR dose given on 31 August 2025 makes
// the next one due on 28 February 2026, not 3 March. The report must expect
// it from the same day, or a skipped dose in the days between is not counted
// as missing.
// Criterion: an MR or HPV dose given on the 31st makes the next dose expected
// from the last day of the month 6 months later, or 9 months later for MR in
// Burundi.
assert_due_on('mr', ['2025-08-31'], 'rwanda', '2026-02-28', 'An MR dose given on the 31st: the next dose is expected from the last day of the month 6 months later (Rwanda, 2025-08-31 to 2026-02-28)', $passed, $failed);
assert_due_on('hpv', ['2025-08-31'], 'rwanda', '2026-02-28', 'An HPV dose given on the 31st: the next dose is expected from the last day of the month 6 months later (2025-08-31 to 2026-02-28)', $passed, $failed);
assert_due_on('mr', ['2025-05-31'], 'burundi', '2026-02-28', 'An MR dose given on the 31st: the next dose is expected from the last day of the month 9 months later for MR in Burundi (2025-05-31 to 2026-02-28)', $passed, $failed);

// Boundaries: a 30-day month that is not February, a leap February, and the
// 30th, which also runs past the end of February.
assert_due_on('mr', ['2025-03-31'], 'rwanda', '2025-09-30', 'Boundary: MR given on 2025-03-31: the next dose is expected from 2025-09-30 (Rwanda)', $passed, $failed);
assert_due_on('mr', ['2024-12-31'], 'burundi', '2025-09-30', 'Boundary: MR given on 2024-12-31: the next dose is expected from 2025-09-30 (Burundi)', $passed, $failed);
assert_due_on('hpv', ['2023-08-31'], 'rwanda', '2024-02-29', 'Boundary: HPV given on 2023-08-31: the next dose is expected from 2024-02-29', $passed, $failed);
assert_due_on('mr', ['2025-08-30'], 'rwanda', '2026-02-28', 'Boundary: MR given on 2025-08-30: the next dose is expected from 2026-02-28 (Rwanda)', $passed, $failed);
// Guards: a second dose given on the new due day still completes the course.
assert_complete('mr', ['2025-08-31', '2026-02-28'], 'rwanda', 'guard: MR given on 2025-08-31 and 2026-02-28 completes the course', $passed, $failed);
assert_complete('hpv', ['2025-08-31', '2026-02-28'], 'rwanda', 'guard: HPV given on 2025-08-31 and 2026-02-28 completes the course', $passed, $failed);

// Criterion: a dose given on the 1st to the 28th is due on the same day of
// the month as before.
assert_due_on('mr', ['2025-08-15'], 'rwanda', '2026-02-15', 'A dose given on the 1st to the 28th is due on the same day of the month as before: MR given on 2025-08-15 is due on 2026-02-15 (Rwanda)', $passed, $failed);
assert_due_on('mr', ['2025-05-15'], 'burundi', '2026-02-15', 'guard: MR given on 2025-05-15 is due on 2026-02-15 (Burundi)', $passed, $failed);
assert_due_on('hpv', ['2025-08-28'], 'rwanda', '2026-02-28', 'guard: HPV given on 2025-08-28 is due on 2026-02-28', $passed, $failed);

// Criterion: intervals in days or weeks are unchanged. Four weeks after the
// 31st of a month is not the last day of the next month.
assert_due_on('dtp', ['2025-08-31'], 'rwanda', '2025-09-28', 'Intervals in days or weeks are unchanged: DTP given on 2025-08-31 makes the next dose due 4 weeks later, on 2025-09-28', $passed, $failed);
assert_due_on('opv', ['2025-01-31'], 'rwanda', '2025-02-28', 'guard: OPV given on 2025-01-31 makes the next dose due 4 weeks later, on 2025-02-28', $passed, $failed);
assert_due_on('pcv13', ['2024-01-31'], 'burundi', '2024-02-28', 'guard: PCV13 given on 2024-01-31 makes the next dose due 4 weeks later, on 2024-02-28, in a leap year', $passed, $failed);
// Rwanda's second IPV dose is 28 days after the first, the child being well
// past 36 weeks of age.
assert_due_on('ipv', ['2025-08-31'], 'rwanda', '2025-09-28', 'guard: IPV given on 2025-08-31 on Rwanda makes the second dose due 28 days later, on 2025-09-28', $passed, $failed);

report_results('Expected immunisation', $passed, $failed);
