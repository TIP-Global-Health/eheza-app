<?php

/**
 * @file
 * Standalone Nutrition report tests.
 *
 * Runs without Drupal bootstrap: hedley_reports.module is loaded directly and
 * the nutrition report generator is called on hand-built patient data. The
 * report shows the last 12 months, so visit dates are relative to today.
 */

/**
 * Stub drupal_is_cli() so the generator does not print progress.
 */
function drupal_is_cli() {
  return FALSE;
}

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
 * Returns a day of a month before the current one, as Y-m-d.
 *
 * Example: month_day(3, 15) on 2026-10-20 gives 2026-07-15.
 */
function month_day($months_ago, $day) {
  $date = new DateTime(date('Y-m-01'));
  $date->modify("-$months_ago months");
  $date->modify('+' . ($day - 1) . ' days');
  return $date->format('Y-m-d');
}

/**
 * Returns the date six years before the given Y-m-d date.
 */
function six_years_before($date) {
  return (new DateTime($date))->modify('-6 years')->format('Y-m-d');
}

/**
 * Runs the generator on patients given as arrays.
 */
function generate(array $patients) {
  return hedley_reports_generate_nutrition_report_data(json_decode(json_encode($patients)));
}

/**
 * Reads one monthly cell, like '100%', from the generated tables.
 */
function monthly_cell(array $tables, $type, $row, $months_ago) {
  $key = (new DateTime(month_day($months_ago, 1)))->format('Y-m');
  foreach ($tables as $table) {
    if ($table['type'] === $type) {
      $column = array_search($key, $table['period']);
      return $column === FALSE ? "month $key not in table" : $table[$row][$column];
    }
  }
  return NULL;
}

/**
 * A CHW group visit with a moderate stunting z-score.
 */
function stunted_visit($date) {
  return "$date -2.5,,,,";
}

// The day a child turns six.
assert_same(hedley_reports_sixth_birthday('2019-06-15'), '2025-06-15', 'sixth birthday of 2019-06-15', $passed, $failed);
assert_same(hedley_reports_sixth_birthday('2020-02-29'), '2026-03-01', 'a child born on 29 February is six on 1 March', $passed, $failed);
assert_same(hedley_reports_sixth_birthday(''), FALSE, 'an unreadable birth date has no sixth birthday', $passed, $failed);
assert_same(hedley_reports_sixth_birthday('2019-02-30'), FALSE, 'an impossible birth date has no sixth birthday', $passed, $failed);

// A child who is six today keeps the visits made before turning six.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => six_years_before(month_day(6, 15)),
    'group_nutrition' => [
      'chw' => [
        stunted_visit(month_day(9, 10)),
        stunted_visit(month_day(3, 10)),
      ],
    ],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 9), '100%', 'a visit before six counts', $passed, $failed);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 3), '0%', 'a visit after six does not count', $passed, $failed);

// A visit the day before the sixth birthday counts, one on the birthday does
// not. The birthday is the 1st of a month, so the two land in different months.
$sixth_birthday = month_day(5, 1);
$day_before = (new DateTime($sixth_birthday))->modify('-1 day')->format('Y-m-d');
$tables = generate([
  [
    'id' => 1,
    'birth_date' => six_years_before($sixth_birthday),
    'group_nutrition' => [
      'chw' => [
        stunted_visit($day_before),
        stunted_visit($sixth_birthday),
      ],
    ],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 6), '100%', 'a visit the day before the sixth birthday counts', $passed, $failed);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 5), '0%', 'a visit on the sixth birthday does not count', $passed, $failed);

// Two visits or more counts only the visits made before six. Child 2 is
// under six, with two normal visits, so it is the only one in the two-visit
// table.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => six_years_before(month_day(7, 1)),
    'group_nutrition' => [
      'chw' => [
        stunted_visit(month_day(8, 10)),
        stunted_visit(month_day(4, 10)),
      ],
    ],
  ],
  [
    'id' => 2,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => [
      'chw' => [
        month_day(8, 12) . ' -1,,,,',
        month_day(4, 12) . ' -1,,,,',
      ],
    ],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 8), '50%', 'both children count in the one-visit table', $passed, $failed);
assert_same(monthly_cell($tables, 'prevalence-2', 'stunting_moderate', 8), '0%', 'a child with one visit before six is left out of the two-visit table', $passed, $failed);

// A family-nutrition MUAC visit counts only before six (MUAC 12 is MAM).
$tables = generate([
  [
    'id' => 1,
    'birth_date' => six_years_before(month_day(6, 15)),
    'individual' => [
      'family-nutrition-muac' => [
        [
          month_day(9, 10) . ' 12',
          month_day(3, 10) . ' 12',
        ],
      ],
    ],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'acute_malnutrition_mam', 9), '100%', 'a MUAC visit before six counts', $passed, $failed);
assert_same(monthly_cell($tables, 'prevalence-1', 'acute_malnutrition_mam', 3), '0%', 'a MUAC visit after six does not count', $passed, $failed);

// An adult, and a person whose birth date cannot be read, add nothing.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => '1990-01-01',
    'group_nutrition' => ['chw' => [stunted_visit(month_day(4, 10))]],
  ],
  [
    'id' => 2,
    'birth_date' => '',
    'group_nutrition' => ['chw' => [stunted_visit(month_day(4, 10))]],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 4), '0%', 'an adult and an unreadable birth date add no visit', $passed, $failed);

$total = $passed + $failed;
echo "\n";
echo "Nutrition report tests: $total total, $passed passed, $failed failed.\n";

if ($failed > 0) {
  echo "FAILED\n";
  exit(1);
}

echo "OK\n";
exit(0);
