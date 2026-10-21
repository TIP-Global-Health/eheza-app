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

// Bands, as [normal, moderate, severe] lists of person 1.
$normal = [[1], [], []];
$moderate = [[], [1], []];
$severe = [[], [], [1]];
$no_band = [[], [], []];

// Bands of a z-score.
assert_same(hedley_reports_categorize_z_score(-3.0, 1), $severe, 'z-score -3.0 is severe', $passed, $failed);
assert_same(hedley_reports_categorize_z_score(-2.99, 1), $moderate, 'z-score -2.99 is moderate', $passed, $failed);
assert_same(hedley_reports_categorize_z_score(-2.0, 1), $moderate, 'z-score -2.0 is moderate', $passed, $failed);
assert_same(hedley_reports_categorize_z_score(-1.99, 1), $normal, 'z-score -1.99 is normal', $passed, $failed);
assert_same(hedley_reports_categorize_z_score(NULL, 1), $no_band, 'no z-score is in no band', $passed, $failed);

// Bands of MUAC and edema.
assert_same(hedley_reports_categorize_acute_malnutrition(11.4, FALSE, 1), $severe, 'MUAC 11.4 is SAM', $passed, $failed);
assert_same(hedley_reports_categorize_acute_malnutrition(11.5, FALSE, 1), $moderate, 'MUAC 11.5 is MAM', $passed, $failed);
assert_same(hedley_reports_categorize_acute_malnutrition(12.49, FALSE, 1), $moderate, 'MUAC 12.49 is MAM', $passed, $failed);
assert_same(hedley_reports_categorize_acute_malnutrition(12.5, FALSE, 1), $normal, 'MUAC 12.5 is normal', $passed, $failed);
assert_same(hedley_reports_categorize_acute_malnutrition(13.0, TRUE, 1), $severe, 'edema with MUAC 13 is SAM', $passed, $failed);
assert_same(hedley_reports_categorize_acute_malnutrition(NULL, TRUE, 1), $no_band, 'edema without MUAC is in no band', $passed, $failed);

// The edema token is read from the wire, at group and well-child visits.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => ['chw' => [month_day(4, 10) . ' ,,,13,1']],
    'individual' => ['well-child' => [[month_day(5, 10) . '|,,,13,1|']]],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'acute_malnutrition_sam', 4), '100%', 'edema at a group visit is SAM', $passed, $failed);
assert_same(monthly_cell($tables, 'prevalence-1', 'acute_malnutrition_sam', 5), '100%', 'edema at a well-child visit is SAM', $passed, $failed);

/**
 * Empty nutrition metrics with the given buckets filled in.
 */
function metrics(array $buckets) {
  return array_merge(hedley_reports_empty_nutrition_metrics(), $buckets);
}

// Prevalence counts each child once.
$prevalence = hedley_reports_generate_prevalence_nutrition_metrics_results(metrics([
  'stunting_moderate' => [1, 1],
  'stunting_normal' => [2],
]));
assert_same($prevalence['stunting_moderate'], '50%', 'child 1 moderate twice and child 2 normal is 50%', $passed, $failed);
$prevalence = hedley_reports_generate_prevalence_nutrition_metrics_results(metrics(['stunting_moderate' => [1], 'stunting_normal' => [1]]));
assert_same($prevalence['stunting_moderate'], '100%', 'a child moderate at one visit and normal at another is 100% moderate', $passed, $failed);
$prevalence = hedley_reports_generate_prevalence_nutrition_metrics_results(metrics([
  'acute_malnutrition_mam' => [1],
  'acute_malnutrition_sam' => [2],
  'acute_malnutrition_normal' => [3, 4],
]));
assert_same([$prevalence['acute_malnutrition_mam'], $prevalence['acute_malnutrition_sam']], ['25%', '25%'], 'MAM and SAM share the MUAC denominator', $passed, $failed);
$prevalence = hedley_reports_generate_prevalence_nutrition_metrics_results(metrics([]));
assert_same($prevalence['stunting_moderate'], '0%', 'no measured children is 0%', $passed, $failed);

/**
 * Incidence for the current and previous period buckets.
 */
function incidence(array $current, array $previous) {
  return hedley_reports_generate_incidence_nutrition_metrics_results(metrics($current), metrics($previous));
}

// Incidence counts children newly moderate or severe.
assert_same(incidence(['stunting_moderate' => [1]], ['stunting_normal' => [1]])['stunting_moderate'], '100%', 'normal before, moderate now, counts', $passed, $failed);
assert_same(incidence(['stunting_moderate' => [1]], ['stunting_moderate' => [1]])['stunting_moderate'], '0%', 'moderate before, moderate now, does not count', $passed, $failed);
assert_same(incidence(['stunting_moderate' => [1]], ['stunting_severe' => [1]])['stunting_moderate'], '0%', 'severe before, moderate now, does not count', $passed, $failed);
assert_same(incidence(['stunting_severe' => [1]], ['stunting_moderate' => [1]])['stunting_severe'], '100%', 'moderate before, severe now, counts', $passed, $failed);
assert_same(incidence(['stunting_moderate' => [1]], ['stunting_normal' => [2]])['stunting_moderate'], '0%', 'a child not tested before does not count', $passed, $failed);
$tested_before = ['stunting_normal' => [1, 2, 2, 3, 4]];
assert_same(incidence(['stunting_moderate' => [1]], $tested_before)['stunting_moderate'], '25%', 'the denominator is the children tested before, each once', $passed, $failed);
assert_same(incidence(['acute_malnutrition_mam' => [1]], ['acute_malnutrition_normal' => [1, 2]])['acute_malnutrition_mam'], '50%', 'MUAC: normal before, MAM now, counts', $passed, $failed);

// The months each period column takes.
assert_same(hedley_reports_resolve_quarter_date_info(strtotime('2027-01-15')), [2026, 4], 'in January, the previous quarter is the last of the year before', $passed, $failed);
assert_same(array_map('intval', hedley_reports_resolve_quarter_date_info(strtotime('2026-05-31'))), [2026, 1], 'on 31 May, the previous quarter is the first', $passed, $failed);

$expected_months = [];
for ($months_ago = 1; $months_ago <= 12; $months_ago++) {
  $expected_months[] = substr(month_day($months_ago, 1), 0, 7);
}
$tables = generate([]);
assert_same($tables[0]['period'], $expected_months, 'the monthly columns are the 12 months before this one', $passed, $failed);

// The previous set of a month is the three months before it.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => [
      'chw' => [
        month_day(4, 10) . ' -1,,,,',
        stunted_visit(month_day(1, 10)),
      ],
    ],
  ],
  [
    'id' => 2,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => ['chw' => [month_day(5, 10) . ' -1,,,,']],
  ],
]);
assert_same(monthly_cell($tables, 'incidence-month-1', 'stunting_moderate', 1), '100%', 'a visit 3 months before the column is in its previous set, one 4 months before is not', $passed, $failed);

/**
 * The quarter before the given (year, quarter), as [year, quarter].
 */
function quarter_before(array $year_quarter) {
  [$year, $quarter] = $year_quarter;
  return $quarter == 1 ? [$year - 1, 4] : [$year, $quarter - 1];
}

/**
 * The first day of a quarter, as Y-m-d.
 */
function quarter_start(array $year_quarter) {
  [$year, $quarter] = $year_quarter;
  return sprintf('%d-%02d-10', $year, 3 * ($quarter - 1) + 1);
}

// The first quarter column holds the last complete quarter, and is
// captioned with it.
$last_quarter = quarter_before([(int) date('Y'), (int) ceil(date('n') / 3)]);
$quarter_before_last = quarter_before($last_quarter);
$tables = generate([
  [
    'id' => 1,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => [
      'chw' => [
        quarter_start($quarter_before_last) . ' -1,,,,',
        stunted_visit(quarter_start($last_quarter)),
      ],
    ],
  ],
]);
foreach ($tables as $table) {
  if ($table['type'] === 'incidence-quarter-1') {
    assert_same($table['period'][0], implode('-', $last_quarter), 'the first quarter column is captioned with the last complete quarter', $passed, $failed);
    assert_same($table['stunting_moderate'][0], '100%', 'the first quarter column holds the last complete quarter', $passed, $failed);
  }
}

// A visit on 31 December before the starting year is dropped, one on
// 1 January is kept. Either one is the previous year of a moderate visit
// two years ago.
$this_year = (int) date('Y');
$cases = [
  '12-31' => [$this_year - 4, '0%'],
  '01-01' => [$this_year - 3, '100%'],
];
foreach ($cases as $day => [$year, $expected]) {
  $tables = generate([
    [
      'id' => 1,
      'birth_date' => ($this_year - 5) . '-06-01',
      'group_nutrition' => ['chw' => ["$year-$day -1,,,,", stunted_visit(($this_year - 2) . '-06-01')]],
    ],
  ]);
  foreach ($tables as $table) {
    if ($table['type'] === 'incidence-year-1') {
      $column = array_search((string) ($this_year - 2), $table['period']);
      assert_same($table['stunting_moderate'][$column], $expected, "a normal visit on $year-$day as the previous year", $passed, $failed);
    }
  }
}

// Well-child and individual nutrition visits count only before six.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => six_years_before(month_day(6, 15)),
    'individual' => [
      'well-child' => [
        [
          month_day(9, 10) . '|-2.5,,,,|',
          month_day(3, 10) . '|-2.5,,,,|',
        ],
      ],
      'nutrition' => [
        [
          stunted_visit(month_day(8, 10)),
          stunted_visit(month_day(2, 10)),
        ],
      ],
    ],
  ],
]);
assert_same(
  [
    monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 9),
    monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 8),
    monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 3),
    monthly_cell($tables, 'prevalence-1', 'stunting_moderate', 2),
  ],
  ['100%', '100%', '0%', '0%'],
  'well-child and individual nutrition visits count only before six',
  $passed,
  $failed
);

// One group visit and one family-nutrition MUAC visit make a child impacted.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => month_day(30, 1),
    'group_nutrition' => ['chw' => [stunted_visit(month_day(8, 10))]],
    'individual' => ['family-nutrition-muac' => [[month_day(4, 10) . ' 13']]],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-2', 'stunting_moderate', 8), '100%', 'one group and one family-nutrition MUAC visit make a child impacted', $passed, $failed);

// A mother with only family-nutrition visits adds nothing.
$tables = generate([
  [
    'id' => 1,
    'birth_date' => '1995-03-01',
    'individual' => [
      'family-nutrition' => [[month_day(4, 10), month_day(3, 10)]],
    ],
  ],
]);
assert_same(monthly_cell($tables, 'prevalence-1', 'acute_malnutrition_mam', 4), '0%', 'a mother with only family-nutrition visits adds nothing', $passed, $failed);

$total = $passed + $failed;
echo "\n";
echo "Nutrition report tests: $total total, $passed passed, $failed failed.\n";

if ($failed > 0) {
  echo "FAILED\n";
  exit(1);
}

echo "OK\n";
exit(0);
