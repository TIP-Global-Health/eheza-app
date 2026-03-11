<?php

/**
 * @file
 * ANC recurring data report.
 *
 * This report shows the number of visits a woman has had in each pregnancy
 * in three ways
 * Pregnancy visit: ANC for humans, `prenatal` in Drupal.
 */

require_once __DIR__ . '/report_common.inc';

$limit_date = drush_get_option('limit_date', FALSE);
$region = drush_get_option('region', FALSE);

if (!$limit_date) {
  drush_print('Please specify --limit_date option');
  exit;
}

drush_print("# ANC report - " . $limit_date);

$queries = [
  // As the group of all pregnancies.
  "All Preganancies - Any location" => file_get_contents(__DIR__ . '/anc-report-all-pregnancies.SQL'),
  // As the group of pregnancies which is current - defined as NOT > 30 days
  // past the EDD and AND NOT "closed" (if today is November 1, my pregnancy
  // is current if it is Nov 15 of the same year and the pregnancy
  // has not been closed). The closed-ness of the pregnancy was introduced
  // recently, so this filter is not added there.
  // @todo later, after 1-2 years, it might make sense to add closed filter.
  "Active Pregnancies - Any Location" => file_get_contents(__DIR__ . '/anc-report-active-pregnancies.SQL'),
  // As a group of pregnancies that is completed - defined as > 30 past
  // the EDD OR "closed".
  // @todo introduce "closed" when meaningful
  "Completed Pregnancies - Any Location" => file_get_contents(__DIR__ . '/anc-report-completed-pregnancies.SQL'),
  // Health center encounters (nurse) only.
  "All Preganancies - Health Center" => file_get_contents(__DIR__ . '/anc-report-all-pregnancies-nurse.SQL'),
  "Active Pregnancies - Health Center" => file_get_contents(__DIR__ . '/anc-report-active-pregnancies-nurse.SQL'),
  "Completed Pregnancies - Health Center" => file_get_contents(__DIR__ . '/anc-report-completed-pregnancies-nurse.SQL'),
  // CHW encounters only.
  "All Preganancies - CHW" => file_get_contents(__DIR__ . '/anc-report-all-pregnancies-chw.SQL'),
  "Active Pregnancies - CHW" => file_get_contents(__DIR__ . '/anc-report-active-pregnancies-chw.SQL'),
  "Completed Pregnancies - CHW" => file_get_contents(__DIR__ . '/anc-report-completed-pregnancies-chw.SQL'),
];

// For the upper end, we group the grouped counters all together, so let's say
// 1 visit => 6
// 5+ visits => 15.
$group_limit = 5;
$region_clause = ($region) ? "AND field_district_value LIKE '%$region%'" : "";

foreach ($queries as $label => $query) {
  $table = new HedleyAdminTextTable([$label, 'Counter']);
  $data = [];
  $results = db_query($query, [':limit' => $limit_date])->fetchAll(PDO::FETCH_ASSOC);
  $sum_group_limit_or_above = 0;
  foreach ($results as $result) {
    if ($result['val'] < $group_limit) {
      $data[] = [
        $result['val'] . ' visits',
        $result['counter'],
      ];
    }
    else {
      $sum_group_limit_or_above += $result['counter'];
    }
  }
  if (!empty($sum_group_limit_or_above)) {
    $data[] = [
      $group_limit . '+ visits',
      $sum_group_limit_or_above,
    ];
  }
  drush_print($table->render($data));
}

$queries = [
  // As the group of all pregnancies.
  "First Visits - Any location" => file_get_contents(__DIR__ . '/anc-first-visit-all-pregnancies.SQL'),
];

foreach ($queries as $label => $query) {
  $data = [];
  $results = db_query($query, [':limit' => $limit_date])->fetchAll(PDO::FETCH_ASSOC);
  foreach ($results as $result) {
    $total_first_visits = $result['first_trimester'] + $result['second_trimester'] + $result['third_trimester'];
    $first_trimester_percentage = ($result['first_trimester'] != 0) ? $result['first_trimester'] / $total_first_visits : '0';
    $second_trimester_percentage = ($result['second_trimester'] != 0) ? $result['second_trimester'] / $total_first_visits : '0';
    $third_trimester_percentage = ($result['third_trimester'] != 0) ? $result['third_trimester'] / $total_first_visits : '0';

    $data = [
      [
        'First Trimester',
        $result['first_trimester'],
        $first_trimester_percentage,
      ],
      [
        'Second Trimester',
        $result['second_trimester'],
        $second_trimester_percentage,
      ],
      [
        'Third Trimester',
        $result['third_trimester'],
        $third_trimester_percentage,
      ],
    ];
  }

  $text_table = new HedleyAdminTextTable(['Trimester', 'Count', 'Percentage']);
  $text_table->addData($data);

  drush_print('## ' . $label);
  drush_print($text_table->render());
}
