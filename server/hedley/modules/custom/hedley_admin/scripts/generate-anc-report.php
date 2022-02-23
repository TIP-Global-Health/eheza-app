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
if (!$limit_date) {
  drush_print('Please specify --limit_date option');
  exit;
}

drush_print("# ANC report - " . $limit_date);

$queries = [
  // As the group of all pregnancies.
  "# Patients who have had " => file_get_contents(__DIR__ . '/anc-report-all-pregnancies.SQL'),
  // As the group of pregnancies which is current - defined as NOT > 30 days
  // past the EDD and AND NOT "closed" (if today is November 1, my pregnancy
  // is current if it is Nov 15 of the same year and the pregnancy
  // has not been closed). The closed-ness of the pregnancy was introduced
  // recently, so this filter is not added there.
  // @todo later, after 1-2 years, it might make sense to add closed filter.
  "Of Active Pregnancies (within 30 days of EDD AND not completed)" => file_get_contents(__DIR__ . '/anc-report-active-pregnancies.SQL'),
  // As a group of pregnancies that is completed - defined as > 30 past
  // the EDD OR "closed".
  // @todo introduce "closed" when meaningful
  "Of Completed Pregnancies (30 days beyond EDD)" => file_get_contents(__DIR__ . '/anc-report-completed-pregnancies.SQL'),
];

// For the upper end, we group the grouped counters all together, so let's say
// 1 visit => 6
// 5+ visits => 15.
$group_limit = 5;
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
