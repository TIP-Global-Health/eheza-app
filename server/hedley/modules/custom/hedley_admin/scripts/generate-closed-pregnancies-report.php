<?php

/**
 * @file
 * Closed pregnancies recurring report.
 */

require_once __DIR__ . '/report_common.inc';

$limit_date = drush_get_option('limit_date', FALSE);
if (!$limit_date) {
  drush_print('Please specify --limit_date option');
  exit;
}

drush_print("# Closed pregnancies report  - " . $limit_date);

$queries = [
  "Outcomes of completed pregnancies (30 days beyond EDD)" => file_get_contents(__DIR__ . '/closed-pregnancies-outcome-completed-pregnancies.SQL'),
  "Outcome Location" => file_get_contents(__DIR__ . '/closed-pregnancies-outcome-location.SQL')
];

foreach ($queries as $label => $query) {
  $table = new HedleyAdminTextTable([$label, 'Counter']);
  $results = db_query($query, [':limit' => $limit_date])->fetchAll(PDO::FETCH_ASSOC);
  $data = [];
  foreach ($results as $result) {
    $data[] = [
      $result['type'],
      $result['counter'],
    ];
  }
  drush_print($table->render($data));
}
