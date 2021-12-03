<?php

require_once __DIR__ . '/report_common.inc';

$queries = [
  "Outcomes of completed pregnancies (30 days beyond EDD)" =>   "
SELECT
  field_outcome_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome
WHERE field_outcome_value NOT IN ('referred-to-hc', 'illness-resolved')
GROUP BY
 field_outcome_value
",
  "Outcome Location" => "
SELECT
  field_outcome_location_value AS type, COUNT(*) AS counter
FROM
  field_data_field_outcome_location
GROUP BY
  field_outcome_location_value;",
];

foreach ($queries as $label => $query) {
  $table = new HedleyAdminTextTable([$label, 'Counter']);
  $results = db_query($query)->fetchAll(PDO::FETCH_ASSOC);
  $data = [];
  foreach ($results as $result) {
    $data[] = [
      $result['type'],
      $result['counter'],
    ];
  }
  drush_print($table->render($data));
}